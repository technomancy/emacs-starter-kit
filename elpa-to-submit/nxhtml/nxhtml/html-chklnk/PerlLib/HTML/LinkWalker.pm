### File:	LinkWalker.pm
### Author:	Lennart Borgman
###	All rights reserved

##########################################################
### UserAgent module
##########################################################
package LWP::WalkerUA;
require LWP::UserAgent;
@ISA = qw(LWP::UserAgent);

### Mirror to another file (why???)
sub mirror
{
    my($self, $url, $file, $mirr_tmp) = @_;
    die "no mirr_tmp" unless defined $mirr_tmp;

    LWP::Debug::trace('()');
    my $request = new HTTP::Request('GET', $url);

    if (-e $file) {
	my($mtime) = (stat($file))[9];
	if($mtime) {
	    $request->header('If-Modified-Since' =>
			     HTTP::Date::time2str($mtime));
	}
    }
    my $tmpfile = "$file-$$";

    my $response = $self->request($request, $tmpfile);
    if ($response->is_success) {

	my $file_length = (stat($tmpfile))[7];
	my($content_length) = $response->header('Content-length');

	if (defined $content_length and $file_length < $content_length) {
	    unlink($tmpfile);
	    die "Transfer truncated: " .
		"only $file_length out of $content_length bytes received\n";
	} elsif (defined $content_length and $file_length > $content_length) {
	    unlink($tmpfile);
	    die "Content-length mismatch: " .
		"expected $content_length bytes, got $file_length\n";
	} else {
	    # OK
	    if (-e $mirr_tmp) {
		# Some dosish systems fail to rename if the target exists
		chmod 0777, $mirr_tmp;
		unlink $mirr_tmp;
	    }
	    rename($tmpfile, $mirr_tmp) or
		die "Cannot rename '$tmpfile' to '$mirr_tmp': $!\n";

	    if (my $lm = $response->last_modified) {
		# make sure the file has the same last modification time
		utime $lm, $lm, $mirr_tmp;
	    }
	}
    } else {
	unlink($tmpfile);
    }
    return $response;
}


##########################################################
### Parser module
##########################################################
package HTML::WalkerParser;
require HTML::ParserTagEnd;
@ISA = qw(HTML::ParserTagEnd);
use strict;
use vars qw(%LINK_ELEMENT);

# Elements that might contain links and the name of the link attribute
%LINK_ELEMENT =
(
 body   => 'background',
 base   => 'href',
 a      => 'href',
 img    => [qw(src lowsrc usemap)],   # 'lowsrc' is a Netscape invention
 form   => 'action',
 input  => 'src',
'link'  => 'href',          # need quoting since link is a perl builtin
 frame  => 'src',
 applet => [qw(codebase code)],
 area   => 'href',
 iframe  => 'src',   # Netscape 2.0 extention
 embed  => 'src',   # used in Netscape 2.0 for Shockwave and things like that
);

my %LINKATTRIBS = (
	"href" 			=> 1,
	"src"			=> 1,
	"action"		=> 1,
	"background" 	=> 1,
	"usemap"		=> 1,
	"code"			=> 1,
	"codebase"		=> 1,
	"lowsrc"		=> 1,
	);
my %MAYBECONT = (
	a => 'href',
	area   => 'href',
	form   => 'action',
	frame  => 'src',
	iframe  => 'src',
	);

sub maybecont($$) {
	my $tag = shift;
	my $att = shift;
	return unless exists $MAYBECONT{$tag};
	return ($MAYBECONT{$tag} eq $att);
}

sub new {
    my($class, $parsed_fh) = @_;
    my $self = $class->SUPER::new;
    $self->{parsed_fh} = $parsed_fh;
    $self;
}







##########################################################
### Walker module
##########################################################
package HTML::LinkWalker;
use strict;

use IO::File;
use File::Copy qw();
use File::Path qw();
use PathSubs qw();
use HTML::Entities;
use FindBin qw();


##########################################################
### Globals
##########################################################
my $ua;
my $m_ua_personality = "LinkWalker/0.9";
my %m_is_outside;
my %m_is_container;
my $m_bOnlyCont;
my @m_sLinkRoots;
my $m_subReport;
my $m_subAction;
my $m_subMirrorAction;


#############################
### Collecting info
#############################
my %m_CheckedLinks;
my %m_MissedLinks;

sub tell_bad_link($$$$$) {
	my $what = shift;
	my $file = shift;
	my $lnum = shift;
	my $link = shift;
	my $line = shift;
	$file = "START" unless defined $file;
	$lnum = "(start)" unless defined $lnum;
	my $longMsg = "<<$what>>";
	my $shortMsg = $what;
	if (defined $link) {
		my @lines = split("\\s+", $line);
		my $disp_line = join("\n\t\t  ", @lines);
		$longMsg .= ",\n\t\tlink=$link\n\t\t$disp_line";
	}
	my @msg = ($shortMsg, $longMsg);
	$m_CheckedLinks{$file}->{ERR}->{$lnum} = \@msg;
	&$m_subReport("\t* Error * " . $what . "\n");
} # tell_bad_link


#############################
### Helpers
#############################

sub get_contenttype($) {
	my $response = shift;
	my @rh = $response->header("Content-Type");
	for my $r (@rh) {
		my $c = $r;
		if ((my $iPos = index($r, ";")) > -1) {
			$c = substr($r, 0, $iPos);
		}
		return $c;
	}
}
sub is_linked_contenttype($) {
	my $response = shift;
	return (get_contenttype($response) eq "text/html");
}

sub ending_is_container($) {
	my $link_addr = shift;
	$link_addr =~ s!#.*$!!;
	$link_addr =~ s!\?.*$!!;
	return (($link_addr =~ m!\.s?html?$!i) ? 1 : 0);
}

my $m_sMirrorRoot;
my $m_bMirror = 1;

sub mk_mirror_name($) {
	my $orig_name = shift;
	$orig_name =~ tr!\\!/!;
	my $mirr_name = $orig_name;
	my ($orig_host) = ($orig_name =~ m!(^https?://[^/]*)!i);
	if (defined $orig_host) {
		my $host = $orig_host;
		$host =~ tr!:!_!;
		$host =~ tr!/!_!;
		$mirr_name =~ s!^$orig_host!$host!;
		if (substr($mirr_name, -1) eq "/") { $mirr_name .= "default.html"; }
	} else {
		die "Can't find host in $orig_name\n";
	}
	my $mirr_full = sMirrorRoot() . $mirr_name;
	if (!$m_bMirror) {
		my $sExt = $mirr_name; $sExt =~ s!.*\.([^\.]*$)!$1!;
		$mirr_full = sMirrorRoot() . "temp.$sExt";
	}
	my $mirr_fold = $mirr_full;
	$mirr_fold =~ s![^/]*$!!;
	File::Path::mkpath($mirr_fold, 0, 0777);
	return $mirr_full;
}

#############################
### Checks
#############################
sub is_outside($) {
	my $uq_link_addr = shift;
	if (!exists $m_is_outside{$uq_link_addr}) {
		$m_is_outside{$uq_link_addr} = test_is_outside($uq_link_addr, \@m_sLinkRoots);
	}
	return $m_is_outside{$uq_link_addr};
}
sub set_is_container($$) {
	my $uq_link_addr = shift;
	return if exists $m_is_container{$uq_link_addr};
	$m_is_container{$uq_link_addr} = shift;
}
sub is_outside_container($) {
	my $uq_link_addr = shift;
	if (exists $m_is_container{$uq_link_addr}) {
		if ($m_is_container{$uq_link_addr}) {
			return is_outside($uq_link_addr);
		}
	}
}
sub test_is_outside($$) {
	my $uq_link_addr = shift;
	my $link_roots   = shift;
	if (defined $link_roots) {
		my $in_roots;
		for my $link_root (@$link_roots) {
			if (substr($uq_link_addr, 0, length($link_root)) eq $link_root) {
				return 0;
			}
		}
		return 1;
	}
} # is_outside



##########################################################
### Parsing
##########################################################


### Parser subs
sub HTML::WalkerParser::declaration {
    my($self, $decl) = @_;
	return unless defined $self->{parsed_fh};
	my $fh = $self->{parsed_fh};
	print $fh "<!" . $decl . ">";
}
my $m_start_cb;
sub HTML::WalkerParser::start {
    my($self, $tag, $attr, $ended) = @_;
	&$m_start_cb($tag, $attr);
	return unless defined $self->{parsed_fh};
	my $t = "<$tag";
	for my $k (keys %$attr) {
		my $encoded = encode_entities($$attr{$k});
		$t .= qq( $k="$encoded");
	}
	if ($ended) {
		$t .= " />";
	} else {
		$t .= ">";
	}
	my $fh = $self->{parsed_fh};
	print $fh $t;
}
sub HTML::WalkerParser::end {
	my ($self, $tag) = @_;
	return unless defined $self->{parsed_fh};
	my $fh = $self->{parsed_fh};
	print $fh "</" . $tag . ">";
}
sub HTML::WalkerParser::text {
	my ($self, $txt) = @_;
	return unless defined $self->{parsed_fh};
	my $fh = $self->{parsed_fh};
	print $fh $txt;
}
sub HTML::WalkerParser::comment {
    my($self, $comment) = @_;
	return unless defined $self->{parsed_fh};
	my $fh = $self->{parsed_fh};
	print $fh "<!--" . $comment . "-->";
}




### Main parsing routine

sub parse_file($$$$$$$$$) {
	my ($file_name, $parsed_fh, $uq_link_addr, $link_roots,
		$ref_links, $ref_anchs, $ref_lines, $ref_tagname, $ref_attname) = @_;
	my $fh;
	if (-d $file_name) {
		$file_name = PathSubs::uniq_dir($file_name) . "default.html";
		$uq_link_addr .= "/" unless substr($uq_link_addr, -1) eq "/";
		$uq_link_addr .= "default.html";
		&$m_subReport("dir => $file_name\n");
	}
	$fh = new IO::File($file_name);
	die "Can't read $file_name: $!\n" unless defined $fh;
	my $base_href;
	my $n;
	my $line;
	my $uq_link_fold = $uq_link_addr; $uq_link_fold =~ s![^/]*$!!;

	my $start_cb =
		sub {
			my ($tag, $attr_hash) = @_;
			for my $k (keys %$attr_hash) {
				if (($k eq "id") || ($k eq "name")) {
					my $v = $$attr_hash{$k};
					$$ref_anchs{$v} = $n;
					$$ref_lines{$n} = $line;
				} elsif (exists $LINKATTRIBS{$k}) {
					my $v = $$attr_hash{$k};
					next if $v =~ m!^javascript:!;
					next if $v =~ m!^ftp://!;
					next if $v =~ m!^mailto://!;
					if ($tag eq "base") { $base_href = $v if $k eq "href"; next; }
					my $v_abs; my $v_rel;
					my $v_is_abs = PathSubs::is_abs_path($v);
					if ($v_is_abs) {
						$v_abs = $v;
						$v_rel = PathSubs::mk_relative_link($uq_link_addr, $v_abs);
					} else {
						$v_rel = $v;
						if (defined $base_href) {
							$v_abs = PathSubs::mk_abs_link($base_href, $v);
						} else {
							if (substr($v_rel, 0, 1) ne "#") {
								$v_abs = $uq_link_fold . $v_rel;
							} else {
								$v_abs = $uq_link_addr . $v_rel;
							}
							$v_abs = PathSubs::resolve_dotdot($v_abs);
						}
					}
					next if exists $m_CheckedLinks{$v_abs};
					if (is_outside($v_abs)) {
						if (!$v_is_abs) {
							if (ending_is_container($v_abs)) {
								$m_CheckedLinks{$v_abs} = {};
								tell_bad_link("Outside relative link ($v_rel)",
									$uq_link_addr, $n, $v, $line);
							}
						}
						### Skip outside absolute links
						### Could be things like banners etc...
						next;
					}
					$$ref_links{$v_rel} = $n;
					$$ref_lines{$n} = $line;
					if (substr($v_rel, 0, 1) ne "#") {
						my $v_rel_name = $v_rel;
						$v_rel_name =~ s!#.*$!!;
						$v_rel_name =~ s!\?.*$!!;
						$$ref_tagname{$v_rel_name} = $tag;
						$$ref_attname{$v_rel_name} = $k;
					}
					if ($v_is_abs && ($v_rel ne $v)) { $$attr_hash{$k} = $v_rel; }
				}
			}
		}; # $start_cb

	$m_start_cb = $start_cb;
	my $p = HTML::WalkerParser->new($parsed_fh);
	while ($line = <$fh>) {
		$n++;
		$p->parse($line);
	}
	$fh->close();
} # parse_file



##########################################################
### Do the walk...
##########################################################
sub walk_link($$;$$$$) {
	die "$#_" unless ($#_ == 1 || $#_ == 5);
	my $link_fold   = shift;
	my $link_file   = shift;
	my $parent_url  = shift;
	my $parent_lnum = shift;
	my $parent_link = shift;
	my $parent_line = shift;

	my $link_addr   = $link_fold . $link_file;
	my $uq_link_addr;
	my $is_file = ($link_addr !~ m!^https?://!i);
	if ($is_file) {
		$uq_link_addr = PathSubs::uniq_file($link_addr);
	} else {
		$uq_link_addr = PathSubs::resolve_dotdot($link_addr);
	}
	return if exists $m_CheckedLinks{$uq_link_addr};
	return if exists  $m_MissedLinks{$uq_link_addr};
	$m_CheckedLinks{$uq_link_addr} = {};
	my $link_is_container = ending_is_container($uq_link_addr);
	if ($link_is_container) {
		set_is_container($uq_link_addr, 1);
		return if is_outside($uq_link_addr);
	} else {
		return if $m_bOnlyCont;
	}
	my $response;
	my $contenttype;
	my $bDoRewrite;
	my $file_name;
	if ($is_file) {
		if (!-r $uq_link_addr) {
			tell_bad_link("Can't read file ($uq_link_addr)",
				$parent_url, $parent_lnum, $parent_link, $parent_line);
			$m_MissedLinks{$uq_link_addr} = 1;
			return;
		}
		$file_name = $uq_link_addr;
	} else {
		$file_name = mk_mirror_name($uq_link_addr);
		if (!defined $ua) {
			$ua = new LWP::UserAgent;
			$ua->agent($m_ua_personality);
			#$ua->delay(0.1);
		}
		if ($m_bMirror) {
			$response = $ua->mirror($uq_link_addr, $file_name);
			&$m_subMirrorAction($uq_link_addr, $file_name, $response);
		} else {
			my $request = new HTTP::Request('GET', $uq_link_addr);
			$response = $ua->request($request, $file_name);
		}
		#dump_response($response); exit;
		if ($response->code != 304) {
			if (!$response->is_success) {
				tell_bad_link($response->status_line . " ($uq_link_addr)",
					$parent_url, $parent_lnum, $parent_link, $parent_line);
				$m_MissedLinks{$uq_link_addr} = 1;
				return;
			}
			$bDoRewrite = $m_bMirror;
			$contenttype = get_contenttype($response);
			$link_is_container = is_linked_contenttype($response);
		}
		if ($uq_link_addr ne $response->base) {
			if ($m_bMirror) {
				my $base_file = mk_mirror_name($response->base);
				if (!File::Copy::copy($file_name, $base_file)) {
					die "Can't copy($file_name, $base_file): $!\n";
				}
				if (my $lm = $response->last_modified) { utime $lm, $lm, $base_file; }
				$file_name = $base_file;
			}
			$uq_link_addr = $response->base;
		}
	}
	### Test again, could be new info from net!
	if ($link_is_container) {
		set_is_container($uq_link_addr, 1);
		return if is_outside($uq_link_addr);
	} else {
		return if $m_bOnlyCont;
		return;
	}
	&$m_subReport("$uq_link_addr ...");

	my %links;
	my %anchs;
	my %lines;
	my %tagname;
	my %attname;
	my $parsed_fh;
	my $parsed_file;
	my $file_to_parse = $file_name;
	if ($bDoRewrite) {
		$parsed_file = $file_to_parse . "-p$$";
		&$m_subReport(" <<GET");
		$parsed_fh = new IO::File("> $parsed_file");
		die "Can't create $parsed_file: $!\n" unless defined $parsed_fh;
		print $parsed_fh "<!-- parsed version -->\n";
	}
	&$m_subReport("\n");
	parse_file($file_to_parse, $parsed_fh, $uq_link_addr,
		\@m_sLinkRoots,
		\%links, \%anchs, \%lines, \%tagname, \%attname);
	if (defined $parsed_fh) {
		$parsed_fh->close();
		if (-e $file_name) { unlink $file_name or die "Can't unlink $file_name: $!"; }
		rename($parsed_file, $file_name) or die "Can't rename($parsed_file, $file_name): $!\n";
	    if (my $lm = $response->last_modified) { utime $lm, $lm, $file_name; }
	}
	### Now we know...
	if ($link_is_container) { return if is_outside($uq_link_addr); }

	$m_CheckedLinks{$uq_link_addr}->{ANC} = \%anchs;
	my $file_dir;
	if ($is_file) {
		$file_dir = $uq_link_addr;
		$file_dir =~ s![^/]*$!!;
		#chdir $file_dir;
	}
	my $container_folder = $uq_link_addr; $container_folder =~ s![^/]*$!!;
	&$m_subAction($uq_link_addr, $file_name, $contenttype);
	for my $link (sort keys %links) {
		# Next line is for onclick lines in prepared docs
		next if ($link eq "#");
		my $lnum = $links{$link};
		my $line = $lines{$lnum};
		if ($link eq "") {
			tell_bad_link("Empty link", $uq_link_addr, $lnum, $link, $line);
			next;
		}
		if ($link =~ m!(.*)\?!) { $link = $1; }
		my $anchor;
		if ($link =~ m!(.*)#(.*)!) { $link = $1; $anchor = $2; }
		if ($link eq "") {
			if (!exists $anchs{$anchor}) {
				tell_bad_link("Anchor not found ($anchor)", $uq_link_addr, $lnum, $link, $line);
			}
			next;
		}
		my $sub_fold;
		my $sub_file;
		my $uq_sublink; 
		if ($link =~ m!^https?://!i) {
			$sub_fold = "";
			$sub_file = $link;
			$uq_sublink = $link;
		} else {
			$sub_file = $link;
			if ($is_file) {
				$sub_fold = $file_dir;
				$uq_sublink = PathSubs::uniq_file($sub_fold . $sub_file);
			} else {
				$sub_fold = $container_folder;
				$uq_sublink = $sub_fold . $sub_file;
			}
		}
		next if (exists $m_CheckedLinks{$uq_sublink});
		if (defined $anchor) {
			$m_CheckedLinks{$uq_link_addr}->{EXTANC}->{$uq_sublink} =
					{ ANC=> $anchor, LINE=>$line, LNUM=>$lnum};
		}
		if ($m_bOnlyCont) {
			die "link=$link\tattr=$tagname{$link}\n" unless exists $tagname{$link};
			next unless maybecont($tagname{$link}, $attname{$link});
		}
		if (is_outside($uq_link_addr)) {
			if (maybecont($tagname{$link}, $attname{$link}) ) {
				next;
			}
		}
		walk_link($sub_fold, $sub_file, $uq_link_addr, $lnum, $link, $line);
	}
} # walk_link




############################################
### Some more checks!
############################################
sub check_external_anchors() {
	&$m_subReport("\nChecking external anchors...\n");
	for my $f (sort keys %m_CheckedLinks) {
		my $fnode = $m_CheckedLinks{$f};
		if (exists ${$fnode}{"EXTANC"}) {
			my $extanc_hash = ${$fnode}{"EXTANC"};
			for my $fx (keys %$extanc_hash) {
				next unless (exists $m_CheckedLinks{$fx});
				my $ea_hash = ${$extanc_hash}{$fx};
				my $ea = ${$ea_hash}{ANC};
				my $fxnode = $m_CheckedLinks{$fx};
				my $fx_anc_hash = ${$fxnode}{"ANC"};
				if (!exists ${$fx_anc_hash}{$ea}) {
					my $line = ${$ea_hash}{LINE};
					my $lnum = ${$ea_hash}{LNUM};
					&$m_subReport("From $f\n");
					tell_bad_link("Ext anchor not found ($fx#$ea)",
						$f, $lnum, "$fx#$ea", $line);
				}
			}
		}
	}
} # check_external_anchors



#############################
### Reporting
#############################
sub report_errors($$) {
	my $bSum = shift;
	my $bDet = shift;
	my $errors_reported;
	my $errors_found;
	for my $f (sort keys %m_CheckedLinks) {
		my $fnode = $m_CheckedLinks{$f};
		if (exists ${$fnode}{ERR}) {
			$errors_found = 1;
			last unless $bSum;
			if (!defined $errors_reported) {
				$errors_reported = 1;
				&$m_subReport("\n\n*********** Summary ERRORS and WARNINGS **********\n");
			}
			&$m_subReport("$f\n");
			my $err_hash = ${$fnode}{ERR};
			for my $e (sort keys %$err_hash) {
				my $refE = ${$err_hash}{$e};
				&$m_subReport("\t" . ${$refE}[0] . "\n");
			}
		}
	}
	undef $errors_reported;
	if ($bDet) {
		for my $f (sort keys %m_CheckedLinks) {
			my $fnode = $m_CheckedLinks{$f};
			if (exists ${$fnode}{ERR}) {
				if (!defined $errors_reported) {
					$errors_reported = 1;
					&$m_subReport("\n\n*********** Detailed ERRORS and WARNINGS **********\n");
				}
				&$m_subReport("$f\n");
				my $err_hash = ${$fnode}{ERR};
				for my $e (sort keys %$err_hash) {
					my $refE = ${$err_hash}{$e};
					&$m_subReport("\tat line $e: " .  ${$refE}[1] . "\n");
				}
			}
		}
	}
	if ($errors_found) {
		die "\n*** There where errors ***\n";
	} else {
		&$m_subReport("No errors found\n");
	}
} # report_errors

sub dump_response($) {
	my $response = shift;
		&$m_subReport( $response->code . " " . $response->message . "\n");
		&$m_subReport( "****************************************\n");
		#&$m_subReport( $response->request . "\n");
		#&$m_subReport( "****************************************\n");
		#&$m_subReport( $response->previous . "\n");
		#&$m_subReport( "****************************************\n");
		&$m_subReport( 	"  i=" . $response->is_info .
				", s=" . $response->is_success .
				", r=" . $response->is_redirect .
				", e=" . $response->is_error . "\n");
		&$m_subReport( "****************************************\n");
		&$m_subReport( "content: " . $response->content . "\n");
		&$m_subReport( "****************************************\n");
		&$m_subReport( "base: " . $response->base . "\n");
		&$m_subReport( "****************************************\n");
		&$m_subReport( $response->as_string);
		&$m_subReport( "****************************************\n");
		&$m_subReport( $response->current_age . "\n");
		&$m_subReport( "****************************************\n");
		my @rh = $response->header("Content-Type");
		for my $r (@rh) { &$m_subReport( "ct: $r\n"); }
		&$m_subReport( "****************************************\n");
} # dump_response


#############################
### Parameters
#############################
sub sMirrorRoot() {
	my $val = shift;
	$m_sMirrorRoot = PathSubs::get_temp_path() . "LinkWalker/" unless defined $m_sMirrorRoot;
	my $old = $m_sMirrorRoot;
	$m_sMirrorRoot = PathSubs::uniq_dir($val) if defined $val;
	return $old;
}
sub bMirror(;$) {
	my $val = shift;
	my $old = $m_bMirror;
	$m_bMirror = $val if defined $val;
	$old;
}

sub subReporter(;$) {
	my $val = shift;
	my $old = $m_subReport;
	$m_subReport = $val if defined $val;
	$old
}
sub subAction(;$) {
	my $val = shift;
	my $old = $m_subAction;
	$m_subAction = $val if defined $val;
	$old
}
sub bOnlyCont(;$) {
	my $val = shift;
	my $old = $m_bOnlyCont;
	$m_bOnlyCont = $val if defined $val;
	$old
}
sub ua_personality(;$) {
	my $val = shift;
	my $old = $m_ua_personality;
	$m_ua_personality = $val if defined $val;
	$old
}

sub clear_roots() { @m_sLinkRoots = (); }
sub get_roots() { return \@m_sLinkRoots; }
sub add_root($) { push @m_sLinkRoots, shift; }
sub add_files_root($) {
	my $file = shift;
	my $default_root;
	my ($host) = ($file =~ m!(^https?://[^/]*)!i);
	if (defined $host) {
		$default_root = $file;
	} else {
		die "Can't find $file\n" unless -e $file;
		$default_root = PathSubs::uniq_file($file);
	}
	$default_root =~ s![^/]*$!!;
	add_root($default_root);
}

### Default actions
sub default_sub {}
$m_subReport 		= \&default_sub;
$m_subAction 		= \&default_sub;
$m_subMirrorAction 	= \&default_sub;

1;
