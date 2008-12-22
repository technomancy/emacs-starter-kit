#! perl

# Copyright 2006, 2007 Lennart Borgman, http://OurComments.org/. All
# rights reserved.
#
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.

use strict;
use warnings;

use IO::File;
use File::Spec;
use File::Find;

sub check_file($);

#############################
### Collecting info
#############################
my $m_site_dir;   # Site root directory (every file should be in this)
my %m_CheckedFiles;
my %m_FilesToCheck;
my %m_MissedFiles;
my $m_errors_found;

sub tell_bad_link($$$$$) {
	my $what = shift;
	my $file = shift;
	my $lnum = shift;
	my $link = shift;
	my $line = shift;
        $line =~ s/^\s+|\s+$//g;
	$m_CheckedFiles{$file}->{"ERR"}->{$lnum} = "$what\n    Link: \"$link\"\n";
        #$line";
}

#############################
### Helpers
#############################
sub add_file_to_check($) {
    $m_FilesToCheck{File::Spec->canonpath(shift)} = 1;
}
# sub full_uq_file($) {
# 	my $file = shift;
# 	my $full_file = $file;
#         if (! File::Spec->file_name_is_absolute($full_file)) {
#             #$full_file = Win32::GetFullPathName($file);
#             $full_file = File::Spec->rel2abs($full_file, $m_site_dir);
#         }
#         if (($^O eq "MSWin32") || ($^O eq "cygwin")) {
#             $full_file =~ tr!A-Z!a-z!;
#         }
#         #print "ull_uq_file: full_file=$file\n";
# 	return $full_file;
# }

#############################
### Checks
#############################
sub check_next_file() {
    if (scalar(keys %m_FilesToCheck) > 0) {
        my @FilesToCheck = sort keys %m_FilesToCheck;
        my $next_file = $FilesToCheck[0];
        delete $m_FilesToCheck{$next_file};
        check_file($next_file);
    }
}
sub not_a_local_file($) {
    my $url = shift;
    (
     $url =~ m!^javascript:!
     ||
     $url =~ m!^mailto:!
     ||
     $url =~ m!^[a-z]+://!
    );
} 

sub check_file($) {
	my $fname = shift;
        if (! File::Spec->file_name_is_absolute($fname)) {
            die "check_file: File is not abs: $fname";
        }
        my $only_name = (File::Spec->splitpath($fname))[2];
	print "Checking $fname ... ";
        sleep 0.5;
	$m_CheckedFiles{$fname} = {};
	my %links;
	my %anchs;
	my %lines;
	my $fh = new IO::File($fname);
	die "Can't read $fname: $!\n" unless defined $fh;
	my $whole;
	my $n;
        my $found_errors = 0;
	while (my $line = <$fh>) {
		$n++;
		chomp $line;
		$whole = $line;
		while ($whole =~ m!(?:\s|^)id="(.*?)"!g) {
			$anchs{$1} = $n;
			$lines{$n} = $line;
		}
		while ($whole =~ m!(?:\s|^)name="(.*?)"!g) {
			$anchs{$1} = $n;
			$lines{$n} = $line;
		}
		while ($whole =~ m!(?:\s|^)href="(.*?)"!g) {
			my $l = $1;
			next if not_a_local_file($l);
                        if ($l =~ m!^#!) {
                            $l = $only_name . $l;
                        }
			$links{$l} = $n;
			$lines{$n} = $line;
		}
		while ($whole =~ m!(?:\s|^)src="(.*?)"!g) {
			my $l = $1; $l =~ tr!A-Z!a-z!;
			$links{$l} = $n;
			$lines{$n} = $line;
		}
	}
	$fh->close();
	$m_CheckedFiles{$fname}->{ANC} = \%anchs;
        my ($fv, $fd, $ff) = File::Spec->splitpath($fname);
        my $fdir = File::Spec->catpath($fv, $fd, "");
	for my $link (sort keys %links) {
		# Next line is for onclick lines
		next if ($link eq "#");
		my $lnum = $links{$link};
		my $line = $lines{$lnum};
		if ($link eq "") {
			tell_bad_link("empty link", $fname, $lnum, $link, $line);
                        $found_errors = 1;
			next;
		}
		if ($link =~ m!(.*)\?!) { $link = $1; }
		my $anchor;
		if ($link =~ m!(.*)#(.*)!) { $link = $1; $anchor = $2; }
		if ($link eq "") {
			if (!exists $anchs{$anchor}) {
				tell_bad_link("bad internal anchor ref ($anchor)", $fname, $lnum, $link, $line);
                                $found_errors = 1;
			}
			next;
		}
		$link =~ m!([^\.]*)$!;
		my $link_file_type = $1;
		my $subfile = $link;
                if (!File::Spec->file_name_is_absolute($subfile)) {
                    $subfile = File::Spec->catpath($fv, $fd, $link);
                }
                $subfile = File::Spec->canonpath($subfile);
                die "Contained .." if $subfile =~ m/\.\./;
		next if (exists $m_MissedFiles{$subfile});
		if (! -r $subfile) {
			tell_bad_link("Can't read linked file: $!", $fname, $lnum, $link, $line);
                        $found_errors = 1;
			$m_MissedFiles{$subfile} = 1;
			next;
		}
		next unless $link_file_type =~ m!^html?$!i;
		if (defined $anchor) {
			$m_CheckedFiles{$fname}->{EXTANC}->{$subfile} =
					{ ANC=> $anchor, LINE=>$line, LNUM=>$lnum};
		}
		next if (exists $m_CheckedFiles{$subfile});
		#check_file($subfile);
                my $rel_root = File::Spec->abs2rel($subfile, $m_site_dir);
                if (substr($rel_root, 0, 2) eq "..") {
                    tell_bad_link("Reference to file outside site", $fname, $lnum, $link, $line);
                    $found_errors = 1;
                } else {
                    #$m_FilesToCheck{$subfile} = 1;
                    add_file_to_check($subfile);
                }
	}
        if ($found_errors) {
            print "Errors found\n";
        } else {
            print "Ok\n";
        }
        sleep 0.5;
        check_next_file();
} # check_file


sub check_external_anchors() {
	for my $f (sort keys %m_CheckedFiles) {
		my $fnode = $m_CheckedFiles{$f};
		if (exists ${$fnode}{"EXTANC"}) {
			my $extanc_hash = ${$fnode}{"EXTANC"};
			for my $fx (keys %$extanc_hash) {
				next unless (exists $m_CheckedFiles{$fx});
				my $ea_hash = ${$extanc_hash}{$fx};
				my $ea = ${$ea_hash}{ANC};
				my $fxnode = $m_CheckedFiles{$fx};
				my $fx_anc_hash = ${$fxnode}{"ANC"};
				if (!exists ${$fx_anc_hash}{$ea}) {
					my $line = ${$ea_hash}{LINE};
					my $lnum = ${$ea_hash}{LNUM};
					tell_bad_link("Hash not found", $f, $lnum, "$fx#$ea", $line);
				}
			}
		}
	}
} # check_external_anchors



#############################
### Reporting
#############################
sub report_errors() {
	for my $f (sort keys %m_CheckedFiles) {
		my $fnode = $m_CheckedFiles{$f};
		if (exists ${$fnode}{"ERR"}) {
			if (!defined $m_errors_found) {
				$m_errors_found = 1;
				print "\n\n*********** Error details: **********\n";
                                sleep 0.5;
			}
			#print "\n$f";
			my $err_hash = ${$fnode}{"ERR"};
			for my $e (sort keys %$err_hash) {
                            print "\n$f";
                            print " at line $e:\n    " . ${$err_hash}{$e} . "\n";
                            sleep 0.5;
			}
		}
	}
	if ($m_errors_found) {
		die "\n*** There where errors ***\n";
	} else {
		print "Everything that was checked is ok\n";
	}
} # report_errors

#############################
### Help
#############################
sub usage() {
    die "Usage: $0 --site=SITE-DIR --start=START-FILE\n";
}

#############################
### Parameters
#############################
#my $m_start_file; # File to start checking in
sub get_params() {
    usage() unless $#ARGV > -1;
    for (my $i = 0; $i <= $#ARGV; $i++) {
        my ($k, $v) = ($ARGV[$i] =~ m!-?-?(.*?)=(.*)!);
        if ($k eq "site") {
            $m_site_dir = $v;
        } elsif( $k eq "start") {
            #$m_FilesToCheck{$v} = 1;
            add_file_to_check($v);
        } else {
            print STDERR "Unknown parameter: $ARGV[$i]\n";
            usage();
        }
    }
    foreach my $key (keys %m_FilesToCheck) {
        die "Can't find $key\n" unless -e $key;
    }
    if (! $m_site_dir) {
        print STDERR "No site directory given\n";
        usage();
    }
    die "Can't find $m_site_dir\n" unless -d $m_site_dir;
    if ((scalar keys %m_FilesToCheck) == 0) {
        my $add_files =
            sub {
                return unless m/.html?$/i;
                return if -d $_;
                #$m_FilesToCheck{$File::Find::name} = 1;
                add_file_to_check($File::Find::name);
            };
        File::Find::find($add_files, $m_site_dir);
    }
}

sub check_canonpath() {
    my $testpath = "/test/../some.txt";
    if ($testpath eq File::Spec->canonpath($testpath)) {
        my $errmsg = <<_BADCANON_

** Fatal Error:

   File::Spec->canonpath does not clean up path.

   If you are doing this from Emacs with html-chklnk-check-site-links
   it may be because you are using Cygwin as your shell.  You can cure
   this in the following ways:

   1) Use w32shell.el - this will temporary switch to "cmd" as shell.
   2) Use the default shell on w32.

_BADCANON_
;

        die $errmsg;
    }
}

#############################
### Main
#############################

check_canonpath();

$| = 1; # flush or blush!

print "\n";
get_params();

check_next_file();
check_external_anchors();
report_errors();
