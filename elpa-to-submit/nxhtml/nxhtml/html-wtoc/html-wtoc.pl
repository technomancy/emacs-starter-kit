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
use File::Copy;
use File::Spec;
#use File::Path qw();
use File::Path;
use File::Find qw();
use FindBin;

use lib "$FindBin::Bin/PerlLib";
use PathSubs qw();
use html_tags qw(
*html header
div
table Tr td
p hr br
a span img b
);

### Script start parameters
my $m_param_action;
my $m_param_files  = 1;
my $m_param_pnum   = 0;
my $m_param_single = 0;
my $m_param_Template;
my $m_param_InPages;
my $m_param_OutRoot;
my @m_param_InRoot;
my $m_param_Overwrite;

### Globals
my $m_iAlwaysOpenedLevel = 0;
my $m_sCommonIn;
my $m_sInPagesFolder;
my $m_sTemplateFolder;
my $m_sStartTemplate;
my $m_sBodyTemplate;
#my $m_sEndTemplate;
my $m_bBorders = 0;
my @pages;
my %page_num;
my %js_show_page;
my $m_TemplateTime;
my $m_InPagesTime;
my %m_linked_files;

sub get_params();
sub get_template();
sub read_page_list($);
sub find_pages($$);
sub write_pages();
sub send_page();
sub find_template_files();
sub find_linked_from_pages();
sub copy_wtoc_files();
sub copy_linked_files();

#push @pages, [$ind, $tit, $full_fil, $anc, $hrf, $trg, $tip];
sub IND { 0 }
sub TIT { 1 }
sub FULL_FIL { 2 }
sub ANC { 3 }
sub HRF { 4 }
sub TRG { 5 }
sub TIP { 6 }

##########################################################
### Main
##########################################################
print "\n";
get_params();
if ($m_param_action eq "FIND") {
    find_pages(\@m_param_InRoot, $m_param_InPages);
} elsif($m_param_action eq "MERGE") {
    get_template();
    read_page_list($m_param_InPages);
    find_template_files();
    copy_wtoc_files();
    if ($m_param_files) {
        write_pages();
    } else {
        send_page();
    }
    find_linked_from_pages();
    copy_linked_files();
} elsif($m_param_action eq "TOC") {
}
exit;

sub copy_if_newer_or_overwrite($$) {
    my $in_file = shift;
    my $out = shift;
    my $out_file = $out;
    if (-d $out) {
        my ($in_v, $in_d, $in_f) = File::Spec->splitpath( $in_file );
        my ($out_v,$out_d,$out_f) = File::Spec->splitpath( $out, 1 );
        $out_file = File::Spec->catpath( $out_v, $out_d, $in_f );
    }
    my $should_write = 1;
    if (-e $out_file) {
        if ($m_param_Overwrite) {
            my $in_mdt = (stat $in_file)[9];
            my $outmdt = (stat $out_file)[9];
            if (($outmdt > $in_mdt)) {
                $should_write = 0;
            }
        } else {
            $should_write = 0;
        }
    }
    if ($should_write) {
        if (!File::Copy::syscopy($in_file, $out_file)) {
            die "syscopy($in_file, $out_file): $!";
        } else {
            print "  $in_file => $out_file\n";
        }
    }
} # copy_if_newer_or_overwrite

sub copy_wtoc_files() {
    print "\n**** Copy html-wtoc files\n";
    mkdir $m_param_OutRoot, 0777;
    my $css_file = $FindBin::Bin . "/html-wtoc.css";
    copy_if_newer_or_overwrite($css_file, $m_param_OutRoot);
    my $js_file = $FindBin::Bin . "/html-wtoc.js";
    copy_if_newer_or_overwrite($js_file,  $m_param_OutRoot);
    my $OutRootImg = $m_param_OutRoot . "img/";
    mkpath($OutRootImg);
    my $imgsrc = $FindBin::Bin . "/img/";
    opendir(IMGDIR, $imgsrc) or die "Can't opendir $imgsrc: $!";
    while (my $imgfile = readdir(IMGDIR)) {
        my $outimg = $OutRootImg . $imgfile;
        $imgfile = $imgsrc . $imgfile;
        #print STDERR ">>>$imgfile\n";
        if (-f $imgfile) {
            copy_if_newer_or_overwrite($imgfile, $outimg);
        }
    }
    closedir(IMGDIR);
} # copy_wtoc_files

sub add_to_linked_files($$) {
    my $from_file = shift;
    my $to_file   = shift;
    if (exists $m_linked_files{$to_file}) {
        my $old_from = $m_linked_files{$to_file};
        unless ($old_from eq $from_file) {
            die "Both $from_file and $old_from should be copied to $to_file";
        }
    }
    $m_linked_files{$to_file} = $from_file;
} # add_to_linked_files

sub copy_linked_files() {
    print "\n**** Copy linked files\n";
    my %pages;
    for my $pnum (0..$#pages) {
        $pages{ full_in_name($pnum) } = 1;
    }
    for my $to_file (keys %m_linked_files) {
        my $from_file = $m_linked_files{$to_file};
        unless (exists $pages{$from_file}) {
            if (-e $from_file) {
                mkpath4file($to_file);
                copy_if_newer_or_overwrite($from_file, $to_file);
            }
        }
    }
} # copy_linked_files

sub find_linked_files($;$) {
    my $in_file = shift;
    my $out_file = shift;
    $out_file = in2out($in_file) unless ($out_file);
    my $whole = get_file($in_file);
    while ($whole =~ m!(?:\s|^)(?:href|src)="(.*?)"!gis) {
        my $l = $1;
        next unless $l =~ m!\.(?:css|js|jpg|jpeg|gif|png)$!;
        if (!File::Spec->file_name_is_absolute($l)) {
            next if $l =~ m!^javascript:!;
            next if $l =~ m!^http://!;
            next if $l =~ m!^ftp://!;
            next if $l =~ m!^mailto:!;
        }
        my $rel_l = $l;
        my $full_in = $l;
        if (File::Spec->file_name_is_absolute($l)) {
            $rel_l = PathSubs::mk_relative_link($in_file, $l);
        } else {
            $full_in = PathSubs::mk_absolute_link($in_file, $l);
        }
        my $full_out = PathSubs::mk_absolute_link($out_file, $rel_l);
        add_to_linked_files($full_in, $full_out);
    }
} # find_linked_files

sub find_template_files() {
    print "\n**** Find files referenced in template file\n";
    my $in_file  = $m_param_Template;
    my $out_file = $m_param_OutRoot . "dummy.htm";
    find_linked_files($in_file, $out_file);
}
sub find_linked_from_pages() {
    for my $pnum (0..$#pages) {
        next unless defined $pages[$pnum][FULL_FIL];
        next unless $pages[$pnum][FULL_FIL] ne "";
        next if defined $pages[$pnum][TRG];
        find_linked_files( full_in_name($pnum) );
    }
}

sub should_write_merged($$) {
    my $pnum = shift;
    my $out_file = shift;
    my $should_write = 1;
    if (-e $out_file) {
        if ($m_param_Overwrite) {
            my $srcmdt = page_src_time($pnum);
            my $outmdt = (stat $out_file)[9];
            if (($outmdt > $srcmdt)
                && ($outmdt > $m_TemplateTime)
                && ($outmdt > $m_InPagesTime)) {
                $should_write = 0;
            }
        } else {
            $should_write = 0;
        }
    }
    return $should_write;
}
sub write_pages() {
    #print STDERR "*** param_OutRoot=$m_param_OutRoot\n";
    if ($m_param_single) {
        my $out_file = $m_param_OutRoot . "single_$m_param_pnum.html";
        if (should_write_merged($m_param_pnum, $out_file)) {
            my $page = create_single_page($m_param_pnum);
            $page = shrink($page);
            create_file_and_path($out_file, $page);
        }
    } else {
        my $iPages = 0;
        print "\n*** Creating pages:\n";
        for my $pnum (0..$#pages) {
            next unless defined $pages[$pnum][FULL_FIL];
            next unless $pages[$pnum][FULL_FIL] ne "";
            next if defined $pages[$pnum][TRG];
            $iPages++;
            my $out_file = full_out_name($pnum);
            if (should_write_merged($pnum, $out_file)) {
                my $page = create_page($pnum);
                next unless $page;
                print " Creating page $iPages:  " . full_in_name($pnum) . "\n";
                $page = shrink($page);
                print "\t=>  $out_file\n";
                create_file_and_path($out_file, $page);
            }
        }
    }
} # write_pages

sub send_page() {
    my $page = ($m_param_single ?
                create_single_page($m_param_pnum)
                :
                create_page($m_param_pnum) );
    print $page;
} # send_page

##########################################################
### Params
##########################################################
sub die_usage() {
    my $sScript = $0;
    $sScript =~ tr!\\!/!;
    $sScript =~ s!.*/(.*)!$1!;
    die qq(Usage:
  Making preliminary file list:
    $sScript find in="in-dir" pages="pages-file" [overwrite=1]

  Merging pages and table of contents:
    $sScript merge pages="pages-file" outroot="out-dir" template="template-file" [overwrite=1]

        \n);
}
#use Getopt::Long;
sub get_params() {
    $| = 1;
    for my $arg (@ARGV) { print " "; print $arg; } print "\n\n";
    die_usage() unless $#ARGV > 0;
    $m_param_action   = $ARGV[0];
    $m_param_action =~ tr/a-z/A-Z/;
    #push @m_param_InRoot, $FindBin::Bin . "/doc/";
    #$m_param_OutRoot = $FindBin::Bin . "/tmp/";
    #$m_param_Template = $FindBin::Bin . "/doc/home_template.htm";
    #$m_param_InPages = $FindBin::Bin . "/doc/toc_pages.txt";
    for (my $i = 1; $i <= $#ARGV; $i++) {
        my ($k, $v) = ($ARGV[$i] =~ m!(.*?)=(.*)!);
        $v =~ tr!\\!/!;
        if ($k eq "in") {
            $v = PathSubs::uniq_file($v);
            $v .= "/" unless substr($v, -1) eq "/";
            push @m_param_InRoot, $v;
        } elsif( $k eq "outroot") {
            $v = PathSubs::uniq_dir($v);
            $v .= "/" unless substr($v, -1) eq "/";
            $m_param_OutRoot = $v;
        } elsif( $k eq "pages") {
            $v = PathSubs::uniq_file($v);
            $m_param_InPages = $v;
        } elsif( $k eq "template") {
            $v = PathSubs::uniq_file($v);
            $m_param_Template = $v;
        } elsif( $k eq "overwrite" ) {
            $m_param_Overwrite = $v;
        } elsif( $k eq "openedlevel" ) {
            $m_iAlwaysOpenedLevel = $v * 1;
        } else {
            die "Unknown parameter: $ARGV[$i]\n";
        }
    }
    if($m_param_action eq "FIND") {
        if ($#m_param_InRoot < 0) { die_usage(); }
        if (! defined $m_param_InPages) { die_usage(); }
    } elsif($m_param_action eq "MERGE") {
        if (! defined $m_param_InPages) { die_usage(); }
        if (! defined $m_param_OutRoot) { die_usage(); }
        if (! defined $m_param_Template) { die_usage(); }
        $m_sTemplateFolder = $m_param_Template;
        $m_sTemplateFolder =~ s![^/]*$!!;
    } else {
        die_usage();
    }

    $m_sInPagesFolder = $m_param_InPages;
    $m_sInPagesFolder =~ s![^/]*$!!;
    print "Parameters:\n";
    print "    "  .  $m_param_action . "\n";
    print "    pages=" .  $m_param_InPages . "\n";
    print "    outroot=" .  $m_param_OutRoot . "\n";
    print "    template=" .  $m_param_Template . "\n";
    if (defined $m_param_Overwrite) {
        print "    overwrite="  .  $m_param_Overwrite . "\n";
    }
    #if ($#m_param_InRoot == -1) { push @m_param_InRoot,$m_sInPagesFolder; }
}

sub get_template() {
    my $sTemplate = get_file($m_param_Template, 1);
    $m_TemplateTime = (stat $m_param_Template)[9];
    $m_InPagesTime  = (stat $m_param_InPages)[9];
    $sTemplate =~ s/<!--.*?-->//gs;
    if (  $sTemplate =~ m!(.*?<body.*?>)(.*)</body>!si  ) {
        $m_sStartTemplate = $1;
        $m_sBodyTemplate = $2;
        #$m_sEndTemplate = $3;
    } else {
        die "Can't find body of template\n";
    }
} # get_template

sub read_page_list($) {
    my $sPagesFile = shift;
    my @in_files;
    open(P,$sPagesFile) or die "Can't open toc list file $sPagesFile: $!\n";
    while (my $sLine = <P>) {
        chomp $sLine;
        $sLine =~ s/^\s+|\s+$//g;
        next if $sLine eq "";
        next if substr($sLine, 0, 1) eq ";";
        #print STDERR "$sLine\n";
        my ($ind, $tit, $ref, $tip, $trg, $ico)
            = map { s/^\s+|\s+$//g; $_; } split("###", $sLine);
        #warn "trg=$trg\n" if defined $trg;
        my ($fil, $anc) = ("", "");
        my $hrf = "";
        my $full_fil = "";
        #$ref = "" unless defined $ref;
        #print STDERR "ref=$ref\n";
        if (defined $ref) {
            if (defined $trg) { undef $trg unless $trg ne ""; }
            if ((defined $trg) || ($ref =~ m/https?:/i)) {
                $hrf = $ref;
            } else {
                ($fil, $anc) = split('#', $ref);
                if ($ind >= 0) {
                    if (File::Spec->file_name_is_absolute($fil)) {
                        $full_fil = $fil;
                    } else {
                        $full_fil = PathSubs::uniq_file($m_sInPagesFolder . $fil);
                    }
                }
            }
        }
        if ((!$tip) && ($full_fil ne "")) {
            $tip = get_title($full_fil);
        }
        push @pages, [$ind, $tit, $full_fil, $anc, $hrf, $trg, $tip];
        push @in_files, $full_fil if !defined $trg;
    }
    close P;
    $m_sCommonIn = get_common_root(\@in_files). "/";
} # read_page_list



sub get_common_root($) {
    my $psRoots = shift;
    my @sCommon;
    for my $s (@$psRoots) {
        my $full_s = PathSubs::uniq_file($s);
        my @full_s = split("/", $full_s);
        if ($#sCommon == -1) {
            @sCommon = @full_s;
        } else {
            my $iMax = $#sCommon; if ($#full_s < $iMax) { $iMax = $#full_s; }
            for (my $i = 0; $i <= $iMax; $i++) {
                if ($sCommon[$i] ne $full_s[$i]) {
                    #print STDERR "$i:  $sCommon[$i] != $full_s[$i]\n";
                    @sCommon = @sCommon[0..$i-1];
                    last;
                }
            }
        }
    }
    my $sCommon = join("/", @sCommon);
    return $sCommon;
} # get_common_root


sub find_pages($$) {
    my $pasInRoot  = shift;
    my $sOutFile   = shift;
    if (!$m_param_Overwrite) {
        die "Don't want to overwrite existing output file $sOutFile!\n" if -e $sOutFile;
    }
    my $root_level;
    my $sList;
    my $handle_file =
        sub {
            return unless m/.html?/i;
            return if -d $_;
            my $fname = PathSubs::uniq_file($_);
            die "Can't read $fname\n" unless -r $_;
            my $title = get_title($_);
            my $level = $fname =~ tr!/!!;
            $level -= $root_level;
            my $rel_fname = PathSubs::mk_relative_link($sOutFile, $fname);
            $sList .= "$level ### $title ### $rel_fname\n";
    };
    for my $sInRoot (@$pasInRoot) {
        $sInRoot = PathSubs::uniq_file($sInRoot);
        chop($sInRoot) if (substr($sInRoot, -1) eq "/");
        $root_level = $sInRoot =~ tr!/!!;
        File::Find::find($handle_file, $sInRoot);
    }
    create_file($sOutFile, $sList);
} # find_pages


##########################################################
### File - page helpers
##########################################################

sub file_name($) {
    my $num = shift;
    return $pages[$num][FULL_FIL];
}
sub file_anchor($) {
    my $num = shift;
    return $pages[$num][ANC];
}
sub file_href($) {
    my $num = shift;
    #die $pages[$num][HRF] if defined $pages[$num][HRF];
    return $pages[$num][HRF];
}
sub file_target($) {
    my $num = shift;
    return $pages[$num][TRG];
}
sub file_title($) {
    my $num = shift;
    return $pages[$num][TIT];
}
sub file_tip($) {
    my $num = shift;
    return $pages[$num][TIP];
}
sub full_in_name($) {
    my $num = shift;
    my $name = file_name($num);
    return $name;
}
sub full_out_href($) {
    my $num = shift;
    my $anchor = file_anchor($num);
    my $full_href = full_out_name($num);
    warn "full_href is null" unless $full_href;
    if ((defined $anchor) && ($anchor ne "")) { $full_href .= "#" . $anchor; }
    return $full_href;
}
sub full_out_name($) {
    my $num = shift;
    my $in_name = file_name($num);
    return unless $in_name;
    my $anchor = file_anchor($num);
    #$m_param_OutRoot . $name;
    $anchor = "";
    my $name = substr($in_name, length($m_sCommonIn));
    if ($anchor) {
        my $base;
        my $ext;
        for (my $i = length($name);$i>0;$i--) {
            if (substr($name, $i, 1) eq ".") {
                $base = substr($name, 0, $i-1);
                $ext  = substr($name, $i);
                $name = $base . "_sharp_" . $anchor . $ext;
                last;
            }
        }
    }
    $m_param_OutRoot . $name;
}
sub replace_name_link($) {
    my $page = shift;
    for my $k (keys %page_num) {
        my $num = $page_num{$k};
        my $href = ($m_param_single ? "javascript:ShowPage($num)" : file_name($num));
        $page =~ s!%%$k%%!$href!gs;
    }
    return $page;
}

##########################################################
### File name helpers
##########################################################
sub in2out($) {
    my $in_name = shift;
    die "in2out: File name is not abs: $in_name" unless File::Spec->file_name_is_absolute($in_name);
    my $name = substr($in_name, length($m_sCommonIn));
    $m_param_OutRoot . $name;
}

##########################################################
### File reading/writing
##########################################################

sub mkpath4file($) {
    my $file = shift;
    my $path = $file;
    $path =~ s|[^/]*$||;
    File::Path::mkpath($path);
}
sub create_file($$) {
    my ($out_file, $page) = @_;
    if (!$m_param_Overwrite) {
        if (-e $out_file) { die "Will not overwrite $out_file\n"; }
    }
    open(OUT, ">$out_file") or die "Can't create $out_file: $!";
    print OUT $page;
    close OUT;
    chmod 0111|((stat $out_file)[2]&07777), $out_file
}
sub create_file_and_path($$) {
    my ($out_file, $page) = @_;
    mkpath4file($out_file);
    create_file($out_file, $page);
}


sub get_file($$) {
    my ($file, $need) = @_;
    if (open(FL, $file)) {
        local $/;
        my $whole = <FL>;
        close FL;
        return $whole;
    } else {
        my $err = $!;
        die "Can't open $file: $err\n" if $need;
        return "";
    }
}

sub get_title($) {
    my $file = shift;
    open(H, $file) or die "Can't open and get title from $file: $!";
    while (my $line = <H>) {
        if ($line =~ m!<title>(.*?)</title>!i) { close H; return $1; }
    }
    close H;
}



##########################################################
### Html parsing etc
##########################################################

sub get_head_from_file($) {
    my $fname = shift;
    my $err;
    my $head = get_head(get_file($fname, 1), \$err);
    die "\n\n$fname\n\t" . $err if defined $err;
    return $head;
}
# BUG: These actually requires parsing of the file, but it does not
# seem very important:
sub get_head($$) {
    my $html = shift;
    my $perr  = shift;
    return "" unless $html;
    $html =~ s/<!--.*?-->//g;
    if ($html =~ m!<head.*?>(.*)</head>!is) {
        return $1;
    }
    $$perr = "Can't find <head>-tag in $html\n";
}
sub get_body($) {
    my $html = shift;
    return "" unless $html;
    $html =~ s/<!--.*?-->//gs;
    if ($html =~ m!<body[^>]*>(.*)</body>!is) {
        return $1;
    }
    die "Can't find <body>-tag in $html\n";
}

sub shrink($) {
    my $str = shift;
    my $out_str = "";
    my @str = split("\n", $str);
    my $in_pre = 0;
    for my $s (@str) {
        if ($s =~ m!<pre>!i)  { $in_pre = 1; }
        if ($s =~ m!</pre>!i) { $in_pre = 0; }
        $s =~ s!^(\s*)!! unless $in_pre;
        $out_str .= $s . "\n";
    }
    return $out_str;
    $str =~ s!^(\s*)!!gm;
    $str;
}


##########################################################
### Making what we see
##########################################################

sub mk_search() {
    return "" if ! $m_param_single;
    return qq[
            <a href="javascript:show_search()" xstyle="font-size: 8pt"
            title="Show Search Form"
            ><img src="img/search.gif" border="$m_bBorders" align="left"></a>
            <a href="javascript:show_search()" xstyle="font-size: 8pt"
            title="Show Search Form"
            class="html-wtoc-search"
            >Sök</a>
            ];
}
sub mk_main_table($$$$$) {
    my $left         = shift;
    my $main         = shift;
    my $srch_table   = shift;
    my $sFile        = shift;
    my $pNum         = shift;
    my $search_tr = "";
    if ($m_param_single) {
        $search_tr =
            Tr(
                td("&nbsp;&nbsp;")
                . td({-valign=>'bottom', }, mk_search(), ) )
    }
    my $cont_table =
        table(
            { -border=>"$m_bBorders", -cellpadding=>0, -cellspacing=>0,
              -width=>"100%",
              -id=>"html-wtoc-contents",
              #-style=>"display:",
              -summary=>"Table of contents",
            },
            Tr(
                #td("&nbsp;&nbsp;")
                td({-class=>"html-wtoc-margin"})
                . td({-valign=>'top'}, $left) )
            . $search_tr
        )
        ;
    my $page = $m_sBodyTemplate;
    $page = replace_template_links($m_sBodyTemplate, $sFile);
    $page    =~ s!%%TOC%%!$cont_table!;
    $page    =~ s!%%PAGE%%!$main!;
    return $page;
} # mk_main_table


sub find_ind_level_prev($) {
    my $lThis = shift;
    for (my $i = $lThis - 1; $i > 0; $i--) {
        my $ind_lev = $pages[$i][IND];
        if ($ind_lev < 50) { return $ind_lev; }
    }
    return undef;
}
sub find_ind_level_next($) {
    my $lThis = shift;
    #print "find_ind_level_next($lThis)";
    #print ", ";
    #print file_title($lThis);
    #print "\n";
    #for (my $i = $lThis; $i < $#pages; $i++) {
    for (my $i = $lThis + 1; $i <= $#pages; $i++) {
        my $ind_lev = $pages[$i][IND];
        if ($ind_lev < 50) { return $ind_lev; }
    }
    return undef;
}








sub mk_opener_elem($$$) {
    my $iPi = shift;
    my $sHref = shift;
    my $bOpened = shift;
    my $Aattrib =
    {
        -id  =>"opener_$iPi",
    };
    if ($sHref) { $$Aattrib{href} = $sHref; }
    my $sImg;
    my $sAlt;
    if ($bOpened) {
        $sImg = "down";
        $sAlt = "Close";
    } else {
        $sImg = "right";
        $sAlt = "Open";
    }
    return
        a(
            $Aattrib,
            img({
                -src=>"img/$sImg.gif",
                -alt=>$sAlt,
                -border=>0,
                -width=>12,
                -height=>12,
                },
            ),
        );
} # mk_opener_elem

sub mk_content($) {
    my $pnum = shift;
    if (!$pages[$pnum]) {
        return br();
    }
    my $cont;
    my @father;
    my @child_trace;
    my $this_indent = $pages[$pnum][IND];
    my $this_file   = $pages[$pnum][FULL_FIL];
    if ($this_indent == -2) {
        return "";
    }
    my $this_href   = full_out_name($pnum);
    #my $anchor = file_anchor($pnum);
    #if (defined $anchor) { $this_href .= "#" . $anchor; }
    my @size;
    $size[0] = "1em";
    $size[1] = "0.8em";
    $size[2] = "0.8em";



    ### Open all main level nodes
    my @opened; # rename to visible!!!!!
    for my $pi (0..$#pages) {
        my $indent = $pages[$pi][IND];
        if ($indent <= $m_iAlwaysOpenedLevel) {
            $opened[$pi] = 1;
        } else {
            $opened[$pi] = 0;	# more simple to handle
        }
    }



    ### Open ancestors and older sisters (if not a standalone node)
    my $pnum_indent = $pages[$pnum][IND];
    my $high_open = $pnum_indent;
    my $standalone_open = 10;
    if ($high_open < $standalone_open) { ### Not a standalone node
        for (my $pi = $pnum; $pi >= 0; $pi--) {
            my $pi_indent = $pages[$pi][IND];
            if ($high_open >= $pi_indent) {
                $opened[$pi] = 1;
                $high_open = $pi_indent;
                for (my $ps = $pi+1; $ps <= $#pages; $ps++) {
                    my $ps_indent = $pages[$ps][IND];
                    last if $ps_indent < $pi_indent;
                    $opened[$ps] = 1 if $ps_indent == $pi_indent;
                }
            }
            last if $pi_indent == 0;
        }
    }




    ### Open direct childs and younger sisters
    my $maybe_child  = 1;
    my $more_sisters = 1;
    my $max_open_indent = $pnum_indent;
    for my $pi ($pnum+1..$#pages) {
        my $pi_indent = $pages[$pi][IND];
        if ($pi_indent <= $max_open_indent) { $maybe_child  = 0; }
        if ($pi_indent < $pnum_indent)      { $more_sisters = 0; }
        if ($pi_indent == $pnum_indent) {
            if ($more_sisters) { $opened[$pi] = 1; }
            $maybe_child = 0;
        } elsif ($pi_indent == $pnum_indent+1) {
            if ($maybe_child) { $opened[$pi] = 1; }
        }
    }
    #exit if $pnum == 3;




    ### Open all in the same file (necessary for non-JavaScript)
    for my $pi (0..$#pages) {
        my $file = $pages[$pi][FULL_FIL];
        #printf STDERR "file - open=(%s)\n", $file;
        #if ($file eq $this_file) {
        if ($file eq $this_file) {
            $opened[$pi] = 1;
        }
        if ($file eq "") {
            if ($pi < $#pages) {
                if ($pages[$pi][IND] < $pages[$pi+1][IND]) {
                    $opened[$pi+1] = 1;
                }
            }
        }
        if ($pages[$pi][IND] > 10) {
            $opened[$pi] = 0;
            #print ">>>>>>>>\$opened[$pi] = 0;\n";
        }
        #print STDERR "+++++++++\$opened[$pi] = $opened[$pi]\n";
    }




    ### Make the actual contents
    my $tooltip;
    my $child_id;
    for my $pi (0..$#pages) {
#         if (!$pages[$pi][FULL_FIL] && !$pages[$pi][HRF]) {
#             my $txt = file_title($pi); #$pages[$pi][TIT];
#             $txt = qq(</p><hr width="50%" align="left" /><p style='margin-top:0'>) if $txt eq "-";
#             $cont .= $txt;
#             $cont .= br();
#             next;
#         }
        my $txt = file_title($pi); #$pages[$pi][TIT];
        if ($txt eq "-") {
            $txt = qq(</p><hr width="50%" align="left" /><p style='margin-top:0'>);
            $cont .= $txt;
            $cont .= br();
            next;
        }
        #if ($pages[$pi][TRG]) {
        #	next;
        #}
        #next if ! defined $opened[$pi];
        #next if ! $opened[$pi];
        my $ind_lev = $pages[$pi][IND];
        next if $ind_lev > 50;
        my $ind_lev_next = find_ind_level_next($pi);
        #my $ind_lev_prev = find_ind_level_prev($pi);

        my $this_entry = "";

        ### Child id from previous row
        if (defined $child_id) {
            my $display = "";
            if (!$opened[$pi]) {
                $display = qq(style="display:none");
            } else {
            }
            $this_entry .= "\n<div id=\"$child_id\" $display>\n";
            undef $child_id;
        }
        my $opener_elem = ""; #qq(<img src="img/blank12.gif" width=12 height=12 alt=" ">);
        my $childs_are_visible = ($pi == $pnum);
        if ($pi < $#pages) {
            if ($pages[$pi][IND] < $pages[$pi+1][IND]) {
                if ($opened[$pi+1]) { $childs_are_visible = 1; }
            }
        }
        #if ($pages[$pi][IND] < $m_iAlwaysOpenedLevel) { $childs_are_visible = 1; }

        my $file_href;
        my $target;
        my $href;
        my $href_self;
        my $target_attrib;
        my $title  = file_title($pi);
        my $file_name = file_name($pi);
        if ($title) {
            $file_href = file_href($pi); # || "";
            $target = file_target($pi);
            $href =
                ($file_name ?
                 ($m_param_files ?
                  ($m_param_single ? "JavaScript:ShowPage($pi);"    :
                   ($file_href ne ""? $file_href
                    :
                    PathSubs::mk_relative_link($this_href, full_out_href($pi))))
                  :
                  ($m_param_single ? "JavaScript:ShowPage($pi)" : "?pnum=$pi")
                 )
                 :
                 (File::Spec->splitpath($this_href))[2]);
            if ($pi == $pnum) {
                $href_self = $this_href;
                if ($href_self =~ m!([^/\\]*$)!) {
                    $href_self = $1;
                }
            }
            $target_attrib = (defined $target? qq(target="$target"): "");
        } else {
            $href = "";
            $target_attrib = "";
        }

        if (defined $ind_lev_next && $ind_lev_next > $ind_lev) {
            $child_id = "toc_child_$pi";
            #print "    child_id=$child_id\n";
            push @child_trace, $child_id;
            $opener_elem = mk_opener_elem($pi,
                                          ($href? $href : $href_self),
                                          $childs_are_visible);
        }
        $title =~ s/_/&nbsp;/go;
        my $indent = ($ind_lev ? "&nbsp;" x (($ind_lev-1) * 4) : "");
        my $size = $size[$ind_lev];
        $title = b($title) if $ind_lev == 0;

        my $Aattrib =
        {
            id=>"toc_link_$pi",
            onclick=>"html_wtoc_nailing(this)",
        };
        if (!$file_name) {
            $Aattrib =
            {
                id=>"opener_text_$pi",
            };
        }
        if ($pi == $pnum) {
            ### Current page
            $$Aattrib{class} = "html-wtoc-currcont";
            $$Aattrib{title} = "You are here";
            $$Aattrib{href}  = $href_self;
            $this_entry .=
                table({
                    -cellspacing=>0,
                    -cellpadding=>0,
                    -class=>"html-wtoc-contline",
                    -border=>0,
                    -summary=>"Formatter",
                      },
                      Tr({
                         },
                         td({
                            },
                            a(
                                $Aattrib,
                                $indent . $title . "&nbsp;"
                            )
                         )
                         . td({
                             -class=>"html-wtoc-mark",
                              },
                              $opener_elem
                         )
                      )
                );




        } else {
            ### Link to other page
            if (file_title($pi)) {
                $tooltip = $pages[$pi][TIP];
                if (!defined $tooltip) { $tooltip = "Go to the page $title"; }
                $$Aattrib{class} = "html-wtoc-contents-a";
                my $a_or_span;
                if (!defined $href) {
                    $a_or_span = span($Aattrib, $indent . $title);
                } else {
                    $$Aattrib{title} = $tooltip;
                    $$Aattrib{href}  = $href;
                    if (defined $target) { $$Aattrib{target} = $target; }
                    $a_or_span = a($Aattrib, $indent . $title);
                }
                $this_entry .=
                    table({
                        -cellspacing=>0,
                        -cellpadding=>0,
                        -class=>"html-wtoc-contline",
                        -border=>0,
                        -summary=>"Formatter",
                          },
                          Tr({
                             },
                             td({
                                },
                                $a_or_span
                             )
                             . td({
                                 -class=>"html-wtoc-mark",
                                  },
                                  $opener_elem
                             )
                          )
                    );
            } else {
                $this_entry .= $indent . " " . $title;
                #die $this_entry;
            }
        }
        if ((!defined $ind_lev_next) || $ind_lev_next <= $ind_lev) {
            my $ind_end = $ind_lev;
            if (defined $ind_lev_next) { $ind_end = $ind_lev_next+1; }
            for (my $i = $ind_end; $i <= $ind_lev; $i++) {
                my $end_id = pop @child_trace;
                if (defined $end_id) {
                    $this_entry .= "</div><!-- end child $end_id -->";	# end childs' span
                    #print "   end $end_id\n";
                }
            }
        }
        $cont .= $this_entry;
        $father[$ind_lev] = $pi;
    } #for my $pi (0..$#pages)

    $cont = div({-class=>"html-wtoc-contents"}, $cont) . p("&nbsp;");
    #$cont =~ s|<|\n<|gms;
    #$cont =~ tr!\n\r! !;
    $cont =~ s{
            (\ssrc=)"(.*?)"
        }
    {
        my $s1 = $1;
        my $src = $2;
        if (!PathSubs::is_abs_path($src)) {
            my $srcabs = PathSubs::mk_absolute_link(full_out_name(0), $src);
            $src = PathSubs::mk_relative_link(full_out_name($pnum), $srcabs);
        };
        "${s1}\"$src\"";
    }egsmx;
    $cont;
} # mk_content

sub mk_main_window($) {
    my $pnum = shift;
    my $full_name = full_in_name($pnum);
    return unless defined $full_name;
    return get_body(get_file($full_name, 1));
}








##########################################################
### The JavaScripts and styles we need
##########################################################

sub mk_style($) {
    return "";
    my $pnum = shift;
    my $rel_link =
        PathSubs::mk_relative_link(full_out_name($pnum), $m_param_OutRoot . "html-wtoc.css");
    return qq(<link rel="stylesheet" href="$rel_link" type="text/css">\n);
}
sub mk_js($) {
    my $pnum = shift;
    return <<__HTML_END_JS_PNUM__;
      <script type="text/JavaScript">
        var iCurrentChild = $pnum;
        var iMaxChildNum  = $#pages;
      </script>
__HTML_END_JS_PNUM__
        return "";
    my $single_js = "";
    if ($m_param_single) {
        $single_js = qq[if (!document.all) { navigate("0.html"); }];
        my $page_info = "var page_name = new Array;\n";
        for my $i (0..$#pages) {
            my $page_name = file_title($i); #$pages[$i][TIT];
            $page_info .= qq[ page_name[$i] = "$page_name";\n];
        }
        $single_js .= $page_info;
    }
    my $sch_link =
        PathSubs::mk_relative_link(full_out_name($pnum), $m_param_OutRoot . "search.js");
    my $top_link =
        PathSubs::mk_relative_link(full_out_name($pnum), $m_param_OutRoot . "html-wtoc.js");
    return <<__HTML_END_JS__;
      <script type="text/JavaScript" src="$sch_link"></script>
      <script type="text/JavaScript" src="$top_link"></script>
      <script type="text/JavaScript">
        $single_js
      </script>
__HTML_END_JS__
}

##########################################################
### Page creation
##########################################################

sub replace_template_links($$) {
    my $template   = shift;
    my $sFile = shift;
    $template =~ s{\ssrc="(.*?)"}
    {
        my $sSrc  = $m_param_OutRoot . $1;
        my $sRelSrc = PathSubs::mk_relative_link($sFile, $sSrc);
        qq( src="$sRelSrc");
    }exg;
    $template =~ s{\shref="(.*?)"}
    {
        my $sOld = $1;
        if ((lc substr($sOld, 0, 11)) eq "javascript:") {
            qq( href="$sOld");
        } elsif (PathSubs::is_abs_netpath($sOld)) {
            qq( href="$sOld");
        } else {
            my $sSrc  = $m_param_OutRoot . $sOld;
            my $sRelSrc = PathSubs::mk_relative_link($sFile, $sSrc);
            qq( href="$sRelSrc");
        }
    }exg;
    return $template;
} # replace_template_links

sub mk_start_of_page($) {
    my $pnum = shift;
    my $page = "";
    my $page_style = mk_style($pnum);
    my $page_js    = mk_js($pnum);
    my $sFile = full_out_name($pnum);
    my $head       = "";
    $head .= $page_js;
    $head .= $page_style;
    $head .= get_head_from_file(full_in_name($pnum));
    $page .= header if !$m_param_files;
    $page .= replace_template_links($m_sStartTemplate, $sFile);
    $page =~ s!<title>HEAD</title>!$head!;
    my $focus_pnum = $pnum;
    my $ind_lev = $pages[$pnum][IND];
    if ($ind_lev > 50) { $focus_pnum = 0; }
    $page =~ s!%%PNUM%%!$focus_pnum!;
    return $page;
} # mk_start_of_page

my %m_sCreatedPages;
sub page_src_time($) {
    my $pnum = shift;
    my $src_file = $pages[$pnum][FULL_FIL];
    return (stat $src_file)[9];
}
sub create_page($) {
    my $pnum = shift;
    return unless $pages[$pnum][FULL_FIL];

    my $out_name = full_out_name($pnum);
    return if exists $m_sCreatedPages{$out_name};
    $m_sCreatedPages{$out_name} = 1;

    my $page = mk_start_of_page($pnum);
    my $cont_win = mk_content($pnum);
    my $main_win = mk_main_window($pnum);
    $page .= mk_main_table(
        $cont_win,
        $main_win,
        "",
        $out_name,
        $pnum,
        );
    $page .= end_html;
    $page = replace_name_link($page);
    return $page;
} # create_page


__END__


    ##########################################################
    ### Unused currently
    ##########################################################

    sub build_ShowPage() {
        for my $num (0..$#pages) {
            $page_num{$pages[$num][FULL_FIL]} = $num;
            my $fon = full_out_name($num);
            if ($fon) { $js_show_page{$fon} = "ShowPage($num);"; }
        }
}
build_ShowPage();


sub mk_meta_enter_exit() {
    return <<__HTML_EE__;
      <meta HTTP-EQUIV="Page-Enter" content="RevealTrans (Duration=0.1, Transition=31)">
      <meta HTTP-EQUIV="Page-Exit"  content="RevealTrans (Duration=1, Transition=23)">
__HTML_EE__
}

##########################################################
### Single page
##########################################################

sub mk_noscript() {
    return <<__HTML_END_NOSCRIPT__;
      <noScript>
        Sorry, there is not yet any version for non-JavaScript browsers.
        You need to enable JavaScript to see the rest of the pages!
__HTML_END_NOSCRIPT__
}

sub create_single_page($) {
    my $pnum = shift;

    my $page = mk_start_of_page($pnum);
    my $left_col = "";
    my $main     = "";
    for my $pi (0..$#pages) {
        next unless $pages[$pi][FULL_FIL];
        my $display = ($pi == $pnum ? 'style="display: block"' : 'Style="display: none"');
        my $pi_left_col = replace_rel_link(mk_content($pi),    full_out_name($pi));
        my $pi_main     = replace_rel_link(mk_main_window($pi), full_out_name($pi));
        my $pi_margin   = "";
        $left_col .= "\n<div id='left_col_$pi' $display>" .  $pi_left_col . "</div>\n";
        $main     .= "\n<div id='main_$pi'     $display>" .  $pi_main     . "</div>\n";
    }
    my $search_table = qq[
        <table border="0" width="100%" height="200"
        cellpadding="0" cellspacing="0"
        xbgcolor="yellow"
        class="html-wtoc-search"
        id="search" style="display:none">
    		  <tr>
        <td>&nbsp;&nbsp;</td>
        <td align="left" valign="top" height="1">
        <form onsubmit="return do_search(input.value);"
        class="html-wtoc-search-form"
        >
        <input id="input" size="14"
        ><input type="image" name="Search" value="Search"
    				title="Search"
    				src="img/search.gif"
    				align="top"
        >
        </td>
        </form>
    		  </tr>
    		  <tr valign="top">
        <td>&nbsp;&nbsp;</td>
        <td id="hits" valign="top">
        </td>
    		  </tr>
    		  <tr>
        <td>&nbsp;&nbsp;</td>
        <td valign="bottom">

        <a href="javascript:hide_search()" xstyle="font-size: 8pt"
        title="Show Menu"
        ><img src="img/nosearch.gif" border=0 align="left"></a>
        <a href="javascript:hide_search()"
        title="Show Menu"
        >Göm sökning</a>
        </td>
    		  </tr>
        </table>
        ];
    $page .= mk_main_table(
        $left_col,
        $main,
        $search_table,
        full_out_name($pnum),
        $pnum,
        );
    $page .= mk_noscript();
    $page .= end_html;
    $page =~ s/(\d+)\.html/javascript:ShowPage($1);/gs;
    #$page =~ s/<body(.*?)>/<body$1 onload="ShowPage(0)">/gis;
    $page =~ s/<body(.*?)>/<body$1 onload="HTML_WTOC_NS.onload_actions()">/gis;
    $page = replace_name_link($page);
    return $page;
} # create_single_page

my $abs_pos_tbl =
    qq(
    <table border="$m_bBorders" cellpadding=0 cellspacing=0
    width="100%" height=70
    bgcolor="white"
    style="
    position: absolute;
    left: 0;
    top: 0;
    "
    >
    <tr>
    <td>
    </td>
    </tr>
    </table>
    );


##########################################################
### Index.htm
##########################################################

# sub mk_index_page($) {
#     my $page = shift;
#     my $check_browser = qq[ //if (document.all) { navigate("single_0.html"); }\n];
#     #$page =~ s/(<script.*?>)/$1\n$check_browser/s;
#     mkdir $m_param_OutRoot, 0777;
#     my $out_file = $m_param_OutRoot . "index.htm";
#     create_file_and_path($out_file, $page);
# }




##########################################################
### Links handling
##########################################################


sub replace_rel_link($$) {
    my ($page, $page_file) = @_;
    my $qr;
    $page =~
        s{
            (src|href)="(.*?)"
        }{
            my $src_href = $1;
            my $href = $2;
            if (!PathSubs::is_abs_path($href)) {
                $href = PathSubs::mk_absolute_link($page_file, $href);
                $href =~ tr|\\|/|;
                if (exists $js_show_page{$href}) {
                    $href = "javascript:$js_show_page{$href}";
                }
            }
            qq($src_href="$href");
    }xegsm;

    $page;
}

