# Copyright 2006 Lennart Borgman, http://OurComments.org/. All rights
# reserved.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

package PathSubs;

#####################################################
###     This package contains general path handling
###     routines and some win32 specific dito.
### The latter should ev be moved to a new module!
#####################################################
use strict;

use File::Spec;

### Absolute path names

sub is_abs_path ($) {
   my $path = shift;
   return 0 if $path eq "";
   return 1 if File::Spec->file_name_is_absolute($path);
   #return 1 if substr($path, 1, 1) eq ":";     # MSWin32
   #return 1 if substr($path, 0, 1) eq "/";
   return 1 if $path =~ /^https?:/i;
   return 1 if $path =~ /^file:/i;
   return 1 if $path =~ /^javascript:/i;
   return 1 if $path =~ /^mailto:/i;
}
sub is_abs_netpath($) {
   my $path = shift;
   return 1 if $path =~ /^https?:/i;
   # New
   return 1 if $path =~ /^ftp:/i;
   return 1 if $path =~ /^mailto:/i;
}


sub uniq_file($) {
        my $fname = shift;
        $fname =~ s!^\s+|\s+$!!g;
        return "" if ($fname eq "");
        $fname = File::Spec->rel2abs($fname);
        if (!File::Spec->file_name_is_absolute($fname)) {
            die "File name is not absolute: $fname";
        }
        #print STDERR "uniq_file($fname)\n";
        $fname =~ tr!\\!/!;
        if (-e $fname)  {
                #print STDERR "exists $fname\n";
                ### There is an error in 522, compensate for this!
                #die substr($fname, -1);
                if (substr($fname, -1) eq "/") { chop $fname; }
                #print STDERR "exists $fname\n";
                ### Translate ..
                if (substr($fname, 1, 1) eq ":") {
                    my $ffname = Win32::GetFullPathName($fname);
                    ### Get case
                    my $lfname = Win32::GetLongPathName($ffname);
                    #print STDERR "lexists $lfname\n";
                    $fname = $lfname if ($lfname ne "");
                }
        } else {
                #print STDERR "NOT exists $fname\n";
                if (substr($fname, -1) eq "/") { chop $fname; }
                my $head = "";
                if (substr($fname, 0, 2) eq "//") {
                        $head = "//";
                        $fname = substr($fname, 2);
                }
                my @fname = split("/", $fname);
                my $tail = pop @fname;
                $fname = uniq_dir($head . join("/", @fname)) . $tail;
        }
        if (substr($fname, 1, 1) eq ":") {
                $fname = uc(substr($fname, 0, 1)) . substr($fname, 1);
                #print STDERR "fname $fname\n";
        }
        $fname =~ tr!\\!/!;
                #print STDERR "fname ($fname)\n";
        return $fname;
}
sub uniq_dir($) {
        my $dir = shift;
        my $uq_dir = uniq_file($dir);
        if (substr($uq_dir, -1) ne "/") { $uq_dir .= "/"; }
        return $uq_dir;
}



### Relative paths
sub _get_link_root($) {
        my $lnk = shift;
        if ($lnk =~ m!^(/|ftp://[^/]*|https?://[^/]*|[a-z]:/)!i) {
                return $1;
        } else {
                return "";
        }
}

sub resolve_dotdot($) {
        my $orig_url = shift;
        my $root = _get_link_root($orig_url);
        return $orig_url if length($root) == length($orig_url);
        my $url = substr($orig_url, length($root));
        if (substr($root, -1) eq "/") {
                chop $root;
                $url = "/$url";
        }
        #die "$root\n$url";
        my $iPosSearch = 2;
        #print "url=$url\n";
        while ((my $iPos = index($url, "/../", $iPosSearch)) > -1) {
                my $sLeft = substr($url, 0, $iPos);
                if (substr($sLeft, -2) eq "..") {
                        $iPosSearch += 3;
                        next;
                }
                my $sRight = substr($url, $iPos+3);
                #print "url=$url\n";
                #print "iPos=$iPos\n";
                #print "sLeft=$sLeft\n";
                $sLeft =~ s!/[^/]*$!!;
                #print "sLeft=$sLeft\n";
                #print "sRight=$sRight\n";
                $url = $sLeft . $sRight;
                #print "\t***url=$url\n";
                #print "url=$url\n";
        }
        if (index($url, "../") > -1) {
                return $orig_url;
        }
        return $root . $url;
}

sub mk_relative_link($$;$) {
    my $from = shift;
    my $to   = shift;
    my $norm = shift;
    if ($norm) {
        $from = uniq_file($from);
        $to   = uniq_file($to);
    }
    if (-e $from) {
        $from = uniq_file($from);
    } else {
        $from = resolve_dotdot($from);
    }
    if (-e $to) {
        $to   = uniq_file($to);
    } else {
        $to = resolve_dotdot($to);
    }
    my $root_from = _get_link_root($from);
    my $root_to   = _get_link_root($to  );
    if ($root_from ne $root_to) {
        return $to;
    }
    my @from = split "/", $from;
    my @to   = split "/", $to;
    while (@to) {
        last if ($to[0] ne $from[0]);
        shift @to;
        shift @from;
    }
    if (@to == 1 && @from == 1) {
        if (length($to[0]) > length($from[0])) {
                        if (substr($to[0], 0, length($from[0])+1) eq ($from[0] . "#")) {
                                return substr($to[0], length($from[0]));
                        }
        }
    }
    my $rl;
    for (1..$#from) { $rl .= "../"; }
    $rl .= join("/", @to);

    return $rl;
}



sub mk_absolute_link($$) {
        my $from   = shift;
        my $rel_to = shift;
        my $abs = $from;
        $abs =~ s![^/]*$!!;
        $abs .= $rel_to;
        if (!is_abs_netpath($abs)) { $abs = uniq_file($abs); }
        $abs;
}


1;
