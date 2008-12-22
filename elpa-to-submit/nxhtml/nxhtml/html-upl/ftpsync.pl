#!/usr/bin/perl
#
# ftpsync.pl
#
# See attached README file for any details, or call
# ftpsync.pl -h
# for quick start.
#
# LICENSE
#
#    FTPSync.pl (ftpsync) is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# FTPSync.pl (ftpsync) is also eMail-Ware, which means that the initial author
# (Christoph Lechleitner) would like to get an eMail at ftpsync@ibcl.at, if
# - if anyone uses the script on production level,
# - if anyone distributes or advertises it in any way,
# - if anyone starts to (try to) improve it.
#
#
################################################################################

#
# Options etc.
#
#print "Starting imports.\n"; # For major problem debugging
printf STDERR "argv=@ARGV\n";

use File::Find;
use File::Listing;
use Net::FTP;
use strict;
# flushing ...
use IO::Handle;
STDOUT->autoflush(1);
STDERR->autoflush(1);

sub dosync();
sub print_syntax();
sub print_options();
sub buildremotetree();
sub buildlocaltree();
sub listremotedirs();
sub parseRemoteURL();

# Option Variables
#print "Defining variables.\n"; # For major problem debugging
# meta
my $returncode=0;
my $configfile=$ENV{"HOME"}."/.ftpsync";
# basics
my $localdir="";
my $remoteURL="";
my $syncdirection="";
my $ftpuser="ftp";
my $ftppasswd="anonymous";
my $ftpserver="localhost";
my $ftpdir="";
my $ftptimeout=120;
my $syncoff=0;
# verbosity
my $doverbose=1;
my $dodebug=0;
my $doquiet=0;
my $doinfoonly=0;
my $infotext="";
my $docheckfirst=0;

# Read command line options/parameters
#print "Reading command line options.\n"; # For major problem debugging
my $curopt;
my @cloptions=();
for $curopt (@ARGV) {
  if ($curopt =~ /^cfg=/) {
    $configfile=$';
    if (! -r $configfile) { print "Config file does not exist: ".$configfile."\n"; $returncode+=1; }
  } else {
    push @cloptions, $curopt;
  }
}

# Read Config File, if given
my @cfgfoptions=();
if ($configfile ne "") {
  if (-r $configfile) {
    #print "Reading config file.\n"; # For major problem debugging
    open (CONFIGFILE,"<$configfile");
    while (<CONFIGFILE>) {
      $_ =~ s/([ 	\n\r]*$|\.\.|#.*$)//gs;
      if ($_ eq "") { next; }
      if ( ($_ =~ /[^=]+=[^=]+/) || ($_ =~ /^-[a-zA-Z]+$/) ) { push @cfgfoptions, $_; }
    }
    close (CONFIGFILE);
  } # else { print "Config file does not exist.\n"; } # For major problem debugging
} # else { print "No config file to read.\n"; } # For major problem debugging

# Parse Options/Parameters
print "Parsing all options.\n"; # For major problem debugging
my $noofopts=0;
for $curopt (@cfgfoptions, @cloptions) {
  if ($curopt =~ /^-[a-zA-Z]/) {
    my $i;
    for ($i=1; $i<length($curopt); $i++) {
      my $curoptchar=substr($curopt,$i,1);
      $noofopts++;
      if    ($curoptchar =~ /[cC]/)  { $docheckfirst=1; }
      elsif ($curoptchar =~ /[dD]/)  { $dodebug=1; $doverbose=1; $doquiet=0; }
      elsif ($curoptchar =~ /[gG]/)  { $syncdirection="get"; }
      elsif ($curoptchar =~ /[hH?]/) { print_syntax(); exit 0; }
      elsif ($curoptchar =~ /[iI]/)  { $doinfoonly=1; }
      elsif ($curoptchar =~ /[pP]/)  { $syncdirection="put"; }
      elsif ($curoptchar =~ /[qQ]/)  { $dodebug=0; $doverbose=0; $doquiet=1; }
      elsif ($curoptchar =~ /[vV]/)  { $doverbose++; }
      else  { print "ERROR: Unknown option: \"-".$curoptchar."\"\n"; $returncode+=1; }
    }
  }
  elsif ($curopt =~ /^ftp:\/\/(([^@\/\\\:]+)(:([^@\/\\\:]+))?@)?([a-zA-Z01-9\.]+)\/(.*)/) {
    $remoteURL = $curopt;
    parseRemoteURL();
    if ( $syncdirection eq "" )
    { $syncdirection="get"; }
  }
  elsif ($curopt =~ /^[a-z]+=.+/) {
    my ($fname, $fvalue) = split /=/, $curopt, 2;
    if    ($fname eq "cfg")       { next; }
    elsif ($fname eq "ftpdir")    { $ftpdir     =$fvalue;
                                    if ($ftpdir ne "/") { $ftpdir=~s/\/$//; }
                                    if ( $syncdirection eq "" ) { $syncdirection="get"; }
                                  }
    elsif ($fname =~ m/ftppass(w(or)?d)?/i)
                                  { $ftppasswd=$fvalue;
                                    if ( $syncdirection eq "" ) { $syncdirection="get"; }
                                  }
    elsif ($fname eq "ftpserver") { $ftpserver  =$fvalue;
                                    if ( $syncdirection eq "" ) { $syncdirection="get"; }
                                  }
    elsif ($fname eq "ftpuser")   { $ftpuser    =$fvalue;
                                    if ( $syncdirection eq "" ) { $syncdirection="get"; }
                                  }
    elsif ($fname eq "localdir")  { $localdir   =$fvalue; $localdir=~s/\/$//;
                                    if ( $syncdirection eq "" ) { $syncdirection="put"; }
                                  }
    elsif ($fname eq "timeout")   { if ($fvalue>0) { $ftptimeout =$fvalue; } }
  }
  else {
    if ($localdir eq "") {
      $localdir = $curopt;
      if ( $syncdirection eq "" )
      { $syncdirection="put"; }
    } else {
      print "ERROR: Unknown parameter: \"".$curopt."\"\n"; $returncode+=1
    }
  }
}
if ($noofopts == 0) { print_syntax(); exit 0; }

if($ftpuser   eq "?") { print "User: ";     $ftpuser=<STDIN>;   chomp($ftpuser);   }
if($ftppasswd eq "?") { print "Password: "; $ftppasswd=<STDIN>; chomp($ftppasswd); }

if ($dodebug) { print_options(); }
# check options
if ( ($localdir  eq "") || (! -d $localdir) )
{ print "ERROR: Local directory does not exist: ".$localdir."\n"; $returncode+=1; }
#if ($localdir  eq "")   { print "ERROR: No localdir given.\n";  $returncode+=1; }
#if ( ($remoteURL eq "") { print "ERROR: No remoteURL given.\n"; $returncode+=1; }
if ($ftpserver eq "") { print "ERROR: No FTP server given.\n"; $returncode+=1; }
if ($ftpdir    eq "") { print "ERROR: No FTP directory given.\n"; $returncode+=1; }
if ($ftpuser   eq "") { print "ERROR: No FTP user given.\n"; $returncode+=1; }
if ($ftppasswd eq "") { print "ERROR: No FTP password given.\n"; $returncode+=1; }
if ($returncode > 0) { die "Aborting due to missing or wrong options! Call ftpsync -? for more information.\n";  }


#print "Exiting.\n"; exit 0;

if ($dodebug) { print "\nFind out if ftp server is online & accessible.\n"; }
my $doftpdebug=($doverbose > 2);
my $ftpc = Net::FTP->new($ftpserver,Debug=>$doftpdebug,Timeout=>$ftptimeout) || die "Could not connect to $ftpserver\n";
if ($dodebug) { print "Logging in as $ftpuser with password $ftppasswd.\n" }
$ftpc->login($ftpuser,$ftppasswd) || die "Could not login to $ftpserver as $ftpuser\n";
my $ftpdefdir=$ftpc->pwd();
if ($dodebug) { print "Remote directory is now ".$ftpdefdir."\n"; }
if ($ftpdir !~ /^\//) # insert remote login directory into relative ftpdir specification
{ if ($ftpdefdir eq "/")
  { $ftpdir = $ftpdefdir . $ftpdir; }
  else
  { $ftpdir = $ftpdefdir . "/" . $ftpdir; }
  if (!$doquiet)
  { print "Absolute remote directory is $ftpdir\n"; }
}
if (substr($ftpdir, -1) eq "/") {
  if (!$doquiet)
  { print "  Remote directory ends in /, removing this\n"; }
  chop($ftpdir);
}
if ($dodebug) { print "Changing to remote directory $ftpdir.\n" }
$ftpc->binary()
    or die "Cannot set binary mode :\n\t" . $ftpc->message;
$ftpc->cwd($ftpdir)
    or die "Cannot cwd to $ftpdir :\n\t" . $ftpc->message;
if ($ftpc->pwd() ne $ftpdir) {
  my $pwd = $ftpc->pwd();
  die "Could not change to remote base directory $ftpdir (at $pwd)\n"; }
if ($dodebug) { print "Remote directory is now ".$ftpc->pwd()."\n"; }

if (! $doquiet) { print "\nDetermine s offset.\n"; }
if ($syncdirection eq "put") { clocksync($ftpc,"syncfile"); }

#  local & remote tree vars
#chdir $localdir;
my $ldl=length($localdir) + 1;
#my $ldl=length($localdir);
my %localfiledates=();
my %localfilesizes=();
my %localdirs=();
my %locallinks=();

my %remotefilesizes=();
my %remotefiledates=();
my %remotedirs=();
my %remotelinks=();
my $curremotesubdir="";

# Build local & remote tree
if (! $doquiet) { print "\nBuilding local file tree.\n"; }
buildlocaltree();
if (! $doquiet) { print "\nBuilding remote file tree.\n"; }
buildremotetree();
listremotedirs();
#if ($dodebug) { print "Quitting FTP connection.\n" }
#$ftpc->quit();

#print "Exiting.\n"; exit 0;

# Work ...
if ($doinfoonly) { $docheckfirst=0; }
if ($docheckfirst)
{ print "Simulating synchronization.\n";
  $doinfoonly=1;
  dosync();
  $doinfoonly=0;
  print "\nOK to really update files? (y/n) [n] ";
  my $yn=<STDIN>;
  if ($yn =~ /^y/i)
  { print "OK, going to do it.\n";
  }
  else
  { print "OK, exiting without actions.\n";
    exit 1;
  }
}
if ($doinfoonly)   { print "\nSimulating synchronization.\n"; }
elsif (! $doquiet) { print "\nStarting synchronization.\n"; }
dosync();

if (!$doquiet) { print "Done.\n"; }

if ($dodebug) { print "Quitting FTP connection.\n" }
$ftpc->quit();

exit 0;



#
# Subs
#

sub buildlocaltree() {
  find (\&noticelocalfile, $localdir."/");
  sub noticelocalfile {
    if ($ldl > length($File::Find::name)) { return; }
    #printf "name=%s, length(name)=%d, ldl=$ldl\n", $File::Find::name, length($File::Find::name);
    my $relfilename=substr($File::Find::name,$ldl);
    if (length($relfilename) == 0) { return; }
    if (-d $_) {
      if ($dodebug) { print "Directory: ".$File::Find::name."\n"; }
      elsif (! $doquiet) { print ":"; }
      $localdirs{$relfilename}="$relfilename";
    }
    elsif (-f $_) {
      #my @curfilestat=lstat $File::Find::name;
      my @curfilestat=lstat $_;
      my $curfilesize=$curfilestat[7];
      my $curfilemdt=$curfilestat[9];
      if ($dodebug) { print "File: ".$File::Find::name."\n";
                      print "Modified ".$curfilemdt."\nSize ".$curfilesize." bytes\n"; }
      elsif (! $doquiet) { print "."; }
      $localfiledates{$relfilename}=$curfilemdt;
      $localfilesizes{$relfilename}=$curfilesize;
    }
    elsif (-l $_) {
      if ($dodebug) { print "Link: ".$File::Find::name."\n"; }
      elsif (! $doquiet) { print ","; }
      $locallinks{$relfilename}="$relfilename";
    } else {
      #print "u ".$File::Find::name."\n";
      if (! $doquiet) { print "Ignoring file of unknown type: ".$File::Find::name."\n"; }
    }
    #if (! ($doquiet || $dodebug)) { print "\n"; }
    #print "File mode is ".@curfilestat[2]."\n";
  }
  if ($dodebug) {
    print "Local dirs (relative to ".$localdir."/):\n";
    my $curlocaldir="";
    foreach $curlocaldir (keys(%localdirs))
    { print $curlocaldir."/\n"; }
    print "Local files (relative to ".$localdir."/):\n";
    my $curlocalfile="";
    foreach $curlocalfile (keys(%localfiledates))
    { print $curlocalfile."\n"; }
  }
}


sub buildremotetree() {
  my @currecursedirs=();
  #$ftpc->ls()
  #  or die $ftpc->message . "\nCannot ls remote dir " . $ftpc->pwd();
  my @rfl = $ftpc->dir();
  # or @rfl=(); # we have to survive empty remote directories !!!
  my $currf="";
  my $curyear = (gmtime(time))[5] + 1900;
  my %monthtonr=();
  $monthtonr{"Jan"}=1; $monthtonr{"Feb"}=2; $monthtonr{"Mar"}=3; $monthtonr{"Apr"}=4; $monthtonr{"May"}=5; $monthtonr{"Jun"}=6;
  $monthtonr{"Jul"}=7; $monthtonr{"Aug"}=8; $monthtonr{"Sep"}=9; $monthtonr{"Oct"}=10; $monthtonr{"Nov"}=11; $monthtonr{"Dec"}=12;
  if ($dodebug) { print "Remote pwd is ".$ftpc->pwd()."\nDIRing.\n"; }
  my $curlsline;
  foreach $curlsline (parse_dir(\@rfl)) {
    my ($cfname,$cftype,$cfsize,$cftime,$mode)=@$curlsline;
    #if ($dodebug) { print "Analysing remote file/dir ".$currf."\n" };
    if ( $cftype ) {
      if ($cfname eq ".") { next; }
      if ($cfname eq "..") { next; }
      if (substr($cftype,0,1) eq 'l') {  # link, rest of string = linkto
        my $curnrl;
        if ($curremotesubdir eq "") { $curnrl = $cfname; }
        else                        { $curnrl = $curremotesubdir."/".$cfname; }
        $remotelinks{$curnrl}=$cfname;
        if ($dodebug) { print "Link: ".$curnrl." -> ".$cfname."\n"; }
      }
      elsif ($cftype eq 'd') {
        my $curnewrsd;
        if ($curremotesubdir eq "") { $curnewrsd = $cfname; }
        else                        { $curnewrsd = $curremotesubdir."/".$cfname; }
        $remotedirs{$curnewrsd}=$curnewrsd;
        if ($dodebug) { print "Directory: ".$curnewrsd."\n"; }
        elsif (! $doquiet) { print ":"; }
        push @currecursedirs, $cfname;
      }
      elsif ($cftype eq 'f') {  #plain file
        my $curnewrf;
        if ($curremotesubdir eq "") { $curnewrf = $cfname; }
        else                        { $curnewrf = $curremotesubdir."/".$cfname; }
        #$remotefiledates{$curnewrf}=$cftime;
        $remotefiledates{$curnewrf}=$ftpc->mdtm($cfname)+$syncoff;
        if ($remotefiledates{$curnewrf} le 0) { die "Timeout detecting modification time of $curnewrf\n"; }
        $remotefilesizes{$curnewrf}=$cfsize;
        if ($remotefilesizes{$curnewrf} lt 0) { die "Timeout detecting size of $curnewrf\n"; }
        if ($dodebug) { print "File: ".$curnewrf."\n"; }
        elsif (! $doquiet) { print "."; }
      }
      elsif (! $doquiet) { print "Unkown file: $curlsline\n"; }
    }
    elsif ($dodebug) { print "Ignoring.\n"; }
  }
  #recurse
  my $currecurseddir;
  foreach $currecurseddir (@currecursedirs)
  { my $oldcurremotesubdir;
    $oldcurremotesubdir=$curremotesubdir;
    if ($curremotesubdir eq "") { $curremotesubdir = $currecurseddir; }
    else                        { $curremotesubdir .= "/".$currecurseddir; }
    my $curcwddir="";
    if ($ftpdir eq "/")
    { $curcwddir=$ftpdir.$curremotesubdir; }
    else
    { $curcwddir=$ftpdir."/".$curremotesubdir; }
    if ($dodebug) { print "Change dir: ".$curcwddir."\n"; }
    $ftpc->cwd($curcwddir)
      or die "Cannot cwd to $curcwddir :\n\t" . $ftpc->message ;
    if ($ftpc->pwd() ne $curcwddir) {
      die "Could not cwd to $curcwddir :\n\t" . $ftpc->message ; }
    if (! $doquiet) { print "\n"; }
    buildremotetree();
    $ftpc->cdup();
    $curremotesubdir = $oldcurremotesubdir;
  }
}


# Synchronize clocks.
sub clocksync {
    my $conn = shift @_;
    my $fn = shift @_;
    my $fndidexist=1;

    if(! -f $fn) {
      open(SF, ">$fn") or die "Cannot create $fn for time sync option";
      close(SF);
      $fndidexist=0;
    }
    -z $fn or
      die "File $fn for time sync must be empty.";
    my $putsyncok=1;
    $conn->put($fn) or $putsyncok=0;
    if (!$putsyncok)
    { unlink($fn);  # cleanup!
      die "Cannot send timesync file $fn";
    }

    my $now_here1 = time();
    my $now_there = $conn->mdtm($fn) or
      die "Cannot get write time of timesync file $fn";
    my $now_here2 = time();

    if ($now_here2 < $now_there)      # remote is in the future
    { $syncoff=($now_there - $now_here1);
      $syncoff -= $syncoff % 60;
      $syncoff = 0-$syncoff;
    }
    else
    #if ($now_here1 > $now_there)      # remote is the past # or equal
    { $syncoff=($now_here2 - $now_there);
      $syncoff -= $syncoff % 60;
    }

    $conn->delete($fn);

    my $hrs = int(abs($syncoff)/3600);
    my $mins = int(abs($syncoff)/60) - $hrs*60;
    my $secs = abs($syncoff) - $hrs*3600 - $mins*60;
    if (! $doquiet) {
      printf("Clock sync offset: %d:%02d:%02d\n", $hrs, $mins, $secs);
    }
    unlink ($fn) unless $fndidexist;
}


sub dosync()
{
  chdir $localdir || die "Could not change to local base directory $localdir\n";
  if ($syncdirection eq "put") {
    # create dirs missing at the target
    if    ($doinfoonly) { print "\nWould create new remote directories.\n"; }
    elsif (! $doquiet)  { print "\nCreating new remote directories.\n"; }
    my $curlocaldir;
    foreach $curlocaldir (sort { return length($a) <=> length($b); } keys(%localdirs))
    { if (! exists $remotedirs{$curlocaldir})
      { if ($doinfoonly) { print $curlocaldir."\n"; next; }
        if ($doverbose)  { print $curlocaldir."\n"; }
        elsif (! $doquiet) { print "d"; }
        if ($ftpc->mkdir($curlocaldir) ne $curlocaldir) { die "Could not create remote subdirectory $curlocaldir\n"; }
      }
    }
    # copy files missing or too old at the target, synchronize timestamp _after_ copying
    if    ($doinfoonly) { print "\nWould copy new(er) local files.\n"; }
    elsif (! $doquiet)  { print "\nCopying new(er) local files.\n"; }
    my $curlocalfile;
    foreach $curlocalfile (sort { return length($b) <=> length($a); } keys(%localfiledates))
    { my $dorefresh=0;
      if    (! exists $remotefiledates{$curlocalfile}) {
        $dorefresh=1;
        $infotext="New: ".$curlocalfile." (".$localfilesizes{$curlocalfile}." bytes)\n";
        if ($doinfoonly)   { print $infotext; next; }
        elsif ($doverbose) { print $infotext; }
        elsif (! $doquiet) { print "n"; }
      }
      elsif ($remotefiledates{$curlocalfile} < $localfiledates{$curlocalfile}) {
        $dorefresh=1;
        $infotext="Newer: ".$curlocalfile." (".$localfilesizes{$curlocalfile}." bytes, ".$localfiledates{$curlocalfile}." versus ".$remotefiledates{$curlocalfile}.")\n";
        if ($doinfoonly) { print $infotext; next; }
        if ($doverbose)  { print $infotext; }
        elsif (! $doquiet) { print "u"; }
      }
      elsif ($remotefilesizes{$curlocalfile} != $localfilesizes{$curlocalfile}) {
        $dorefresh=1;
        $infotext="Changed (different sized): ".$curlocalfile." (".$localfilesizes{$curlocalfile}."  versus ".$remotefilesizes{$curlocalfile}." bytes)\n";
        if ($doinfoonly) { print $infotext; next; }
        if ($doverbose)  { print $infotext; }
        elsif (! $doquiet) { print "u"; }
      }
      if (! $dorefresh) { next; }
      if ($dodebug) { print "Really PUTting file ".$curlocalfile."\n"; }
      if ($ftpc->put($curlocalfile, $curlocalfile) ne $curlocalfile)
      { print STDERR "Could not put localfile $curlocalfile\n"; }
      my $retries = 3;
      while ( ($ftpc->size($curlocalfile) != (lstat $curlocalfile)[7]) and ($retries-- > 0) )
      { if (! $doquiet) { print "Re-Transfering $curlocalfile\n"; }
        if ($ftpc->put($curlocalfile, $curlocalfile) ne $curlocalfile)
        { print STDERR "Could not re-put localfile $curlocalfile\n"; }
      }
      my $newremotemdt=$ftpc->mdtm($curlocalfile)+$syncoff;
      utime ($newremotemdt, $newremotemdt, $curlocalfile);
    }
    # delete files too much at the target
    if    ($doinfoonly) { print "\nWould delete obsolete remote files.\n"; }
    elsif (! $doquiet)  { print "\nDeleting obsolete remote files.\n"; }
    my $curremotefile;
    foreach $curremotefile (keys(%remotefiledates))
    { if (not exists $localfiledates{$curremotefile})
      { if ($doinfoonly) { print $curremotefile."\n"; next; }
        if ($doverbose)  { print $curremotefile."\n"; }
        elsif (! $doquiet) { print "r"; }
        if ($ftpc->delete($curremotefile) ne 1) { die "Could not delete remote file $curremotefile\n"; }
      }
    }
    # delete dirs too much at the target
    if    ($doinfoonly) { print "\nWould delete obsolete remote directories.\n"; }
    elsif (! $doquiet)  { print "\nDeleting obsolete remote directories.\n"; }
    my $curremotedir;
    foreach $curremotedir (sort { return length($b) <=> length($a); } keys(%remotedirs))
    { if (! exists $localdirs{$curremotedir})
      { if ($doinfoonly) { print $curremotedir."\n"; next; }
        if ($doverbose)  { print $curremotedir."\n"; }
        elsif (! $doquiet) { print "R"; }
        if ($ftpc->rmdir($curremotedir) ne 1) { die "Could not remove remote subdirectory $curremotedir\n"; }
      }
    }
  } else { # $syncdirection eq "GET"
    # create dirs missing at the target
    if    ($doinfoonly) { print "\nWould create new local directories.\n"; }
    elsif (! $doquiet)  { print "\nCreating new local directories.\n"; }
    my $curremotedir;
    foreach $curremotedir (sort { return length($a) <=> length($b); } keys(%remotedirs))
    { if (! exists $localdirs{$curremotedir})
      { if ($doinfoonly) { print $curremotedir."\n"; next; }
        if ($doverbose)  { print $curremotedir."\n"; }
        elsif (! $doquiet) { print "d"; }
        mkdir($curremotedir) || die "Could not create local subdirectory $curremotedir\n";
      }
    }
    # copy files missing or too old at the target, synchronize timestamp _after_ copying
    if    ($doinfoonly) { print "\nWould copy new(er) remote files.\n"; }
    elsif (! $doquiet)  { print "\nCopying new(er) remote files.\n"; }
    my $curremotefile;
    foreach $curremotefile (sort { return length($b) <=> length($a); } keys(%remotefiledates))
    { my $dorefresh=0;
      if    (! exists $localfiledates{$curremotefile}) {
        $dorefresh=1;
        $infotext="New: ".$curremotefile." (".$remotefilesizes{$curremotefile}." bytes)\n";
        if ($doinfoonly) { print $infotext; next; }
        if ($doverbose)  { print $infotext; }
        elsif (! $doquiet) { print "n"; }
      }
      elsif ($remotefiledates{$curremotefile} > $localfiledates{$curremotefile}) {
        $dorefresh=1;
        $infotext="Newer: ".$curremotefile." (".$remotefilesizes{$curremotefile}." bytes, ".$remotefiledates{$curremotefile}." versus ".$localfiledates{$curremotefile}.")\n";
        if ($doinfoonly) { print $infotext; next; }
        if ($doverbose)  { print $infotext; }
        elsif (! $doquiet) { print "u"; }
      }
      elsif ($remotefilesizes{$curremotefile} != $localfilesizes{$curremotefile}) {
        $dorefresh=1;
        $infotext="Changed (different sized): ".$curremotefile." (".$remotefilesizes{$curremotefile}." bytes)\n";
        if ($doinfoonly) { print $infotext; next; }
        if ($doverbose)  { print $infotext; }
        elsif (! $doquiet) { print "c"; }
      }
      if (! $dorefresh) { next; }
      if ($dodebug) { print "Really GETting file ".$curremotefile."\n"; }
      my $rc=$ftpc->get($curremotefile, $curremotefile);
      if ( ($rc eq undef) or ($rc ne $curremotefile) )
      { print STDERR "Could not get file ".$curremotefile."\n"; }
      my $retries=3;
      while ( ($ftpc->size($curremotefile) != (lstat $curremotefile)[7]) and ($retries-- > 0) )
      { if (! $doquiet) { print "Re-Transfering $curremotefile\n"; }
        if ( ($rc eq undef) or ($rc ne $curremotefile) )
        { print STDERR "Could not get file ".$curremotefile."\n"; }
      }
      my $newlocalmdt=$remotefiledates{$curremotefile};
      utime ($newlocalmdt, $newlocalmdt, $curremotefile);
    }
    # delete files too much at the target
    if    ($doinfoonly) { print "\nWould delete obsolete local files.\n"; }
    elsif (! $doquiet)  { print "\nDeleting obsolete local files.\n"; }
    my $curlocalfile;
    foreach $curlocalfile (sort { return length($b) <=> length($a); } keys(%localfiledates))
    { if (not exists $remotefiledates{$curlocalfile})
      { if ($doinfoonly) { print $curlocalfile."\n"; next; }
        if ($doverbose)  { print $curlocalfile."\n"; }
        elsif (! $doquiet) { print "r"; }
        if (unlink($curlocalfile) ne 1) { die "Could not remove local file $curlocalfile\n"; }
      }
    }
    # delete dirs too much at the target
    if    ($doinfoonly) { print "\nWould delete obsolete local directories.\n"; }
    elsif (! $doquiet)  { print "\nDeleting obsolete local directories.\n"; }
    my $curlocaldir;
    foreach $curlocaldir (keys(%localdirs))
    { if (! exists $remotedirs{$curlocaldir})
      { if ($doinfoonly) { print $curlocaldir."\n"; next; }
        if ($doverbose)  { print $curlocaldir."\n"; }
        elsif (! $doquiet) { print "d"; }
        rmdir($curlocaldir) || die "Could not remove local subdirectory $curlocaldir\n";
      }
    }
  }
}


sub listremotedirs() {
  if ($dodebug) {
    print "Remote dirs (relative to ".$ftpdir."):\n";
    my $curremotedir="";
    foreach $curremotedir (keys(%remotedirs))
    { print $curremotedir."/\n"; }
    print "Remote files (relative to ".$ftpdir."):\n";
    my $curremotefile="";
    foreach $curremotefile (keys(%remotefiledates))
    { print $curremotefile."\n"; }
    print "Remote links (relative to ".$ftpdir."):\n";
    my $curremotelink="";
    foreach $curremotelink (keys(%remotelinks))
    { print $curremotelink." -> ".$remotelinks{$curremotelink}."\n"; }
  }
}
sub parseRemoteURL() {
  if ($remoteURL =~ /^ftp:\/\/(([^@\/\\\:]+)(:([^@\/\\\:]+))?@)?([a-zA-Z01-9\.]+)\/(.*)/) {
    #print "DEBUG: parsing ".$remoteURL."\n";
    #print "match 1 = ".$1."\n";
    #print "match 2 = ".$2."\n";
    #print "match 3 = ".$3."\n";
    #print "match 4 = ".$4."\n";
    #print "match 5 = ".$5."\n";
    #print "match 6 = ".$6."\n";
    #print "match 7 = ".$7."\n";
    if (length($2) > 0) { $ftpuser=$2; }
    if (length($4) > 0) { $ftppasswd=$4; }
    $ftpserver=$5;
    $ftpdir=$6;
    #if ($ftpdir eq "") { $ftpdir="/"; }
  }
}


sub print_syntax() {
  print "\n";
  print "FTPSync.pl 1.27 (2004-08-23)\n";
  print "\n";
  print " ftpsync [ options ] [ localdir remoteURL ]\n";
  print " ftpsync [ options ] [ remoteURL localdir ]\n";
  print " options = [-dgpqv] [ cfg|ftpuser|ftppasswd|ftpserver|ftpdir=value ... ] \n";
  print "   localdir    local directory, defaults to \".\".\n";
  print "   ftpURL      full FTP URL, scheme\n";
  print '               ftp://[ftpuser[:ftppasswd]@]ftpserver/ftpdir'."\n";
  print "               ftpdir is relative, so double / for absolute paths as well as /\n";
  print "   -c | -C     like -i, but then prompts whether to actually do work\n";
  print "   -d | -D     turns debug output (including verbose output) on\n";
  print "   -g | -G     forces sync direction to GET (remote to local)\n";
  print "   -h | -H     turns debugging on\n";
  print "   -i | -I     forces info mode, only telling what would be done\n";
  print "   -p | -P     forces sync direction to PUT (local to remote)\n";
  print "   -q | -Q     turnes quiet operation on\n";
  print "   -v | -V     turnes verbose output on\n";
  print "   cfg=        read parameters and options from file defined by value.\n";
  print "   ftpserver=  defines the FTP server, defaults to \"localhost\".\n";
  print "   ftpdir=     defines the FTP directory, defaults to \".\" (/wo '\"') \n";
  print "   ftpuser=    defines the FTP user, defaults to \"ftp\".\n";
  print "   ftppasswd=  defines the FTP password, defaults to \"anonymous\".\n";
  print "\n";
  print " Later mentioned options and parameters overwrite those mentioned earlier.\n";
  print " Command line options and parameters overwrite those in the config file.\n";
  print " Don't use '\"', although mentioned default values might motiviate you to.\n";
  print "\n";
}


sub print_options() {
  print "\nPrinting options:\n";
  # meta
  print "returncode    = ", $returncode    , "\n";
  print "configfile    = ", $configfile    , "\n";
  # basiscs
  print "syncdirection = ", $syncdirection , "\n";
  print "localdir      = ", $localdir      , "\n";
  # FTP stuff
  print "remoteURL     = ", $remoteURL     , "\n";
  print "ftpuser       = ", $ftpuser       , "\n";
  print "ftppasswd     = ", $ftppasswd     , "\n";
  print "ftpserver     = ", $ftpserver     , "\n";
  print "ftpdir        = ", $ftpdir        , "\n";
  # verbsityosity
  print "doverbose     = ", $doverbose     , "\n";
  print "dodebug       = ", $dodebug       , "\n";
  print "doquiet       = ", $doquiet       , "\n";
  #
  print "doinfoonly    = ", $doinfoonly    , "\n";
  print "\n";
}
