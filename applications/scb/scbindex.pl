#!/usr/local/bin/perl -w

#+
#  Name:
#     scbindex.pl

#  Purpose:
#     Make index of all Starlink source code files.

#  Language:
#     Perl 5

#  Description:
#     This program attempts to find all the source code files which form
#     part of the USSC.  Given a starting point in the directory tree, 
#     typically /star/sources, it looks underneath it for anything which
#     looks like a source file, specifically files with the following 
#     extensions: .f .gen
#     These files may be sitting 'naked' in one of the subordinate 
#     directories, or may be in a (compressed or uncompressed) tar file,
#     or may be in a tar file within such a tar file.  If more than one
#     instance of a given file is found, the 'most accessible' one is
#     used, i.e. first naked, then in a tar file, then in a tar file 
#     in a tar file.
#
#     When such a file is found, it is searched for program modules
#     which could be referred to by other programs (e.g. functions and
#     subroutines).  For each instance of such a module, the name of 
#     the module and information about its location is written to a 
#     DBM file.
#
#     It also indexes all files, by upper-cased filename rather than
#     module name, in a second specified directory.  This will 
#     typically be /star/include.
#
#     The resulting DBM file can then be read by a separate program to
#     locate the source code for a module, using no information other
#     than its name.

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     24-AUG-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Directory locations.

$srcdir = "/local/star/sources";     # head of source tree
$incdir = "/star/include";     # include directory
$tmpdir = "/local/junk/scb";         # scratch directory

#  Index file location.

$indexfile = "/local/devel/scb/index";

#  Flags.

$verbose = 1;

#  Shell commands required.

$tar = "tar";
$cat = "cat";
$zcat = "uncompress -c";
$gzcat = "gzip -dc";
$generic = "generic";

#  Declarations.

local $path;      # Logical path name used for recording module position.

sub index_list;
sub index_incdir;
sub index_dir;
sub index_tar;
sub index_f;
sub index_gen;
sub pushd;
sub popd;

#  Required libraries.

use Fcntl;
use Cwd;
use SDBM_File;

#  Set up scratch directory.

print "rm -rf $tmpdir\n";
system "rm -rf $tmpdir" and die "Couldn't clean out $tmpdir: $?\n";
system "mkdir -p $tmpdir" and die "Couldn't create $tmpdir: $?\n";

#  Open index file, tied to index hash %locate.

tie %locate, SDBM_File, $indexfile, O_CREAT | O_RDWR, 0644;
%locate = ();

$path = 'INCLUDE/';
chdir $incdir or die "Couldn't enter $incdir\n";
index_incdir '.';

$path = 'SOURCE/';
chdir $srcdir or die "Couldn't enter $srcdir\n";
index_dir '.';

untie %locate;

exit;

####################################################################

#  Examine list of files; identify each file as a naked file of an 
#  interesting type, a tar file, or a directory, and treat accordingly.
#  Arguments are a list of files to be examined and indexed.
#  Must be entered with 
#      $path               logical pathname of the current file
#      current directory   actual directory in which the files reside

sub index_list {

   foreach $file (@_) {
      if (-d $file) { 
         index_dir $file; 
      }
      elsif ($file =~ /\.tar\b/) {
         index_tar $file;
      }
      elsif ($file =~ /\.f$/) {
         index_f $file;
      }
      elsif ($file =~ /\.gen$/) {
         index_gen $file;
      }
   }
}

sub index_incdir {

   my $dir = shift;
   local $path = "$path$dir/";
   pushd $dir;
   indexinc_list (<*>);
   popd;
}

sub indexinc_list {
   my $include;
   foreach $file (@_) {
      $include = undef;
      if ($file =~ /\.h$/) {
         $include = $file;
      }
      elsif ($file !~ /\./) {
         $include = $file;
         $include =~ tr/a-z/A-Z/;
      }
      if ($include) {
         printf "%-20s <-  %s\n", $include, "$path$file" if ($verbose);
         $locate{$include} = "$path$file";
      }
   }
}

#  Service routines to handle each different sort of file. 
#  
sub index_dir {

   my $dir = shift;

   local $path = "$path$dir/";
   pushd $dir;
   index_list <*>;
   popd;
}

sub index_tar {

   my $tarfile = shift;
   local $path = "$path$tarfile>";

   $tarfile = cwd . '/' . $tarfile;

#  Define a (possibly null) decompression filter.

   $tarfile =~ /\.tar(\.?.*)$/;
   my $ext = $1;
   my %filter = ( 
                  ''    => $cat, 
                  '.Z'  => $zcat,
                  '.gz' => $gzcat,
                );

#  Define pipelines for listing and extracting from tar file.

   my $tcomm = "$filter{$ext} $tarfile | $tar tf -";
   my $xcomm = "$filter{$ext} $tarfile | $tar xf -";

#  Unpack tar file.

   pushd $tmpdir;
   system "$xcomm"            and die "Error in command '$xcomm': $?\n";
   open TART, "$tcomm|"       or  die "Error in command '$tcomm'\n";
   my @files;
   while (<TART>) {
      chomp;
      push @files, $_;
   }
   close TART                 or  die "Error closing '$tcomm'\n";

#  Pass list of files to indexing routine.

   index_list @files;
   
#  Tidy up.

   unlink @files               or die "Error removing files\n";
   popd;
}

sub tarlevel {

#  This is a compact, although rather obscure, routine to count the
#  number of '>' characters in a string.  When applied to the logical
#  pathname, a high number indicates that it's buried deep in layers 
#  of tar files, and a low number that it's easier to get to.

   return ($_[0] =~ y/>/>/);
}

sub index_fortran {
   my $file = shift;
   my $global;
   open F, $file or die "Couldn't open $file in directory ".cwd."\n";
   while (<F>) {
      next if (/^[*cC]/);      # Ignore comments.
      chomp;                   # Discard end of line character.
      tr/a-z/A-Z/;             # Fold case to upper.
      s/^......//;             # Discard first six characters.
      s/ //g;                  # Remove spaces.
      if (/^(SUBROUTINE|FUNCTION|ENTRY|BLOCKDATA)([^(]+)/) {
         $global = $2;
         printf "%-20s <-  %s\n", $global, $path if ($verbose);
         $locate{$global} = $path
            unless (   defined ($locate{$global})
                    && tarlevel ($locate{$global}) > tarlevel ($path) );
      }
   }
   close F;
}

sub index_f {
   my $file = shift;
   local $path = "$path$file";
   index_fortran $file;
}

sub index_gen {
   my $file = shift;
   local $path = "$path$file";
   system "$generic $file" and die "Command '$generic $file' failed: $?\n";
   $file =~ s/\.gen$/.f/;
   index_fortran $file;
   unlink $file or die "Failed to remove file $file\n";
}
   

#  Pushd and popd do the same thing as their C-shell namesakes.

sub pushd {
   my $dir = shift;
   push @dirstack, cwd;
   chdir $dir or die "Couldn't change directory to $dir\n";
}

sub popd {
   my $dir = pop @dirstack;
   chdir $dir or die "Couldn't change directory to $dir\n";
}

