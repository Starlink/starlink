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
#     It also indexes all include files from a second specified 
#     directory, by filename (capitalised if it looks like a fortran
#     include) rather than module name.  This will typically be
#     /star/include.
#
#     The location of files is stored in the dbm file as a 'logical
#     pathname'; this resembles an ordinary pathname except that
#     it starts with one of the literals 'SOURCE' or 'INCLUDE',
#     and may contain the special character '>' which indicates
#     inclusion in a (possibly compressed) tar file.  Thus possible
#     logical pathnames would be:

    #           missing.

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
$incdir = "/star/include";           # include directory
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

#  Initialise and open index file, tied to index hash %locate.

unlink "$indexfile.pag", "$indexfile.dir";
tie %locate, SDBM_File, $indexfile, O_CREAT | O_RDWR, 0644;

#  Index include files.

$path = 'INCLUDE/';
chdir $incdir or die "Couldn't enter $incdir\n";
index_incdir '.';
@a = %locate;
print "Include files indexed: ", $#a;

#  Index source files.

$path = 'SOURCE/';
chdir $srcdir or die "Couldn't enter $srcdir\n";
index_dir '.';

#  Terminate processing.

untie %locate;

exit;

########################################################################
#  Subroutines.
########################################################################

########################################################################
sub index_list {

#  Examine and index a list of source files; identify each file as a 
#  naked file of an interesting type, a tar file, or a directory, and 
#  hand off to a routine designed to deal with that type.  
#  Any other files are ignored.
#
#  Arguments are a list of files in the current directory to be 
#  examined and indexed.
#
#  Must be entered with 
#     $path               logical pathname of the current directory.

   foreach $file (@_) {
      if (-d $file) {                 #  directory.
         index_dir $file; 
      }
      elsif ($file =~ /\.tar\b/) {    #  tar archive (possibly compressed).
         index_tar $file;
      }
      elsif ($file =~ /\.f$/) {       #  fortran source file.
         index_f $file;
      }
      elsif ($file =~ /\.gen$/) {     #  generic fortran source file.
         index_gen $file;
      }
   }
}


########################################################################
sub index_dir {

#  Examine and index a directory file.
#
#  Argument is a directory name in the current directory.
#
#  Must be entered with 
#     $path               logical pathname of the current directory.

   my $dir = shift;

   local $path = "$path$dir/";
   pushd $dir;
   index_list <*>;
   popd;
}

########################################################################
sub index_tar {

#  Examine and index a (possibly compressed) tar file.
#
#  Argument is a tarfile name in the current directory.
#
#  Must be entered with 
#     $path               logical pathname of the current directory.

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

########################################################################
sub tarlevel {

#  This is a compact, although rather obscure, routine to count the
#  number of '>' characters in a string.  When applied to the logical
#  pathname, a high number indicates that it's buried deep in layers 
#  of tar files, and a low number that it's easier to get to.

   return ($_[0] =~ y/>/>/);
}

########################################################################
sub index_fortran {

#  Examine and index a fortran source file.
#
#  Argument is a fortran source file name in the current directory.
#
#  Must be entered with 
#     $path               logical pathname of the current FILE (not dir).

#  Note the parsing is not perfect - it would be fooled for instance
#  by a subroutine definition in which the name of the subroutine was
#  split between continuation lines.

   my $file = shift;
   my $global;
   my @ftypes = qw/INTEGER REAL DOUBLEPRECISION COMPLEX LOGICAL
                    CHARACTER BYTE UBYTE WORD UWORD/;
   my $ftypdef = '(' . join ('|', @ftypes) . ')\**(\(.*\))?[0-9]*';

#  Open source file and cycle through it.

   open F, $file or die "Couldn't open $file in directory ".cwd."\n";
   while (<F>) {

#     Strip lines of syntactically uninteresting parts.

      next if (/^[*cC]/);               # Ignore F77 comments.
      s/!.*$//;                         # Discard inline comments.
      chomp;                            # Discard end of line character.
      s/^......//;                      # Discard first six characters.
      s/ //g;                           # Remove spaces.
      tr/a-z/A-Z/;                      # Fold case to upper.
      s/^$ftypdef//o if (/FUNCTION/);   # Discard leading type specifiers.

#     Identify lines containing global module names.

      if (/^(SUBROUTINE|FUNCTION|ENTRY|BLOCKDATA)([^(]+)/) {
         $global = $2;
         printf "%-20s <-  %s\n", $global, $path if ($verbose);

#        Index except where a more accessible path has already been found.

         $locate{$global} = $path
            unless (   defined ($locate{$global})
                    && tarlevel ($locate{$global}) > tarlevel ($path) );
      }
   }
   close F;
}

########################################################################
sub index_f {

#  Examine and index a fortran source file.
#
#  Argument is a fortran source file name in the current directory.
#
#  Must be entered with 
#     $path               logical pathname of the current directory.

   my $file = shift;
   local $path = "$path$file";
   index_fortran $file;
}

########################################################################
sub index_gen {

#  Examine and index a .gen (fortran Generic) source file.
#
#  Argument is a generic source file name in the current directory.
#
#  Must be entered with 
#     $path               logical pathname of the current directory.

   my $file = shift;
   local $path = "$path$file";
   system "$generic $file" and die "Command '$generic $file' failed: $?\n";
   $file =~ s/\.gen$/.f/;
   index_fortran $file;
   unlink $file or die "Failed to remove file $file\n";
}

########################################################################
sub index_incdir {

#  Examine and index a directory of files containing include files.

   my $dir = shift;
   local $path = "$path$dir/";
   pushd $dir;
   indexinc_list (<*>);
   popd;
}

########################################################################
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
   


########################################################################
sub pushd {

#  Pushd does the same thing as its C-shell namesake.

   my $dir = shift;
   push @dirstack, cwd;
   chdir $dir or die "Couldn't change directory to $dir\n";
}

########################################################################
sub popd {

#  Popd does the same thing as its C-shell namesake.

   my $dir = pop @dirstack;
   chdir $dir or die "Couldn't change directory to $dir\n";
}

