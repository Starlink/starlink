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
#     inclusion in a (possibly compressed) tar file.  Thus some
#     example logical pathnames would be:
#
#         SOURCE/ast.tar.Z>ast_source.tar>frame.f
#         SOURCE/figaro/figaro_applic.tar>applic/bclean.f
#         INCLUDE/par_err
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

sub index_list;
sub index_incdir;
sub index_dir;
sub index_tar;
sub index_f;
sub index_gen;
sub indexinc_list;
sub write_entry;
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

chdir $incdir or die "Couldn't enter $incdir\n";
index_incdir "INCLUDE/", ".";

#  Index source files.

chdir $srcdir or die "Couldn't enter $srcdir\n";
index_dir "SOURCE/", ".";

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

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my @files = @_;        #  Files in current directory to be indexed.

#  Local variables.

   my $file;

   foreach $file (@files) {
      if (-d $file) {                 #  directory.
         index_dir $path, $file; 
      }
      elsif ($file =~ /\.tar\b/) {    #  tar archive (possibly compressed).
         index_tar $path, $file;
      }
      elsif ($file =~ /\.f$/) {       #  fortran source file.
         index_f $path, $file;
      }
      elsif ($file =~ /\.gen$/) {     #  generic fortran source file.
         index_gen $path, $file;
      }
   }
}


########################################################################
sub index_dir {

#  Examine and index a directory file.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $dir = shift;       #  Directory in current directory to be indexed.

   pushd $dir;
   index_list "$path$dir/", <*>;
   popd;
}

########################################################################
sub index_tar {

#  Examine and index a (possibly compressed) tar file.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $tarfile = shift;   #  Tarfile in current directory to be indexed.

#  Define a (possibly null) decompression filter.

   $tarfile =~ /\.tar(\.?.*)$/;
   my $ext = $1;
   my %filter = ( 
                  ''    => $cat, 
                  '.Z'  => $zcat,
                  '.gz' => $gzcat,
                );

#  Define pipelines for listing and extracting from tar file.

   my $fqtarfile = cwd . '/' . $tarfile;
   my $tcomm = "$filter{$ext} $fqtarfile | $tar tf -";
   my $xcomm = "$filter{$ext} $fqtarfile | $tar xf -";

#  Unpack tar file.

   pushd $tmpdir;
   system "$xcomm"        and die "Error in command '$xcomm': $?\n";
   open TART, "$tcomm|"   or  die "Error in command '$tcomm'\n";
   my @files;
   while (<TART>) {
      chomp;
      push @files, $_;
   }
   close TART             or  die "Error closing '$tcomm'\n";

#  Pass list of files to indexing routine.

   index_list "$path$tarfile>", @files;
   
#  Tidy up.

   unlink @files               or die "Error removing files\n";
   popd;
}

########################################################################
sub index_fortran_file {

#  Examine and index a fortran source file.
#
#  Note the parsing is not perfect - it would be fooled for instance
#  by a subroutine definition in which the name of the subroutine was
#  split between continuation lines.
#
#  Probably this could be made (much) more efficient, but it's not 
#  expected that this code should have to be run very often.

#  Arguments.

   my $path = shift;      #  Logical pathname of FILE to be indexed.
   my $file = shift;      #  File in current directory to be indexed.

#  Local constants.

   my @ftypes = qw/INTEGER REAL DOUBLEPRECISION COMPLEX LOGICAL
                    CHARACTER BYTE UBYTE WORD UWORD/;
   my $ftypdef = '(' . join ('|', @ftypes) . ')\**(\([^\)]\))?[0-9]*';

#  Open source file and cycle through it.

   open F, $file or die "Couldn't open $file in directory ".cwd."\n";
   while (<F>) {

#     Strip lines of syntactically uninteresting parts.

      next if (/^[*cC]/);               # Ignore F77 comments.
      s/!.*$//;                         # Discard inline comments.
      chomp;                            # Discard end of line character.
      s/^......//;                      # Discard first six characters.
      s/ //g;                           # Remove spaces.
      next unless ($_);                 # Ignore empty lines.
      tr/a-z/A-Z/;                      # Fold case to upper.
      s/^$ftypdef//o if (/FUNCTION/);   # Discard leading type specifiers.

#     Write to database if line contains a module name.

      write_entry $2, $path
         if (/^(SUBROUTINE|FUNCTION|ENTRY|BLOCKDATA)([^(]+)/);
   }
   close F;
}

########################################################################
sub index_f {

#  Examine and index a fortran source file.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $file = shift;      #  .f file in current directory to be indexed.

   index_fortran_file "$path$file", $file;
}

########################################################################
sub index_gen {

#  Examine and index a .gen (fortran Generic) source file.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $file = shift;      #  .gen file in current directory to be indexed.

#  Process file to produce normal fortran source code.

   system "$generic $file" and die "Command '$generic $file' failed: $?\n";

#  Hand file over to routine which handles normal fortran source code.

   my $ffile = $file;
   $ffile =~ s/\.gen$/.f/;
   index_fortran_file "$path$file", $ffile;

#  Tidy up.

   unlink $file or die "Failed to remove file $file\n";
}

########################################################################
sub index_incdir {

#  Examine and index a directory of files containing include files.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $dir = shift;       #  Directory in current directory to be indexed.

   pushd $dir;
   indexinc_list "$path$dir/", <*>;
   popd;
}

########################################################################
sub indexinc_list {

#  Examine and index a list of include files.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my @files = @_;        #  Files in current directory to be indexed.

#  Local variables.

   foreach $file (@files) {
      if ($file =~ /\.h$/) {              #  C type header file.
         write_entry $file, "$path$file";
      }
      elsif ($file !~ /\./) {             #  Fortran type header file.
         write_entry uc ($file), "$path$file";
      }
   }
}
   

########################################################################
sub tarlevel {

#  This is a compact, although rather obscure, routine to count the
#  number of '>' characters in a string.  When applied to the logical
#  pathname, a high number indicates that it's buried deep in layers 
#  of tar files, and a low number that it's easier to get to.

#  Arguments.

   my $path = shift;

   return ($path =~ y/>/>/);
}

########################################################################
sub write_entry {

#  Write index entry to database.

#  Arguments.

   my $name = shift;      #  Name of module.
   my $location = shift;  #  Logical pathname of module.

#  Optionally log entry to stdout.

   printf "%-20s <-  %s\n", $name, $location if ($verbose);

#  Write entry to database, except if a more accessible entry already
#  exists.

   $locate{$name} = $location
       unless (   defined ($locate{$name})
               && tarlevel ($locate{$name}) > tarlevel ($location) );
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

