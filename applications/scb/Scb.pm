#!/usr/local/bin/perl -w

package libscb;

#  This perl module contains some of the general routines used by 
#  the source code index generator (scbindex.pl) and the browser itself
#  (scb.pl).  The routines are documented separately.
#
#  It requires a routine "error" (which should do something like "die")
#  to be available in the calling (main::) package.
#  The point of this is so that the calling package can intercept 
#  exceptions.

require Exporter;
@ISA = qw/Exporter/;

#  Names of routines and variables defined here to be exported.

@EXPORT = qw/tarxf popd pushd module_name starpack
             incdir srcdir indexfile taskfile/;

########################################################################
#  Global variables.
########################################################################

#  Directory locations.

$main::srcdir = "/local/star/src/from-ussc";  # head of source tree
$main::incdir = "/star/include";              # include directory

#  Index file locations.

$main::indexfile = "/local/devel/scb/index";
$main::taskfile  = "/local/devel/scb/tasks";



########################################################################
#  Subroutines.
########################################################################

sub error {
   main::error (@_);
}


########################################################################
sub tarxf {

#  Extracts from a (optionally compressed) tar archive.
#  Returns a list of all the files extracted (excludes directories).
#  As in the real thing, if it only has one argument then all files
#  are extracted, but if a list of arguments follow only the named
#  ones are extracted.  In this case, an error results if the number
#  of files extracted is not the same as the number of elements in
#  the request list.

#  Arguments.

   my $tarfile = shift;
   my @files = @_;

#  Define necessary shell commands.

   $tar = "tar";
   $cat = "cat";
   $zcat = "uncompress -c";
   $gzcat = "gzip -dc";

#  Define a (possibly null) compression filter.

   $tarfile =~ /\.tar(\.?.*)$/;
   my $ext = $1;
   my %filter = (
                  ''    => $cat,
                  '.Z'  => $zcat,
                  '.gz' => $gzcat,
                );

#  Unpack the tar file, reading the list of file names into a list.

   my $command = "$filter{$ext} $tarfile | $tar xvf - " . join ' ', @files;
   my @extracted;
   open TAR, "$command|" or error "$command failed";
   while (<TAR>) {
      chomp;
      push @extracted, $_;
   }
   close TAR             or error "Error terminating tar";

#  Check we got them all.

   error "Failed to extract all requested files"
      if (@files && @files != @extracted);

#  Eliminate any directories from the list.

   my ($file, @fextracted);
   while ($file = shift @extracted) {
      push @fextracted, $file unless (-d $file);
   }

#  Return list.

   return @fextracted;
}

########################################################################

#  Constants used by module_name.

@ftypes = qw/INTEGER REAL DOUBLEPRECISION COMPLEX LOGICAL CHARACTER
             BYTE UBYTE WORD UWORD/;
$ftypdef = '(' . join ('|', @ftypes) . ')\**(\([^\)]\))?[0-9]*';

sub module_name {

#  Examines a line of source code to see whether it identifies the 
#  start of an indexable module (e.g. Fortran SUBROUTINE or FUNCTION).
#  Returns the name if so, or undef otherwise.
#
#  Note the parsing is not perfect - the main limitation is that since
#  the routine is called once for each line (and is stateless) it will 
#  not cope with module declarations whose important parts are split
#  across lines.
#
#  Currently handles Fortran (including Generic) and C source.
#  Currently, the C parser only identifies functions defined using the
#  macros in /star/include/f77.h as Fortran-callable routines.
#
#  Probably this could be made (much) more efficient.

#  Arguments.

   my $filetype = shift;      #  'f' for fortran, 'c' for C.
   local $_ = shift || $_;    #  $_ used if none specified.

   my ($type, $name);

#  Fortran source file.

   if ($filetype eq 'f') {

#     Strip lines of syntactically uninteresting parts.

      return (undef) if (/^[*cC]/);     # Ignore F77 comments.
      s/!.*$//;                         # Discard inline comments.
      chomp;                            # Discard end of line character.
      s/^......//;                      # Discard first six characters.
      s/ //g;                           # Remove spaces.
      return (undef) unless ($_);       # Ignore empty lines.
      tr/a-z/A-Z/;                      # Fold case to upper.
      s/^$ftypdef//o if (/FUNCTION/);   # Discard leading type specifiers.

      ($type, $name) = /^(SUBROUTINE|FUNCTION|ENTRY|BLOCKDATA)([^(]+)/;
   }

#  C source file.

   elsif ($filetype eq 'c') {

#     Very feeble attempt to strip comments.

      chomp;
      s%/\*.*\*/%%g;       #  discard comments contained in this line.
      s%/\*.*%%;           #  discard text from any comment which starts here.
      return undef unless ($_);

#     Check for macro flagging start of Fortran-callable routine.

      ($type, $name) =
         /^\s*F77_(SUBROUTINE|[A-Z]+_FUNCTION|EXTERNAL_NAME) *\(\s*(\w+)\s*\)/;
      $name =~ tr/a-z/A-Z/ if $name;

   }

   return $name;
}

########################################################################
sub starpack {

#  Identifies the package name from a logical path.

   local $_ = shift;
   /^(\w+)#/;
   return $1;
}

########################################################################
# include for pushd and popd.

use Cwd;

########################################################################
sub pushd {

#  Pushd does the same thing as its C-shell namesake.

   my $dir = shift;
   push @dirstack, cwd;
   chdir $dir or error "Couldn't change directory to $dir";
}

########################################################################
sub popd {

#  Popd does the same thing as its C-shell namesake.

   my $dir = pop @dirstack;
   chdir $dir or error "Couldn't change directory to $dir";
}


1;
