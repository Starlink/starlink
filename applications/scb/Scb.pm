#!/usr/local/bin/perl -w

package Scb;

#+
#  Name:
#     Scb.pm

#  Purpose:
#     Module containing utility routines for source code browser programs.

#  Language:
#     Perl 5

#  Invocation:
#     use Scb;

#  Description:
#     Utility routines and global variables for source code browser
#     and indexer programs.  Via their definitions in this module
#     the locations of various files and directories are determined.
#     Both build-time (via the mk script) and run-time values of 
#     some environment variables affect these locations, as documented
#     in the "Global variables" section.

#  Notes:
#     This module reports errors using the routine &main::error 
#     (this is to make sensible handling of exceptions easier).
#     Thus such a routine "error", must have been declared in the 
#     main program.  If no special exception handling is required,
#     this can be done with a definition like:
#
#        sub error { die @_ }

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Set up export of names into main:: namespace.

use Exporter;
@ISA = qw/Exporter/;

#  Names of routines and variables defined here to be exported.

@EXPORT = qw/tarxf mkdirp popd pushd starpack rmrf parsetag
             $incdir $srcdir $bindir $scb_tmpdir $scbindex_tmpdir
             $mimetypes_file
             $htxserver
             $docslisfile
             $func_indexfile $file_indexfile $taskfile
             %tagger/;

@EXPORT_OK = qw/error/;

#  Declare all variables explicitly.

use strict 'vars';
use vars qw/ $incdir $srcdir $bindir $scb_tmpdir $scbindex_tmpdir
             $mimetypes_file
             $htxserver
             $docslisfile
             $func_indexfile $file_indexfile $taskfile
             %tagger/;

#  Map error handler routine to that from the main:: namespace.

sub error { &main::error (@_) }

#  Includes.

use Cwd;

########################################################################
#  Global variables.
########################################################################

#  The capitalised variable names in this section may be replaced by
#  environment variables or mk script variables by the makefile at 
#  build time.  The sequences "$VARIABLE_NAME" are substituted for 
#  by sed(1), so it is important that the quoting syntax (in the pre-
#  built files) is not modified in these assignments.  Since the mk 
#  script/makefile modify these files at build time, if you are looking
#  at the built version of this script it may look confusing.  The
#  pre-built version can be seen by extracting the script directly
#  from the scb_source.tar tar file.
#
#  The script is written so that it works in its pre-built form also, 
#  using default values.
#
#  Some of the values determined by build-time environment variables
#  (or pre-build defaults) can be overridden at run-time by environment 
#  variables of the same name, if they exist.

#  Starlink source tree directory locations.

my $STARLINK = "/star"; 
my $SCB_SOURCES = "$STARLINK/sources"; 

my $star = "$STARLINK";
$srcdir = $ENV{'SCB_SOURCES'} || "$SCB_SOURCES";
$bindir = "$star/bin";
$incdir = "$star/include";
$docslisfile = "$star/docs/docs_lis";

#  Temporary directory locations.

my $SCB_BROWSER_TMP = "/usr/tmp/scb";
my $SCB_INDEXER_TMP = "/usr/tmp/scbindex";
$scb_tmpdir      = $ENV{'SCB_BROWSER_TMP'} || "$SCB_BROWSER_TMP";
$scbindex_tmpdir = $ENV{'SCB_INDEXER_TMP'} || "$SCB_INDEXER_TMP";

#  System file locations.

my $MIMETYPES_FILE = "/etc/mime.types"; 
$mimetypes_file = "$MIMETYPES_FILE";

#  HTX server base URL

my $HTXSERVER_URL = "http://star-www.rl.ac.uk/cgi-bin/htxserver";
$htxserver = "$HTXSERVER_URL";

#  Index file locations.

my $SCB_INDEX = cwd;
my $indexdir = $ENV{'SCB_INDEX'} || "$SCB_INDEX" || cwd;
$func_indexfile = "$indexdir/func";
$file_indexfile = "$indexdir/file";
$taskfile       = "$indexdir/tasks";

#  Language-specific tagging routines.
#     By defining the hash of references to functions %tagger, file tagging 
#     can be done without explicit reference to any of the tagging routines.
#     All that is required is a sequence like:
#
#        use Scb.pm;
#        $tagged = &{$tagger{$ftype}} ($source, $ftype);
#
#     where $ftype is the file type (usually the filename extension), as
#     given in the keys to %tagger below.  The $ftype argument to the 
#     tagging routine is in general optional, but the routine may wish 
#     to know what kind of file it has been given.

use CTag;
use FortranTag;

%tagger = ( 
            c   => \&CTag::tag,
            h   => \&CTag::tag,
            C   => \&CTag::tag,
            cc  => \&CTag::tag,
            cpp => \&CTag::tag,
            cxx => \&CTag::tag,

            f   => \&FortranTag::tag,
            gen => \&FortranTag::tag,
          );


########################################################################
#  Local variables.
########################################################################

#  Define necessary shell commands.
#  Note: this is used in a CGI program, so you should be sure that these 
#  commands do what they ought to, for security reasons.
#  In the case that this is being used from a CGI program, the path 
#  $ENV{'PATH'} should have been stripped down to a minimum 
#  ('/bin:/usr/bin' is probably sufficient).

my $tar = "tar";
my $cat = "cat";
my $zcat = "uncompress -c";
my $gzcat = "gzip -dc";


########################################################################
#  Subroutines.
########################################################################


########################################################################
sub tarxf {

#+
#  Name:
#     tarxf

#  Purpose:
#     Extract files from a tar archive.

#  Language:
#     Perl 5

#  Invocation:
#     @fextracted = tarxf $tarfile;
#     @fextracted = tarxf $tarfile @files;

#  Description:
#     Extracts either all files, or a list of named files, from the 
#     named tar archive into their archived positions relative to the
#     current directory, just as tar(1).  The tar archive may however
#     be compressed using gzip(1) or compress(1).

#  Arguments:
#     $tarfile = string.
#        Name of the tar archive.  If it ends '.Z' or '.gz' it is 
#        supposed to be compressed using compress(1) or gzip(1) 
#        respectively.
#     @files = list of strings (optional).
#        If present, @files contains a list of filenames to be extracted
#        from the tar archive.  An error results if any cannot be extracted.
#        If absent, all files will be extracted.

#  Return value:
#     @fextracted = list of strings.
#        List of files successfully extracted.

#  Notes:
#     To accomodate differences which exist between the output of tar 
#     on different platforms with the 'xv' flags, the list of extracted
#     files is generated in a separate step from doing the extraction
#     itself.  This may be somewhat wasteful.

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get parameters.

   my ($tarfile, @files) = @_;

#  Define a (possibly null) compression filter.

   $tarfile =~ /\.tar(\.?.*)$/;
   my $ext = $1;
   my %filter = (
                  ''    => $cat,
                  '.Z'  => $zcat,
                  '.gz' => $gzcat,
                );

#  Unpack the tar file, reading the list of file names into a list.

   my $command = "$filter{$ext} $tarfile | $tar xf - " . join ' ', @files;
   system $command and error "$command failed: $!\n";

#  In list context, generate and return list of regular files extracted.
#  Otherwise, return the null value.

   if (wantarray) {
      my @extracted;
      $command = "$filter{$ext} $tarfile | $tar tf - " . join ' ', @files;
      open TAR, "$command|" or error "$command failed: $!\n";
      while (<TAR>) {
         chomp;
         push @extracted, $_ if (-f $_);
      }
      close TAR             or error "$command failed: $!\n";
      return @extracted;
   }
   else {
      return undef;
   }
}


########################################################################
sub mkdirp {

#+
#  Name:
#     mkdirp

#  Purpose:
#     Make directory and parents if necessary.

#  Language:
#     Perl 5

#  Invocation:
#     mkdirp ($dir, $mode)

#  Description:
#     Creates the given directory and, if required, its parents (like
#     mkdir -p).  Any directories created are given the specified 
#     access mode.  Directories which already exist are not modified.
#     The routine exits using the 'error' routine if any of the creations
#     fails.  The given access mode is not modified by the current umask.

#  Arguments:
#     $dir = string.
#        Filename of the directory to be created.
#     $mode = integer.
#        Access mode (presumably in octal) for creation of new directories.

#  Return value:

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     03-NOV-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get arguments.

   my ($dir, $mode) = @_;
   $dir .= '/';

#  Change umask so that mode supplied to mkdir is not modified, saving the
#  old value.

   my $umask = umask 0;

#  Step through directory name a ('/'-delimited) element at a time, creating
#  any which don't already exist.

   my $element;
   for (my $i = 0; $i >= 0;  $i = index ($dir, '/', $i+1)) {
      $element = substr $dir, 0, $i;
      unless (-d $element || $element eq '') {
         mkdir $element, $mode or error "mkdir $dir: $!";
      }
   }

#  Restore old umask.

   umask $umask;
}


########################################################################
sub starpack {

#+
#  Name:
#     starpack

#  Purpose:
#     Identify the Starlink package name from a logical path.

#  Language:
#     Perl 5

#  Invocation:
#     $package = starpack ($location);

#  Description:
#     Returns the name of the Starlink package into which a logical
#     pathname points.  This is the part before the first '#' sign,
#     so that e.g.:
#
#        starpack ("ndf#ndf_source.tar>ndf_open.f")  eq  'ndf'

#  Arguments:
#     $location = string.
#        Logical pathname of file.

#  Return value:
#     $package = string.
#        Name of the package.  If no package is identified, the empty
#        string (not undef) is returned.

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get parameter.

   my $location = shift;

#  Pattern match to find package identifier.

   $location =~ /^(\w+)#/;

#  Return value.

   return $1 || '';
}

########################################################################
sub rmrf {

#+
#  Name:
#     rmrf

#  Purpose:
#     Remove a directory.

#  Language:
#     Perl 5

#  Invocation:
#     rmrf ($dir);

#  Description:
#     This routine deletes a single directory and all its subdirectories.
#     It examines its arguments rather carefully to try to avoid any 
#     unintentional deletions.
#     This is entirely a precautionary measure - it is never expected 
#     that this function will be called with dangerous arguments, but
#     (especially since it may run under CGI control) it seems prudent
#     to be as safe as possible.
#     If anything looks untoward, an error is generated and the user
#     directed to this routine.  Amongst other things, the target
#     directory is checked to see whether it looks like the name of
#     something which has temporary files in it; according to naming
#     conventions this might innocently fail to be the case.  In that
#     case that check can be modified or removed.

#  Arguments:
#     $dir = string.
#        Single word giving the relative or absolute pathname of a 
#        directory.

#  Return value:

#  Notes:
#     If an exception is generated, it is handled using die() rather
#     than error(), since this routine may be called by error, and
#     we don't want to get into an infinite loop.

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get parameter.

   my $dir = shift;

#  Assemble command (by presenting a list, rather than a string, to
#  system() we guarantee that no shell processing will be done on 
#  the arguments).

   my @cmd = ("/bin/rm", "-rf", "$dir");
   my $cmd = join ' ', @cmd;

#  Form a very cautious opinion of whether it is safe to proceed.

   my $ok = 1
         && ($dir =~ /te*mp|junk|scratch/i)
         && ($dir !~ /[&|<> ;]/)
         && ($dir !~ /\.\./)
   ;

#  Execute the command or exit with error message.

   if ($ok) {
      system (@cmd) and die "Error in $cmd: $!\n";
   }
   else {
      die "Internal: command $cmd may be dangerous - see Scb::rmrf\n";
   }
}


########################################################################
#  Saved variable for pushd and popd.

my @dirstack;

########################################################################
sub pushd {

#+
#  Name:
#     pushd

#  Purpose:
#     Change directory and push old one to stack.

#  Language:
#     Perl 5

#  Invocation:
#     pushd ($dir);

#  Description:
#     This function does the same as its C shell namesake, changing
#     directory to the value given by its argument and pushing the 
#     current directory onto a stack whence it can be recalled by 
#     a subsequent popd.

#  Arguments:
#     $dir = string.
#        Directory to change to.

#  Return value:

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get parameter.

   my $dir = shift;

#  Save current directory.

   push @dirstack, cwd;

#  Change to new directory.

   chdir $dir or error "Failed to change directory to $dir\n";
}

########################################################################
sub popd {

#+
#  Name:
#     popd

#  Purpose:
#     Change to previous directory on stack.

#  Language:
#     Perl 5

#  Invocation:
#     popd;

#  Description:
#     This function does the same as its C shell namesake, popping a
#     directory from the stack and changing to it.

#  Arguments:

#  Return value:

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get directory name from stack.

   my $dir = pop @dirstack or error "Directory stack is empty\n";

#  Change to directory.

   chdir $dir or error "Failed to change directory to $dir\n";
}


########################################################################
sub parsetag {  

#+
#  Name:
#     parsetag

#  Purpose:
#     Parse an SGML-like tag.

#  Language:
#     Perl 5

#  Invocation:
#     %tag = parsetag ($tag);

#  Description:
#     Examines an SGML-like tag and returns a hash containing the name
#     and attributes.  A start tag will look like
#
#        <tagname att1='val1' att2=val2 att3>  (start tag)
#
#     and will yield a tag hash:
#
#        Start => 'tagname'
#        End   => ''
#        att1  => 'val1'
#        att2  => 'val2'
#        att3  => undef
#
#     and an end tag will look like
#
#        </tagname>
#
#     and will yield a tag hash:
#
#        Start => ''
#        End   => 'tagname'
#
#     Thus one of the hash keys 'Start' and 'End' will have an empty 
#     string value, and the other will have a non-empty string value.
#     End tags can have attributes too but usually don't.  Parsing is
#     as per usual in SGML/HTML, in particular:
#     
#        - Attribute names are case insensitive (always returned lower case)
#        - Attribute value may be ''- or ""-delimited, or be a single
#             alphanumeric word without delimiters, or be nonexistent

#  Arguments:
#     $tag = string.

#  Return value:
#     %tag = hash of strings.
#        Parsed content of tag.  See above.

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get parameter.

   my $tag = shift;

#  Initialise 'Start' and 'End' pseudo-attributes.

   my %tag = (Start => '', End => '');
  
#  Identify tag name, and whether it is starting or ending.

   $tag =~ m%<(/?)\s*(\w+)\s*%g
      or error "Internal: '$tag' doesn't look like a tag.\n";
   $tag{ $1 ? 'End' : 'Start' } = lc $2;

#  Work through tag, writing attribute-value pairs to %tag hash.

   while ($tag =~ m%(\w+)\s*(?:=\s*(?:(["'])(.*?)\2|(\w*)))?%g) {
      $tag{lc $1} = $3 || $4;
   }

#  Return tag hash.

   return %tag;
}



1;
