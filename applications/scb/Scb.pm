#!/usr/local/bin/perl -w

package Scb;

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

@EXPORT = qw/tarxf popd pushd starpack rmrf parsetag
             $incdir $srcdir $bindir $indexfile $taskfile/;

#  Includes.

use Cwd;

########################################################################
#  Global variables.
########################################################################

#  Directory locations.

$srcdir = "/local/star/sources";        # head of source tree
$bindir = "/star/bin";                  # Starlink binaries directory
$incdir = "/star/include";              # Starlink include directory

#  Index file locations.

$indexfile = cwd . "/index";
$taskfile  = cwd . "/tasks";


########################################################################
#  Local variables.
########################################################################

#  Define necessary shell commands.
#  Note: this is used in a CGI program, so you should be sure that these 
#  commands do what they ought to, for security reasons.

   $tar = "tar";
   $cat = "cat";
   $zcat = "uncompress -c";
   $gzcat = "gzip -dc";


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
sub starpack {

#  Identifies the package name from a logical path.

   local $_ = shift;
   /^(\w+)#/;
   return $1;
}

########################################################################
sub rmrf {

#  This routine erases recursively a directory (rm -rf).
#  It does it rather carefully to try to avoid anything nasty happening.
#  Note the use of 'die' rather than 'error', since the routine may be
#  called by error, and we don't want to get into an infinite loop.

   my $dir = shift;

   die "Warning: don't like the look of 'rm -rf $dir'\n"
      unless ($dir =~ /tmp|junk/);
   system "rm -rf $dir" and die "Error in rm -rf $dir: $?\n";
}


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


########################################################################
sub parsetag {  
   
#  Parses an SGML-type tag to return a hash giving the values of the
#  elements in it.  Element names are folded to lower case, and the
#  special hash keys 'Start' and 'End' give the name of the tag;
#  only one of 'Start' or 'End' may be defined.
      
   my $tag = shift;
   my %tag = (Start => '', End => '');
  
   $tag =~ m%<(/?)\s*(\w+)\s*%g
      or die "Internal: $tag doesn't look like a tag.\n";
   $tag{ $1 ? 'End' : 'Start' } = lc $2;
   while ($tag =~ m%(\w+)\s*(?:=\s*(?:(["'])(.*?)\2|()(\w*)))?%g) {
      $tag{lc $1} = $3;
   }
   return %tag;
}




1;
