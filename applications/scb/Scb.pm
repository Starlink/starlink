#!/usr/local/bin/perl -w

package libscb;
require Exporter;
@ISA = qw/Exporter/;
@EXPORT = qw/tarxf popd pushd/;

use Cwd;


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
   open TAR, "$command|" or die "$command failed\n";
   while (<TAR>) {
      chomp;
      push @extracted, $_;
   }
   close TAR             or die "Error terminating tar\n";

#  Check we got them all.

   die "Failed to extract all requested files\n"
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

1;
