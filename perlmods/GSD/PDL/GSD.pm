#!/bin/perl
 
package PDL::IO::GSD;
 
use PDL::Core;     # Grab the Core names
use DynaLoader; use Carp; use SelfLoader;use strict;
 
use vars qw/@EXPORT_OK @ISA @EXPORT_STATIC %gsdtypes/;
 
# Starlink data type conversion
use vars qw/$VERSION/;
 
$VERSION = '0.90';

# Set GSD -> PDL data types
%gsdtypes = ("B"    =>$PDL_B,
             "W"    =>$PDL_S,
             "I"    =>$PDL_L,
             "R"    =>$PDL_F,
             "D"    =>$PDL_D
             );

@EXPORT_OK = qw( rgsd );
@ISA = qw( PDL::Exporter DynaLoader SelfLoader ); 
@EXPORT_STATIC = qw( rgsd );
 
1;
 
__DATA__
 

#######################################################################
#  RGSD
#     Routine to read GSD files into PERLDL
########################################################################

sub rgsd {  # Read an GSD format file into a PDL
 
  croak 'Usage: ($data) = rgsd($file) or PDL->rndf(...) '  if ($#_!=1);
  my $class = shift;
  my $file = shift; my $pdl = $class->new(); my $xpdl;
 
  eval 'use GSD' unless $gsd_loaded++;
  croak 'Cannot use GSD library' if $@ ne "";

  # If file is not there
  croak "Cannot find $file" unless -e "$file.dat";

  $status = gsdOpenRead($file,$version,$label,$no_items,$fptr,$file_dsc,
			$item_dsc,$data_ptr);

  croak "Error opening $file\n" if $status != 0;

  # Read C13DAT as data array

  $name = 'C13DAT';
  $status = gsdFind($file_dsc,$item_dsc,$name,$num,$unit,$type,$array);
  
  # Initialise arrays
  @dimvals = ();
  @dimnm = ();
  @dimunt = ();
  
  $status = gsdInqSize($file_dsc,$item_dsc,$data_ptr,$num,\@dimnm,
		       \@dimunt, \@dimvals,$actdims,$size);

  $$pdl{Dims} = [@dimvals];
  $$pdl{Datatype} = $gsdtypes{$type};
  $call = "gsdGet$array\l$type"."p"; 	# Map as packed string
  $status = &{$call}($file_dsc, $item_dsc, $data_ptr, $num, 1,
		     $size, 1, $size, $$pdl{Data}, $actdims);
  print "Reading $name ($type) [@dimvals]\n" if $PDL::verbose;
  $$pdl{Hdr}{Units} = $unit;

  croak "Error reading $name\n" if $status != 0;

  # Can read all scalar bits into Hdr and Arrays into GSD
  
  for ($i=1; $i<$no_items; $i++) {
    
    $status = gsdItem($file_dsc,$item_dsc,$i,$name,$unit,$type,$array);
    $name =~ s/\s+$//;    

    next if $name eq 'C13DAT';
    $call = "gsdGet$array\l$type";

    if ($array) {

      # Initialise arrays (otherwise smaller arrays just overwrite start)
      @dimvals = ();
      @dimnm = ();
      @dimunt = ();

      $status = gsdInqSize($file_dsc,$item_dsc,$data_ptr,$i,\@dimnm,\@dimunt,
			   \@dimvals,$actdims,$size);
      if ($type eq 'C') {
	$status = &{$call}($file_dsc,$item_dsc,$data_ptr,$i,1,$size,1,$size,
			   \@values,$actvals);
	print "Reading $name ($type) [@dimvals]\n" if $PDL::verbose;
	$$pdl{GSD}{$name} = [@values];
      } else {

	$call .= 'p';  # Access by pointer
	$$pdl{GSD}{$name} =  $class->new();
	$temppdl = $$pdl{GSD}{$name};
	$$temppdl{Dims} = [@dimvals];
	print "Reading $name ($type) [@dimvals]\n" if $PDL::verbose;
	$$temppdl{Datatype} = $gsdtypes{$type};
	$status = &{$call}($file_dsc, $item_dsc, $data_ptr, $i, 1,
			   $size, 1, $size, $$temppdl{Data}, $actdims);
	# Remove white space
	grep(s/\s+//g,@dimunt);
	grep(s/\s+//g,@dimnm);
	$$temppdl{Hdr}{'axunits'} = [@dimunt];
	$$temppdl{Hdr}{'axnames'} = [@dimnm];
	$$temppdl{Hdr}{'units'} = $unit;
	$temppdl->flush();
	undef $temppdl;
      }


      
    } else {
      
      $status = &{$call}($file_dsc,$item_dsc,$data_ptr,$i,$newdata);
      $newdata =~ s/\s+$// if $type eq "C";

      $$pdl{Hdr}{$name} = $newdata;
    }

    $status != 0 && do {
      print "Warning! Error whilst reading $name\n";
      $status = 0;
    };
  }
  
  # Close the GSD file
  $status = gsdClose($fptr,$file_dsc,$item_dsc,$data_ptr);

  $pdl->flush();  # Flush changes through

  return $pdl if $status == 0;

}

__END__
# Documentation

=head1 NAME

  PDL::GSD - PDL module for reading JCMT GSD format files.

=head1 SYNOPSIS

  use PDL::GSD;

  $a = rgsd 'test';

=head1 DESCRIPTION

This module adds the ability to read James Clerk Maxwell telescope GSD
format files into PDL.  rgsd produces PDLs of the following structure:

  $a          - the C13DAT component
  $$a{Hdr}    - All the SCALAR components found in the file
  $$a{GSD}    - The array components (stored as PDLs for numeric arrays
		and perl arrays for Character arrays)

This command does not try to read an error component into {Errors} in
the special case of UKT14 bolometer data. Only C13DAT is treated as a special
case.

=head1 NOTES

The perl GSD module must be avaialble.

=head1 AUTHOR

This module was written by Tim Jenness, timj@jach.hawaii.edu.

=head1 SEE ALSO

L<PDL> for general information on the Perl Data language,
L<GSD> for information on the NDF module.

=cut
