package PDL::IO::NDF;

=head1 NAME

PDL::IO::NDF - PDL Module for reading and writing Starlink
                 N-dimensional data structures as PDLs.

=head1 SYNOPSIS

  use PDL::IO::NDF;

  $a = PDL->rndf($file);

  $a = rndf('test_image');
  $a = rndf('test_image', 1);

  $a->wndf($file);
  wndf($a, 'out_image');

  propndfx($a, 'template', 'out_image');

=head1 DESCRIPTION

This module adds the ability to read and write Starlink N-dimensional data
files as N-dimensional PDLs.

=head1 FUNCTIONS

=cut


@EXPORT_OK = qw/rndf wndf/;
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

@ISA    = qw( PDL::Exporter );

use PDL::Core;
use PDL::Types;
use Carp;
use strict;

# Starlink data type conversion
use vars qw/%pdltypes %startypes $ndf_loaded $VERSION $EXTNAME/;

$VERSION = '1.02';

# Set PDL -> Starlink data types
%pdltypes = ("$PDL_B"  => "_BYTE",
	     "$PDL_S"  => "_WORD",
	     "$PDL_US" => "_UWORD",
	     "$PDL_L"  => "_INTEGER",
	     "$PDL_F"  => "_REAL",
	     "$PDL_D"  => "_DOUBLE"
	    );

# Set Starlink -> PDL data types
%startypes = ("_BYTE"    =>$PDL_B,
	      "_UBYTE"   =>$PDL_B,
	      "_WORD"    =>$PDL_S,
	      "_UWORD"   =>$PDL_US,
	      "_INTEGER" =>$PDL_L,
	      "_REAL"    =>$PDL_F,
	      "_DOUBLE"  =>$PDL_D
	     );


# This is the name I use in the PDL header hash that contains the
# NDF extensions so that they can be recreated

$EXTNAME = 'NDF_EXT';

=head2 rndf()

=for ref

Reads a piddle from a NDF format data file.

=for example

     $pdl = rndf('file.sdf');
     $pdl = rndf('file.sdf',1);

The '.sdf' suffix is optional. The optional second argument turns off
automatic quality masking and returns a quality array as well.

Header information and NDF Extensions are stored in the piddle as a hash
which can be retreived with the C<$pdl-E<gt>gethdr> command.
Array extensions are stored in the header as follows:

     $a - the base DATA_ARRAY

If C<$hdr = $a-E<gt>gethdr>;

then:

      %{$hdr}        contains all the FITS headers plus:
      $$hdr{Error}   contains the Error/Variance PDL
      $$hdr{Quality} The quality byte array (if reqeusted)
      @{$$hdr{Axis}} Is an array of piddles containing the information
                     for axis 0, 1, etc.
      $$hdr{NDF_EXT} Contains all the NDF extensions
      $$hdr{Hist}    Contains the history information
      $$hdr{NDF_EXT}{_TYPES} - Data types for non-PDL NDF extensions so that
                           wndf can reconstruct a NDF.

All extension information is stored in the header hash array.
Extension structures are preserved in hashes, so that the PROJ_PARS
component of the IRAS.ASTROMETRY extension is stored in
$$hdr{NDF_EXT}{IRAS}{ASTROMETRY}{'PROJ_PARS'}. All array structures are
stored as arrays in the Hdr: numeric arrays are stored as PDLs,
logical and character arrays are stored as plain Perl arrays. FITS
arrays are a special case and are expanded as scalars into the header.

=cut

# This is one form of the new command

sub rndf {PDL->rndf(@_)}

# And this is the real form
# Allows the command to be called in OO form or as a function
sub PDL::rndf {  # Read a piddle from a NDF file

  my $class = shift;
  barf 'Usage: $a = rndf($file,[$nomask]); $a = PDL->rndf(...) '
    if ($#_!=0 && $#_!=1);

  # Setup the Header Hash
  my $header = {};

  # Read in the filename
  # And setup the new PDL
  my $file = shift; my $pdl = $class->new;
  my $nomask = shift if $#_ > -1;

  my ($infile, $status, $indf, $entry, @info, $value);

  eval 'use NDF' unless $ndf_loaded++;
  croak 'Cannot use NDF library' if $@ ne "";

  # Strip trailing .sdf if one is present
  # File is the first thing before a .
  $file =~ s/\.sdf$//;
  $infile = (split(/\./,$file))[0];

  # If file is not there
  croak "Cannot find $infile.sdf" unless -e "$infile.sdf";

  # Set status
  $status = &NDF::SAI__OK;

  # Begin NDF and ERR session
  err_begin($status);
  ndf_begin();

  # Open the file
  ndf_find(&NDF::DAT__ROOT, $file, $indf, $status);

  # unset automatic quality masking if $nomask is defined
  $nomask = 0 unless (defined $nomask);
  $nomask = 1 if $nomask != 0;
  ndf_sqmf($nomask, $indf, $status);

  # Read the data array (need to pass the header as well)
  rdata($indf, $pdl, $nomask, $header, $class, $status);

  # Read the axes
  raxes($indf, $pdl, $header, $class, $status);

  # Read the header
  rhdr($indf, $pdl, $header, $class, $status);

  # Read history information
  rhist($indf, $pdl, $header, $status);

  # Read labels
  @info = ('Label', 'Title', 'Units');
  for $entry (@info) {
    $value = 'NULL';
    ndf_cget($indf, $entry, $value, $status);
    $$header{"$entry"} = $value if $value ne 'NULL';
    print "$entry is $value\n" if ($PDL::verbose && $value ne 'NULL');
    undef $value;
  }

  # Tidy up
  ndf_annul($indf, $status);
  ndf_end($status);

  if ($status != &NDF::SAI__OK) {
    err_flush($status);         # Flush the error system
    err_end($status);
    croak("rndf: obtained NDF error");
  }

  err_end($status);

  # Put the header into the main pdl
  $pdl->sethdr($header);

  return $pdl;
}


=head2 wndf()

=for ref

Writes a piddle to a NDF format file:

=for example

   $pdl->wndf($file);
   wndf($pdl,$file);

wndf can be used for writing PDLs to NDF files. 
The '.sdf' suffix is optional. All the extensions
created by rndf are supported by wndf.  This means that error, axis
and quality arrays will be written if they exist. Extensions are also
reconstructed by using their name (ie FIGARO.TEST would be expanded as
a FIGARO extension and a TEST component). Hdr keywords Label, Title
and Units are treated as special cases and are written to the label,
title and units fields of the NDF.

Header information is written to corresponding NDF extensions.
NDF extensions can also be created in the {NDF} hash by using a key
containing '.', ie {NDF}{'IRAS.DATA'} would write the information to
an IRAS.DATA extension in the NDF. rndf stores this as
$$hdr{NDF}{IRAS}{DATA} and the two systems are interchangeable.

rndf stores type information in {NDF}{'_TYPES'} and below so that
wndf can reconstruct the data type of non-PDL extensions. If no
entry exists in _TYPES, wndf chooses between characters, integer and
double on a best guess basis.  Any perl arrays are written as CHAR
array extensions (on the assumption that numeric arrays will exist as
PDLs).

=cut

# This is one form of the new command
# OO version.

*wndf = \&PDL::wndf;

# And this is the real form
# Allows the command to be called in OO form or as a function
sub PDL::wndf {  # Write a PDL to an NDF format file

  barf 'Usage: wndf($pdl,$file)' if $#_!=1;

  my ($indf, $place, $status, $outndf);
  my (@lbnd, @ubnd);

  my ($pdl, $outfile) = @_;
  eval 'use NDF' unless $ndf_loaded++;
  croak 'Cannot use NDF library' if $@ ne "";

  # Check that we are dealing with a PDL
  croak 'Argument is not a PDL variable' unless $pdl->isa('PDL');

  # Set status
  $status = &NDF::SAI__OK;

  # Strip trailing .sdf if one is present
  $outfile =~ s/\.sdf$//;

  # Begin context
  ndf_begin();
  err_begin($status);

  # Open the new file
  ndf_place(&NDF::DAT__ROOT(), $outfile, $place, $status);

  # Need to create data_array component
  # Change the bounds to match the PDL

  @ubnd = $pdl->dims;
  @lbnd = ();
  @lbnd = map { 1 } (0..$#ubnd);

  ndf_new($pdltypes{$pdl->get_datatype}, $#ubnd+1, \@lbnd, \@ubnd, $place,
          $outndf, $status);

  # Set the application name
  ndf_happn('PDL::wndf', $status);

  # Write the data to this file
  wdata($outndf, $pdl, $status);

  # Write the header and title
  whdr($outndf, $pdl, $status);

  # Create a history extension
  ndf_hcre($outndf, $status);

  # Annul the identifier
  ndf_annul($outndf, $status);

  # End context
  ndf_end($status);

  if ($status != &NDF::SAI__OK) {
    err_flush($status);
    croak "Bad STATUS in wndf\n";
  }

  err_end($status);

  return 1;
}


=head2 propndfx()

Routine to write a PDL to an NDF by copying the extension information
from an existing NDF and writing DATA,VARIANCE, QUALITY and AXIS info
from a PDL (if they exist).

Extensions, labels and history are propogated from the old NDF.
No new extension information is written.

This command has been superseded by L<wndf()|/wndf()>.

=cut

*propndfx = \&PDL::propndfx;

sub PDL::propndfx {  # Write a PDL to a NDF format file

  barf 'Usage: propndfx($pdl, $infile, $outfile)' if $#_!=2;

  my ($indf, $in_place, $status, $outplace, $outndf);

  my ($pdl, $infile, $outfile) = @_;
  eval 'use NDF' unless $ndf_loaded++;
  croak 'Cannot use NDF library' if $@ ne "";

  # Set status
  $status = &NDF::SAI__OK;

  # Strip trailing .sdf if one is present
  $outfile =~ s/\.sdf$//;
  $infile =~ s/\.sdf$//;
  # File is the first thing before a .
  my $file = (split(/\./,$infile))[0];

  croak "$file does not exist\n" unless -e "$file.sdf";

  # Begin context
  ndf_begin();
  err_begin($status);

  # Open the old file
  ndf_find(&NDF::DAT__ROOT(), $infile, $indf, $status);

  # Set the application name
  ndf_happn('PDL::propndfx', $status);

  # Open the new file
  ndf_place(&NDF::DAT__ROOT(), $outfile, $outplace, $status);

  # Copy the extension information to outfile
  ndf_scopy($indf, ' ', $outplace, $outndf, $status);

  # Annul the input file identifier
  ndf_annul($indf, $status);

  # Write the data to this file
  wdata($outndf, $pdl, $status);

  # Annul the identifier
  ndf_annul($outndf, $status);

  # End context
  ndf_end($status);

  if ($status != &NDF::SAI__OK) {
    err_flush($status);
    croak "Bad STATUS in propndfx\n";
  }

  err_end($status);

  return 1;
}


=head1 NOTES

The perl NDF module must be available. This is available from the 
author or from Starlink (http://www.starlink.rl.ac.uk).

If an NDF is read which contains AST World Coordinate information
(a .WCS component) this information is currently ignored. Currently
WCS information can only be written and stored using standard FITS headers.
See http://rlspc5.bnsc.rl.ac.uk/star/docs/sun211.htx/sun211.html#xref_
for more information on AST.

=head1 AUTHOR

This module was written by Tim Jenness E<lt>t.jenness@jach.hawaii.eduE<gt>.
Copyright (C) Tim Jenness 1997-2000. All Rights Reserved.

=head1 SEE ALSO

L<PDL::FAQ> for general information on the Perl Data language,
L<NDF> for information on the NDF module.

=cut



#######################################################################

# These are the generic routines used by the rndf command

#  RDATA
#    Read Data, Quality and Variance

sub rdata {
  my ($indf, $pdl, $nomask, $header, $class, $status) = @_;

  return $status if $status != &NDF::SAI__OK;

  my ($maxdims, $ndim, @dim, @comps, $dcomp, $tcomp, $exist);
  my ($type, $data_pntr, $el, $temppdl, $nbytes, $badbit, $dref);

  ####################################################################
  #                      D  I  M  S                                  #
  ####################################################################

  # Find the dimensions of the data array
  $maxdims = 100;		# Maximum number of dimensions allowed

  &ndf_dim($indf, $maxdims, \@dim, $ndim, $status);
  @dim = @dim[0..$ndim-1];

  print "Dims = @dim\n" if $PDL::verbose;

  $$header{Extensions} = [];

  ####################################################################
  #    D A T A  +  V  A  R  I  A  N  C  E  +  Q U A L I T Y          #
  ####################################################################

  @comps = ('Data','Error');
  push(@comps, 'Quality') if $nomask;

  for $dcomp (@comps) {

    $tcomp = $dcomp;
    $tcomp = 'Variance' if $tcomp eq 'Error';

    ndf_state($indf, $tcomp, $exist, $status);

    if ($exist && ($status == &NDF::SAI__OK)) {

      # Find the type of the data array
      ndf_type($indf, $tcomp, $type, $status);

      # Map the data array
      ndf_map($indf, $dcomp, $type, 'READ', $data_pntr, $el, $status);

      if ($status == &NDF::SAI__OK) {

	print "Reading $dcomp array...\n" if $PDL::verbose;

	# Create a temporary PDL to deal with separate components
	if ($dcomp eq 'Data') {
	  $temppdl = $pdl;
	} else {
	  $dcomp = 'Errors' if $dcomp eq 'Error';
	  $$header{"$dcomp"} = $class->new;
	  $temppdl = $$header{"$dcomp"};
	}

	# Set up the PDL
	$temppdl->set_datatype($startypes{$type});
	$temppdl->setdims([@dim]);
	$dref = $temppdl->get_dataref;


	# How many bytes in this data type?
	$type =~ s/^_|^_U//;
	$nbytes = &NDF::byte_size($type);

	mem2string($data_pntr, $nbytes*$el, $$dref);

	push(@{$$header{Extensions}}, $dcomp) if $dcomp ne 'Data';

	if ($dcomp eq 'Quality') {
	  # Quality bad bit mask
	  ndf_bb($indf, $badbit, $status);

	  my $qhdr = {};
	  $$qhdr{BADBIT} = $badbit;
	  $temppdl->sethdr($qhdr);
	}

	# Flush and Clear temporary PDL
	$temppdl->upd_data();
	undef $temppdl;
      }

      ndf_unmap($indf, $tcomp, $status);
    }
  }
  err_flush($status) if $status != &NDF::SAI__OK;

  return $status;
}



  ####################################################################
  #                      A  X  I  S                                  #
  ####################################################################

# Axes are stored as follows:
#  Array of PDL axes as an array in header of main pdl (called 'Axis')
#  Header of this PDL contains label and errors and units etc.

sub raxes {
  my ($indf, $pdl, $header, $class, $status) = @_;

  return $status if $status != &NDF::SAI__OK;

  my ($there, $naxis, @dims, $axcomp, $exist, $axtype, $axpntr, $el);
  my ($nbytes, $temppdl, $tcomp, $daxref, $dref);
  my ($axhdr, $ndims);

  # Read in axis information
  ndf_state($indf, 'Axis', $there, $status);

  $ndims = $pdl->getndims; # Find number of dimensions

  if ($there) {

    # Label from 0..$#dims to match array index handling
    # Want to put the axes into an array of axes
    # And update the extensions field
    $$header{Axis} = [];
    push(@{$$header{Extensions}}, "Axis");

    for $naxis (0..$ndims-1) {
      # Does a CENTRE component exist
      $axcomp = 'CENTRE';
      ndf_astat($indf, $axcomp, $naxis+1, $exist, $status);

      if ($exist && ($status == &NDF::SAI__OK)) {

	print "Reading Axis $naxis...\n" if $PDL::verbose;
	ndf_atype($indf, $axcomp, $naxis+1, $axtype, $status);

	ndf_amap($indf, $axcomp, $naxis+1, $axtype, 'READ', $axpntr,
		 $el, $status);

	# Set up new PDL for axis info if map was okay
	if ($status == &NDF::SAI__OK) {
	  print "Number of elements: $el\n" if $PDL::verbose;
	  $$header{Axis}[$naxis] = $class->new;
	  $temppdl = $$header{Axis}[$naxis];
	  $temppdl->set_datatype($startypes{$axtype});
	  $temppdl->setdims([$el]);
	  $dref = $temppdl->get_dataref;


	  # How many bytes?
	  $axtype =~ s/^_|^_U//;
	  $nbytes = &NDF::byte_size($axtype);

	  mem2string($axpntr, $nbytes*$el, $$dref);
	  $temppdl->upd_data();

	  # Header info for axis
	  $axhdr = {};   # somewhere to put the pdl

	  # Read VARIANCE array if there
	  $axcomp = 'Error';
	  $tcomp = 'Variance';
	  ndf_astat($indf, $tcomp, $naxis+1, $exist, $status);

	  if ($exist && ($status == &NDF::SAI__OK)) {
	    print "Reading Axis $naxis errors...\n" if $PDL::verbose;
	    ndf_atype($indf, $tcomp, $naxis+1, $axtype, $status);

	    ndf_amap($indf, $axcomp, $naxis+1, $axtype, 'READ', $axpntr,
		     $el, $status);

	    # Set up new PDL for axis info if map was okay
	    if ($status == &NDF::SAI__OK) {

	      $$axhdr{Errors} = $class->new;
	      $$axhdr{Extensions} = ["Errors"];

	      $$axhdr{Errors}->set_datatype($startypes{$axtype});
	      $$axhdr{Errors}->setdims([$el]);
	      $daxref = $$axhdr{Errors}->get_dataref;

	      # How many bytes?
	      $axtype =~ s/^_|^_U//;
	      $nbytes = &NDF::byte_size($axtype);

	      mem2string($axpntr, $nbytes*$el, $$daxref);
	      $$axhdr{Errors}->upd_data();

	    }
	  }

	  # Get label and units
	  for my $entry ('Units', 'Label') {
	    ndf_astat($indf, $entry, $naxis+1, $exist, $status);
	    if ($exist) {
	      my $value = '';
	      ndf_acget($indf,$entry, $naxis+1, $value, $status);
	      $axhdr->{"$entry"} = $value;
	    }
	  }
	}
	ndf_aunmp($indf,'*',$naxis+1,$status);

	# Now store this header into temppdl
	$temppdl->sethdr($axhdr);

      } else {
	err_annul($status) unless $status == &NDF::SAI__OK;
      }

    }

  }

 return $status;
}


  ####################################################################
  #                      E  X  T  E  N  S  I  O  N  S                #
  ####################################################################

sub rhdr {
  my ($indf, $pdl, $header, $class, $status) = @_;

  return $status if $status != &NDF::SAI__OK;

  my ($nextn, $xname, $xloc, $extn, $size, @fits, $entry, $value);
  my ($comment, $item, $el);

  # Read in any header information from the extensions

  ndf_xnumb($indf, $nextn, $status);
  print "Reading in header information...\n"
    if (($nextn > 0) && $PDL::verbose);

  # Read in extensions one at a time
  for $extn (1..$nextn) {

    $xname = 'NULL';
    ndf_xname($indf, $extn, $xname, $status);

    undef $xloc;
    ndf_xloc($indf, $xname, 'READ', $xloc, $status);

    # FITS is a special case and must be expanded
    if ($xname eq 'FITS') {
      # Read in the data
      dat_size($xloc, $size, $status);
      &dat_getvc($xloc, $size, \@fits, $el, $status);

      if ($status == &NDF::SAI__OK) {
	print "Reading FITS header...\n" if $PDL::verbose;
	for $entry (0..$el-1) {
	  ($item, $value, $comment) = &fits_get_nth_item(\@fits,$entry);
	  $$header{$item} = $value;
	  $$header{'_COMMENTS'}{$item} = $comment;
	}
      } else {
	print "Error mapping $xname\n";
	err_annul($status);
      }
      undef @fits;
    } else {

      # Read in extensions to $EXTNAME
      $status = &find_prim($xloc, \%{$$header{$EXTNAME}}, $class, $status);
    }

    dat_annul($xloc, $status);
  }
  return $status;
}

  ####################################################################
  #                      H  I  S  T  O  R  Y                         #
  ####################################################################

sub rhist {
  my ($indf, $pdl, $header, $status) = @_;

  return $status if $status != &NDF::SAI__OK;

  my ($nrec, $app, $date, $i);

  # Read HISTORY information into a Hist header

  ndf_hnrec($indf,$nrec,$status);

  if ($status == &NDF::SAI__OK && ($nrec > 0)) {
    print "Reading history information into 'Hist'...\n" if $PDL::verbose;

    $$header{Hist}{'Nrecords'} = $nrec;

    # Loop through the history components and find last "APPLICATION"
    for $i (1..$nrec) {

      ndf_hinfo($indf, 'APPLICATION', $i, $app, $status);
      ndf_hinfo($indf, 'DATE', $i, $date, $status);

      $$header{Hist}{"Application$i"} = $app;
      $$header{Hist}{"Date$i"} = $date;

      print "  $app on $date\n" if $PDL::verbose;
    }
  } else {
    err_annul($status);
  }
  return $status;
}



################ Generic routines ###########################
#################### INTERNAL ###############################

# Find primitive components below a given HDS locator
# FITS information is stored in Hdr
# NDF extensions stored in NDF

sub find_prim {

  my ($loc, $href, $class, $status) = @_;
  my ($prim, $type, $size, @dim, $ndims, $struct, $name, $nloc, $el);
  my ($value, @values, $item, $entry, $maxdims);
  my ($packtype, $ncomp, $packed, $comp);
  my ($pntr, $nbytes, $comment, $dref);

  # Return if bad status
  return  0 if $status != &NDF::SAI__OK;

  # Most dimensions
  $maxdims = 100;

  # Is it a primitive component
  dat_prim($loc, $prim, $status);

  # Type, size and shape
  dat_type($loc, $type, $status);
  dat_size($loc, $size, $status);
  dat_shape($loc, $maxdims, \@dim, $ndims, $status);
  dat_name($loc, $name, $status);

  if ($prim) {
    print "\tReading component: $name ($type) " if $PDL::verbose;

    # Store type of information
    $$href{"_TYPES"}{"$name"} = $type;

    # Read it into the header
    if (($size == 1) && ($ndims == 0)) {
      if ($type  eq '_DOUBLE') {
	# dat_get0c doesn't return full precision for doubles
	dat_get0d($loc, $value, $status);
	if ($status != &NDF::SAI__OK) {
	  print "Error mapping scalar 0d $type $name\n";
	  err_flush($status);
	}

      } else {
	dat_get0c($loc, $value, $status);
	if ($status != &NDF::SAI__OK) {
	  print "Error mapping scalar 0c $name\n";
	  err_flush($status);
	}

      }

      $$href{"$name"} = $value if ($status == &NDF::SAI__OK);
      undef $value;

    } else {
      @dim = @dim[0..$ndims-1];

      # Inform the user of dimensions
      print "[@dim]" if $PDL::verbose;

      # Map arrays (don't need to unpack them)
      if ($type =~ /CHAR|LOG/) {

	# Read in the data
	dat_getvc($loc, $size, \@values, $el, $status);

	if ($status != &NDF::SAI__OK) {
	  print "Error mapping $name\n";
	} else {
	  $$href{"$name"} = [@values];
	  undef @values;
	}

      } else {

	# map the data
	dat_mapv($loc, $type, 'READ', $pntr, $el, $status);

	if ($status != &NDF::SAI__OK) {
	  print "Error mapping $name\n";
	}


	if ($status == &NDF::SAI__OK) {
	  # Create the pdl
	  $$href{"$name"} = $class->new;
	  $$href{"$name"}->set_datatype($startypes{$type});
	  $$href{"$name"}->setdims([@dim]);
	  $dref = $$href{"$name"}->get_dataref();

	  # How many bytes in this data type?
	  $type =~ s/^_|^_U//;
	  $nbytes = &NDF::byte_size($type);

	  mem2string($pntr, $nbytes*$el, $$dref);
	  $$href{"$name"}->upd_data();
	}

	dat_unmap($loc,$status);
      }
    }
    print "\n" if $PDL::verbose;

  } else {

    my ($ndimx, @dim, $ndim);
    dat_shape($loc, 20, \@dim, $ndim, $status);

    # If we have a single dimension ($ndim=0) then
    # proceed. Cant yet cope with multi-dimensional
    # extensions
    if ($ndim == 0) {
      dat_ncomp($loc, $ncomp, $status);
      print "$name ($type) has $ncomp components\n" if $PDL::verbose;

      # Store type of extension
      $$href{$name}{"_TYPES"}{STRUCT}{"$name"} = $type;

      for $comp (1..$ncomp) {
	dat_index($loc, $comp, $nloc, $status);
	$status = &find_prim($nloc, \%{$$href{$name}}, $class, $status);
	dat_annul($nloc, $status);
      }
    } else {
      print "Extension $name is an array structure -- skipping\n"
	if $PDL::verbose;
    }

  }
  return $status;
}


###### Routines for WRITING data ######################################

sub wdata {

  my ($outndf, $pdl, $status) = @_;
  my (@bounds, $ndims, @lower, @comps, $dcomp, $temppdl, $type);
  my ($pntr, $el, $nbytes, $axis, $axcomp, $axpntr, %hdr, %axhdr);
  my ($entry, $value, $i, $axndims, $axtype, $tcomp, @acomps);


  # Return if bad status
  return  0 if $status != &NDF::SAI__OK;

  ####################################################################
  #                      D  I  M  S                                  #
  ####################################################################
  # Change the bounds to match the PDL
  @bounds = $pdl->dims;
  $ndims = $#bounds;
  @lower = map { 1 } (0..$ndims);

  &ndf_sbnd($ndims+1, \@lower, \@bounds, $outndf, $status);


  ####################################################################
  #    D A T A  +  V  A  R  I  A  N  C  E  +  Q U A L I T Y          #
  ####################################################################
  # Map data, variance, quality...
  @comps = ('Data','Errors','Quality');

  # Retrieve header and check that the header is a hash ref
  my $hdr = $pdl->gethdr;
  if (ref($hdr) eq 'HASH') {
    %hdr = %$hdr;
  } else {
    %hdr = ();
  }

  for $dcomp (@comps) {

    if ($dcomp eq 'Data') {
      $temppdl = $pdl;
    } else {
      if (exists $hdr{"$dcomp"}) {
        $temppdl = $hdr{"$dcomp"};
        # Check that we have a PDL
        next unless UNIVERSAL::isa($temppdl, 'PDL');
      } else {
        next;			# Skip this component
      }
    }

    # Check that we have some data
    if ($temppdl->nelem > 0) {

      if (($dcomp eq 'Quality') && ($temppdl->get_datatype != $PDL_B)) {
        # Quality must be bytes so convert to BYTE if this is not true
        $temppdl = byte ($$pdl{"$dcomp"});
      }

      $type = $pdltypes{$temppdl->get_datatype};
      $type = '_UBYTE' if $dcomp eq 'Quality'; # No choice for QUALITY

      # Set the output type of the NDF
      $tcomp = $dcomp;
      $tcomp = 'Variance' if $tcomp eq 'Errors';
      ndf_stype($type, $outndf, $tcomp, $status) unless $dcomp eq 'Quality';

      print "Mapping $dcomp , $type\n" if $PDL::verbose;
      # Map NDF
      $dcomp = 'Error' if $dcomp eq 'Errors';
      ndf_map($outndf, $dcomp, $type, 'WRITE', $pntr, $el, $status);

      if ($status == &NDF::SAI__OK) {
        # Number of bytes per entry
        $nbytes = PDL::Core::howbig($temppdl->get_datatype);

        # Convert to 1D data stream
        my $p1d = $temppdl->clump(-1);
        # Total number of bytes
        $nbytes *= $p1d->getdim(0);

        # Copy to disk
        string2mem(${$temppdl->get_dataref}, $nbytes, $pntr);

        # Write badbit mask
        if ($dcomp eq 'Quality') {
          ndf_sbb($$temppdl{Hdr}{BADBIT}, $outndf, $status)
            if defined $$temppdl{Hdr}{BADBIT};
        }
      }
      # Unmap Data
      ndf_unmap($outndf, $tcomp, $status);

    }

    # free the temporary pdl
    undef $temppdl;

  }


  ####################################################################
  #                      A  X  I  S                                  #
  ####################################################################
  # A X I S information is expected as an array in the header
  # called 'Axis'. These PDLs contain the CENTRE data and any further
  # info is stored in their header.
  # @bounds is accessed to find expected shape


  # Simply look in the header for a Axis
  if (exists $hdr{Axis}) {
     # Check that we have an array
     if (ref($hdr{Axis}) eq 'ARRAY') {
       # Now loop over axes
       for my $i (0..$#{$hdr{Axis}} ) {
	 # Loop unless status is bad
	 last unless $status == &NDF::SAI__OK;

	 # Extract the ith axis PDL from the array
         my $axis = $hdr{Axis}->[$i];

         # If we have a PDL
         if (UNIVERSAL::isa($axis, 'PDL')) {
            # We now want to copy the data and if necessary the
            # Error array. Since there are only two I will do it the
            # long way by explcitly storing data and then error

            # Set data type
            $axtype = $pdltypes{$axis->get_datatype};
            ndf_astyp($axtype, $outndf, 'Centre', $i+1, $status);

            # Okay we can now map this pdl
            ndf_amap($outndf, 'Centre', $i+1, $axtype, 'WRITE', $axpntr,
                   $el, $status);

            # Check that we have the correct number of entries
	    my $nelem = $axis->nelem;
            if ($el == $nelem) {
	      print "Mapping axis " , $i+1 , "\n"  if $PDL::verbose;
	    
              # Number of bytes per entry
              $nbytes = PDL::Core::howbig($axis->get_datatype) * $el;
	    
              # Copy to disk
              string2mem( $ { $axis->get_dataref }, $nbytes, $axpntr)
                 if ($status == &NDF::SAI__OK);

            } else {
              carp "Axis ",$i+1 .
                   " is the wrong size ($el values required but got ".
                   $nelem . ")- ignoring";
            }
            # Unmap
            ndf_aunmp($outndf, '*', $i+1, $status);

            # Errors
            my $axhdr = $axis->gethdr; # Retrieve and check header
            if (ref($axhdr) eq 'HASH') {
              %axhdr = %$axhdr;
            } else {
              %axhdr = ();
            }

	    # Look for an Errors component in the header hash
            if (exists $axhdr{Errors}) {
               my $axerr = $axhdr{Errors};
               if (UNIVERSAL::isa($axerr, 'PDL')) {

                 # Set data type
                 $axtype = $pdltypes{$axerr->get_datatype};
                 ndf_astyp($axtype, $outndf, 'Variance', $i+1, $status);

                 # Okay we can now map this pdl
                 ndf_amap($outndf, 'Errors', $i+1, $axtype, 'WRITE',
                   $axpntr, $el, $status);

                 # Check that we have the correct number of entries
		 my $nelem = $axerr->nelem;
		 print "Nelem: $nelem and $el\n";
                 if ($nelem == $el) {
                   print "Mapping errors for axis " . $i+1 . "\n"
                     if $PDL::verbose;

                   # Number of bytes per entry
                   $nbytes = PDL::Core::howbig($axerr->get_datatype) * $el;

                   # Copy to disk
                   string2mem($ {$axerr->get_dataref}, $nbytes, $axpntr)
                     if ($status == &NDF::SAI__OK);

                 } else {
                    carp "Error PDL for Axis ",$i+1,
                       " is the wrong size ($el values required but got ".
                        $axerr->nelem . ")- ignoring";
                 }
                 # Unmap
                 ndf_aunmp($outndf, '*', $i+1, $status);

               }

            }

            # Now character components
            foreach my $char (keys %axhdr) {
              if ($char =~ /LABEL|UNITS/i) {
                $value = $axhdr{"$char"};
                ndf_acput($value, $outndf, $char, $i+1, $status)
                  if length($value) > 0;
              }

            }

         }

       }

     }

  }

}


# Write header information to NDF extension
# This routine does not reconstruct an NDF identically to one that
# was read in. FITS extensions are made. All non-integer numeric types
# are written as doubles (apart from PDLs which carry there own type
# information) unless _TYPES information exists in {NDF}.


sub whdr {

  my ($outndf, $pdl, $status) = @_;

  my (%header, @fitsdim, $fitsloc, $value);
  my (%unused, @fits, $hashref, $hdr);

  # Return if bad status
  return 0 if $status != &NDF::SAI__OK;

  print "Writing header information...\n" if $PDL::verbose;

  # Write FITS header from {Hdr}

  # Retrieve and check header from PDL
  $hdr = $pdl->gethdr;
  if (ref($hdr) eq 'HASH') {
    %header = %$hdr;
  } else {
    %header = ();
  }

  %unused = ();
  @fits = ();

  foreach my $key (sort keys %header) {

    next if $key eq '_COMMENTS';

    if ($key =~ /^TITLE$|^UNITS$|^LABEL$/i) {
      # This is not extension info
      ndf_cput($header{$key}, $outndf, $key, $status)
        if length($header{$key}) > 0;
    }

    # Only write scalars
    if (not ref($header{"$key"})) {

       push(@fits, fits_construct_string($key,$header{$key},
            $header{'_COMMENTS'}{$key}) );

    } else {
      # Deal with later
      $unused{$key} = $header{$key};
    }

  }

  # Write the FITS extension
  if ($#fits > -1) {
    push(@fits, "END");
    # Write it out
    @fitsdim = ($#fits+1);
    ndf_xnew($outndf, 'FITS', '_CHAR*80', 1, @fitsdim, $fitsloc, $status);
    dat_put1c($fitsloc, $#fits+1, \@fits, $status);
    dat_annul($fitsloc, $status);
  }

  # Loop through all NDF header info and array Hdr information
  # Note that %unused still contains our NDF hash so
  # we must remove that before proceeding (else we end up with
  # an 'NDF_EXT' extension!

  delete $unused{$EXTNAME};

  foreach $hashref (\%unused, $header{$EXTNAME}) {
     whash($hashref, $outndf, '', '', $status);
  }
}


#-----------------------------------------------------
# This routine writes a hash array to a NDF extension
# Keys that have leading underscores are skipped
# $hashname contains the extension name to use by default.
# This is extended  by any '.' in the key.

sub whash {

  my ($hash, $outndf, $hashname, $stypes, $status) = @_;

  my ($key, $comp, @structures, $extension, $xtype, @dim, $ndims, $loc);
  my (@locs, $there, $type, %header, $packed, $length);
  my (@bounds, $nbytes, $pntr, $maxsize, $which, $el, $path);
  my ($oldhash, $oldtypes, $structs);

  if (defined $hash) {
    %header = %$hash;
  } else {
    %header = ();
  }


  my $root = 1 if length($hashname) == 0; # Mark a ROOT structure

  # Return if bad status
  return 0 if $status != &NDF::SAI__OK;

  foreach $key (sort keys %header) {

    next if $key =~ /^_/;

    # If the key matches the Extensions (Axis or errors)
    # then skip. Should probably use the Extensions array
    # itself to work out which components to skip

    next if $key eq 'Axis';
    next if $key eq 'Errors';
    next if $key eq 'Extensions';
    next if $key eq 'Hist';

    # Deal with HASH arrays early
    ref($header{"$key"}) eq 'HASH' && do {
      $oldhash = $hashname;
      $oldtypes = $stypes;
      $hashname .= ".$key";
      $hashname =~ s/^\.//; # remove leading dot
      $stypes .= ".".$header{"$key"}{'_TYPES'}{STRUCT}{"$key"};
      $stypes =~ s/^\.//; # remove leading dot
      whash(\%{$header{"$key"}}, $outndf, $hashname, $stypes, $status);
      $hashname = $oldhash;
      $stypes = $oldtypes;
      next;
    };



    $path = $hashname . ".$key";
    $path =~ s/^\.//; # remove leading dot

    if ($key =~ /^TITLE$|^UNITS$|^LABEL$/i) {
      # This is not extension info
      ndf_cput($header{$key}, $outndf, $key, $status);
    } else {

      # Find nested structures
      @structures = split(/\./, uc($path));

      # Last entry in structure is the important name
      $comp = pop(@structures);

      # Find list of structures
      $structs = join(".", @structures);
      $structs = 'PERLDL' unless (defined $structs && $structs =~ /./);
      $stypes = 'PERLDL_HDR' unless (defined $stypes && $stypes =~ /./);

      $loc = mkstruct($outndf, $structs, $stypes, $status);
      undef $stypes;

      # Now write the component
      # $loc is a locator to the last structure component
      $type = $header{"_TYPES"}{"$key"};

      # What is the data type?
      unless ($type =~ /./) {
        # number or character or array or pdl
        # All arrays are chars - should be PDLs if nums
        ref($header{"$key"}) eq 'ARRAY' && ($type = "_CHAR");

        ref($header{"$key"}) eq 'PDL' &&
          ($type = $pdltypes{$header{"$key"}{"Datatype"}});

        ref($header{"$key"}) || do {
          if ($header{"$key"} =~ /^(-?)(\d*)(\.?)(\d*)([Ee][-\+]?\d+)?$/) {
            $header{"$key"} =~ /\.|[eE]/
              && ($type = "_DOUBLE")
                ||   ($type = "_INTEGER");
          } else {
            ($type = '_CHAR')   # Character
          }
        };

      }

      # Create and write component
      # PDLs

      if (ref($header{"$key"}) eq 'PDL') {
        my $pdl = $header{"$key"};
        @bounds = $pdl->dims;
	my $n = $#bounds + 1;
        dat_new($loc, $comp, $type, $n, \@bounds, $status);
        $nbytes = PDL::Core::howbig($pdl->get_datatype) *
            $pdl->nelem;
        cmp_mapv($loc, $comp, $type, 'WRITE', $pntr, $el, $status);
        string2mem(${$pdl->get_dataref}, $nbytes, $pntr)
          if ($status == &NDF::SAI__OK);
        cmp_unmap($loc, $comp, $status);
      }

      # SCALARS
      ref($header{"$key"}) || do {

        if ($type =~ /^_CHAR/) {
          $length = length($header{"$key"});
          dat_new0c($loc, $comp, $length, $status);
          cmp_put0c($loc, $comp, $header{"$key"}, $status)
            if $length > 0;
	} elsif ($type =~ /^_LOG/) {
	  # In this case, add a check for FALSE or TRUE as strings
	  dat_new0l($loc, $comp, $status);
	  my $value = $header{$key};
	  if ($value eq 'FALSE') {
	    $value = 0;
	  } elsif ($value eq 'TRUE') {
	    $value = 1;
	  }
	  cmp_put0l($loc, $comp, $value, $status);
        } else {
	  no strict "refs"; # Turn off strict
          $which = lc(substr($type, 1,1));
          &{"dat_new0$which"}($loc, $comp, $status);
          &{"cmp_put0$which"}($loc, $comp, $header{"$key"}, $status);
        }
      };

      # CHARACTER ARRAYS
      ref($header{"$key"}) eq 'ARRAY' && do {
        # First go through the array and remove any
        # entries that are references. I draw the line at storing
        # nested arrays
        my @norefs = grep { not ref($_) } @{$header{"$key"}};

        ($maxsize, $packed) = &NDF::pack1Dchar(\@norefs);
        $ndims = 1;
        @dim = ($#norefs+1);
        dat_newc($loc, $comp, $maxsize, $ndims, @dim, $status);
        cmp_putvc($loc, $comp, $dim[0], @norefs, $status);
      };

      # Annul all the locators
      dat_annul($loc, $status);

    }
  }

}

# Subroutine to make extension structures of arbritrary depth
# Just pass dot separated string to this function
# A locator to the final structure is returned

sub mkstruct {

  my ($outndf, $structs, $types, $status) = @_;
  my ($extension, @locs, @dim, $ndims, $xtype, $loc, $retloc);
  my ($set, $prmry, $there);

  my (@structures) = split('\.', $structs);
  my (@types) = split('\.', $types);

  return &NDF::DAT__NOLOC if $#structures < 0;
  return &NDF::DAT__NOLOC if $status != &NDF::SAI__OK;

  # Does extension exist

  $extension = shift(@structures);
  $xtype = shift(@types);

  ndf_xstat($outndf, $extension, $there, $status);

  # Make it if necessary
  unless ($there) {
    print "Writing $extension extension ($xtype)...\n" if $PDL::verbose;
    @dim = (0);         # No pdl extensions!
    $ndims = 0;
    ndf_xnew($outndf, $extension, $xtype, $ndims, @dim, $loc, $status);
  } else {                      # Get the locator to the extension
    ndf_xloc($outndf, $extension, 'UPDATE', $loc, $status);
  }


  @locs = ();           # store the locators so that they can be annulled later
  push(@locs, $loc);

  # Make the structures, end up with a locator to the correct component
  for (0..$#structures) {
    dat_there($loc, $structures[$_], $there, $status);
    unless ($there) {   # Make it if it isnt there
      my $type = $types[$_];
      @dim = (0);
      dat_new($loc, $structures[$_], $type, 0, @dim, $status);
    }
    dat_find($loc, $structures[$_], $loc, $status);
    push(@locs, $loc);  # Store the new locator
  }

  # Clone the locator
  dat_clone($loc, $retloc, $status);
  $set = 1;
  $prmry = 1;
  dat_prmry($set, $retloc, $prmry, $status);

  # Annul all except the last locator
  foreach (@locs) {
    dat_annul($_, $status);
  }

  return &NDF::DAT__NOLOC unless $status == &NDF::SAI__OK;
  return $retloc;

}



1;
