#!/bin/perl

package PDL::Io::NDF;

use PDL::Core;     # Grab the Core names
use DynaLoader; use Carp; use SelfLoader;use strict;

use vars qw/@EXPORT_OK @ISA @EXPORT_STATIC/;

# Starlink data type conversion
use vars qw/%pdltypes %startypes $ndf_loaded $VERSION/;

'$Revision$ ' =~ /.*:\s(.*)\s\$/ && ($VERSION = "$1");

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


@EXPORT_OK = qw( wndf rndf propndfx );
@ISA = qw( PDL::Exporter DynaLoader SelfLoader ); 
@EXPORT_STATIC = qw( rndf );

1;

__DATA__

############################################################################
#  WNDF
#    Routine to write a PDL to an NDF.
#    Header information is written to NDF extensions.
#    DATA,VARIANCE,QUALITY, AXIS are written from a PDL
#    (if they exist)
############################################################################

sub wndf {  # Write a PDL to a NDF format file

  use strict;

  croak 'Usage: wndf($pdl, $outfile)' if $#_!=1;

  my ($indf, $place, $status, $place, $outndf);
  my (@lbnd, @ubnd);

  my ($pdl, $outfile) = @_; 
  eval 'use NDF' unless $ndf_loaded++;
  croak 'Cannot use NDF library' if $@ ne "";

  # Set status
  $status = &NDF::SAI__OK;
  
  # Strip trailing .sdf if one is present
  $outfile =~ s/\.sdf$//;

  # Begin context
  ndf_begin();
  err_begin($status);

  # Open the new file
  ndf_place(&NDF::DAT__ROOT, $outfile, $place, $status);

  # Need to create data_array component
  # Change the bounds to match the PDL

  @ubnd = dims($pdl);
  @lbnd = ();
  for (0..$#ubnd) {
    @lbnd[$_] = 1;
  }

  ndf_new($pdltypes{$$pdl{Datatype}}, $#ubnd+1, @lbnd, @ubnd, $place, 
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
  err_end($status);

  croak "Bad STATUS in wndf\n" if $status != &NDF::SAI__OK; 

  return 1;
}


############################################################################
#  PROPNDFX
#    Routine to write a PDL to an NDF by copying the extension from an
#    existing NDF and writing DATA,VARIANCE,QUALITY, AXIS from a PDL
#    (if they exist)
#    Extensions, labels and history are propogated from the old NDF.
#    No new extension information is written.
############################################################################

sub propndfx {  # Write a PDL to a NDF format file

  use strict;

  croak 'Usage: propndfx($pdl, $infile, $outfile)' if $#_!=2;

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
  $file = (split(/\./,$infile))[0];

  croak "$file does not exist\n" unless -e "$file.sdf";

  # Begin context
  ndf_begin();
  err_begin($status);

  # Open the old file
  ndf_open(&NDF::DAT__ROOT, $infile, 'READ', 'OLD', $indf, $in_place, $status);

  # Set the application name
  ndf_happn('PDL::propndfx', $status);

  # Open the new file
  ndf_place(&NDF::DAT__ROOT, $outfile, $outplace, $status);

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
  err_end($status);

  croak "Bad STATUS in propndfx\n" if $status != &NDF::SAI__OK; 

  return 1;
}

#########################################################################
# Routine to read an NDF and extensions into a PDL
# If a second argument is passed for $nomask, automatic quality masking
# will be switched off and a QUALITY array will also be mapped
#
# Header information is populated with all the extensions, FITS information,
# title and data units. Quality bad bit mask is stored
# in the Hdr extensions of the Quality PDL.
#########################################################################

sub rndf {  # Read an NDF format file into a PDL

  use strict;

  croak 'Usage: ($data) = rndf($file,[$nomask]) or PDL->rndf(...) ' 
    if ($#_!=1 && $#_!=2);
  my $class = shift;
  my $file = shift; my $pdl = $class->new();
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

  # Read the data array
  rdata($indf, $pdl, $nomask, $status);

  # Read the axes
  raxes($indf, $pdl, $status);

  # Read the header
  rhdr($indf, $pdl, $status);

  # Read history information
  rhist($indf, $pdl, $status);

  # Read labels
  @info = ('Label', 'Title', 'Units');
  for $entry (@info) {
    $value = 'NULL';
    ndf_cget($indf, $entry, $value, $status);
    $$pdl{Hdr}{"$entry"} = $value if $value ne 'NULL';
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
  $pdl->flush();  # A final flush  

  return $pdl;
}


###########################################################################
###########################################################################
###########################################################################
#    INTERNAL SUBROUTINES 
###########################################################################
###########################################################################
###########################################################################

#  RDATA
#    Read Data, Quality and Variance

sub rdata {
  use strict;
  my ($indf, $pdl, $nomask, $status) = @_;

  return $status if $status != &NDF::SAI__OK;

  my ($maxdims, @dim, $ndim, @dim, @comps, $dcomp, $tcomp, $exist);
  my ($type, $data_pntr, $el, $temppdl, $nbytes, $badbit);

  ####################################################################
  #                      D  I  M  S                                  #
  ####################################################################
  
  # Find the dimensions of the data array
  $maxdims = 100;		# Maximum number of dimensions allowed

  &ndf_dim($indf, $maxdims, \@dim, $ndim, $status);
  @dim = @dim[0..$ndim-1];
  
  print "Dims = @dim\n" if $PDL::verbose;
  
  $$pdl{Extensions} = [];

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
	  $$pdl{"$dcomp"} = pdl();
	  $temppdl = $$pdl{"$dcomp"};
	}
      
	$$temppdl{Dims} = [@dim];
	$$temppdl{Datatype} = $startypes{$type};
     
	# How many bytes in this data type? 
	$type =~ s/^_|^_U//;
	$nbytes = &NDF::byte_size($type);
      
	mem2string($data_pntr, $nbytes*$el, $$temppdl{Data});

	push(@{$$pdl{Extensions}}, $dcomp) if $dcomp ne 'Data';

	if ($dcomp eq 'Quality') {
	  # Quality bad bit mask
	  ndf_bb($indf, $badbit, $status);
	  $$temppdl{Hdr}{BADBIT} = $badbit;
	}

	# Flush and Clear temporary PDL
	$temppdl->flush();
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

sub raxes {
  use strict;
  my ($indf, $pdl, $status) = @_;

  return $status if $status != &NDF::SAI__OK;

  my ($there, $naxis, @dims, $axcomp, $exist, $axtype, $axpntr, $el);
  my ($nbytes, $entry, $value, $temppdl, $tcomp);

  # Read in axis information
  ndf_state($indf, 'Axis', $there, $status);

  @dims = dims $pdl; # Find number of dimensions
  
  if ($there) {

    # Label from 0..$#dims to match array index handling

    for $naxis (0..$#dims) {
      # Does a CENTRE component exist
      $axcomp = 'CENTRE';
      ndf_astat($indf, $axcomp, $naxis+1, $exist, $status);

      if ($exist && ($status == &NDF::SAI__OK)) {

	print "Reading Axis$naxis...\n";
	ndf_atype($indf, $axcomp, $naxis+1, $axtype, $status);
	
	ndf_amap($indf, $axcomp, $naxis+1, $axtype, 'READ', $axpntr, 
		 $el, $status);
	
	# Set up new PDL for axis info if map was okay
	if ($status == &NDF::SAI__OK) {
	  $$pdl{"Axis$naxis"} = pdl();
	  $temppdl = $$pdl{"Axis$naxis"};
	  # Update EXTENSIONS array
	  push(@{$$pdl{Extensions}}, "Axis$naxis");

	  $$temppdl{Dims} = [$el];
	  $$temppdl{Datatype} = $startypes{$axtype};
  
	  # How many bytes?
	  $axtype =~ s/^_|^_U//;
	  $nbytes = &NDF::byte_size($axtype);
	  
	  mem2string($axpntr, $nbytes*$el, $$temppdl{Data});

	  # Read VARIANCE array if there
	  $axcomp = 'Error';
	  $tcomp = 'Variance';
	  ndf_astat($indf, $tcomp, $naxis+1, $exist, $status);

	  if ($exist && ($status == &NDF::SAI__OK)) {
	    print "Reading Axis$naxis errors...\n";
	    ndf_atype($indf, $tcomp, $naxis+1, $axtype, $status);
	
	    ndf_amap($indf, $axcomp, $naxis+1, $axtype, 'READ', $axpntr, 
		     $el, $status);
	    # Set up new PDL for axis info if map was okay
	    if ($status == &NDF::SAI__OK) {
	      $$pdl{"Axis$naxis"}{Errors} = pdl();
	      $temppdl = $$pdl{"Axis$naxis"}{Errors};
	      # Update EXTENSIONS array
	      $$pdl{"Axis$naxis"}{Extensions} = ["Errors"];
	      
	      $$temppdl{Dims} = [$el];
	      $$temppdl{Datatype} = $startypes{$axtype};
	      
	      # How many bytes?
	      $axtype =~ s/^_|^_U//;
	      $nbytes = &NDF::byte_size($axtype);
	      
	      mem2string($axpntr, $nbytes*$el, $$temppdl{Data});
	      $$pdl{"Axis$naxis"}{Errors}->flush();
	    }
	  }

	  # Get label and units
	  for $entry ('Units', 'Label') {
	    ndf_astat($indf, $entry, $naxis+1, $exist, $status);
	    if ($exist) {
	      ndf_acget($indf,$entry, $naxis+1, $value, $status);
	      $$pdl{"Axis$naxis"}{Hdr}{"$entry"} = $value;
	    }
	  }
	}
	ndf_aunmp($indf,'*',$naxis+1,$status);

	$$pdl{"Axis$naxis"}->flush();	
	
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
  use strict;
  my ($indf, $pdl, $status) = @_;

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
	  $$pdl{Hdr}{$item} = $value;
	  $$pdl{Hdr}{'_COMMENTS'}{$item} = $comment;
	}
      } else {
	print "Error mapping $xname\n";
	err_annul($status);
      }
      undef @fits;
    } else {

      # Read in extensions to {NDF}
      $status = &find_prim($xloc, \%{$$pdl{NDF}}, $status);
    }

    dat_annul($xloc, $status);
  }
  return $status;
}  
  
  ####################################################################
  #                      H  I  S  T  O  R  Y                         #
  ####################################################################
  
sub rhist {
  use strict;
  my ($indf, $pdl, $status) = @_;

  return $status if $status != &NDF::SAI__OK;

  my ($nrec, $app, $date, $i);
  
  # Read HISTORY information into a Hist header
  
  ndf_hnrec($indf,$nrec,$status);
  
  if ($status == &NDF::SAI__OK && ($nrec > 0)) {
    print "Reading  history information into 'Hist'...\n" if $PDL::verbose;
    
    $$pdl{Hist}{'Nrecords'} = $nrec;
    
    # Loop through the history components and find last "APPLICATION"
    for $i (1..$nrec) {
      
      ndf_hinfo($indf, 'APPLICATION', $i, $app, $status);
      ndf_hinfo($indf, 'DATE', $i, $date, $status);
      
      $$pdl{Hist}{"Application$i"} = $app;
      $$pdl{Hist}{"Date$i"} = $date;
      
      print "  $app on $date\n" if $PDL::verbose;
    }
  } else {
    err_annul($status);    
  }
  return $status;
}  

#####  - INTERNAL SUBROUTINES - ####

# Find primitive components below a given HDS locator
# FITS information is stored in Hdr
# NDF extensions stored in NDF

sub find_prim {

  use strict;

  my ($loc, $href, $status) = @_;
  my ($prim, $type, $size, @dim, $ndims, $struct, $name, $nloc, $el);
  my ($value, @values, $item, $entry, $maxdims);
  my ($packtype, $ncomp, $packed, $comp);
  my ($pntr, $nbytes, $comment);

  # Return if bad status
  return  0 if $status != &NDF::SAI__OK;

  # Most dimensions
  $maxdims = 100;

  # Is it a primitive component
  dat_prim($loc, $prim, $status);

  # Type, size and shape
  dat_type($loc, $type, $status);
  dat_size($loc, $size, $status);
  dat_shape($loc, $maxdims, @dim, $ndims, $status);
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
	dat_getvc($loc, $size, @values, $el, $status);

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
	  $$href{"$name"} = pdl();
	  $$href{"$name"}{Dims} = [@dim];
	  $$href{"$name"}{Datatype} = $startypes{$type};


	  # How many bytes in this data type? 
	  $type =~ s/^_|^_U//;
	  $nbytes = &NDF::byte_size($type);
      
	  mem2string($pntr, $nbytes*$el, $$href{"$name"}{Data});
	}
    
	dat_unmap($loc,$status);
	$$href{"$name"}->flush(); # Flush PDL	
      }
    }
    print "\n" if $PDL::verbose;      

  } else {

    dat_ncomp($loc, $ncomp, $status);
    print "$name ($type) has $ncomp components\n" if $PDL::verbose;

    # Store type of extension
    $$href{$name}{"_TYPES"}{STRUCT}{"$name"} = $type;

    for $comp (1..$ncomp) {
      dat_index($loc, $comp, $nloc, $status);
      $status = &find_prim($nloc, \%{$$href{$name}}, $status);
      dat_annul($nloc, $status);
    }

  }
  return $status;
}


# This subroutine maps data,error,quality and axes to
# and NDF identifier

sub wdata {

  use strict;

  my ($outndf, $pdl, $status) = @_;
  my (@bounds, $ndims, @lower, @comps, $dcomp, $temppdl, $type);
  my ($pntr, $el, $nbytes, $naxis, @axbnd, $axis, $axcomp, $axpntr);
  my ($entry, $value, $i, $axndims, $axtype, $tcomp, @acomps);


  # Return if bad status
  return  0 if $status != &NDF::SAI__OK;

  ####################################################################
  #                      D  I  M  S                                  #
  ####################################################################
  # Change the bounds to match the PDL
  @bounds = dims($pdl);
  $ndims = $#bounds;
  @lower = ();
  for $i (0..$ndims) {
    $lower[$i] = 1;
  }

  ndf_sbnd($ndims+1, @lower, @bounds, $outndf, $status);

  ####################################################################
  #    D A T A  +  V  A  R  I  A  N  C  E  +  Q U A L I T Y          #
  ####################################################################
  # Map data, variance, quality...
  @comps = ('Data','Errors','Quality');

  for $dcomp (@comps) {

    if ($dcomp eq 'Data') {
      $temppdl = $pdl;
    } else {
      $temppdl = $$pdl{"$dcomp"};
    }

    if (length($$temppdl{Data}) > 0) {

      if (($dcomp eq 'Quality') && ($$pdl{"$dcomp"}{Datatype} != $PDL_B)) {
	# Quality must be bytes so convert to BYTE if this is not true
	$temppdl = byte ($$pdl{"$dcomp"});
      } 

      $type = $pdltypes{$$temppdl{Datatype}};
      $type = '_UBYTE' if $dcomp eq 'Quality'; # No choice for QUALITY

      # Set the output type of the NDF
      $tcomp = $dcomp;
      $tcomp = 'Variance' if $tcomp eq 'Errors';
      ndf_stype($type, $outndf, $tcomp, $status) unless $dcomp eq 'Quality';

      print "Mapping $dcomp, $type\n" if $PDL::verbose;
      # Map NDF
      $dcomp = 'Error' if $dcomp eq 'Errors';
      ndf_map($outndf, $dcomp, $type, 'WRITE', $pntr, $el, $status);

      if ($status == &NDF::SAI__OK) {
	$nbytes = length $$temppdl{Data};
	string2mem($$temppdl{Data}, $nbytes, $pntr);

	# Write badbit mask
	if ($dcomp eq 'Quality') {
	  ndf_sbb($$temppdl{Hdr}{BADBIT}, $outndf, $status) 
	    if defined $$temppdl{Hdr}{BADBIT};
	}
      }
      # Unmap Data
      ndf_unmap($outndf, $tcomp, $status);

    }
    undef $temppdl;
  }

  ####################################################################
  #                      A  X  I  S                                  #
  ####################################################################
  #  A X I S information (stored as Axis0, Axis1 etc...)
  # @bounds is accessed to find expected shape
  
  foreach $axis (keys %$pdl) {
    if ($axis =~ /^Axis/) {	# Found an axis
      $naxis = substr($axis,4);
      
      @acomps = ('Centre', 'Errors');
      
      foreach $axcomp (@acomps) {

	$tcomp = $axcomp;
	if ($axcomp eq 'Centre') {
	  $temppdl = $$pdl{$axis};
	} elsif ($axcomp eq 'Errors') {
	  next unless defined $$pdl{$axis}{"$axcomp"};
	  $temppdl = $$pdl{$axis}{"$axcomp"};
	  $axcomp = 'Errors';
	  $tcomp = 'Variance';
	} else {
	  next;
	}
	
	@axbnd = dims($temppdl);
	$axndims = $#axbnd;
	# Check bounds
	print "Ndims = $axndims and @axbnd and $bounds[$naxis]\n";
	if (($axndims == 0) && ($axbnd[0] == $bounds[$naxis])) {
	  $axtype = $pdltypes{$$temppdl{Datatype}};
	  ndf_astyp($axtype, $outndf, $tcomp, $naxis+1, $status);
	  
	  ndf_amap($outndf, $axcomp, $naxis+1, $axtype, 'WRITE', $axpntr, 
		   $el, $status);
	  
	  if ($status == &NDF::SAI__OK) {
	    print "Mapping $axis $axcomp...\n";
	    $nbytes = length($$temppdl{Data});
	    string2mem($$temppdl{Data}, $nbytes, $axpntr);
	    
	    # Map label and units
	    if ($axcomp eq 'Centre') {
	      foreach $entry (keys %{$$temppdl{Hdr}}) {
		if ($entry =~ /LABEL|UNITS/i) {
		  $value = $$temppdl{Hdr}{"$entry"};
		  ndf_acput($value, $outndf, $entry, $naxis+1, $status)
		    if length($value) > 0;
		}
	      }
	    }
	  }
	  
	  ndf_aunmp($outndf, '*', $naxis+1, $status);
	  
	} else {
	  print "$axis does not match the corresponding PDL size\n";
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

  use strict;

   my ($outndf, $pdl, $status) = @_;

   my ($key, %header, @fitsdim, $fitsloc, $value);
   my (%unused, @fits, $hashref);

   # Return if bad status
   return 0 if $status != &NDF::SAI__OK;

   print "Writing header information...\n" if $PDL::verbose;

   # Write FITS header from {Hdr}

   %header = %{$$pdl{Hdr}};
   %unused = ();
   @fits = ();

   foreach $key (sort keys %header) {

     next if $key eq '_COMMENTS';

     if ($key =~ /^TITLE$|^UNITS$|^LABEL$/i) {
       # This is not extension info
       ndf_cput($header{$key}, $outndf, $key, $status) 
	 if length($header{$key} > 0);
     }

     # Only write scalars
     unless (ref($header{"$key"}) =~ /./) {
       
       push(@fits, fits_construct_string($key,$header{$key},$header{'_COMMENTS'}{$key}) );

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
     dat_put1c($fitsloc, $#fits+1, @fits, $status);
     dat_annul($fitsloc, $status);
   }

   # Loop through all NDF header info and array Hdr information

   foreach $hashref (\%unused, \%{$$pdl{NDF}}) {
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
  my ($oldhash, $oldtypes);

  %header = %$hash;

  $root = 1 if length($hashname) == 0; # Mark a ROOT structure
  
  # Return if bad status
  return 0 if $status != &NDF::SAI__OK;
  
  foreach $key (sort keys %header) {
    
    next if $key =~ /^_/;

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
      $structs = 'PERLDL' unless ($structs =~ /./);
      $stypes = 'PERLDL_HDR' unless ($stypes =~ /./);
	
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
	    ($type = '_CHAR')	# Character
	  }
	};
	
      }
      
      # Create and write component
      # PDLs
      
      if (ref($header{"$key"}) eq 'PDL') {
	@bounds = dims($header{"$key"});
	dat_new($loc, $comp, $type, $#bounds+1, @bounds, $status);
	$nbytes = length($header{"$key"}{Data});
	cmp_mapv($loc, $comp, $type, 'WRITE', $pntr, $el, $status);
	string2mem($header{"$key"}{Data}, $nbytes, $pntr)
	  if ($status == &NDF::SAI__OK);
	cmp_unmap($loc, $comp, $status);
      }
      
      # SCALARS
      ref($header{"$key"}) || do {
	
	if ($type =~ /_CHAR/) {
	  $length = length($header{"$key"});
	  dat_new0c($loc, $comp, $length, $status);
	  cmp_put0c($loc, $comp, $header{"$key"}, $status)
	    if $length > 0;
	} else {
	  $which = lc(substr($type, 1,1));
	  &{"dat_new0$which"}($loc, $comp, $status);
	  &{"cmp_put0$which"}($loc, $comp, $header{"$key"}, $status);
	}
      };
      
      # CHARACTER ARRAYS
      ref($header{"$key"}) eq 'ARRAY' && do {
	($maxsize, $packed) = &NDF::pack1Dchar(\@{$header{"$key"}});
	$ndims = 1;
	@dim = ($#{$header{"$key"}}+1);
	dat_newc($loc, $comp, $maxsize, $ndims, @dim, $status);
	cmp_putvc($loc, $comp, $dim[0], @{$header{"$key"}}, $status);
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
  my ($set, $prmry);

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
    @dim = (0);		# No pdl extensions!
    $ndims = 0;
    ndf_xnew($outndf, $extension, $xtype, $ndims, @dim, $loc, $status);
  } else {			# Get the locator to the extension
    ndf_xloc($outndf, $extension, 'UPDATE', $loc, $status);
  }

  
  @locs = ();      	# store the locators so that they can be annulled later
  push(@locs, $loc);

  # Make the structures, end up with a locator to the correct component
  for (0..$#structures) {
    dat_there($loc, $structures[$_], $there, $status);
    unless ($there) {	# Make it if it isnt there
      $type = $types[$_];
      @dim = (0);
      dat_new($loc, $structures[$_], $type, 0, @dim, $status);
    }
    dat_find($loc, $structures[$_], $loc, $status);
    push(@locs, $loc);	# Store the new locator
  }

  # Clone the locator
  dat_clone($loc, $retloc, $status);
  $set = 1;
  $primry = 1;
  dat_prmry($set, $retloc, $prmry, $status);

  # Annul all except the last locator
  foreach (@locs) {
    dat_annul($_, $status);
  }      

  return &NDF::DAT__NOLOC unless $status == &NDF::SAI__OK;
  return $retloc;

}


1;

__END__
# Documentation

=head1 NAME

  PDL::NDF - PDL Module for reading and writing Starlink N-dimensional data 
structures as PDLs.

=head1 SYNOPSIS

  use PDL::NDF;

  $a = rndf('test_image');
  $a = rndf('test_image', 1);

  wndf($a, 'out_image');

  propndfx($a, 'template', 'out_image');

=head1 DESCRIPTION

This module add the ability to read and write Starlink N-dimensional data 
files as N-dimensional PDLs. Three routines are currently available:

=head2 rndf

rndf can be used for reading NDF files into PDL. Data, Quality, Variance, Axis
and numerical array extensions are read in as PDLs. All extensions are read
into the Hdr hash of the base PDL.

The first form of rndf performs automatic bad pixel masking from the quality
array. Use of the optional second parameter turns off this feature and the
quality array is mapped.

A NDF pdl (eg $a) has the following structure:

  $a   - the base DATA_ARRAY        
  $$a{Error} - the variance array 
  $$a{Quality}  - Quality byte array
  $$a{Axis0}    - the axis array associated with the first dimension
  $$a{Axis1} etc 
  $$a{Hdr}      - All FITS header information
  $$a{Hdr}{_COMMENTS} - Comments associated with the FITS information

  $$a{Hist}     - History information (for information only)
  $$a{NDF}      - NDF extension information
  $$a{NDF}{_TYPES} - Data types for non-PDL NDF extensions.

All extension information is stored in the Hdr associative array.
Extension structures are preserved in hashes, so that the PROJ_PARS
component of the IRAS.ASTROMETRY extension is stored in
$$a{NDF}{IRAS}{ASTROMETRY}{'PROJ_PARS'}. All array structures are
stored as arrays in the Hdr: numeric arrays are stored as PDLs,
logical and character arrays are stored as plain Perl arrays. FITS
arrays are a special case and are expanded as scalars into the header.

=head2 wndf

wndf can be used for writing PDLs to NDF files. All the extensions
created by rndf are supported by wndf.  This means that error, axis
and quality arrays will be written if they exist. Extensions are also
reconstructed by using their name (ie FIGARO.TEST would be expanded as
a FIGARO extension and a TEST component). Hdr keywords Label, Title
and Units are treated as special cases and are written to the label,
title and units fields of the NDF.

Hash arrays in {NDF} or {Hdr} are written to corresponding NDF extensions.
NDF extensions can also be created in the {NDF} hash by using a key
containging '.', ie {NDF}{'IRAS.DATA'} would write the information to
an IRAS.DATA extension in the NDF.

rndf stores type information in {NDF}{'_TYPES'} and below so that wndf
can reconstruct the data type of non-PDL extensions. If no entry
exists in _TYPES, wndf chooses between characters, integer and double
on a best guess basis.  Any perl arrays are written as CHAR array
extensions (on the assumption that and numeric arrays will exist as
PDLs).

All {NDF} extensions are written back to NDF extensions. Any scalar
arguments in {Hdr} are written to a NDF FITS extension. Any non-scalar
entries in {Hdr} are written to a PERLDL extension. All entried in
{NDF} which are not associated with a specific extension (via decimal
points) are written to a PERLDL extension.


=head2 propndfx

The limitations in wndf's ability to reconstruct an NDF of exactly the
same structure as an input NDF (because of loose typing and FITS
extensions) means that it is sometimes desirable to propogate an
extension from an already existing NDF rather than letting wndf try to
get it right.

In many case it is good enough to simply read in an NDF, modify the
data array and write out a new NDF with the old extension
information. propndfx supplies this ability. Title, label, units and
all extensions are supplied from the template NDF, whereas Data,
Variance, Quality and Axis are supplied from the PDL (if they exist).

It is still possible to modify the new extension field by hand using
the NDF module itself.

=head1 NOTES

The perl NDF module must be accessible.


=head1 AUTHOR

This module was written by Tim Jenness, timj@jach.hawaii.edu.

=head1 SEE ALSO

L<PDL> for general information on the Perl Data language,
L<NDF> for information on the NDF module.

=cut
