package Starlink::HDSPACK;

=head1 NAME

Starlink::HDSPACK - routines for high level HDS manipulation

=head1 SYNOPSIS

  use Starlink::HDSPACK;

  copobj("file.more.fits","file1.more", $status);
  delobj("file.more.fits");

=head1 DESCRIPTION

This module provides wrapper routines for common HDS manipulations.
Functions are provided for copying data structures between locations
and for deleting structures.


=cut

use Carp;
use strict;
use NDF;
use base qw/Exporter/;

use vars qw/ $VERSION @EXPORT_OK $DEBUG/;

use constant SAI__OK => &NDF::SAI__OK;
use constant SAI__ERROR => &NDF::SAI__ERROR;

'$Revision$ ' =~ /.*:\s(.*)\s\$/ && ($VERSION = $1);

$DEBUG = 0;
@EXPORT_OK = qw( copobj retrieve_locs delobj);

=head2 FUNCTIONS

The following functions are available:

=over 4

=item B<retrieve_locs>

Given a full path to a HDS structure, returns an array of 
locators corresponding to each structure referenced in the path.

  ($status, @locators) = retrieve_locs("file.more.fits", $mode, $status);

The mode argument specifies the file access type. Can be 'READ', 'WRITE'
or 'UPDATE'.

In the above example, C<@locators> would contain three entries,
a locator to the file itself, a locator to the C<more> structure
and a locator to the C<fits> component.

Bad status is returned if the named components do not exist or cannot
be opened.

This routine assumes we are in a valid error context (eg err_begin()
has been called).

The locators returned by this routine must be annulled by the caller.

=cut

sub retrieve_locs {
  croak 'Usage: retrieve_locs($name, $mode, $status)'
    unless scalar(@_) == 3;

  my ($name, $mode, $status) = @_;

  # Return if bad status on entry
  return ($status) if $status != SAI__OK;

  # Check that name is defined
  unless (defined $name) {
    $status = SAI__ERROR;
    err_rep('NONAME','Starlink::HDSPACK::retrieve_locs - no object name defined', $status);
    return ($status);
  }

  # Split the name into components
  my @components = split(/\./, $name);

  # Store for the locators
  my @locators = ();

  # Open the file
  my $file = shift(@components);
  hds_open($file, $mode, my $ploc, $status);

  # Now get the locators from the lower levels
  if ($status == SAI__OK) {
    push(@locators, $ploc);
  }

  ($status, my @sublocs) = _find_loc($ploc, $status, @components)
    if @components;

  # Store the component locators
  push(@locators, @sublocs) if $status == SAI__OK;

  # Return them
  return ($status, @locators);

}

=item copobj

Copy an HDS object to a specified output structure.

  $status = copobj("file.more.fits", "file2.more.fits2", $status);

The above example will copy the FITS component from C<file.sdf> to
the C<FITS2> extension of C<file2.sdf>. If the target structure
already exists it is overwritten.

This routine assumes we are in a valid error context (eg err_begin()
has been called).

=cut

# Subroutine
# Arg1: Source object
# Arg2: Target hds structure name
# Returns STATUS

sub copobj {

  croak 'Usage: copobj(source, target, status)' unless scalar(@_) == 3;
  
  # Read args
  my ($source, $target, $status) = @_;

  # Return status if not good
  return $status if $status != SAI__OK;

  # Check args
  unless (defined $source) {
    $status = SAI__ERROR;
    err_rep('NOSOURCE','Starlink::HDSPACK::copobj - no source object defined', $status);
    return $status;
  }

  unless (defined $target) {
    $status = SAI__ERROR;
    err_rep('NOTARGET','Starlink::HDSPACK::copobj - no target structure defined', $status);
    return $status;
  }


  # First remove the final target name -- we don't want a locator
  # to that since we are putting our copied object there
  # need a non-greedy pattern match that starts from the back...
  # benchmarking shows that reverse is twice as fast a joining
  # back together after splitting.
  (my $outname, $target) = split(/\./, reverse($target), 2);
  $target = reverse($target);
  $outname = reverse($outname);

  print "Source location: $source\n" if $DEBUG;
  print "Output location: $target called $outname\n" if $DEBUG;

  # Find HDS locators to all the structures listed
  # The first structure in the array is the file locator.
  # the last locator in the array points to the requested structure/object
  ($status, my @srclocs) = retrieve_locs($source, 'READ', $status);
  ($status, my @tarlocs) = retrieve_locs($target, 'UPDATE', $status)
    if length($target) > 0;

  # If tarlocs contains nothing and status is good, we have
  # to use HDS_COPY rather than DAT_COPY so that a top level container
  # can be created with the first HDS structure.

  if ($status == SAI__OK && scalar(@tarlocs) == 0 && scalar(@srclocs) != 0) {
    print "Create new HDS container... $outname\n" if $DEBUG;

    # Need to read the structure name from the locator so that
    # we dont change it (should match the end of the requested source string)
    dat_name($srclocs[-1], my $structname, $status);
    
    # Copy to the new root
    hds_copy($srclocs[-1], $outname, $structname, $status);

  } elsif ((scalar(@srclocs) != 0) && (scalar(@tarlocs) != 0)) {
    # Else if both source and target contain something we can
    # use dat_copy

    # Check to see that the last locator actually is a structure
    # component (else we can't put anything into it)
    my $istruct;
    if ($status == SAI__OK) {
      dat_struc($tarlocs[-1], $istruct, $status);
      if (!$istruct && $status == SAI__OK) {
	$status = SAI__ERROR;
	err_rep("STRUCT","copobj: Target object ($target) is not an HDS structure", $status);
      }
    }
    
    # Check to see if the target object is present
    # If it is - we delete it
    my $there;
    if ($status == SAI__OK) {
      dat_there($tarlocs[-1], $outname, $there, $status);
      if ($status != SAI__OK) {
	err_rep('THERE', "copobj: Error checking for existence of ${target}.$outname", $status);
      }
      
    }

    # erase it
    dat_erase($tarlocs[-1], $outname, $status) if $there; 

    # Copy from one to the other
    dat_copy($srclocs[-1], $tarlocs[-1], $outname, $status);

  }

  # Annul all the locators in reverse order
  foreach my $loc (reverse(@srclocs), reverse(@tarlocs) ) {
    dat_annul($loc, $status);
  }

  return $status;
}

=item delobj

Delete the named object structure.

  $status = delobj("file.more.fits", $status);

This example will remove the FITS extension from C<file.sdf>.

This routine assumes we are in a valid error context (eg err_begin()
has been called).

=cut


sub delobj {
  
  croak 'Usage: delobj(object, status)' unless scalar(@_) == 2;

  # read arguments
  my ($object, $status) = @_;

  # Return status if not good
  return $status if $status != SAI__OK;

  # Check that arg is defined
  unless (defined $object) {
    $status = SAI__ERROR;
    err_rep('NOSOURCE','Starlink::HDSPACK::delobj - no object defined', $status);
    return $status;
  }

  # Chop off the last component
  (my $delname, $object) = split(/\./, reverse($object), 2);
  $object = reverse($object);
  $delname = reverse($delname);

  # Find the HDS locator referenced
  ($status, my @srclocs) = retrieve_locs($object, 'UPDATE', $status)
    if length($object) > 0;

  # If no locators were retrieved we assume that an entire container
  # was specifed - open it and erase
  if (scalar(@srclocs) == 0) {

    # Open it
    hds_open($delname, 'WRITE', my $ploc, $status);
    hds_erase($ploc, $status);

  } else {
    
    # Check to see if it is there
    dat_there($srclocs[-1], $delname, my $there, $status);

    # erase it
    dat_erase($srclocs[-1], $delname, $status) if $there;

    # Annul all the locators in reverse order
    foreach my $loc (reverse(@srclocs) ) {
      dat_annul($loc, $status);
    }

  }

  return $status;
}





# Retrieve a locator to the specified structure
# Args: - Locator to parent
#       - status
#       - array of component names

# Returns:
#  locators to all lower levels
# Note that this means that we create an array for each level
# in the structure which contains locators from all levels below it!
# these disappear as you unwind the recursion

sub _find_loc {

   my $parent = shift;
   my $status = shift;
   return undef if $status != SAI__OK;

   my $nextcmp = shift;

   # Get next locator
   print "Looking for $nextcmp\n" if $DEBUG; 
   dat_find($parent, $nextcmp, my $child, $status);

   # If we have more things on the argument stack we call ourselves
   my @locators = ();
   ($status, @locators) = _find_loc($child, $status, @_) if @_;

   # Now put $child onto the list
   unshift(@locators, $child);

   return ( $status, @locators );
}


1;

__END__

=back

=head1 SEE ALSO

Starlink User Note 92 (SSN/92).
L<NDF>

=head1 REVISION

$Id$

=head1 AUTHOR

Tim Jenness (t.jenness@jach.hawaii.edu)

=head1 COPYRIGHT

Copyright (C) Particle Physics and Astronomy Research Council.
All Rights Reserved.

=cut
