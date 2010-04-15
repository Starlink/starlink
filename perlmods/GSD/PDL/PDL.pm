package GSD::PDL;

=head1 NAME

GSD::PDL - subclass of GSD where data are returned as PDLs rather than arrays

=head1 SYNOPSIS

  use GSD::PDL;

  tie %hash, 'GSD::PDL', $filename;

  $pdl = $hash{C13DAT};

=head1 DESCRIPTION

This is a simple subclass of the standard GSD module. It
overrides one of the internal routines in order to return
PDL objects rather than perl arrays whenever a GSD array item
is to be returned. This is far more efficient than using perl arrays.
(although it should be implemented at the XS level for maximum
performance).

=cut

use strict;
use Carp;
use PDL::LiteF;
use GSD;
use base qw/ GSD /;

use vars qw/ $VERSION /;
$VERSION = '0.90';


=head2 METHODS

The C<Get1x> methods are modified to return PDLs instead of arrays.

  $pdl = $gsd->Get1d($itemno, \@dimvals, \@start, \@end);

similarly for real and integer items.

The returned PDL object is in class PDL. The dimensions of the returned
pdl are derived from difference between C<@start> and C<@end>.

=cut

sub Get1d ($$$$$) {
  my $ndims = ( ref($_[2]) ? scalar(@{$_[2]}) : 1);
  # Get packed data
  my $status = gsdGet1dp($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			 $_[0]->{DATA_PTR}, $_[1], $ndims, $_[2],
			 $_[3], $_[4], my $packed, my $actvals);
  if ($status == 0) {
    # The dimensions of the array are derived from start and end arrays
    return $_[0]->_topdl(double, $_[3], $_[4], $packed);
  } else {
    return undef;
  }
}

sub Get1r ($$$$$) {
  my $ndims = ( ref($_[2]) ? scalar(@{$_[2]}) : 1);
  # Get packed data
  my $status = gsdGet1rp($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			 $_[0]->{DATA_PTR}, $_[1], $ndims, $_[2],
			 $_[3], $_[4], my $packed, my $actvals);
  if ($status == 0) {
    # The dimensions of the array are derived from start and end arrays
    return $_[0]->_topdl(float, $_[3], $_[4], $packed);
  } else {
    return undef;
  }
}

sub Get1i ($$$$$) {
  my $ndims = ( ref($_[2]) ? scalar(@{$_[2]}) : 1);
  # Get packed data
  my $status = gsdGet1ip($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			 $_[0]->{DATA_PTR}, $_[1], $ndims, $_[2],
			 $_[3], $_[4], my $packed, my $actvals);
  if ($status == 0) {
    # The dimensions of the array are derived from start and end arrays
    return $_[0]->_topdl(long, $_[3], $_[4], $packed);
  } else {
    return undef;
  }
}


# Internal routine to convert a packed string to a PDL of the
# correct type and dimensions
#
#   $gsd->_topdl($type, \@start, \@end, $packed)
#
# where type is the standard PDL type as returned by
# float or double

# @start and @end can be scalars rather than array refs


sub _topdl {
    my @dims;
    my @start = ( ref($_[2]) ? @{$_[2]} : ($_[2]));
    my @end =   ( ref($_[3]) ? @{$_[3]} : ($_[3]));
    croak "Start and end arrays must have the same dimensionality!"
      unless $#start == $#end;
    foreach (0..$#start) {
      push(@dims, $end[$_] - $start[$_] + 1);
    }
    # Create a new pdl of the right size and type
    my $pdl = PDL->zeroes($_[1], @dims);
    $ { $pdl->get_dataref} = $_[4];
    $pdl->upd_data;
    return $pdl;
}

=head1 AUTHOR

This module was written by Tim Jenness, E<lt>t.jenness@jach.hawaii.eduE<gt>

Copyright (C) 1995-2000 Tim Jenness and the UK Particle Physics and
Astronomy Research Council. All Rights Reserved.

=head1 CVS REVISION

 $Id$

=cut

1;
