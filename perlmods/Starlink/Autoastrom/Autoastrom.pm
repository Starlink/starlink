package Starlink::Autoastrom;

=head1 NAME

Starlink::Autoastrom - Perform automated astrometric corrections on
an astronomical frame.

=head1 SYNOPSIS

use Starlink::Autoastrom;

my $auto = new Starlink::Autoastrom( ndf => $ndf );
$auto->solve;

=head1 DESCRIPTION

This module performs automated astrometric corrections on an astronomical
frame. It is essentially a wrapper around L<Starlink::Astrom> with bits
added on to allow one to pass an NDF and have its astrometry corrected.

=cut

use Carp;
use strict;

# We need a wack of other modules.
use Starlink::AST;
use Starlink::Astrom;
use Starlink::Extractor;
use Astro::Coords;
use Astro::Correlate;
use Astro::Catalog;
use Astro::Catalog::Query::2MASS;
use Astro::FITS::HdrTrans qw/ translate_from_FITS /;
use Astro::FITS::Header;
use Astro::FITS::Header::NDF;
use Astro::WaveBand;
use NDF;

use vars qw/ $VERSION $DEBUG /;

$VERSION = '0.01';
$DEBUG = 0;

=head1 METHODS

=head2 Constructor

=over 4

=item B<new>

  $auto = new Starlink::Autoastrom( ndf => $ndf );

The constructor returns an C<Starlink::Autoastrom> object.

=cut

sub new {
  my $proto = shift;
  my $class = ref( $proto ) || $proto;

# Retrieve the arguments.
  my %args = @_;

# Make sure there's some kind of frame to work on. This is
# given in the 'ndf' argument.
  if( ! defined( $args{'ndf'} ) ) {
    croak "Must supply an NDF name to Starlink::Autoastrom constructor";
  }
  my $ndf = $args{'ndf'};

# Create the object.
  my $auto = {};

# Set up the NDF.
  $auto->{NDF} = $ndf;

# Bless and return.
  bless( $auto, $class );
  return $auto;
}

=back

=head2 Accessor Methods

=over 4

=item B<ndf>

Retrieve or set the NDF that will have its astrometry solved.

  my $ndf = $auto->ndf;
  $auto->ndf( $ndf );

Returns a string.

=cut

sub ndf {
  my $self = shift;
  if( @_ ) {
    my $ndf = shift;
    $self->{NDF} = $ndf;
  }
  return $self->{NDF};
}

=back

=head2 General Methods

=over 4

=item B<solve>

Perform automated astrometry correction for the supplied NDF.

  $auto->solve;

This method modifies the WCS for the NDF in place.

=cut

sub solve {
  my $self = shift;

# Retrieve the name of the NDF, croaking if it's undefined.
  if( ! defined( $self->ndf ) ) {
    croak "Must supply NDF in order to perform automated astrometry correction";
  }
  my $ndf = $self->ndf;
  my $hdr = new Astro::FITS::Header::NDF( File => $ndf );
  tie my %hdr_keywords, "Astro::FITS::Header", $hdr;
  my %generic_headers = translate_from_FITS( \%hdr_keywords );

# Determine the bounds of the NDF.
  my $STATUS = 0;
  ndf_find( &NDF::DAT__ROOT, $ndf, my $access, $STATUS );
  if( $STATUS != &NDF::SAI__OK ) {
    croak "Could not find NDF. Starlink error code $STATUS returned";
  }
  ndf_bound( $access, 2, my @lbnd, my @ubnd, my $ndim, $STATUS );
  if( $STATUS != &NDF::SAI__OK ) {
    croak "Could not determine NDF bounds. Starlink error code $STATUS returned";
  }
  my $xcen = ( $lbnd[0] + $ubnd[0] ) / 2;
  my $ycen = ( $lbnd[1] + $ubnd[1] ) / 2;

# Determine the coordinates at the midpoint and at the bottom left pixel.
# First try to get one from the .WCS component of the NDF using ndfGtwcs,
# and if that doesn't return an FK5 FrameSet, try using the FITS headers
# using Astro::FITS::Header::NDF.
  my $template = new Starlink::AST::SkyFrame( "System=FK5" );
  my $wcs = ndfGtwcs( $access, $STATUS );
  if( $STATUS != &NDF::SAI__OK ) {
    croak "Could not get a WCS from the NDF. Starlink error code $STATUS returned";
  }
  my $frameset = $wcs->FindFrame( $template, "" );
  if( ! defined( $frameset ) ) {
    my $wcs = $hdr->get_wcs;
    $frameset = $wcs->FindFrame( $template, "" );
  }
  ( my $ra, my $dec ) = $frameset->Tran2( [$xcen, $lbnd[0]], [$ycen, $lbnd[1]], 1 );
  my $racen = $ra->[0];
  my $deccen = $dec->[0];
  my $rabotleft = $ra->[1];
  my $decbotleft = $dec->[1];
  my $cencoords = new Astro::Coords( ra => $racen,
                                     dec => $deccen,
                                     type => 'J2000',
                                     units => 'radians',
                                   );
  my $botleftcoords = new Astro::Coords( ra => $rabotleft,
                                         dec => $decbotleft,
                                         type => 'J2000',
                                         units => 'radians',
                                       );

# Determine the epoch of observation. This should be in the AST FrameSet,
# but if it's not we'll have to resort to using the generic header EPOCH.
# If that doesn't work, default to epoch 2000.0.
  my $epoch = $frameset->GetC("Epoch");
  if( ! defined( $epoch ) ) {
    $epoch = $generic_headers{'EPOCH'};
  }
  if( ! defined( $epoch ) ) {
    $epoch = "2000.0";
  }

# Determine the search radius. The distance method returns the
# answer as an Astro::Coords::Angle object which you can use
# to get the number of degrees, and thus the number of arcminutes
# that Astro::Catalog::Query requires.
  my $search_radius_angle = $cencoords->distance( $botleftcoords );
  my $search_radius = $search_radius_angle->arcmin;

# Extract objects from the NDF. We do this using the Starlink::Extractor
# module. Set the detection threshold to 5.0 sigma for now.
  my $filter = new Astro::WaveBand( Filter => $generic_headers{'FILTER'} );
  my $ext = new Starlink::Extractor;
  $ext->detect_thresh( 5.0 );
  my $ndfcat = $ext->extract( frame => $ndf,
                              filter => $filter );

# We cannot do automated astrometry corrections if we have fewer
# than 4 objects, so croak if we do.
  if( $ndfcat->sizeof < 4 ) {
    croak "Only detected " . $ndfcat->sizeof . " objects in $ndf. Cannot perform automated astrometry corrections with so few objects";
  }

# Query 2MASS.
  $racen = $cencoords->ra2000;
  $deccen = $cencoords->dec2000;
  $racen->str_delim(' ');
  $deccen->str_delim(' ');

  my $twomassquery = new Astro::Catalog::Query::2MASS( RA => "$racen",
                                                       Dec => "$deccen",
                                                       Radius => $search_radius,
                                                     );
  my $twomasscat = $twomassquery->querydb();

# Again, croak if we have fewer than 4 objects.
  if( $twomasscat->sizeof < 4 ) {
    croak "Only retrieved " . $twomasscat->sizeof . " objects from 2MASS. Cannot perform automated astrometry corrections with so few objects";
  }

# Add the NDF's WCS to the 2MASS catalogue, allowing us to get
# X and Y positions for the retrieved objects.
  my $allstars = $twomasscat->allstars;
  foreach my $star ( @$allstars ) {
    $star->wcs( $frameset );
  }

# Perform the correlation.
  my $corr = new Astro::Correlate( catalog1 => $ndfcat,
                                   catalog2 => $twomasscat );
  ( my $corrndfcat, my $corrtwomasscat ) = $corr->correlate( method => 'FINDOFF' );

# And yes, croak if the correlation resulted in fewer than 4 matches.
  if( $corrndfcat->sizeof < 4 ) {
    croak "Only " . $corrndfcat->sizeof . " object matched between reference catalogue and extracted catalogue. Cannot perform automated astrometry corrections with so few objects";
  }

# Merge the two catalogues so that the RA/Dec from 2MASS matches
# with the x/y from the extracted catalogue. This allows us to
# perform the astrometric solution.
  my $merged = new Astro::Catalog;
  $merged->fieldcentre( Coords => $cencoords );
  my $nobjs = $corrndfcat->sizeof;
  for( my $i = 1; $i <= $nobjs; $i++ ) {
    my $ndfstar = $corrndfcat->popstarbyid( $i );
    $ndfstar = $ndfstar->[0];
    my $twomassstar = $corrtwomasscat->popstarbyid( $i );
    $twomassstar = $twomassstar->[0];
    my $newstar = new Astro::Catalog::Star( ID => $twomassstar->id,
                                            Coords => $twomassstar->coords,
                                            X => $ndfstar->x,
                                            Y => $ndfstar->y,
                                            WCS => $twomassstar->wcs,
                                          );
    $merged->pushstar( $newstar );
  }

# Solve astrometry.
  my $astrom = new Starlink::Astrom( catalog => $merged );
  my $newwcs = $astrom->solve;

# Stick the WCS into the NDF.
  ndf_open( &NDF::DAT__ROOT(), $ndf, 'UPDATE', 'OLD', $access, my $place, $STATUS );
  if( $STATUS != &NDF::SAI__OK ) {
    croak "Could not open NDF to write WCS. Starlink error code $STATUS returned";
  }
  ndfPtwcs( $newwcs, $access, $STATUS );
  if( $STATUS != &NDF::SAI__OK ) {
    croak "Could not add WCS to NDF. Starlink error code $STATUS returned";
  }

}

1;
