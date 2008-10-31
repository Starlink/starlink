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

use strict;

use Carp;
use Data::Dumper;
use File::Temp qw/ tempdir /;

# We need a wack of other modules.
#
# That's right, a WACK.
use Starlink::AST;
use Starlink::Astrom;
use Starlink::Extractor;

use Astro::Coords;
use Astro::Correlate;
use Astro::Catalog;
use Astro::Catalog::Query::SkyCat;
use Astro::FITS::HdrTrans qw/ translate_from_FITS /;
use Astro::FITS::Header;
use Astro::FITS::Header::NDF;
use Astro::WaveBand;

use Time::HiRes qw/ time /;

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

# Create the object.
  my $auto = {};
  bless( $auto, $class );

# Configure the object.
  $auto->_configure( \%args );

# Set up default options.
  $auto->aperture( 5.0 ) if ( ! defined( $auto->aperture ) );
  $auto->autocrowded( 0 ) if ( ! defined( $auto->autocrowded ) );
  $auto->catalogue( 'USNO@ESO' ) if( ! defined( $auto->catalogue ) );
  $auto->crowded( 0 ) if ( ! defined( $auto->crowded ) );
  $auto->crowded_threshold( 250 ) if ( ! defined( $auto->crowded_threshold ) );
  $auto->defects( 'warn' ) if ( ! defined( $auto->defects ) );
  $auto->detection_threshold( 5.0 ) if ( ! defined( $auto->detection_threshold ) );
  $auto->insert( 1 ) if ( ! defined( $auto->insert ) );
  $auto->keeptemps( 0 ) if ( ! defined( $auto->keeptemps ) );
  $auto->match( 'FINDOFF' ) if ( ! defined( $auto->match ) );
  $auto->maxfit( 9 ) if ( ! defined( $auto->maxfit ) );
  $auto->maxobj_query( 500 ) if ( ! defined( $auto->maxobj_query ) );
  $auto->maxobj_image( 500 ) if ( ! defined( $auto->maxobj_image ) );
  $auto->maxobj_corr( 500 ) if ( ! defined( $auto->maxobj_corr ) );
  $auto->messages( 1 ) if ( ! defined( $auto->messages ) );
  $auto->obsdata( 'source=USER:AST:FITS,angle=0,scale=1,invert=0' ) if ( ! defined( $auto->obsdata ) );
  $auto->starlink_output( 1 ) if ( ! defined( $auto->starlink_output ) );
  $auto->temp( tempdir( CLEANUP => ( ! $auto->keeptemps ) ) ) if ( ! defined( $auto->temp ) );
  $auto->timeout( 180 ) if ( ! defined( $auto->timeout ) );
  $auto->verbose( 1 ) if ( ! defined( $auto->verbose ) );

# Return.
  return $auto;
}

=back

=head2 Accessor Methods

=over 4

=item B<aperture>

Aperture size to use for aperture photometry.

  my $aperture = $auto->aperture;
  $auto->aperture( 10.0 );

The value is the aperture diameters in pixels. Defaults to 5.0.

=cut

sub aperture {
  my $self = shift;
  if( @_ ) {
    my $aperture = shift;
    $self->{APERTURE} = $aperture;
  }
  return $self->{APERTURE};
}

=item B<autocrowded>

Whether or not to automatically detect whether or not the frame is a crowded field, and then act accordingly.

  my $autocrowded = $auto->autocrowded;
  $auto->autocrowded( 1 );

If set to true, then the automated astrometry step will automatically
detect whether or not the field is considered crowded. If the number
of objects extracted from the central 25% of the frame is greater than
the value returned by the crowded_threshold() accessor, then the field
is considered to be crowded, in which case only the central region of
the image will be used.

If the crowded() accessor is set to 1, then that will override any
value that this accessor is set to. If the crowded() accessor is set
to false, then the value of this accessor takes priority.

The value defaults to 0, or false.

=cut

sub autocrowded {
  my $self = shift;
  if( @_ ) {
    my $autocrowded = shift;
    $self->{AUTOCROWDED} = $autocrowded;
  }
  return $self->{AUTOCROWDED};
}

=item B<bestfitlog>

Retrieve or set the filename to write information about the best fit
to.

  my $bestfitlog = $auto->bestfitlog;
  $auto->bestfitlog( 'bestfit.log' );

Will write the file to the current working directory. If undefined,
which is the default, no log will be written.

=cut

sub bestfitlog {
  my $self = shift;
  if( @_ ) {
    my $bestfitlog = shift;
    $self->{BESTFITLOG} = $bestfitlog;
  }
  return $self->{BESTFITLOG};
}

=item B<catalogue>

Retrieve or set the SkyCat name of the online catalogue to use
for queries.

  my $skycat = $auto->catalogue;
  $auto->catalogue( 'usno@eso' );

Take care to avoid string interpolation with the @ sign. Returns a
string. Defaults to 'USNO@ESO'. The string is upper-cased when stored
and returned.

For a list of available SkyCat catalogue names, see
http://archive.eso.org/skycat/

This method supports multiple SkyCat names separated by commas:

  $auto->catalogue( 'usno@eso,2mass@ukirt' );

If more than one SkyCat name has been supplied, the first will be
tried. If that fails or returns zero objects, then the second will be
returned, and so on until the list of names has been exhausted.

=cut

sub catalogue {
  my $self = shift;
  if( @_ ) {
    my $catalogue = uc( shift );
    $self->{CATALOGUE} = $catalogue;
  }
  return $self->{CATALOGUE};
}

=item B<ccdcatalogue>

Retrieve or set a pre-existing catalogue of objects in the CCD frame.

  my $ccdcatalogue = $auto->ccdcatalogue;
  $auto->ccdcatalogue( 'm31.cat' );

The format is that as produced as output by SExtractor when the
CATALOG_TYPE parameter is set to ASCII_HEAD. The catalogue must
have all of the fields NUMBER, FLUX_ISO, X_IMAGE, Y_IMAGE, A_IMAGE,
B_IMAGE, X2_IMAGE, Y2_IMAGE, ERRX2_IMAGE, ERRY2_IMAGE, and ISOAREA_IMAGE,
of which X2_IMAGE, Y2_IMAGE, A_IMAGE, B_IMAGE, ERRX2_IMAGE, and
ERRY2_IMAGE are not generated by default.

This parameter defaults to undef, which means a catalogue will be
formed by running SExtractor on the input frame instead of relying
on the pre-existing catalogue.

=cut

sub ccdcatalogue {
  my $self = shift;
  if( @_ ) {
    my $ccdcatalogue = shift;
    $self->{CCDCATALOGUE} = $ccdcatalogue;
  }
  return $self->{CCDCATALOGUE};
}

=item B<crowded>

Whether or not the autoastrom is being run on a crowded field. This is
primarily used for speed considerations, as a smaller region of the
frame will be used to detect objects in.

=cut

sub crowded {
  my $self = shift;
  if( @_ ) {
    my $crowded = shift;
    $self->{CROWDED} = $crowded;
  }
  return $self->{CROWDED};
}

=item B<crowded_threshold>

The lower limit of extracted objects before which a field is
considered crowded.

  my $thresh = $auto->crowded_threshold;
  $auto->crowded_threshold( 500 );

See the C<autocrowded> method for more information.

Defaults to 250.

=cut

sub crowded_threshold {
  my $self = shift;
  if( @_ ) {
    my $thresh = shift;
    $self->{CROWDED_THRESHOLD} = $thresh;
  }
  return $self->{CROWDED_THRESHOLD};
}

=item B<defects>

Retrieve or set the keyword that dictates how defects in the
CCD catalogue are treated.

  my $defects = $auto->defects;
  $auto->defects( 'remove' );

This method can take one of four possible keywords:

=over 4

=item ignore - Completely ignore defects.

=item warn - Warn about possible defects, but do nothing further.

=item remove - Remove any suspected defects from the catalogue of
CCD objects.

=item badness - Provide the threshold for defect removal and warnings.
Any objects with a badness greater than the value specified here are
noted or removed.

=back

The badness heuristic works by assigning a 'badness' to each object
detected. Objects with a position variance smaller than one pixel
and whose flux density is significantly higher than the average
are given high scores.

The default behaviour is to warn about possible defects (i.e. the
default keyword is 'warn'), and the default badness level is 1.

To set the badness level, set this keyword to 'badness=2' if, for example,
you wanted the badness threshold to be 2.

=cut

sub defects {
  my $self = shift;
  if( @_ ) {
    my $defects = shift;
    if( $defects !~ /^(ignore|warn|remove|badness)/i ) {
      $defects = 'warn';
    } else {
      $defects = lc( $defects );
    }
    $self->{DEFECTS} = $defects;
  }
  return $self->{DEFECTS};
}

=item B<detected_catalogue>

Retrieve or set a filename that will take the set of objects detected
by the detection step. The file is formatted in Cluster format
with 13 columns:

=over 4

=item * a zero

=item * the object ID number

=item * the RA and Dec in space-separated format

=item * the x and y positions of the source on the CCD

=item * an instrumental magnitude

=item * the error in the instrumental magnitude

=item * extraction flags

=back

  my $detected_catalogue = $auto->detected_catalogue;
  $auto->detected_catalogue( 'detect.cat' );

Defaults to undef, meaning that no such file will be written. If defined,
it will write the catalogue in the current working directory.

=cut

sub detected_catalogue {
  my $self = shift;
  if( @_ ) {
    my $detected_catalogue = shift;
    $self->{DETECTED_CATALOGUE} = $detected_catalogue;
  }
  return $self->{DETECTED_CATALOGUE};
}

=item B<detection_threshold>

The detection threshold above which objects will be detected. This is
equivalent to the DETECT_THRESH parameter to SExtractor.

  my $detection_threshold = $auto->detection_threshold;
  $auto->detection_threshold( 2.5 );

Setting a lower detection will slow down automated astrometry. The
default is 5.0.

=cut

sub detection_threshold {
  my $self = shift;
  if( @_ ) {
    my $thresh = shift;
    $self->{DETECTION_THRESHOLD} = $thresh;
  }
  return $self->{DETECTION_THRESHOLD};
}

=item B<err_output>

Define a callback that will take text for error output.

  $auto->err_output( sub { print "Autoastrom error: " . shift; } );

If no callback is defined, then text will be output to STDERR by the
printerr_me() method.

=cut

sub err_output {
  my $self = shift;
  if( @_ ) {
    $self->{ERR_OUTPUT} = shift;
  }

  return $self->{ERR_OUTPUT};
}

=item B<filter>

=cut

sub filter {
  my $self = shift;
  if( @_ ) {
    my $filter = shift;
    $self->{FILTER} = $filter;
  }
  return $self->{FILTER};
}

=item B<insert>

Whether or not to insert the final astrometric fit into the input NDF
as an AST WCS component. If false, the insertion is not done.

  my $insert = $auto->insert;
  $auto->insert( 1 );

The default is true.

=cut

sub insert {
  my $self = shift;
  if( @_ ) {
    my $insert = shift;
    $self->{INSERT} = $insert;
  }
  return $self->{INSERT};
}

=item B<iterrms_abs>

Retrieve or set the absolute RMS level to reach in determining the solution during the iterative process.

  my $iterrms_abs = $auto->iterrms_abs;
  $auto->iterrms_abs( 0.25 );

Values are in arcseconds. If undefined, which is the default, then iterations will continue until the number of iterations reaches the value stored in the maxiter() accessor.

=cut

sub iterrms_abs {
  my $self = shift;
  if( @_ ) {
    my $iterrms_abs = shift;
    $self->{ITERRMS_ABS} = $iterrms_abs;
  }
  return $self->{ITERRMS_ABS};
}

=item B<iterrms_diff>

Retrieve or set the difference in RMS between iterations to reach in determining the solution during the iterative process.

  my $iterrms_diff = $auto->iterrms_diff;
  $auto->iterrms_diff( 0.001 );

Values are in arcseconds. If undefined, which is the default, then iterations will continue until the number of iterations reaches the value stored in the maxiter() accessor.

=cut

sub iterrms_diff {
  my $self = shift;
  if( @_ ) {
    my $iterrms_diff = shift;
    $self->{ITERRMS_DIFF} = $iterrms_diff;
  }
  return $self->{ITERRMS_DIFF};
}

=item B<keepfits>

Whether or not to keep the final astrometric fit as a FITS-WCS file.

  my $keepfits = $auto->keepfits;
  $auto->keepfits('wcs.fits');

If this parameter is undefined (which is the default), then no FITS-WCS
file will be kept. If it is defined, then the FITS-WCS file will have
the name given as this value. Using the above example, the FITS-WCS
file will be saved as 'wcs.fits' in the current working directory.

=cut

sub keepfits {
  my $self = shift;
  if( @_ ) {
    my $keepfits = shift;
    $self->{KEEPFITS} = $keepfits;
  }
  return $self->{KEEPFITS};
}

=item B<keeptemps>

Whether or not to keep temporary files after processing is completed.

  my $keeptemps = $auto->keeptemps;
  $auto->keeptemps( 1 );

Temporary files are created in a temporary directory that is reported
during execution. The location of this temporary directory can be
controlled using the C<temp> method.

This parameter defaults to false, so all temporary files are deleted
after processing.

=cut

sub keeptemps {
  my $self = shift;
  if( @_ ) {
    my $keeptemps = shift;
    $self->{KEEPTEMPS} = $keeptemps;
  }
  return $self->{KEEPTEMPS};
}

=item B<match>

The matching algorithm to be used.

  my $match = $auto->match;
  $auto->match( 'FINDOFF' );

Currently, the only available matching algorithm is the Starlink
FINDOFF application, part of CCDPACK. FINDOFF has certain limitations
(i.e. it's slow, it doesn't work if you have unequal X and Y scales),
but works if your data get around these limitations.

=cut

sub match {
  my $self = shift;
  if( @_ ) {
    my $match = shift;
    $self->{MATCH} = $match;
  }
  return $self->{MATCH};
}

=item B<matchcatalogue>

Retrieve or set a filename that will take the set of positions matched
by the matching process. The file is formatted like a SExtractor output
file with five columns: the object number, RA and Dec of the source on
the sky, and x and y positions of the source on the CCD.

  my $matchcatalogue = $auto->matchcatalogue;
  $auto->matchcatalogue( 'match.cat' );

Defaults to undef, meaning that no such file will be written. If defined,
it will write the catalogue in the current working directory.

=cut

sub matchcatalogue {
  my $self = shift;
  if( @_ ) {
    my $matchcatalogue = shift;
    $self->{MATCHCATALOGUE} = $matchcatalogue;
  }
  return $self->{MATCHCATALOGUE};
}

=item B<maxfit>

Retrieve or set the maximum number of fit parameters to use to obtain
the astrometric fit.

  my $maxfit = $auto->maxfit;
  $auto->maxfit( 7 );

Allowed values are 4, 6, 7, 8, and 9, and the default is 9.

=cut

sub maxfit {
  my $self = shift;
  if( @_ ) {
    my $maxfit = shift;
    if( $maxfit != 4 &&
        $maxfit != 6 &&
        $maxfit != 7 &&
        $maxfit != 8 &&
        $maxfit != 9 ) {
      $maxfit = 9;
    }
    $self->{MAXFIT} = $maxfit;
  }
  return $self->{MAXFIT};
}

=item B<maxiter>

Retrieve or set the maximum number of iterations to perform.

  my $maxiter = $auto->maxiter;
  $auto->maxiter( 20 );

Defaults to 10. There is an upper limit of 100, and a lower limit of 1.

=cut

sub maxiter {
  my $self = shift;
  if( @_ ) {
    my $maxiter = shift;
    $self->{MAXITER} = $maxiter;
  }

  if( ! defined( $self->{MAXITER} ) ) {
    $self->{MAXITER} = 10;
  }

  if( $self->{MAXITER} > 100 ) {
    $self->{MAXITER} = 100;
  } elsif ( $self->{MAXITER} < 1 ) {
    $self->{MAXITER} = 1;
  }

  return $self->{MAXITER};
}

=item B<maxobj_corr>

Retrieve or set the maximum number of objects to use for correlation.

  my $maxobj_corr = $auto->maxobj_corr;
  $auto->maxobj_corr( 50 );

Defaults to 500. Useful for speeding up processing by setting to a
lower number, or possibly obtaining better matches with larger numbers
of objects. This puts limits on both the query catalogue and the image
catalogue when used as input to the correlation routine.

=cut

sub maxobj_corr {
  my $self = shift;
  if( @_ ) {
    my $max_obj_corr = shift;
    $self->{MAXOBJ_CORR} = $max_obj_corr;
  }
  return $self->{MAXOBJ_CORR};
}

=item B<maxobj_image>

Retrieve or set the maximum number of objects to use from the image.

  my $maxobj_image = $self->maxobj_image;
  $self->maxobj_image( 1000 );

Defaults to 500. Useful for speeding up processing time in
densely-populated fields.

If this limit is smaller than the number of objects detected in the
image, then the filtering will be done radially from the image centre,
i.e. it will use those objects nearer the centre.

=cut

sub maxobj_image {
  my $self = shift;
  if( @_ ) {
    my $max_obj_image = shift;
    $self->{MAXOBJ_IMAGE} = $max_obj_image;
  }
  return $self->{MAXOBJ_IMAGE};
}

=item B<maxobj_query>

Retrieve or set the maximum number of objects to retrieve from the
catalogue server.

  my $max_obj_query = $auto->maxobj_query;
  $auto->maxobj_query( 1000 );

Defaults to 500.

=cut

sub maxobj_query {
  my $self = shift;
  if( @_ ) {
    my $max_obj_query = shift;
    $self->{MAXOBJ_QUERY} = $max_obj_query;
  }
  return $self->{MAXOBJ_QUERY};
}

=item B<messages>

Whether or not to display messages from the Starlink applications.

  my $messages = $auto->messages;
  $auto->messages( 0 );

Defaults to true (1).

=cut

sub messages {
  my $self = shift;
  if( @_ ) {
    my $messages = shift;
    $self->{MESSAGES} = $messages;
  }
  return $self->{MESSAGES};
}

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

=item B<obsdata>

Retrieve or set a source for the observation data, including WCS
information.

  my $obsdata = $auto->obsdata;
  $auto->obsdata( $obsdata );

This method returns or takes a hash reference containing the following
keys:

=over 4

=item source - A colon-separated list of sources of WCS information.
The values may be 'AST', indicating that the information should come
from the AST WCS component of the WCS, 'FITS', indicating that it
should come from any FITS extension in the NDF, or 'USER', indicating
that values given by this method are to be used. The default is
'USER:AST:FITS', so that any WCS information given by this method
has precedence. The values are not case-sensitive. If no WCS information
can be obtained, an error will be thrown.

=item ra - Right ascension of the centre of the pixel grid, given in
colon-separated HMS or decimal hours. This is stored internally
as an C<Astro::Coords::Angle::Hour> object, and if the obsdata()
method is called then the value for this key will also be an
C<Astro::Coords::Angle::Hour> object.

=item dec - Declination of the centre of the pixel grid, given in colon-
separated DMS or decimal degrees. This is stored internally as an
C<Astro::Coords::Angle> object, and if the obsdata() method is called then
the value for this key will also be an C<Astro::Coords::Angle> object.

=item angle - Position angle of the pixel grid. This is the rotation
in degrees counter-clockwise of the declination axis with respect to
the y-axis of the data array. Defaults to 0.

=item scale - Plate scale in arcseconds per pixel. Defaults to 1.

=item invert - If true, the axes are inverted. Defaults to 0.

=back

There are additional observation data keywords that can be defined.
These are used to refine higher-order astrometric fits.

=over 4

=item time - An observation time, given as a Julian epoch (in the
format r), a local sideral time (in the format i:i), or UT (in the
format i:i:i:i:r specifying four-digit year, month, day, hours,
and minutes).

=item obs - An observation station, given either as one of the
SLALIB observatory codes, or in the format i:r:i:r[:r] specifying
longitude, latitude, and optional height. Longitudes are east longitudes,
so west longitudes may be given as minus degrees or longitudes
greater than 180.

=item met - Temperature and pressure at the telescope, in degrees
Kelvin and millibars. The defaults are 278K and a pressure computed
from the observatory height. Format r[:r].

=item col - The effective colour of the observations, as a wavelength
in nanometres. The default is 500nm.

=back

In the format specifications for the above four keywords, r represents
a real, i represents an integer, and optional entries are in [...].

When returned as a hash reference, the keys have been converted to upper-case.
For example, to retrieve the value for the 'source', you would do:

  $source = $auto->obsdata->{'SOURCE'};

=cut

sub obsdata {
  my $self = shift;
  if( @_ ) {
    my $obsdata_input = shift;
    my @obsdata = split( ',', $obsdata_input );
    foreach my $thing ( @obsdata ) {
      ( my $key, my $value ) = split( '=', $thing );
      $key = uc( $key );

      # Perform format checking on the value.

      if( $key eq 'SOURCE' ) {
        my @values = split( ':', $value );
        my @valid = map { uc($_) } grep { /^(ast|fits|user)$/i } @values;
        $value = join ':', @valid;
      }

      if( $key eq 'RA' ) {
        # Convert to Astro::Coords::Angle::Hour object.
        if( $value =~ /^\d+:\d+:[\d\.]+$/ ) {
          $value = new Astro::Coords::Angle::Hour( $value, units => 'sex' );
        } elsif( $value =~ /^[\d\.]+$/ ) {
          $value = new Astro::Coords::Angle::Hour( $value, units => 'hour' );
        } else {
          $self->printstd( "--E Could not parse $value to form Right Ascension from obsdata information.\n" )
            if $self->starlink_output;
          croak "Could not parse $value to form Right Ascension from obsdata information";
        }
      }

      if( $key eq 'DEC' ) {
        # Convert to Astro::Coords::Angle object.
        if( $value =~ /^[+\-]?\d+:\d+:[\d\.]+$/ ) {
          $value = new Astro::Coords::Angle( $value, units => 'sex' );
        } elsif( $value =~ /^-?[\d\.]+$/ ) {
          $value = new Astro::Coords::Angle( $value, units => 'hour' );
        } else {
          $self->printstd( "--E Could not parse $value to form Declination from obsdata information.\n" )
            if $self->starlink_output;
          croak "Could not parse $value to form Declination from obsdata information";
        }
      }

      if( $key eq 'ANGLE' ) {
        if( $value !~ /^-?\d+(\.\d*)?$/ ) {
          $self->printstd( "--W Cannot parse position angle of $value from obsdata information. Setting position angle to 0 degrees.\n" )
            if $self->starlink_output;
          carp "Cannot parse position angle of $value from obsdata information. Setting position angle to 0 degrees";
          $value = 0;
        }
      }

      if( $key eq 'SCALE' ) {
        if( $value !~ /^\d+(\.\d*)?$/ ) {
          $self->printstd( "--W Cannot parse plate scale of $value from obsdata information. Setting plate scale to 1 arcsec/pixel.\n" )
            if $self->starlink_output;
          carp "Cannot parse plate scale of $value from obsdata information. Setting plate scale to 1 arcsec/pixel";
          $value = 1;
        }
      }

      if( $key eq 'INVERT' ) {
        if( ( $value != 1 ) && ( $value != 0 ) ) {
          $self->printstd( "--W Value of invert from obsdata information must be 0 or 1, not $value. Setting invert to 0.\n" )
            if $self->starlink_output;
          carp "Value of invert from obsdata information must be 0 or 1, not $value. Setting invert to 0";
          $value = 0;
        }
      }

      if( $key eq 'TIME' ) {
        if( $value !~ /^\d+(\.\d+)?$/ &&
            $value !~ /^\d+:\d+$/ &&
            $value !~ /^\d+:\d+:\d+:\d+:\d+(\.\d*)?$/ ) {
          $self->printstd( "--E Could not parse time of $value from obsdata information.\n" )
            if $self->starlink_output;
          croak "Could not parse time of $value from obsdata information";
        }
        $value =~ s/:/ /g;
      }

      if( $key eq 'OBS' ) {
        if( $value !~ /^([\w\.])+$/ &&
            $value !~ /^-?\d+:\d+(\.\d*)?:\d+:\d+(\.\d*)?(:\d+(\.\d*)?)?$/ ) {
          # And who says Perl is line noise? :-)
          $self->printstd( "--E Could not parse observatory code of $value from obsdata information.\n" )
            if $self->starlink_output;
          croak "Could not parse observatory code of $value from obsdata information";
        }
      }

      if( $key eq 'MET' ) {
        if( $value !~ /^\d+(\.\d*)?(:\d+(\.\d*)?)?$/ ) {
          $self->printstd( "--E Could not parse meteorological information of $value from obsdata information.\n" )
            if $self->starlink_output;
          croak "Could not parse meteorological information of $value from obsdata information";
        }
      }

      if( $key eq 'COL' ) {
        if( $value !~ /^\d+(\.\d*)?$/ ) {
          $self->printstd( "--E Could not parse effective colour of $value from obsdata information.\n" )
            if $self->starlink_output;
          croak "Could not parse effective colour of $value from obsdata information";
        }
      }

      $self->{OBSDATA}->{$key} = $value;
    }
  }

  # Make sure defaults are set up.
  $self->{OBSDATA}->{ANGLE} = 0 unless defined( $self->{OBSDATA}->{ANGLE} );
  $self->{OBSDATA}->{SCALE} = 1 unless defined( $self->{OBSDATA}->{SCALE} );
  $self->{OBSDATA}->{INVERT} = 0 unless defined( $self->{OBSDATA}->{INVERT} );
  $self->{OBSDATA}->{SOURCE} = 'USER:AST:FITS' unless defined( $self->{OBSDATA}->{SOURCE} );

  return $self->{OBSDATA};
}

=item B<rawcatalogue>

Retrieve the catalogue of detected objects before astrometry
correction.

  $raw = $auto->rawcatalogue;

This method returns an C<Astro::Catalog> object.

=cut

sub rawcatalogue {
  my $self = shift;
  if( @_ ) {
    my $raw = shift;
    $self->{RAWCATALOGUE} = $raw;
  }
  return $self->{RAWCATALOGUE};
}

=item B<resultcatalogue>

Retrieve the catalogue of detected objects with the updated WCS.

  $result = $auto->resultcatalogue;

This method returns an C<Astro::Catalog> object.

=cut

sub resultcatalogue {
  my $self = shift;
  if( @_ ) {
    my $result = shift;
    $self->{RESULTCATALOGUE} = $result;
  }
  return $self->{RESULTCATALOGUE};
}

=item B<skycatconfig>

Retrieve or set the location of the SkyCat configuration file.

  my $skycatconfig = $auto->skycatconfig;
  $auto->skycatconfig( '/home/bradc/skycat.cfg' );

This method checks to see if the file exists, and if it doesn't,
croaks.

If this file is not set, it will first look in the location pointed
to by the C<SKYCAT_CFG> environment variable, second in the
C<$HOME/.skycat/skycat.cfg>, and third in C<$PERLPREFIX/etc/skycat.cfg>.

=cut

sub skycatconfig {
  my $self = shift;
  if( @_ ) {
    my $skycatconfig = shift;
    $skycatconfig =~ s/^file://;
    if( ! -e $skycatconfig ) {
      croak "Could not find SkyCat config file: $skycatconfig";
    }
    $self->{SKYCATCONFIG} = $skycatconfig;
  }
  return $self->{SKYCATCONFIG};
}

=item B<skycatcatalogue_in>

Retrieve or set a filename that will be used in place of a SkyCat
query. The file must be formatted in Cluster format as listed in the
C<detected_catalogue> method.

  my $skycatcatalogue_in = $auto->skycatcatalogue_in;
  $auto->skycatcatalogue_in( 'skycat.cat' );

If not set, then a network query will be done.

=cut

sub skycatcatalogue_in {
  my $self = shift;
  if( @_ ) {
    my $skycatcatalogue_in = shift;
    $self->{SKYCAT_CATALOGUE_IN} = $skycatcatalogue_in;
  }
  return $self->{SKYCAT_CATALOGUE_IN};
}

=item B<skycatcatalogue_out>

Retrieve or set a filename that will take the catalogue as downloaded
from SkyCat. The file is formatted in Cluster format with the same
columns as listed in the C<detected_catalogue> method.

  my $skycat_cat_out = $auto->skycatcatalogue_out;
  $auto->skycatcatalogue_out( 'skycat.out' );

If not set, then no catalogue will be written.

=cut

sub skycatcatalogue_out {
  my $self = shift;
  if( @_ ) {
    my $skycatcatalogue_out = shift;
    $self->{SKYCAT_CATALOGUE_OUT} = $skycatcatalogue_out;
  }
  return $self->{SKYCAT_CATALOGUE_OUT};
}

=item B<starlink_output>

Whether or not to print output as per the original Starlink AUTOASTROM
program.

  my $starlink_output = $auto->starlink_output;
  $self->starlink_output( 0 );

The default is true (1).

Output is displayed to STDOUT. It will be preceded with one of three
prefixes: --I for informative messages, --W for warnings, and --E for
fatal errors.

=cut

sub starlink_output {
  my $self = shift;
  if( @_ ) {
    my $out = shift;
    $self->{STARLINK_OUTPUT} = $out;
  }
  return $self->{STARLINK_OUTPUT};
}

=item B<std_output>

Define a callback that will take text for standard output.

  $auto->std_output( sub { print "Autoastrom says: " . shift; } );

If no callback is defined, then text will be output to STDOUT by the
printstd_me() method.

=cut

sub std_output {
  my $self = shift;
  if( @_ ) {
    $self->{STD_OUTPUT} = shift;
  }
  return $self->{STD_OUTPUT};
}

=item B<temp>

Retrieve or set the directory to be used for temporary files.

  my $temp = $auto->temp;
  $auto->temp( '/tmp' );

If undef (which is the default), a temporary directory will be
created using C<File::Temp>.

=cut

sub temp {
  my $self = shift;
  if( @_ ) {
    my $temp = shift;
    $self->{TEMP} = $temp;
  }
  return $self->{TEMP};
}

=item B<timeout>

Retrieve or set the timeout for Starlink applications to return.

  my $timeout = $auto->timeout;
  $auto->timeout( 30 );

The time is in seconds, and defaults to 180.

=cut

sub timeout {
  my $self = shift;
  if( @_ ) {
    my $timeout = shift;
    $self->{TIMEOUT} = $timeout;
  }
  return $self->{TIMEOUT};
}

=item B<verbose>

Retrieve or set the verbosity level.

  my $verbose = $auto->verbose;
  $auto->verbose( 1 );

If set to true, then much output will be output to STD_ERR. Defaults
to false.

=cut

sub verbose {
  my $self = shift;
  if( @_ ) {
    my $verbose = shift;
    $self->{VERBOSE} = $verbose;
  }
  return $self->{VERBOSE};
}

=back

=head2 General Methods

=over 4

=item B<printerr>

Prints the given arguments.

  $auto->printerr( @args );

This method exists to support sending strings to user-defined
callbacks. Arguments are handled as for the standard Perl print()
function. If a callback has not been defined by the err_output()
method, then the arguments will be printed to STDERR.

=cut

sub printerr {
  my $self = shift;
  my @args = @_;

  my $output = $self->err_output;
  if( defined( $output ) ) {
    $output->( @_ );
  } else {
    print STDERR @_;
  }
}

=item B<printstd>

Prints the given arguments.

  $auto->printstd( @args );

This method exists to support sending strings to user-defined
callbacks. Arguments are handled as for the standard Perl print()
function. If a callback has not been defined by the std_output()
method, then the arguments will be printed to STDOUT.

=cut

sub printstd {
  my $self = shift;
  my @args = @_;

  my $output = $self->std_output;
  if( defined( $output ) ) {
    $output->( @_ );
  } else {
    print @_;
  }
}

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

  $self->printstd( "--I Running AUTOASTROM on " . $self->ndf . ".\n" ) if $self->starlink_output;

# We need some kind of coordinates to use. Go through the list
# of sources given in obsdata->{SOURCE} and find the first one
# that returns.
  my $cencoords;
  my $radius;
  my $xmin;
  my $xmax;
  my $ymin;
  my $ymax;
  my $epoch;
  my $frameset;

  foreach my $wcssource ( split( /\s*:\s*/, $self->obsdata->{SOURCE} ) ) {

    if( $wcssource =~ /AST/ ) {

# Check for an AST component. It needs to be a proper RA/Dec SkyFrame,
# so set up an FK5 template and see if one of those exists in the WCS
# returned via ndfGtwcs.
      my $STATUS = 0;

      err_begin( $STATUS );
      ndf_begin();
      ndf_find( &NDF::DAT__ROOT, $self->ndf, my $ndf_id, $STATUS );
      my $wcs = ndfGtwcs( $ndf_id, $STATUS );
      ndf_annul( $ndf_id, $STATUS );
      ndf_end( $STATUS );

      # Handle errors.
      if( $STATUS != &NDF::SAI__OK ) {
        my ( $oplen, @errs );
        do {
          err_load( my $param, my $parlen, my $opstr, $oplen, $STATUS );
          push @errs, $opstr;
        } until ( $oplen == 1 );
        err_annul( $STATUS );
        err_end( $STATUS );
        $self->printstd( "--E Error retrieving WCS from NDF.\n" ) if $self->starlink_output;
        croak "Error retrieving WCS from NDF:\n" . join "\n", @errs;
      }
      err_end( $STATUS );

      my $template = Starlink::AST::SkyFrame->new( "System=FK5" );
      $frameset = $wcs->FindFrame( $template, "" );

      if( defined( $frameset ) ) {
        $self->printstd( "--I WCS information from AST.\n" ) if $self->starlink_output;
        $self->printerr( "WCS information from AST.\n" ) if $self->verbose;

# Determine the central coordinates and radius of search from information
# contained in the frameset and the NDF.
        ( $cencoords, $radius, $xmin, $xmax, $ymin, $ymax ) = _determine_search_params( frameset => $frameset,
                                                                            ndf => $self->ndf );
        $epoch = $frameset->GetC("Epoch");
        if( ! defined( $epoch ) ) {
          $self->printstd( "--W Epoch not defined in AST FrameSet. Defaulting to 2000.0.\n" ) if $self->starlink_output;
          carp "Epoch not defined in AST FrameSet. Defaulting to 2000.0";
          $epoch = "2000.0";
        }

        last;
      } else {
        $self->printstd( "--I AST WCS information doesn't have a SKY frame.\n" ) if $self->starlink_output;
      }

    } elsif( $wcssource =~ /FITS/ ) {

# Check the FITS header for WCS information.
      my $hdr = new Astro::FITS::Header::NDF( File => $self->ndf );
      my $wcs = $hdr->get_wcs;
      my $template = Starlink::AST::SkyFrame->new( "System=FK5" );
      $frameset = $wcs->FindFrame( $template, "" );
      if( defined( $frameset ) ) {
        $self->printstd( "--I Using WCS information from FITS headers.\n" ) if $self->starlink_output;
        $self->printerr( "WCS information from FITS headers.\n" ) if $self->verbose;

# Determine the central coordinates and radius of search from information
# contained in the frameset and the NDF.
        ( $cencoords, $radius, $xmin, $xmax, $ymin, $ymax ) = _determine_search_params( frameset => $frameset,
                                                                                        ndf => $self->ndf );

        $epoch = $frameset->GetC("Epoch");
        if( ! defined( $epoch ) ) {
          $self->printstd( "--W Epoch not defined in FITS headers. Defaulting to 2000.0.\n" ) if $self->starlink_output;
          $epoch = "2000.0";
        }

        last;
      } else {
        $self->printstd( "--I FITS headers have no useable WCS information.\n" ) if $self->starlink_output;
      }

    } elsif( $wcssource =~ /USER/ ) {

# We need, at a bare minimum, the RA and Dec.
      if( ! defined( $self->obsdata->{RA} ) ) {
        $self->printstd( "--W RA not supplied for USER WCS, not using USER-supplied WCS.\n" ) if $self->starlink_output;
        next;
      }
      if( ! defined( $self->obsdata->{DEC} ) ) {
        $self->printstd( "--W Dec not supplied for USER WCS, not using USER-supplied WCS.\n" ) if $self->starlink_output;
        next;
      }
      $self->printstd( "--I Using WCS information from USER-supplied coordinates.\n" ) if $self->starlink_output;

# Determine the central coordinates and radius of search from information
# contained in the obsdata information and the NDF.
      ( $cencoords, $radius, $xmin, $xmax, $ymin, $ymax ) = _determine_search_params( obsdata => $self->obsdata,
                                                                                      ndf => $self->ndf );
      $frameset = $self->_create_frameset;
      $epoch = $frameset->GetC("Epoch");
      if( ! defined( $epoch ) ) {
        $self->printstd( "--W Epoch not defined in user-supplied information. Defaulting to 2000.0.\n" ) if $self->starlink_output;
        carp "Epoch not defined in user-supplied information. Defaulting to 2000.0";
        $epoch = "2000.0";
      }

      last;
    }
  }

  $self->printstd( sprintf( "Central coordinates: $cencoords\nSearch radius: %.4f arcminutes\n", $radius ) ) if $self->verbose;

# Do filter handling.
  if( ! defined( $self->filter ) ) {
    my $header = new Astro::FITS::Header::NDF( File => $self->ndf );
    tie my %header_keywords, "Astro::FITS::Header", $header, tiereturnsref => 1;
    my %generic_headers;
    eval { %generic_headers = translate_from_FITS(\%header_keywords) };
    if( $@ || ! defined( $generic_headers{'FILTER'} ) ) {
      if( $self->starlink_output ) {
        $self->printstd( "--W Could not obtain filter by translating header.\n" );
        $self->printstd( "--W Looking for FILTER header keyword.\n" );
      }
      if( defined( $header_keywords{'FILTER'} ) ) {
        $self->filter( new Astro::WaveBand( Filter => $header_keywords{'FILTER'} ) );
        $self->printstd( "--W Found FILTER header keyword, value is " . $header_keywords{'FILTER'} . ".\n" ) if $self->starlink_output;
      } else {
        if( $self->starlink_output ) {
          $self->printstd( "--W Could not obtain filter from FILTER header keyword.\n" );
          $self->printstd( "--W Defaulting to J.\n" );
        }
        $self->filter( new Astro::WaveBand( Filter => 'J' ) );
      }
    } else {
      $self->filter( new Astro::WaveBand( Filter => $generic_headers{'FILTER'} ) );
    }
  }

# If we have a user-supplied catalogue, use that. Otherwise, use
# Starlink::Extractor to extract objects from the NDF.
  my $ndfcat;
  if( defined( $self->ccdcatalogue ) ) {
    $self->printstd( sprintf("--I Using EXTRACTOR catalogue in %s", $self->ccdcatalogue ) ) if $self->starlink_output;
    $ndfcat = new Astro::Catalog( Format => 'SExtractor',
                                  File => $self->ccdcatalogue );
  } else {

    $self->printstd( "--I Calling EXTRACTOR.\n" ) if $self->starlink_output;
    $self->printerr( "Calling EXTRACTOR.\n" ) if $self->verbose;
    my $ext = new Starlink::Extractor;
    $ext->quick( 1 );
    $ext->detect_thresh( $self->detection_threshold );
    $ext->phot_apertures( $self->aperture );
    $ext->messages( $self->messages );
    $ext->timeout( $self->timeout );
    if( $self->autocrowded ) {
      $ext->crowded( 1 );
    } else {
      $ext->crowded( $self->crowded );
    }

    my $tmpcat = $ext->extract( frame => $self->ndf,
                                filter => $self->filter,
                                quality => 0,
                              );
    $self->printstd( sprintf( "--I Extracted %d objects from NDF.\n", $tmpcat->sizeof ) )
      if $self->starlink_output;

    if( $self->autocrowded && ! $self->crowded ) {

      # Check to see if the field is crowded or not.
      if( $tmpcat->sizeof > $self->crowded_threshold ) {
        # Crowded field. Go with this catalogue.
        $ndfcat = $tmpcat;

      } else {
        $self->printstd( "--I Sparse field. Redoing extraction.\n" ) if $self->starlink_output;

        # Sparse field. Redo the extraction with a non-crowded field.
        $ext->crowded( 0 );

        $ndfcat = $ext->extract( frame => $self->ndf,
                                 filter => $self->filter,
                                 quality => 0,
                               );

        $self->printstd( sprintf( "--I Extracted %d objects from NDF.\n", $ndfcat->sizeof ) )
          if $self->starlink_output;

      }
    } else {
      $ndfcat = $tmpcat;
    }

    $self->rawcatalogue( $ndfcat );
  }

# We cannot do automated astrometry corrections if we have fewer
# than 2 objects, so croak if we do.
  if( ! defined( $ndfcat ) ) {
    croak "Catalog of extracted objects is undefined";
  }
  if( $ndfcat->sizeof < 2 ) {
    croak "Only detected " . $ndfcat->sizeof . " objects in " . $self->ndf . ". Cannot perform automated astrometry corrections with so few objects";
  }



# Limit the number of objects in the detected catalogue, if necessary.
  my $filtered_ndfcat = new Astro::Catalog;
  if( $self->maxobj_corr && $ndfcat->sizeof > $self->maxobj_corr ) {
    my @ndfstars = $ndfcat->stars;
    my $filter = $self->filter;
    my @sortedstars = map { $_->[0] }
                      sort { $a->[1] <=> $b->[1] }
                      map { [ $_, $_->get_flux_quantity( waveband => $filter, type => 'MAG_ISO' ) ] } @ndfstars;
    @sortedstars = @sortedstars[0..( $self->maxobj_corr - 1 )];
    $filtered_ndfcat->pushstar( @sortedstars );
  } else {
    $filtered_ndfcat = $ndfcat;
  }

# Check to see if we have a catalogue to read in instead of querying
# SkyCat.
  my $querycat;
  if( defined( $self->skycatcatalogue_in ) &&
      -e $self->skycatcatalogue_in ) {

# Read in the catalogue.
    $self->printstd( "--I Using " . $self->skycatcatalogue_in . " as input catalogue for SkyCat.\n" ) if $self->starlink_output;
    $querycat = new Astro::Catalog( Format => 'Cluster',
                                    File => $self->skycatcatalogue_in,
                                  );
    $self->printstd( sprintf( "--I %s read in, %d entries.\n",
                              $self->skycatcatalogue_in,
                              $querycat->sizeof ) )
      if $self->starlink_output;
  } else {

    if( $self->crowded ) {

      my $width = $xmax - $xmin;
      my $height = $ymax - $ymin;

      # Need to get the central coordinates of each of the five query
      # positions. The radii are easy to work out.
      my @coords;
      my @radii;

      # Central coordinates will be the first one done.
      push @coords, $cencoords;

      # The radius is going to be that circle that encompasses a
      # rectangle centred on the central position with sides that are
      # 25% the dimensions of the array.
      #
      # Find the coordinates of one of the corners.
      ( my $corn_ra, my $corn_dec ) = $frameset->Tran2( [$xmin+0.375*$width],
                                                        [$ymin+0.375*$height],
                                                        1);
      my $corn_coords = new Astro::Coords( ra => $corn_ra->[0],
                                           dec => $corn_dec->[0],
                                           type => 'J2000',
                                           units => 'radians' );

      my $r0 = $cencoords->distance( $corn_coords );
      my $rad0 = $r0->arcmin;
      push @radii, $rad0;

      # Now for the corners. They're boxes that are 10% of the
      # width/height. We can set up one Tran2 call to save time. Find
      # coordinates in the following order: bottom-left centre,
      # bottom-left corner, top-left centre, top-left corner,
      # bottom-right centre, bottom-right corner, top-right centre,
      # top-right corner.
      ( my $ra_ref, my $dec_ref ) = $frameset->Tran2( [ $xmin + 0.075 * $width,
                                                        $xmin,
                                                        $xmin + 0.075 * $width,
                                                        $xmin,
                                                        $xmax - 0.075 * $width,
                                                        $xmax,
                                                        $xmax - 0.075 * $width,
                                                        $xmax ],
                                                      [ $ymin + 0.075 * $height,
                                                        $ymin,
                                                        $ymax - 0.075 * $height,
                                                        $ymax,
                                                        $ymin + 0.075 * $height,
                                                        $ymin,
                                                        $ymax - 0.075 * $height,
                                                        $ymax ],
                                                      1 );

      # Bottom-left.
      my $coords1 = new Astro::Coords( ra => $ra_ref->[0],
                                       dec => $dec_ref->[0],
                                       type => 'J2000',
                                       units => 'radians' );
#      push @coords, $coords1;
      my $r1 = $coords1->distance( new Astro::Coords( ra => $ra_ref->[1],
                                                      dec => $dec_ref->[1],
                                                      type => 'J2000',
                                                      units => 'radians' ) );
#      push @radii, $r1->arcmin;

      # Top-left.
      my $coords2 = new Astro::Coords( ra => $ra_ref->[2],
                                       dec => $dec_ref->[2],
                                       type => 'J2000',
                                       units => 'radians' );
#      push @coords, $coords2;
      my $r2 = $coords2->distance( new Astro::Coords( ra => $ra_ref->[3],
                                                      dec => $dec_ref->[3],
                                                      type => 'J2000',
                                                      units => 'radians' ) );
#      push @radii, $r2->arcmin;

      # Bottom-right.
      my $coords3 = new Astro::Coords( ra => $ra_ref->[4],
                                       dec => $dec_ref->[4],
                                       type => 'J2000',
                                       units => 'radians' );
#      push @coords, $coords3;
      my $r3 = $coords3->distance( new Astro::Coords( ra => $ra_ref->[5],
                                                      dec => $dec_ref->[5],
                                                      type => 'J2000',
                                                      units => 'radians' ) );
#      push @radii, $r3->arcmin;

      # Top-right.
      my $coords4 = new Astro::Coords( ra => $ra_ref->[6],
                                       dec => $dec_ref->[6],
                                       type => 'J2000',
                                       units => 'radians' );
#      push @coords, $coords4;
      my $r4 = $coords4->distance( new Astro::Coords( ra => $ra_ref->[7],
                                                      dec => $dec_ref->[7],
                                                      type => 'J2000',
                                                      units => 'radians' ) );
#      push @radii, $r4->arcmin;

      # Do the queries.
      $querycat = new Astro::Catalog;
      for my $i ( 0 .. $#coords ) {
        my $tempquerycat = $self->query_skycat( $coords[$i], $radii[$i] );
        my @newstars = $tempquerycat->allstars;
        $querycat->pushstar( @newstars );
      }

    } else {

# Query the SkyCat catalogue.
      $querycat = $self->query_skycat( $cencoords, $radius );
    }
  }

  # Again, croak if we have fewer than 2 objects.
  if( ! defined( $querycat ) ) {
    croak "Retrieved query catalogue is undefined. Likely cause is "
          . "query failed";
  }
  if( $querycat->sizeof < 2 ) {
    croak "Only retrieved " . $querycat->sizeof . " objects from "
          . $self->catalogue . ". Cannot perform automated astrometry "
          . "corrections with so few objects";
  }

# Dump the SkyCat catalogue to disk if requested, but only if
# either the input SkyCat catalogue isn't defined or it is
# defined and ( isn't the same as the requested output catalogue
# or it doesn't exist ).
  if( defined( $self->skycatcatalogue_out ) ) {
    if( ! defined( $self->skycatcatalogue_in ) ||
        ( defined( $self->skycatcatalogue_in ) &&
          ( ! -e $self->skycatcatalogue_in ||
            $self->skycatcatalogue_in ne $self->skycatcatalogue_out ) ) ) {

      $querycat->write_catalog( Format => 'Cluster',
                                File => $self->skycatcatalogue_out,
                              );
      $self->printstd( "--I Wrote " . $self->skycatcatalogue_out
                       . " to disk, storing SkyCat results.\n" )
        if( $self->starlink_output );
    }
  }

  my $newwcs; # The final WCS to be inserted into the NDF.
  my $merged; # Astro::Catalog object holding merged catalogue.

  my $currentiter = 1;
  my $prev_rms = 0;

# Begin iteration loop.
  while( $currentiter <= $self->maxiter ) {

    $self->printstd( "--I Iteration $currentiter...\n" ) if $self->starlink_output;

# Add the NDF's WCS to the retrieved catalogue, allowing us to get
# X and Y positions for the retrieved objects.
    my $querystars = $querycat->stars;
    my @querystars;
    foreach my $star ( @$querystars ) {

      next if ! defined( $star );

      # Update the object's WCS.
      $star->wcs( $frameset );
    }

# Limit the number of stars in the query catalogue for correlation, if
# necessary. We don't really care which filter we do this in, so take
# the first filter of the first object in the @querystars list.
    @querystars = @$querystars;
    my @queryfilters = $querystars[0]->what_filters;
    my $queryfilter = $queryfilters[0];
    my $filtered_querycat = new Astro::Catalog;
    if( $self->maxobj_corr && $querycat->sizeof > $self->maxobj_corr ) {
      my @sortedstars = map { $_->[0] }
                        sort { $a->[1] <=> $b->[1] }
                        map { [ $_, $_->get_flux_quantity( waveband => $queryfilter, type => 'MAG' ) ] } @querystars;
      @sortedstars = @sortedstars[0..( $self->maxobj_corr - 1 )];
      $filtered_querycat->pushstar( @sortedstars );
    } else {
      $filtered_querycat->pushstar( @querystars );
    }

    if( $self->verbose ) {
      $self->printerr( "Sizes of catalogues used as input to correlation:\n" );
      $self->printerr( " Image: " . $filtered_ndfcat->sizeof . "\n" );
      $self->printerr( " Query: " . $filtered_querycat->sizeof . "\n" );
    }

    $self->printstd( "--I Matching position lists...\n" ) if $self->starlink_output;

# Perform the correlation.
    my $corr = new Astro::Correlate( catalog1 => $filtered_querycat,
                                     catalog2 => $filtered_ndfcat,
                                     keeptemps => $self->keeptemps,
                                     messages => $self->messages,
                                     method => $self->match,
                                     temp => $self->temp,
                                     verbose => $self->verbose,
                                     cat1magtype => 'mag',
                                     cat2magtype => 'mag_iso',
                                   );
    $filtered_querycat->calc_xy( $frameset );

    ( my $corrquerycat, my $corrndfcat ) = $corr->correlate;

# And yes, croak if the correlation resulted in fewer than 2 matches.
    if( $corrndfcat->sizeof < 2 ) {
      croak "Only " . $corrndfcat->sizeof . " object matched between reference catalogue and extracted catalogue. Cannot perform automated astrometry corrections with so few objects";
    }

# Merge the two catalogues so that the RA/Dec from 2MASS matches
# with the x/y from the extracted catalogue. This allows us to
# perform the astrometric solution.
    $merged = new Astro::Catalog;
    $merged->fieldcentre( Coords => $cencoords );
    my $nobjs = $corrndfcat->sizeof;

    foreach my $item ( @{$corrndfcat->stars} ) {
      my $id = $item->id;

      my $queryitem = $corrquerycat->popstarbyid( $id );
      $queryitem = $queryitem->[0];

      next if ! defined( $item );
      next if ! defined( $queryitem );

      $item->id( $queryitem->id );
      $item->coords( $queryitem->coords );
      $item->wcs( $queryitem->wcs );

      my $queryflux = $queryitem->fluxes;
      my @allfluxes = $queryflux->allfluxes;
      my $newfluxes = new Astro::Fluxes;
      my $pref_type;
      foreach my $flux ( @allfluxes ) {
        my $quantity = $flux->quantity( 'mag' );
        my $waveband = $flux->waveband;
        $pref_type = $flux->type . "_CATALOG";
        my $newflux = new Astro::Flux( $quantity, $pref_type , $waveband );
        $newfluxes->pushfluxes( $newflux );
      }
      $item->fluxes( $newfluxes );
      $item->preferred_magnitude_type( $pref_type );

      $merged->pushstar( $item );
    }

    $self->printstd( "--I Matched " . $merged->sizeof . " objects between catalogues.\n" )
      if $self->starlink_output;

# Output the merged catalogue to disk, if requested.
    if( defined( $self->matchcatalogue ) ) {
      $merged->write_catalog( Format => 'SExtractor', File => $self->matchcatalogue );
      $self->printstd( "--I Wrote " . $self->matchcatalogue . " to disk, storing matched objects.\n" )
        if $self->starlink_output;
   }

# If there are fewer than 10 objects in the input catalogue to ASTROM,
# restrict it to a 6-parameter fit, but only if required.
    my $prev_maxfit = $self->maxfit;
    if( $merged->sizeof < 10 && $self->maxfit > 6 ) {
      $self->printstd( "--W Too few matches (" . $merged->sizeof . "). Restricted to 6-parameter fit.\n" ) if $self->starlink_output;
      $self->printerr( "Too few matches (" . $merged->sizeof . "). Restricted to 6-parameter fit.\n" ) if $self->verbose;
      $self->maxfit( 6 );
    }

    $self->printerr( "Input catalogue to astrom has " . $merged->sizeof . " objects\n" ) if $self->verbose;

    $self->printstd( "--I Running ASTROM to generate astrometric solution.\n" ) if $self->starlink_output;

# Solve astrometry.
    my $astrom = new Starlink::Astrom( catalog => $merged,
                                       keepfits => $self->keepfits,
                                       keeptemps => $self->keeptemps,
                                       maxcoeff => $self->maxfit,
                                       obs => { 'time' => $self->obsdata->{TIME},
                                                'obs' => $self->obsdata->{OBS},
                                                'met' => $self->obsdata->{MET},
                                                'col' => $self->obsdata->{COL} },
                                       temp => $self->temp,
                                       verbose => $self->verbose );
    my $results;
    eval { ( $newwcs, $results ) = $astrom->solve; };
    if( $@ ) {
      $self->printstd( "--E Error in finding astrometric solution: $@\n" ) if $self->starlink_output;
      $self->printstd( "--E Try loosening RMS constraints.\n" ) if $self->starlink_output;
      croak "Error in finding astrometric solution: $@\nTry loosening RMS constraints." if $self->verbose;
    }

    if( defined( $self->keepfits ) && $self->starlink_output ) {
      $self->printstd( sprintf( "--I Wrote %s to disk, keeping FITS headers.\n",
                                $self->keepfits ) );
    }

# Set the maxfit back to whatever it was before we went through
# ASTROM.
    $self->maxfit( $prev_maxfit );

# Summarise the fits we obtained to the file pointed to by
# bestfitlog().
    if( defined( $self->bestfitlog ) ) {

      open my $bestfit_fh, ">", $self->bestfitlog or croak "Could not open " . $self->bestfitlog . " for writing: $!";
      print $bestfit_fh "    n nterms         centre        prms       q FITS-WCS\n";
      foreach my $k ( 0..$#{@$results} ) {
        if( $results->[$k]->{STATUS} ) {
          next if $results->[$k]->{nterms} > $self->maxfit;
          printf $bestfit_fh ("%5d %3d %11.11s %12.12s %4.1f %7.7s %s\n",
                              $k,
                              $results->[$k]->{nterms},
                              $results->[$k]->{rasex},
                              $results->[$k]->{decsex},
                              ( defined( $results->[$k]->{prms} ) ?
                                $results->[$k]->{prms} :
                                -1 ),
                              ( defined( $results->[$k]->{'q'} ) ?
                                $results->[$k]->{'q'} :
                                "--" ),
                              ( length( $results->[$k]->{wcs} ) > 32 ?
                                "...".substr( $results->[$k]->{wcs}, -29 ) :
                                $results->[$k]->{wcs} ) );
        } else {
          printf $bestfit_fh ( "%5d NO FIT\n", $k );
        }
      }
      for ( my $k = $#{@$results}; $k >= 0; $k-- ) {
        my %lastastrom = %{$results->[$k]};
        if( $lastastrom{STATUS} ) {
          next if $lastastrom{nterms} > $self->maxfit;
          printf $bestfit_fh ( "Astrom: best fit: %d of %d\n",
                               $k+1, $#{@$results}+1 );
          foreach my $kw ( sort keys %lastastrom ) {
            printf $bestfit_fh "\t%s => %s\n", $kw, $lastastrom{$kw};
          }
          last;
        } else {
          printf $bestfit_fh ("Astrom: Fit %d of %d rejected",
                              $k+1, $#{@$results} + 1 );
          print $bestfit_fh " (", $lastastrom{nterms}, " terms)"
            if defined( $lastastrom{nterms} );
          print $bestfit_fh "\n";
        }
      }
      close $bestfit_fh;
    }

# Get the results for the desired fit.
    my $result;
    foreach my $res ( @$results ) {
      if( $res->{STATUS} ) {
        $result = $res;
      }
      if( $res->{nterms} == $self->maxfit ) {
        last;
      }
    }

    my $curr_rms = $result->{rrms};
    my $nterms = $result->{nterms};
    if( $self->starlink_output ) {
      $self->printstd( "--I ASTROM determined a $nterms-parameter fit.\n" );
      $self->printstd( sprintf( "--I RMS of current fit: %.5f arcseconds.\n",
                                $curr_rms ) );
      $self->printstd( sprintf( "--I Difference to previous RMS: %.5f arcseconds.\n",
                                abs( $curr_rms - $prev_rms ) ) );
    }

    if( $prev_rms != 0 &&
        $curr_rms > $prev_rms ) {

      if( $self->starlink_output ) {
        $self->printstd( sprintf( "--I Current RMS of %.5f is greater than previous RMS of %.5f.\n",
                                  $curr_rms,
                                  $prev_rms ) );
        $self->printstd( "--I Halting iterations and using previous fit.\n" );
      }
      $newwcs = $frameset;
      last;
    }

    if( $curr_rms < $self->iterrms_abs ) {
      if( $self->starlink_output ) {
        $self->printstd( sprintf( "--I Reached absolute RMS level of %.5f arcseconds.\n",
                                  $self->iterrms_abs ) );
        $self->printstd( "--I Halting iterations.\n" );
      }
      last;
    }

    if( abs( $curr_rms - $prev_rms ) < $self->iterrms_diff ) {
      if( $self->starlink_output ) {
        $self->printstd( sprintf( "--I Reached difference RMS level of %.5f arcseconds.\n",
                                  $self->iterrms_diff ) );
        $self->printstd( "--I Halting iterations.\n" );
      }
      last;
    }

    $prev_rms = $curr_rms;
    $frameset = $newwcs;
    $currentiter++;

  } # End iteration loop.

# Stick the WCS into the NDF, if requested.
  if( $self->insert ) {
    my $STATUS = &NDF::SAI__OK;
    err_begin($STATUS);
    ndf_begin();
    ndf_open( &NDF::DAT__ROOT(), $self->ndf, 'UPDATE', 'OLD', my $ndf_id, my $place, $STATUS );

    ndfPtwcs( $newwcs, $ndf_id, $STATUS );
    ndf_annul( $ndf_id, $STATUS );

    # extract error messages and annul error status
    ndf_end($STATUS);
    if( $STATUS != &NDF::SAI__OK ) {
      my ( $oplen, @errs );
      do {
        err_load( my $param, my $parlen, my $opstr, $oplen, $STATUS );
        push @errs, $opstr;
      } until ( $oplen == 1 );
      err_annul( $STATUS );
      err_end( $STATUS );
      croak "Error writing new WCS to NDF:\n" . join "\n", @errs;
    }
    err_end( $STATUS );
    $self->printstd( "--I WCS updated in " . $self->ndf . ".\n" ) if $self->starlink_output;
  }

# Update the NDF catalogue with the new WCS. First get the FK5
# frameset for the WCS.
  my $template = Starlink::AST::SkyFrame->new( "System=FK5" );
  $frameset = $newwcs->FindFrame( $template, "" );
  if( ! defined( $frameset ) ) {
    croak "Could not find FK5 SkyFrame to do X/Y to RA/Dec translation";
  }

# Modify the RA/Dec for each item.
  foreach my $item ( @{$ndfcat->stars} ) {
    my( $ra, $dec ) = $frameset->Tran2( [$item->x],
                                        [$item->y],
                                        1 );
    my $coords = new Astro::Coords( ra    => $ra->[0],
                                    dec   => $dec->[0],
                                    type  => 'J2000',
                                    units => 'radians',
                                  );
    $item->coords( $coords );
    $item->wcs( $newwcs );
    $item->preferred_magnitude_type( 'MAG_APER1' );
  }
  $self->printstd( "--I Updated WCS of objects in detected object catalogue.\n" )
    if $self->starlink_output;

  foreach my $item ( @{$merged->stars} ) {
    my( $ra, $dec ) = $frameset->Tran2( [$item->x],
                                        [$item->y],
                                        1 );
    my $coords =  new Astro::Coords( ra    => $ra->[0],
                                    dec   => $dec->[0],
                                    type  => 'J2000',
                                    units => 'radians',
                                  );
    $item->coords( $coords );
    $item->wcs( $newwcs );
    $item->preferred_magnitude_type( 'MAG_APER1' );
  }

# Write the detected object catalogue, if requested.
  if( defined( $self->detected_catalogue ) ) {
    $ndfcat->write_catalog( Format => 'Cluster',
                            File => $self->detected_catalogue );
    $self->printstd( "--I Wrote detected object catalogue to " . $self->detected_catalogue . ".\n" )
      if $self->starlink_output;
  }

# Set the new catalogue to be the 'fitted' catalogue.
  $self->resultcatalogue( $ndfcat );

# And return the new catalogue.
  return ( $self->resultcatalogue, $merged );

}

=back

=head2 Private Methods

The following methods are private and are not exported.

=over 4

=item B<_configure>

Configures the object.

  $auto->_configure( $args );

Takes one argument, a hash reference. The hash contains key/value pairs
that correspond to the various accessor methods of this module.

=cut

sub _configure {
  my $self = shift;
  my $args = shift;

  foreach my $key ( keys %$args ) {
    if( $self->can( $key ) ) {
      $self->$key( $args->{$key} );
    }
  }
}

=item B<_create_frameset>

Create a Starlink::AST Frameset object from user-supplied observation data
and an NDF.

  my $frameset = $self->_create_frameset;

There must be sufficient information in the observation data stored in the
obsdata accessor to create a frameset. This information is RA, Dec, and
plate scale. The RA and Dec will refer to the central pixel of the NDF.

=cut

sub _create_frameset {
  my $self = shift;

  if( ! defined( $self->obsdata->{RA} ) ||
      ! defined( $self->obsdata->{DEC} ) ) {
    croak "obsdata information must include RA and Dec to form an AST FrameSet";
  }

  my $ra = $self->obsdata->{RA}->degrees;
  my $dec = $self->obsdata->{DEC}->degrees;
  my $scale = $self->obsdata->{SCALE} / 3600.0;
  my $rotangle = $self->obsdata->{ANGLE} * 3.1415926535 / 180.0;
  my $invert = $self->obsdata->{INVERT};
  my $ndf = $self->ndf;

  my $epoch;
  if( defined( $self->obsdata->{TIME} ) ) {
    $epoch = parse_fits_date( $self->obsdata->{TIME} );
  } else {
    $epoch = '2000.0';
  }

  my $sign = 1;
  if( $invert ) {
    $sign = -1;
  }

  # Get the central coordinates of the NDF.
  ( my $xcen, my $ycen ) = central_coordinates( $ndf );

  # Create a string of FITS headers that can represent the WCS.
  # Follow the FITS Paper II convention, using the CDn_n matrix.
  my $fits = '';
  $fits .= sprintf( "RADESYS = 'FK5     '           / Mean IAU 1984 equatorial co-ordinates          " );
  $fits .= sprintf( "WCSAXES =                    2 / Number of axes in world co-ordinate system     " );
  $fits .= sprintf( "CTYPE1  = 'RA---TAN'           / RA tangent-plane axis with no distortion       " );
  $fits .= sprintf( "CTYPE2  = 'DEC--TAN'           / Dec tangent-plane axis with no distortion      " );
  $fits .= sprintf( "CUNIT1  = 'deg     '           / Unit of declination co-ordinates               " );
  $fits .= sprintf( "CUNIT2  = 'deg     '           / Unit of right ascension co-ordinates           " );
  $fits .= sprintf( "CRVAL1  =   %18.12f / [deg] Right ascension at the reference pixel   ", $ra );
  $fits .= sprintf( "CRVAL2  =   %18.12f / [deg] Declination at the reference pixel       ", $dec );
  $fits .= sprintf( "CRPIX1  =   %18.1f / [pixel] Reference pixel along RA axis          ", $xcen );
  $fits .= sprintf( "CRPIX2  =   %18.1f / [pixel] Reference pixel along Dec axis         ", $ycen );
  $fits .= sprintf( "CD1_1   =   %18.12f /                                                ", $sign * $scale * cos( $rotangle ) );
  $fits .= sprintf( "CD1_2   =   %18.12f /                                                ", $sign * $scale * sin( $rotangle ) );
  $fits .= sprintf( "CD2_1   =   %18.12f /                                                ",  -1.0 * $scale * sin( $rotangle ) );
  $fits .= sprintf( "CD2_2   =   %18.12f /                                                ",         $scale * cos( $rotangle ) );
  $fits .= sprintf( "EQUINOX =   %18.1f /                                                 ", $epoch );

  # Pass the header on to the FitsChan creator.
  my $fitschan = new Starlink::AST::FitsChan;
  $fitschan->PutCards( $fits );

  my $frameset = $fitschan->Read;

  return $frameset;
}

=item B<central_coordinates>

Determine the coordinates of the central pixel of an NDF.

  ( $xcen, $ycen ) = central_coordinates( $ndf );

=cut

sub central_coordinates {
  my $ndf = shift;

  my $STATUS = 0;
  err_begin( $STATUS );
  ndf_begin();
  ndf_find( &NDF::DAT__ROOT(), $ndf, my $ndf_id, $STATUS );
  ndf_bound( $ndf_id, 2, my @lbnd, my @ubnd, my $ndim, $STATUS );
  ndf_annul( $ndf_id, $STATUS );
  ndf_end( $STATUS );

  # Handle errors
  if ( $STATUS != &NDF::SAI__OK ) {
    my ( $oplen, @errs );
    do {
      err_load( my $param, my $parlen, my $opstr, $oplen, $STATUS );
      push @errs, $opstr;
    } until ( $oplen == 1 );
    err_annul( $STATUS );
    err_end( $STATUS );
    croak "Error determining central coordinates of NDF:\n" . join "\n", @errs;
  }
  err_end( $STATUS );

  my $xcen = ( $lbnd[0] + $ubnd[0] ) / 2;
  my $ycen = ( $lbnd[1] + $ubnd[1] ) / 2;

  return ( $xcen, $ycen );

}

=item B<determine_search_params>

Determines the search parameters - central coordinates and search radius.

  ( $racen, $deccen, $radius ) = _determine_search_params( frameset => $frameset,
                                                           ndf => $ndf );

There are three possible named parameters:

=item * frameset - A Starlink::AST object containing information about the WCS.

=item * ndf - An NDF that can be queried for x and y dimensions.

=item * obsdata - A hash reference containing information as described in the
obsdata method, above.

Sufficient information must be contained in the input to be able to calculate
the output, which usually means either a frameset or RA and Dec in the obsdata
information. An NDF is definitely mandatory.

This function returns two values, an Astro::Coords object for the centre of the
field and the search radius in arcminutes.

=cut

sub _determine_search_params {
  my %args = @_;

  my $cencoords;
  my $radius;

  my $frameset;
  if( exists( $args{'frameset'} ) && defined( $args{'frameset'} ) ) {
    $frameset = $args{'frameset'};
  }
  my $ndf;
  if( exists( $args{'ndf'} ) && defined( $args{'ndf'} ) ) {
    $ndf = $args{'ndf'};
  } else {
    croak "Must supply an NDF to _determine_search_params";
  }
  my $obsdata;
  if( exists( $args{'obsdata'} ) && defined( $args{'obsdata'} ) ) {
    $obsdata = $args{'obsdata'};
  }

# Determine the bounds of the NDF.
  my $STATUS = 0;
  err_begin( $STATUS );
  ndf_begin();
  ndf_find( &NDF::DAT__ROOT(), $ndf, my $ndf_id, $STATUS );
  ndf_bound( $ndf_id, 2, my @lbnd, my @ubnd, my $ndim, $STATUS );
  ndf_annul( $ndf_id, $STATUS );
  ndf_end( $STATUS );

# Handle errors.
  if( $STATUS != &NDF::SAI__OK ) {
    my ( $oplen, @errs );
    do {
      err_load( my $param, my $parlen, my $opstr, $oplen, $STATUS );
      push @errs, $opstr;
    } until ( $oplen == 1 );
    err_annul( $STATUS );
    err_end( $STATUS );
    croak "Error determining NDF pixel bounds:\n" . join "\n", @errs;
  }
  err_end( $STATUS );

  my $xcen = ( $lbnd[0] + $ubnd[0] ) / 2;
  my $ycen = ( $lbnd[1] + $ubnd[1] ) / 2;

  if( defined( $frameset ) ) {
    ( my $ra, my $dec ) = $frameset->Tran2( [$xcen, $lbnd[0]], [$ycen, $lbnd[1]], 1 );
    my $racen = $ra->[0];
    my $deccen = $dec->[0];
    my $rabotleft = $ra->[1];
    my $decbotleft = $dec->[1];

    $cencoords = new Astro::Coords( ra => $racen,
                                    dec => $deccen,
                                    type => 'J2000',
                                    units => 'radians' );
    my $cornercoords = new Astro::Coords( ra => $rabotleft,
                                          dec => $decbotleft,
                                          type => 'J2000',
                                          units => 'radians' );

    my $radius_angle = $cencoords->distance( $cornercoords );
    $radius = $radius_angle->arcmin;

  } elsif( defined( $obsdata ) ) {
    $cencoords = new Astro::Coords( ra => $obsdata->{RA},
                                    dec => $obsdata->{DEC},
                                    type => 'J2000',
                                    units => 'radians' );

    # Distance between centre and corner is...
    my $rad_pixels = sqrt( ( $lbnd[0] - $xcen ) * ( $lbnd[0] - $xcen ) +
                           ( $lbnd[1] - $ycen ) * ( $lbnd[1] - $ycen ) );

    $radius = $rad_pixels * $obsdata->{SCALE} / 60;
  }

  return ( $cencoords, $radius, $lbnd[0], $ubnd[0], $lbnd[1], $ubnd[1] );

}

=item B<query_skycat>

Perform a wrapped SkyCat query.

  my $catalog = $self->query_skycat( $coords, $radius );

Returns an Astro::Catalog object.

=cut

sub query_skycat {
  my $self = shift;
  my $coords = shift;
  my $radius = shift;

  my $racen = $coords->ra2000;
  my $deccen = $coords->dec2000;
  $racen->str_delim(' ');
  $deccen->str_delim(' ');

  $racen = "$racen";
  $deccen = "$deccen";
  $racen =~ s/^\s+//;
  $deccen =~ s/^\s+//;

  my $skycatstring = $self->catalogue;
  my @skycatnames = split /,/, $skycatstring;

  my $querycat;

  foreach my $skycatname ( @skycatnames ) {

    $self->printstd( sprintf( "--I Obtaining catalogue from %s.\n",
                              $skycatname ) )
      if $self->starlink_output;

    my $query;
    eval {
      $query = new Astro::Catalog::Query::SkyCat( catalog => $skycatname,
                                                  RA => "$racen",
                                                  Dec => "$deccen",
                                                  Radius => $radius,
                                                );
    };
    if( $@ ) {
      $self->printerr( "Error setting up SkyCat query: $@" );
      $self->printerr( "Trying next catalogue in list.\n" );
      next;
    }

    if( defined( $self->skycatconfig ) ) {
      $query->cfg_file( $self->skycatconfig );
    }
    eval {
      $querycat = $query->querydb();
    };

    if( $@ ) {
      $self->printerr( "Error retrieving catalogue from $skycatname: $@" );
      $self->printerr( "Trying next catalogue in list.\n" );
      next;
    }

    $self->printstd( sprintf( "--I Obtained catalogue, %d entries, from %s.\n",
                              $querycat->sizeof,
                              $skycatname ) )
      if $self->starlink_output;

    last if $querycat->sizeof != 0;

    $self->printerr( "Retrieved zero objects from $skycatname. Trying next catalogue.\n" );
    $self->printerr( "Trying next catalogue in list.\n" );
  }

  return $querycat;

}

=item B<ymd2jd>

Converts year, month, and day to a Julian Day number.

  my $jd = ymd2jd( $year, $month, $day );

This function assumes noon for the day in question. The year must be
a four-digit integer between 1000 and 3000. The month must be an integer
between 1 and 12, and the day must be an integer between 1 and 31. There
is no sanity checking on the date, so dates of 31 February are perfectly
valid.

This function returns the Julian Day number. If the input parameters are
outside their defined ranges, undef will be returned.

The formula that does the calculation is from Graham Woan's 'The Cambridge
Handbook of Physics Formulas'.

=cut

sub ymd2jd {
  # Use the standard formula to convert Gregorian dates to Julian
  # Day numbers
  use integer;
  my ($year, $month, $day) = @_;

  # Year is (1000..3000), month is in (1..12), day in (1..31).  The
  # restriction on year is not because of any limitation on the
  # validity of the formula, but to guard against silly parameters
  # (eg, 2-digit dates).
  my $err;
  if( $year < 1000 || $year > 3000 ) {
    carp "Input year to ymd2jd() of $year must be between 1000 and 3000";
    return undef;
  }
  if( $month < 1 || $month > 12 ) {
    carp "Input month to ymd2jd() of $month must be between 1 and 12";
    return undef;
  }
  if( $day < 1 || $day > 31 ) {
    carp "Input day to ymd2jd() of $day must be between 1 and 31";
    return undef;
  }

# Here comes the big calculation...
  return $day - 32075 + 1461 * ( $year + 4800 + ( $month - 14 ) / 12 ) / 4
         + 367 * ( $month - 2 - ( $month - 14 ) / 12 * 12 ) / 12
         - 3 * ( ( $year + 4900 + ( $month - 14 ) / 12 ) / 100 ) / 4;
}

=item B<jd2je>

Converts Julian Day to Julian epoch.

  my $je = jd2je( $jd );

The conversion from JD is from Robin Green's 'Spherical Astronomy',
section 10.5.

=cut

sub jd2je {
  my $jd = shift;
  return 2000.0 + ($jd - 2451545)/365.25;
}

=item B<ymd2je>

Converts year, month, and day to a Julian epoch.

  my $je = ymd2je( $year, $month, $day );

This function assumes noon for the day in question. The year must be
a four-digit integer between 1000 and 3000. The month must be an integer
between 1 and 12, and the day must be an integer between 1 and 31. There
is no sanity checking on the date, so dates of 31 February are perfectly
valid.

If the input values are outside of these ranges, undef will be returned.

=cut

sub ymd2je ($$$) {
  my ($year,$month,$day) = @_;

  my $jd = ymd2jd( $year, $month, $day );
  if( ! defined( $jd ) ) {
    return undef;
  }

  return jd2je( $jd );
}

=item B<parse_fits_date>

Convert a FITS-standard date into a Julian epoch.

  my $je = parse_fits_date( $fits_date );

According to the FITS standard, a FITS-standard date can be of the
form:

  YYYY-MM-DDThh:mm:ss[.s...],
  YYYY-MM-DD
  DD/MM/YY

The last form represents only dates between 1900 and 1999.

If no time is given, the time is taken to be noon.

This function returns the Julian epoch. If the input date is malformed,
then undef will be returned. Leading and trailing whitespace is allowed,
but otherwise the date must adhere to the FITS standard.

The FITS standard can be found at
http://www.cv.nrao.edu/fits/documents/standards/year2000.txt.

=cut

sub parse_fits_date {
  my $fdate = shift;
  if ($fdate =~ /^\s*(\d{4})-(\d{2})-(\d{2})(T(\d{2}):(\d{2}):(\d{2}(\.\d+)?))?\s*$/) {

    # We have a date of the form YYYY-MM-DD[Thh:mm:ss[.s...]].
    if (defined($4)) {

      # We have a date of the form YYYY-MM-DDThh:mm:ss[.s...].
	    my $jd = ymd2jd ($1,$2,$3);

	    # Add time: ymd2jd returns JD at noon.  Cf ymd2je
	    return 2000 + ( $jd + ( ( $5 - 12 ) * 3600 + $6 * 60 + $7 ) /86400.0
                      - 2451545 ) / 365.25;
    } else {
	    return ymd2je ($1,$2,$3);
    }

  } elsif ($fdate =~ m{^\s*(\d{2})/(\d{2})/(\d{2})\s*$}) {

    # We have a date of the form DD/MM/YY.
    return ymd2je ( $3 + 1900, $2, $1);

  } else {

    # Crikey!
    return undef;
  }
}

=head1 CVS VERSION

$Id$

=head1 SEE ALSO

Starlink User Note 242

=head1 AUTHORS

Brad Cavanagh E<lt>b.cavanagh@jach.hawaii.eduE<gt>

=head1 COPYRIGHT

Copyright (C) 2005-2006 Particle Physics and Astronomy Research
Council.  All Rights Reserved.

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful,but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 59 Temple
Place,Suite 330, Boston, MA  02111-1307, USA

=cut

1;
