package Starlink::Extractor;

=head1 NAME

Starlink::Extractor - a Perl wrapper around the Starlink EXTRACTOR
application.

=head1 SYNOPSIS

  use Starlink::Extractor;

  my $extractor = new Starlink::Extractor;
  $extractor->detect_thresh( 1.0 );
  my $clean_param = $extractor->clean_param;

  $catalogue = $extractor->extract( frame => $ndf, filter => $filter );

=head1 DESCRIPTION

This module provides wrapper routines for the Starlink EXTRACTOR
application. Functions are provided for setting various EXTRACTOR
parameters and for extracting detected objects from an NDF.

Any of the configuration options available to EXTRACTOR are available
to be changed or read using this module. For example, to change the
detection threshold to 5 sigma, you would do:

  $extractor->detect_thresh( 5.0 );

Some configuration options consist of two or more numbers, such
as photometry apertures. To set these, use Perl arrays:

  $extractor->phot_apertures( [2.0, 3.0, 5.0] );

The configuration options can also be retrieved:

  $clean_param = $extractor->clean_param;

To perform an extraction on an image, simply call the extract()
method:

  $catalogue = $extractor->extract( $ndf );

This returns an C<Astro::Catalog> object from the output of EXTRACTOR.
See the C<extract()> method's documentation for further information.

=cut

use Carp;
use strict;

use File::Spec;
use File::Temp;
use Class::Struct;

use Starlink::ADAM;
use Starlink::AMS::Init;
use Starlink::AMS::Task;
use Starlink::EMS qw/ :sai get_facility_error /;
use Astro::Catalog;

use NDF;

use base qw/Exporter/;

use vars qw/ $VERSION @EXPORT_OK $DEBUG /;

'$Revision$ ' =~ /.*:\s(.*)\s\$/ && ($VERSION = $1);

$DEBUG = 0;
@EXPORT_OK = qw/ /;

our %config_options;

# Cache the task;
our $TASK;

=head1 METHODS

=head2 Constructor

=over 4

=item B<new>

  $extractor = new Starlink::Extractor( config => 'config.sex' );

The constructor accepts one optional parameter defining an
Extractor configuration file. If the configuration file is
not passed, a set of default configuration options will be
assigned (see the C<defaults> method for more information).

=cut

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

# Retrieve the arguments.
  my %args = @_;

# Create the object.
  my $obj = bless {}, $class;

# Initialise it, populating the various accessors if we're given
# a config file.
  if( exists( $args{'config'} ) ) {
    $obj->parse_config( $args{'config'} );
  }

# Set the defaults for some of the accessors.
  $obj->defaults;

  return $obj;
}

=back

=head2 General Methods

=over 4

=item B<parse_config>

Parse a configuration file, populating accessor methods while we go.

  $extractor->parse_config( 'config.sex' );

Takes one mandatory argument, a string containing the location of the
configuration file.

=cut

sub parse_config {
  my $self = shift;
  my $config = shift;

  return if ! defined( $config );

  open my $fh, $config or croak "Could not open $config: $!";
  while ( my $line = <$fh> ) {

    chomp $line;

    # Remove whitespace from beginning of line.
    $line =~ s/^\s+//;

    # Skip the line if it starts with a # and is therefore a comment.
    next if ( $line =~ /^#/ );

    # Wipe out everything that's a space before a #, the #, and anything
    # after the #.
    $line =~ s/\s+#.*$//;

    # Wipe out trailing whitespace.
    $line =~ s/\s+$//;

    # And skip empty lines.
    next if ( $line eq '' );

    # Now we can actually parse the line.
    $line =~ /(\w+)\s+(.*)/;
    my $key = $1;
    my $value = $2;

    # Parse the value for commas. If there's a comma, assume the value
    # is a list of values.
    if( $value =~ /,/ ) {
      my @values = split( /\s*,\s*/, $value );
      my $method = lc( $key );
      if( $self->can( $method ) ) {
        $self->$method( @values );
      }
    } else {
      my $method = lc( $key );
      if( $self->can( $method ) ) {
        $self->$method( $value );
      }
    }
  }

  # Make sure we have some sensible defaults if some parameters aren't
  # defined.
  $self->defaults;
}

=item B<extract>

Do the extraction.

  my $catalog = $extractor->extract( frame => "gw20041119_112_mos",
                                     filter => $waveband );

The two mandatory named parameters are:

=item frame - The NDF to perform the extraction on.

=item filter - An Astro::WaveBand object describing the filter used.

An optional named parameter is:

=item quality - If set, then only items that have this extraction flag
will be put into the output catalogue. If not set, then every item
will be used.

This method returns an C<Astro::Catalog> object. Before extraction
can be done, the EXTRACTOR_DIR environment variable should be set up.
If this environment variable is not set, then the module will look
for the EXTRACTOR binary in "/star/bin/extractor". If it cannot be
found there, the method will croak.

This method ensures that sensible defaults have been set for some
of the configuration options. For these defaults, see the C<defaults>
method.

This method returns an C<Astro::Catalog> object. See the documentation
for the C<Astro::Catalog::IO::SExtractor> module for how information
from the EXTRACTOR catalogue is turned into an C<Astro::Catalog>
object.

=cut

sub extract {
  my $self = shift;

  my %args = @_;

# Set up a new Astro::Catalog object.
  my $catalog = new Astro::Catalog;

# Deal with arguments.
  if( !defined( $args{'frame'} ) ) {
    croak "Must pass frame name in order to do source extraction";
  }
  if( !defined( $args{'filter'} ) ) {
    croak "Must pass filter in order to do source extraction";
  }
  if( !UNIVERSAL::isa( $args{'filter'}, "Astro::WaveBand" ) ) {
    croak "Must pass filter as Astro::WaveBand object in order to do source extraction";
  }
  my $ndf = $args{'frame'};
  my $filter = $args{'filter'};
  my $quality = $args{'quality'};
  if( ! defined( $quality ) ) {
    $quality = -1;
  }

# Try to find the extractor binary. First, check to see if
# the EXTRACTOR_DIR environment variable has been set.
  my $extractor_bin;
  if( defined( $ENV{'EXTRACTOR_DIR'} ) &&
      -d $ENV{'EXTRACTOR_DIR'} &&
      -e File::Spec->catfile( $ENV{'EXTRACTOR_DIR'}, "extractor" ) ) {
    $extractor_bin = File::Spec->catfile( $ENV{'EXTRACTOR_DIR'}, "extractor" );
  } elsif( -d "/star/bin/extractor" &&
           -e "/star/bin/extractor/extractor" ) {
    $extractor_bin = "/star/bin/extractor/extractor";
  } else {
    croak "Could not find EXTRACTOR binary";
  }
  print "EXTRACTOR binary is in $extractor_bin\n" if $DEBUG;

# Set the defaults, just in case we haven't already.
  $self->defaults;

# Write the configuration file.
  $self->_write_config_temp_file;

# Write the parameter file.
  $self->_write_param_temp_file;

# If the user requested a checkerboard pattern, do it now.
  if( defined( $self->checkerboard ) ) {

    # Get a new temporary file name for the catalog.
    $self->_catalog_file_name( 1 );
    $self->catalog_name( $self->_catalog_file_name );
    $self->_write_config_temp_file;

    my $segments = $self->checkerboard->{SEGMENTS};
    my $interval = $self->checkerboard->{INTERVAL};

    # Calculate the height and width of the regions. We need to get
    # the NDF dimensions for this.
    my $STATUS = 0;
    err_begin( $STATUS );
    ndf_begin();
    ndf_find( &NDF::DAT__ROOT(), $ndf, my $ndf_id, $STATUS );
    ndf_bound( $ndf_id, 2, my @lbnd, my @ubnd, my $ndim, $STATUS );
    ndf_annul( $ndf_id, $STATUS );
    ndf_end( $STATUS );
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
    if( $ndim != 2 ) {
      croak "Cannot run SExtractor on a non-2D image\n";
    }

    my $height = int( ( $ubnd[1] - $lbnd[1] ) / $segments );
    my $width = int( ( $ubnd[0] - $lbnd[0] ) / $segments );

    # Calculate the image regions.
    my %regions;
    for my $i ( 1 .. $segments ) {
      for my $j ( 1 .. $segments ) {
        $regions{ $segments * ( $j - 1 ) + $i } =
          { x_start => ( $i - 1 ) * $width + 1,
            y_start => ( $j - 1 ) * $height + 1,
            x_end => $i * $width,
            y_end => $j * $height,
          };
      }
    }

    # Now, start at 1 and increment by $increment until we hit
    # ($segments**2).
    for ( my $k = 1; $k < ( $segments**2 ); $k = $k + $interval ) {

      my $ndfregion = '(';
      $ndfregion .= $regions{$k}->{x_start};
      $ndfregion .= ":";
      $ndfregion .= $regions{$k}->{x_end};
      $ndfregion .= ",";
      $ndfregion .= $regions{$k}->{y_start};
      $ndfregion .= ":";
      $ndfregion .= $regions{$k}->{y_end};
      $ndfregion .= ")";

      # Do the extraction.
      my $ams = new Starlink::AMS::Init(1);
      my $set_messages = $ams->messages;
      if( ! defined( $set_messages ) ) {
        $ams->messages( $self->messages );
      }
      $ams->timeout( $self->timeout );
      if( ! defined $TASK ) {
        $TASK = new Starlink::AMS::Task("extractor", $extractor_bin );
      }
      my $STATUS = $TASK->contactw;
      if( ! $STATUS ) {
        croak "Could not contact EXTRACTOR monolith";
      }
      $STATUS = $TASK->obeyw("extract", "image=$ndf$ndfregion config=" . $self->_config_file_name );
      if( $STATUS != SAI__OK && $STATUS != &Starlink::ADAM::DTASK__ACTCOMPLETE ) {
        ( my $facility, my $ident, my $text ) = get_facility_error( $STATUS );
        croak "Error in running EXTRACTOR: $STATUS - $text";
      }

      # Form a catalogue from Astro::Catalog.
      my $newcatalog = new Astro::Catalog( Format => 'SExtractor',
                                           File => $self->_catalog_file_name,
                                           ReadOpt => { Filter => $filter },
                                         );

      # Merge it in with the main catalog;
      my @newstars = $newcatalog->allstars;
      $catalog->pushstar( @newstars );
    }

  } else {

    # Just do the extraction.
    my $ams = new Starlink::AMS::Init(1);
    my $set_messages = $ams->messages;
    if( ! defined( $set_messages ) ) {
      $ams->messages( $self->messages );
    }
    $ams->timeout( $self->timeout );
    if( ! defined $TASK ) {
      $TASK = new Starlink::AMS::Task("extractor", $extractor_bin );
    }
    my $STATUS = $TASK->contactw;
    if( ! $STATUS ) {
      croak "Could not contact EXTRACTOR monolith";
    }
    $STATUS = $TASK->obeyw("extract", "image=$ndf config=" . $self->_config_file_name );
    if( $STATUS != SAI__OK && $STATUS != &Starlink::ADAM::DTASK__ACTCOMPLETE ) {
      ( my $facility, my $ident, my $text ) = get_facility_error( $STATUS );
      croak "Error in running EXTRACTOR: $STATUS - $text";
    }

    # Form a catalogue from Astro::Catalog.
    $catalog = new Astro::Catalog( Format => 'SExtractor',
                                   File => $self->_catalog_file_name,
                                   ReadOpt => { Filter => $filter,
                                                Quality => $quality },
                                 );
  }

# Delete the configuration file.
  $self->_delete_config_temp_file if ! $DEBUG;

# Delete the parameter file.
  $self->_delete_param_temp_file if ! $DEBUG;

# Delete the catalog file.
  $self->_delete_catalog_temp_file if ! $DEBUG;

# Return the catalogue.
  return $catalog;
}

=item B<defaults>

Set up some sensible defaults.

  $extractor->defaults;

This method sets the output catalogue name and type, the output
parameters file, and the verbosity of SExtractor to QUIET. It
also sets the MAG_GAMMA parameter to 4.0 if it has not been defined.

=cut

sub defaults {
  my $self = shift;

# We want to maintain complete control over the type of
# results file written, where it's written, and which
# results are in it.
  my $catalog_name = $self->_catalog_file_name;
  my $catalog_type = "ASCII_HEAD";
  my $parameters_name = $self->_param_file_name;

# Set these.
  $self->catalog_name( $catalog_name );
  $self->catalog_type( $catalog_type );
  $self->parameters_name( $parameters_name );

# Set some other mandatory parameters if they're not already set.
  if( ! defined( $self->mag_gamma ) ) { $self->mag_gamma( 4.0 ); }
  if( ! defined( $self->filter ) ) { $self->filter( 'N' ); }
  if( ! defined( $self->verbose_type ) ) { $self->verbose_type( 'QUIET' ); }
  if( ! defined( $self->quick ) ) { $self->quick( 0 ); }

# Set the ADAM task parameters if they're not already set.
  if( ! defined( $self->messages ) ) { $self->messages( 0 ); }
  if( ! defined( $self->timeout ) ) { $self->timeout( 60 ); }
}

=item B<checkerboard>

If set, divide up the image into a checkerboard pattern and only run
extraction on a certain spacing of blocks.

  my $checker = $extractor->checkerboard;
  $extractor->checkerboard( segments => $segments,
                            interval => $interval );

There are two mandatory named arguments. The first denotes how many
segments per side to divide the image up into. The second denotes the
frequency of blocks to use.

For example, if segments is set to 8 and interval is set to 2, then
the image is broken up into 64 blocks (8 segments per side), and every
other block is used. This is exactly a checkerboard pattern, taking
only the black squares.

If either segments or interval is 1, the entire image is used.

This method returns a hash reference.

=cut

sub checkerboard {
  my $self = shift;
  if( @_ ) {
    my %args = @_;
    $self->{CHECKERBOARD} = { SEGMENTS => $args{'segments'},
                              INTERVAL => $args{'interval'} };
  }
  return $self->{CHECKERBOARD};
}

=item B<messages>

Whether or not to display messages from the EXTRACTOR monolith while
processing.

  my $messages = $extractor->messages;
  $extractor->messages( 1 );

If set to true, then messages from the monolith will be printed.

Defaults to false.

=cut

sub messages {
  my $self = shift;
  if( @_ ) {
    my $messages = shift;
    $self->{MESSAGES} = $messages;
  }
  return $self->{MESSAGES};
}

=item B<quick>

Whether or not to do a quick extraction.

  my $quick = $extractor->quick;
  $extractor->quick( 1 );

If set to true, then no astrometric or windowing results will be
calculated. Further, the only photometric results to be calculated
will be the _ISO and _APER1 parameters. Object morphology results will
be calculated.

Defaults to false.

=cut

sub quick {
  my $self = shift;
  if( @_ ) {
    my $quick = shift;
    $self->{QUICK} = $quick;
  }
  return $self->{QUICK};
}

=item B<temp_dir>

Returns or sets the name of the temporary directory used for writing
various Extractor files.

  $extractor->temp_dir( "/tmp" );
  my $temp_dir = $extractor->temp_dir;

If the temporary directory has not been set it will default to /tmp.

=cut

sub temp_dir {
  my $self = shift;
  if ( @_ ) {
    $self->{TmpDir} = shift;
  }
  if( ! defined( $self->{TmpDir} ) ) {
    $self->{TmpDir} = "/tmp";
  }
  return $self->{TmpDir};
}

=item B<timeout>

Return or set the ADAM timeout when communicating with the EXTRACTOR
monolith.

  my $timeout = $extractor->timeout;
  $extractor->timeout( 120 );

Time is in seconds. Defaults to 60.

=cut

sub timeout {
  my $self = shift;
  if( @_ ) {
    my $timeout = shift;
    $self->{TIMEOUT} = $timeout;
  }
  return $self->{TIMEOUT};
}

=back

=begin __PRIVATE_METHODS__

=head1 PRIVATE METHODS

These methods are private to Starlink::Extractor.

=over 4

=item B<_write_config_temp_file>

Writes the configuration file.

  $extractor->_write_config_temp_file;

Writes the file to the temporary directory stored in the
C<temp_dir> method.

=cut

sub _write_config_temp_file {
  my $self = shift;
  my $conffile = $self->_config_file_name;

# Open a filehandle to the config file.
  open my $fh, ">$conffile" or croak "Could not open $conffile for writing: $!\n";

# For each Extractor parameter...
  foreach my $key ( keys %config_options ) {

# ...skip if we haven't defined it.
    next if ( ! defined( $self->$key ) );

# If we have an array reference...
    if( ref( $self->$key ) eq 'ARRAY' ) {
      my @value = $self->$key;
      next if scalar( @value ) == 0;

# ...join the values with commas and print to the file.
      print $fh uc( $key ) . " " . ( join ',', @value ) . "\n";
    } else {

# Otherwise just print the value to the file.
      print $fh uc( $key ) . " " . $self->$key . "\n";
    }
  }

# And close the filehandle.
  close $fh;
}

=item B<_write_param_temp_file>

Writes the output parameter file.

  $extractor->_write_param_temp_file;

Writes the file to the temporary directory stored in the C<temp_dir>
method.

=cut

sub _write_param_temp_file {
  my $self = shift;
  my $paramfile = $self->_param_file_name;
  open my $fh, ">$paramfile" or croak "Could not open $paramfile for writing: $!\n";

  print $fh "NUMBER\n";
  print $fh "X_IMAGE\n";
  print $fh "X_PIXEL\n" if defined $self->checkerboard;
  print $fh "ERRX2_IMAGE\n";
  print $fh "XWIN_IMAGE\n" if ! $self->quick;
  print $fh "ERRX2WIN_IMAGE\n" if ! $self->quick;
  print $fh "Y_IMAGE\n";
  print $fh "Y_PIXEL\n" if defined $self->checkerboard;
  print $fh "ERRY2_IMAGE\n";
  print $fh "YWIN_IMAGE\n" if ! $self->quick;
  print $fh "ERRY2WIN_IMAGE\n" if ! $self->quick;
  print $fh "ALPHA_J2000\n" if ! $self->quick;
  print $fh "DELTA_J2000\n" if ! $self->quick;
  print $fh "FLUX_ISO\n";
  print $fh "FLUXERR_ISO\n";
  print $fh "MAG_ISO\n";
  print $fh "MAGERR_ISO\n";
  print $fh "FLUX_APER\n";
  print $fh "FLUXERR_APER\n";
  print $fh "MAG_APER\n";
  print $fh "MAGERR_APER\n";
  print $fh "FLUX_ISOCOR\n" if ! $self->quick;
  print $fh "FLUXERR_ISOCOR\n" if ! $self->quick;
  print $fh "MAG_ISOCOR\n" if ! $self->quick;
  print $fh "MAGERR_ISOCOR\n" if ! $self->quick;
  print $fh "FLUX_AUTO\n" if ! $self->quick;
  print $fh "FLUXERR_AUTO\n" if ! $self->quick;
  print $fh "MAG_AUTO\n" if ! $self->quick;
  print $fh "MAGERR_AUTO\n" if ! $self->quick;
  print $fh "FLUX_BEST\n" if ! $self->quick;
  print $fh "FLUXERR_BEST\n" if ! $self->quick;
  print $fh "MAG_BEST\n" if ! $self->quick;
  print $fh "MAGERR_BEST\n" if ! $self->quick;
  print $fh "ELLIPTICITY\n";
  print $fh "THETA_IMAGE\n";
  print $fh "ERRTHETA_IMAGE\n";
  print $fh "THETA_SKY\n" if ! $self->quick;
  print $fh "ERRTHETA_SKY\n" if ! $self->quick;
  print $fh "A_IMAGE\n";
  print $fh "ERRA_IMAGE\n";
  print $fh "B_IMAGE\n";
  print $fh "ERRB_IMAGE\n";
  print $fh "A_WORLD\n" if ! $self->quick;
  print $fh "ERRA_WORLD\n" if ! $self->quick;
  print $fh "B_WORLD\n" if ! $self->quick;
  print $fh "ERRB_WORLD\n" if ! $self->quick;
  print $fh "ISOAREA_IMAGE\n";
  print $fh "FLAGS\n";

  close $fh;
}

=item B<_delete_catalog_temp_file>

Deletes the temporary catalog file.

  $extractor->_delete_catalog_temp_file;

=cut

sub _delete_catalog_temp_file {
  my $self = shift;
  my $catfile = $self->_catalog_file_name;
  unlink $catfile;
}

=item B<_delete_config_temp_file>

Deletes the temporary configuration file.

  $extractor->_delete_config_temp_file;

=cut

sub _delete_config_temp_file {
  my $self = shift;
  my $conffile = $self->_config_file_name;
  unlink $conffile;
}

=item B<_delete_param_temp_file>

Deletes the temporary output parameter file.

  $extractor->_delete_config_temp_file;

=cut

sub _delete_param_temp_file {
  my $self = shift;
  my $paramfile = $self->_param_file_name;
  unlink $paramfile;
}

=item B<_config_file_name>

Returns the name of the config file to be passed to the
Extractor task.

  my $config_file_name = $extractor->_config_file_name;

The filename is made up of the temporary directory (set by the
temp_dir method) and the filename "config<n>.sex", where
<n> is the process ID.

=cut

sub _config_file_name {
  my $self = shift;
  if( @_ ) {
    my $new = shift;
    if( $new == 1 ) {
      $self->{_CONFIG_FILE_NAME} = undef;
    }
  }
  if( ! defined( $self->{_CONFIG_FILE_NAME} ) ) {
    my $tmp = new File::Temp( UNLINK => 0 );
    $self->{_CONFIG_FILE_NAME} = "$tmp";
  }
  print "config file is " . $self->{_CONFIG_FILE_NAME} . "\n" if $DEBUG;
  return $self->{_CONFIG_FILE_NAME};
}

=item B<_param_file_name>

Returns the name of the output parameter file to be passed to the
Extractor task.

  my $param_file_name = $extractor->_param_file_name;

The filename is made up of the temporary directory (set by the
temp_dir method) and the filename "extract<n>.param", where
<n> is the process ID.

=cut

sub _param_file_name {
  my $self = shift;
  if( ! defined( $self->{_PARAM_FILE_NAME} ) ) {
    my $tmp = new File::Temp( UNLINK => 0 );
    $self->{_PARAM_FILE_NAME} = "$tmp";
  }
  print "param file is " . $self->{_PARAM_FILE_NAME} . "\n" if $DEBUG;
  return $self->{_PARAM_FILE_NAME};
}

=item B<_catalog_file_name>

Returns the name of the output catalog file created by the
Extractor task.

  my $catalog_file_name = $extractor->_catalog_file_name;

The filename is made up of the temporary directory (set by the
temp_dir method) and the filename "extract<n>.cat", where
<n> is the process ID.

=cut

sub _catalog_file_name {
  my $self = shift;
  if( @_ ) {
    my $new = shift;
    if( $new == 1 ) {
      $self->{_CATALOG_FILE_NAME} = undef;
    }
  }
  if( ! defined( $self->{_CATALOG_FILE_NAME} ) ) {
    my $tmp = new File::Temp( UNLINK => 0 );
    $self->{_CATALOG_FILE_NAME} = "$tmp";
  }
  print "catalog file is " . $self->{_CATALOG_FILE_NAME} . "\n" if $DEBUG;
  return $self->{_CATALOG_FILE_NAME};
}

=item B<CreateAccessors>

Create specific accessor methods on the basis of the supplied
initialiser.

  Starlink::Extractor->CreateAccessors( %members );

An accesssor method will be created for each key supplied in the
hash. The value determines the type of accessor method.

  $ - standard scalar accessor (non reference)
  % - hash (ref) accessor
  @ - array (ref) accessor
  'string' - class name (supplied object must be of specified class)
  @string   - array containing specific class
  %string  - hash containing specific class
  $__UC__ - upper case all arguments (scalar only)
  $__LC__ - lower case all arguments (scalar only)
  $__ANY__ - any scalar (including references)

Scalar accessor accept scalars (but not references) to modify the
contents and return the scalar.  With UC/LC modifiers the scalar
arguments are upcased or down cased. With the ANY modifier scalars can
include references of any type (this is also implied by UC and LC).
Hash/Array accessors accept either lists or a reference (and do not
check contents). They return a list in list context and a reference in
scalar context. If an @ or % is followed by a string this indicates
that the array/hash can only accept arguments of that class (all
arguments or array elements are tested).

Class accessors are the same as scalar accessors except their
arguments are tested to make sure they are of the right class.

=cut

sub CreateAccessors {
  my $caller = shift;
  my %struct = @_;

  my $header = "{\n package $caller;\n use strict;\n use warnings;\nuse Carp;\n"
;

  my $footer = "\n}";

  my $SCALAR = q{
#line 1 OMP::Info::Base::SCALAR
                 sub METHOD {
                   my $self = shift;
                   if (@_) {
                     my $argument = shift;
                     if (defined $argument) {
                       CLASS_CHECK;
                     }
                     $self->{METHOD} = $argument;
                   }
                   return $self->{METHOD};
                 }
               };

  my $ARRAY = q{
#line 1 OMP::Info::Base::ARRAY
                 sub METHOD {
                   my $self = shift;
                   $self->{METHOD} = [] unless $self->{METHOD};
                   if (@_) {
                     my @new;
                     if (ref($_[0]) eq 'ARRAY') {
                       @new = @{$_[0]};
                     } else {
                       @new = @_;
                     }
                     ARRAY_CLASS_CHECK;
                     @{ $self->{METHOD} } = @new;
                   }
                   if (wantarray) {
                     return @{ $self->{METHOD} };
                   } else {
                     return $self->{METHOD};
                   }
                 }
               };

  my $HASH = q{
#line 1 OMP::Info::Base::HASH
               sub METHOD {
                 my $self = shift;
                 $self->{METHOD} = {} unless $self->{METHOD};
                 if (@_) {
                   if (defined $_[0]) {
                     my %new;
                     if (ref($_[0]) eq 'HASH') {
                       %new = %{$_[0]};
                     } else {
                       %new = @_;
                     }
                     HASH_CLASS_CHECK;
                     %{ $self->{METHOD} } = %new;
                   } else {
                     # clear class
                     $self->{METHOD} = undef;
                     return undef;
                   }
                 }
                 if (wantarray) {
                   return (defined $self->{METHOD} ? %{ $self->{METHOD} } : () );
                 } else {
                   $self->{METHOD} = {} if !defined $self->{METHOD};
                   return $self->{METHOD};
                 }
               }
             };

  my $CLASS_CHECK = q{
#line 1 OMP::Info::Base::class_check
                      unless (UNIVERSAL::isa($argument, 'CLASS')) {
                        croak "Argument for 'METHOD' must be of class CLASS and not class '".
                        (defined $argument ? (ref($argument) ? ref($argument)
                                              : $argument) : '<undef>') ."'";
                      }
                     };

  my $ARRAY_CLASS_CHECK = q{
#line 1 OMP::Info::Base::array_class_check
                            for my $argument (@new) {
                              CLASS_CHECK;
                            }
                           };

  my $HASH_CLASS_CHECK = q{
#line 1 OMP::Info::Base::hash_class_check
                           for my $key (keys %new) {
                             my $argument = $new{$key};
                             CLASS_CHECK;
                           }
                          };

  my $REFCHECK = q{ croak "Argument for method 'METHOD' can not be a reference"
                      if ref($argument);
                  };
  my $UPCASE = $REFCHECK . q{ $argument = uc($argument); };
  my $DOWNCASE = $REFCHECK . q{ $argument = lc($argument); };

  # Loop over the supplied keys
  my $class = '';
  for my $key (keys %struct) {
    # Need to create the code
    my $code = $header;

    my $MEMBER = $key;
    my $TYPE = $struct{$key};

    if ($TYPE =~ /^\$/ ) {
      # Simple scalar
      $code .= $SCALAR;

      # Remove the CHECK block
      if ($TYPE =~ /__UC__/) {
        # upper case
        $code =~ s/CLASS_CHECK/$UPCASE/;
      } elsif ($TYPE =~ /__LC__/) {
        # lower case
        $code =~ s/CLASS_CHECK/$DOWNCASE/;
      } elsif ($TYPE =~ /__ANY__/) {
        $code =~ s/CLASS_CHECK//;
      } else {
        # Check references
        $code =~ s/CLASS_CHECK/$REFCHECK/;
      }

    } elsif ($TYPE =~ /^\@/) {

      $code .= $ARRAY;

      # Using a class?
      if ($TYPE =~ /^\@(.+)/) {
        $class = $1;
        $code =~ s/ARRAY_CLASS_CHECK/$ARRAY_CLASS_CHECK/;
        $code =~ s/CLASS_CHECK/$CLASS_CHECK/;
      } else {
        $code =~ s/ARRAY_CLASS_CHECK//;
      }
    } elsif ($TYPE =~ /^\%/) {

      $code .= $HASH;

      # Using a class?
      if ($TYPE =~ /^\%(.+)/) {
        $class = $1;
        $code =~ s/HASH_CLASS_CHECK/$HASH_CLASS_CHECK/;
        $code =~ s/CLASS_CHECK/$CLASS_CHECK/;
      } else {
        $code =~ s/HASH_CLASS_CHECK//;
      }

    } elsif ($TYPE =~ /^\w/) {
      # Hopefully a class
      $class = $TYPE;
      $code .= $SCALAR;
      $code =~ s/CLASS_CHECK/$CLASS_CHECK/;

    }

    # Add the closing block
    $code .= $footer;

    # Replace METHOD with method name
    $code =~ s/METHOD/$MEMBER/g;
    $code =~ s/CLASS/$class/g;

    # Run the code
    eval $code;
    if ($@) {
      croak "Error running method creation code: $@\n Code: $code\n";
    }
  }
}

=back

=begin __PRIVATE__

=head2 Create Accessor Methods

Create the accessor methods from a signature of their contents.

=cut

%config_options = ( analysis_thresh => '@',
                    assoc_data => '@',
                    assoc_name => '$',
                    assoc_params => '@',
                    assoc_radius => '$',
                    assoc_type => '$',
                    assocselec_type => '$',
                    back_filtersize => '@',
                    back_size => '@',
                    back_type => '@',
                    back_value => '@',
                    backphoto_thick => '$',
                    backphoto_type => '$',
                    catalog_name => '$',
                    catalog_type => '$',
                    checkimage_name => '@',
                    checkimage_type => '@',
                    clean => '$',
                    clean_param => '$',
                    deblend_mincont => '$',
                    deblend_nthresh => '$',
                    detect_minarea => '$',
                    detect_thresh => '@',
                    detect_type => '$',
                    filter => '$',
                    filter_name => '$',
                    filter_thresh => '@',
                    fits_unsigned => '$',
                    flag_image => '@',
                    flag_type => '$',
                    gain => '$',
                    interp_maxxlag => '@',
                    interp_maxylag => '@',
                    interp_type => '@',
                    mag_gamma => '$',
                    mag_zeropoint => '$',
                    mask_type => '$',
                    memory_bufsize => '$',
                    memory_objstack => '$',
                    memory_pixstack => '$',
                    parameters_name => '$',
                    phot_apertures => '@',
                    phot_autoparams => '@',
                    phot_autoapers => '@',
                    phot_fluxfrac => '@',
                    pixel_scale => '$',
                    satur_level => '$',
                    seeing_fwhm => '$',
                    starnnw_name => '$',
                    thresh_type => '@',
                    verbose_type => '$',
                    weight_gain => '$',
                    weight_image => '@',
                    weight_type => '@',
);

__PACKAGE__->CreateAccessors( %config_options );

=head1 TODO

=head1 SEE ALSO

Starlink User Note 226.

=head1 AUTHORS

Brad Cavanagh E<lt>b.cavanagh@jach.hawaii.eduE<gt>

=head1 COPYRIGHT

Copyright (C) 2004-2005 Particle Physics and Astronomy Research
Council.  All Rights Reserved.

=cut

1;
