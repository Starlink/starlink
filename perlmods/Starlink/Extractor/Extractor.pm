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
use Starlink::AMS::Init;
use Starlink::AMS::Task;
use Starlink::EMS qw/ :sai /;
use Astro::Catalog;

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

  frame - the NDF to perform the extraction on

  filter - an Astro::WaveBand object describing the filter used

This method returns an C<Astro::Catalog> object. Before extraction
can be done, the EXTRACTOR_DIR environment variable should be set up.
If this environment variable is not set, then the module will look
for the EXTRACTOR binary in "/star/bin/extractor". If it cannot be
found there, the method will croak.

This method ensures that sensible defaults have been set for some
of the configuration options. For these defaults, see the C<defaults>
method.

This method returns an C<Astro::Catalog> object. The magnitudes and
locations of extracted objects are defined as follows:

  Astro::Catalog::Star                     EXTRACTOR output parameter
  ID                                       NUMBER
  ra/dec coords                            ALPHA_J2000/DELTA_J2000
  magnitude                                MAG_ISOCOR
  error in magnitude                       MAGERR_ISOCOR
  x/y position                             X_IMAGE/Y_IMAGE
  x/y positional errors                    X2_IMAGE/Y2_IMAGE
  ellipticity                              ELLIPTICITY
  position_angle_pixel                     THETA_IMAGE
  position_angle_world                     THETA_J2000
  major/minor_axis_pixel                   A_IMAGE/B_IMAGE
  major/minor_axis_world                   A_WORLD/B_WORLD

=cut

sub extract {
  my $self = shift;

  my %args = @_;

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

# Do the extraction.
  my $ams = new Starlink::AMS::Init(1);
  if( ! defined $TASK ) {
    $TASK = new Starlink::AMS::Task("extractor", $extractor_bin );
  }
  my $STATUS = $TASK->contactw;
  if( ! $STATUS ) {
    croak "Could not contact EXTRACTOR monolith";
  }
  $STATUS = $TASK->obeyw("extract", "image=$ndf config=" . $self->_config_file_name );
  if( $STATUS != SAI__OK ) {
    croak "Error in running EXTRACTOR: $STATUS";
  }

# Form a catalogue from Astro::Catalog.
  my $catalog = new Astro::Catalog( Format => 'SExtractor',
                                    File => $self->_catalog_file_name,
                                    ReadOpt => { Filter => $filter },
                                  );

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
  print $fh "Y_IMAGE\n";
  print $fh "ALPHA_J2000\n";
  print $fh "DELTA_J2000\n";
  print $fh "MAG_ISOCOR\n";
  print $fh "MAGERR_ISOCOR\n";
  print $fh "X2_IMAGE\n";
  print $fh "Y2_IMAGE\n";
  print $fh "ERRX2_IMAGE\n";
  print $fh "ERRY2_IMAGE\n";
  print $fh "ELLIPTICITY\n";
  print $fh "THETA_IMAGE\n";
  print $fh "THETA_J2000\n";
  print $fh "A_IMAGE\n";
  print $fh "B_IMAGE\n";
  print $fh "A_WORLD\n";
  print $fh "B_WORLD\n";
  print $fh "ISOAREA_IMAGE\n";

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
  return File::Spec->catdir( $self->temp_dir, "config$$.sex" );
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
  return File::Spec->catdir( $self->temp_dir, "extract$$.param" );
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
  return File::Spec->catdir( $self->temp_dir, "extract$$.cat" );
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

=head1 AUTHORS

Brad Cavanagh E<lt>b.cavanagh@jach.hawaii.eduE<gt>

=head1 COPYRIGHT

Copyright (C) 2004-2005 Particle Physics and Astronomy Research
Council.  All Rights Reserved.

=cut

1;
