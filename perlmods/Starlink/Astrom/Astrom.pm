package Starlink::Astrom;

=head1 NAME

Starlink::Astrom - a Perl wrapper around the Starlink ASTROM
application.

=head1 SYNOPSIS

use Starlink::Astrom;

my $astrom = new Starlink::Astrom( catalog => $catalog );
my $frameset = $astrom->solve;

=head1 DESCRIPTION

This module provides wrapper routines for the Starlink ASTROM
application.

=cut

use Carp;
use strict;
use File::Spec;
use File::Temp qw/ tempfile /;
use File::Basename;

use Starlink::AST;
use Astro::Catalog;
use Astro::FITS::Header::CFITSIO;

use base qw/Exporter/;

use vars qw/ $VERSION $DEBUG /;

$VERSION = '0.01';
$DEBUG = 0;

=head1 METHODS

=head2 Constructor

=over 4

=item B<new>

  $astrom = new Starlink::Astrom( catalog => $catalog );

The constructor accepts one mandatory named parameter defining
the catalogue that will be used to generate an astrometric solution.
This parameter must be an C<Astro::Catalog> object.

The constructor returns an C<Starlink::Astrom> object.

=cut

sub new {
  my $proto = shift;
  my $class = ref( $proto ) || $proto;

  # Retrieve the arguments.
  my %args = @_;

  # Make sure there's a catalog.
  if( ! defined( $args{'catalog'} ) ||
      ! UNIVERSAL::isa( $args{'catalog'}, "Astro::Catalog" ) ) {
    croak "Must supply an Astro::Catalog object to Starlink::Astrom constructor";
  }

  my $catalog = $args{'catalog'};

  # Create the object.
  my $astrom = {};

  # Set up the catalogue.
  $astrom->{CATALOG} = $catalog;

  # Bless and return.
  bless( $astrom, $class );
  return $astrom;

}

=back

=head2 Accessor Methods

=over 4

=item B<catalog>

Retrieve or set the catalogue used to get an astrometric solution.

  my $catalog = $astrom->catalog;
  $astrom->catalog( $catalog );

When setting, argument must be an C<Astro::Catalog> object.

Returns an C<Astro::Catalog> object.

=cut

sub catalog {
  my $self = shift;
  if( @_ ) {
    my $catalog = shift;
    if( UNIVERSAL::isa( $catalog, "Astro::Catalog" ) ) {
      $self->{CATALOG} = $catalog;
    }
  }
  return $self->{CATALOG};
}

=back

=head2 General Methods

=over 4

=item B<solve>

Perform astrometry for the supplied catalogue.

  my $frameset = $astrom->solve;

This method returns a C<Starlink::AST::FrameSet> object that describes
the WCS calculated by ASTROM.

When running this method it attempts to find the astrom.x binary that
performs the astrometric calculations. It first looks in the location
specified by the AUTOASTROM_DIR environment variable, then looks in
/star/bin/autoastrom, then looks in /star/bin. If all three of these
locations fail to have the astrom.x binary, the method will croak with
an error stating such.

=cut

sub solve {
  my $self = shift;

  # Get the catalogue we're supposed to be working on.
  my $catalog = $self->catalog;

  # Try to find the ASTROM binary. First, check to see if
  # the AUTOASTROM_DIR environment variable is sit. If it
  # hasn't, check in /star/bin/autoastrom, and then in /star/bin.
  # If those three don't work, croak with an error.
  my $astrom_bin;
  if( defined( $ENV{'AUTOASTROM_DIR'} ) &&
      -d $ENV{'AUTOASTROM_DIR'} &&
      -e File::Spec->catfile( $ENV{'AUTOASTROM_DIR'}, "astrom.x" ) ) {

    $astrom_bin = File::Spec->catfile( $ENV{'AUTOASTROM_DIR'}, "astrom.x" );

  } elsif( -d File::Spec->catfile( "star", "bin", "autoastrom" ) &&
           -e File::Spec->catfile( "star", "bin", "autoastrom", "astrom.x" ) ) {
    $astrom_bin = File::Spec->catfile( "star", "bin", "autoastrom", "astrom.x" );
  } elsif( -d File::Spec->catfile( "star", "bin" ) &&
           -e File::Spec->catfile( "star", "bin", "astrom.x" ) ) {
    $astrom_bin = File::Spec->catfile( "star", "bin", "astrom.x" );
  } else {
    croak "Could not find astrom.x binary";
  }

  print "astrom.x binary is in $astrom_bin\n" if $DEBUG;

  # We need a temporary file for the astrom input file.
  ( undef, my $astrom_input ) = tempfile();

  # Write the catalog to the temporary file.
  $catalog->write_catalog( Format => 'Astrom', File => $astrom_input );
  print "ASTROM input catalog is in $astrom_input\n" if $DEBUG;

  # We need a base filename for the FITS files. ASTROM will automatically
  # append NN.fits, where NN is the fit number.
  ( undef, my $output_fitsbase ) = tempfile();
  print "ASTROM FITS file base name is $output_fitsbase\n" if $DEBUG;

  # And we need temporary files to hold the report, the summary, and the log.
  ( undef, my $output_report ) = tempfile();
  ( undef, my $output_summary ) = tempfile();
  ( undef, my $output_log ) = tempfile();
  print "ASTROM output report file is in $output_report\n" if $DEBUG;
  print "ASTROM output summary file is in $output_summary\n" if $DEBUG;
  print "ASTROM output log file is in $output_log\n" if $DEBUG;


  # Now we are good to go. Set up the parameter list for ASTROM.
  my $param = "input=$astrom_input report=$output_report summary=$output_summary";
  $param .= " log=$output_log fits=$output_fitsbase";

  # Do the astrometric fit.
  my @astromargs = ( $astrom_bin,
                     "input=$astrom_input",
                     "report=$output_report",
                     "summary=$output_summary",
                     "log=$output_log",
                     "fits=$output_fitsbase");
  my $astromexit = system(@astromargs);
  if( $astromexit != 0 ) {
    croak "Failed calling ASTROM. Exit code $astromexit";
  }

  # Right. Now we have all of the output files. We want to read in the
  # FITS file with the highest number, as that one is the best fit.
  # Grab all of the FITS files (we know what their base is), and find
  # the one with the highest number.
  my $fits_dir = dirname( $output_fitsbase );
  my $fits_base = basename( $output_fitsbase );
  opendir( my $dir_h, $fits_dir ) or croak "Could not open $fits_dir to read in FITS files: $!";
  my @fits_files = grep { /$fits_base/ } readdir( $dir_h );
  closedir( $dir_h );
  @fits_files = sort @fits_files;
  my $highest = $fits_files[-1];
  $highest = File::Spec->catfile( $fits_dir, $highest );

  # Create a Starlink::AST::FrameSet object from this FITS file.
  my $cfitsio = new Astro::FITS::Header::CFITSIO( File => $highest );
  my $wcs = $cfitsio->get_wcs;

  # Remove all of the temporary files, unless debugging is turned on.
  unlink $astrom_input unless $DEBUG;
  unlink $output_report unless $DEBUG;
  unlink $output_summary unless $DEBUG;
  unlink $output_log unless $DEBUG;
  unlink <$output_fitsbase*> unless $DEBUG;

  # And return the Starlink::AST::FrameSet object.
  return $wcs;
}

=head1 CVS VERSION

$Id$

=head1 SEE ALSO

Starlink User Note 5

C<Starlink::AST>

=head1 AUTHORS

Brad Cavanagh E<lt>b.cavanagh@jach.hawaii.eduE<gt>

=head1 COPYRIGHT

Copyright (C) 2005 Particle Physics and Astronomy Research
Council.  All Rights Reserved.

This program is free software; you can redistribute it and/or modify
it under the same conditions as Perl itself.

=cut

1;
