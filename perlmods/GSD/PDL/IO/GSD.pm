package PDL::IO::GSD;

=head1 NAME

PDL::IO::GSD - PDL module for reading JCMT GSD format files

=head1 SYNOPSIS

  use PDL::IO::GSD;

  $pdl = rgsd 'test.data';

=head1 DESCRIPTION

This module adds the ability to read James Clerk Maxwell telescope GSD
format files into PDL. 

=cut

use vars qw/ @EXPORT_OK %EXPORT_TAGS @ISA $VERSION/;

@ISA = qw/ PDL::Exporter /;

@EXPORT_OK = qw/ rgsd /;
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

use PDL::Core;     # Grab the Core names
use Carp;
use strict;

use vars qw/@EXPORT_OK @ISA @EXPORT_STATIC %gsdtypes/;

$VERSION = '0.92';


=head1 FUNCTIONS

=over 4

=item B<rgsd>

Read a GSD file into a PDL object.

  $pdl = rgsd $filename;

C<rgsd> produces PDLs containing the C<C13DAT> component and all other
items in the PDL header. The PDL header (retrievable via the
C<gethdr()> method) is a reference to a hash containing all the SCALAR
GSD items as simple keyword/value pairs and the array items as PDLs
(if numeric) or references to arrays (if character based).

=cut

my $gsd_loaded;

# This allows us to use it as a method or a function
sub rgsd { PDL->rgsd(@_) }

# Now declare the method in the PDL namesapce
sub PDL::rgsd ($$) {  # Read an GSD format file into a PDL

  my $class = shift;
  my $file = shift; 

  # Try to load the GSD::PDL library
  eval 'use GSD::PDL' unless $gsd_loaded++;
  croak 'Cannot use GSD library' if $@ ne "";

  # Create a new GSD object
  my $gsd = new GSD::PDL( $file );

  # raise an error
  croak "Unable to open $file" unless defined $gsd;

  # Read C13DAT

  my $pdl = $gsd->GetByName( 'C13DAT' );

  # check return value
  unless (defined $pdl) {
    carp "Error reading C13DAT component from GSD file. Aborting\n";
    return undef;
  }

  # Find the item number so that we can skip it later
  my $C13DATitemno = $gsd->Find( 'C13DAT' );

  print "Read primary data header (C13DAT) correctly: ", $pdl->info, "\n"
    if $PDL::verbose;

  # Get the header
  my $hdr = {};

  # Loop over all items reading into the header
  for my $i (1..$gsd->nitems) {

    next if $i == $C13DATitemno;

    my ($name, $units, $type, $array) = $gsd->Item( $i );

    # If we have an array we can put more info down
    if ($array) {

      $hdr->{$name} = $gsd->GetByNum( $i );

      if ($PDL::verbose) {
	if (UNIVERSAL::isa($hdr->{$name}, 'PDL')) {
	  printf "Read %-20s : %s\n",$name, $hdr->{$name}->info;
	} else {
	  printf "Read %-20s : (%s)\n",$name,$type;
	}
      }

    } else {

      $hdr->{$name} = $gsd->GetByNum($i);

    }

  }

  # Store the header
  $pdl->sethdr( $hdr );

  return $pdl;

}

=back

=head1 NOTES

=over 4

=item *

The command is read-only. GSD files can not be written or modified.

=item *

Axis units or labels are not stored since there is no standard
place to put that information in PDL.

=item *

The perl GSD module must be available.

=back

=head1 AUTHOR

Tim Jenness, E<lt>t.jenness@jach.hawaii.eduE<gt>

Copyright (C) 1995-2000,2003 Tim Jenness and the UK Particle Physics and
Astronomy Research Council. All Rights Reserved.

=head1 SEE ALSO

L<PDL::FAQ> for general information on the Perl Data language,
L<GSD> and L<GSD::PDL> for information on the GSD module.

=cut

1;
