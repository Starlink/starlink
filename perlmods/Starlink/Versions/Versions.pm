package Starlink::Versions;

=head1 NAME

Starlink::Versions - determine version numbers of Starlink applications

=head1 SYNOPSIS

 use Starlink::Versions qw/ starversion /;

 ($major, $minor, $patchlevel) = starversion('kappa');

 $verstring = starversion_string('kappa');

 if (starversion('surf') gt v1.5.2) {
   ...
 }

=head1 DESCRIPTION

When writing Perl programs that make use of Starlink applications
it is sometimes necessary to check the relevant version number
before attempting specific tasks. This module attempts to determine
the version number and returns it to the caller. It is not guaranteed
to be successful in all cases (since in some cases it is not
clear where to look for the version string) but does try a number of different 
techniques before giving up.

The answer is cached such that a subsequent call will return the
precalculated answer without having to look for it. It is assumed that
a version will not change whilst this program is running!

By default no functions are imported into the users namespace unless
the function is requested explicitly. The C<:Funcs> tag can be
specified to import all functions:

 use Starlink::Versions qw/ :Funcs /;

=cut

use strict;
use base qw/ Exporter /;
use vars qw/ $VERSION @EXPORT_OK $DEBUG %EXPORT_TAGS/;

use Symbol;             # For lexical file handles
use Starlink::Config;   # For location of root starlink dir
use File::Spec;         # For catfile()

@EXPORT_OK = qw/ 
  starversion starversion_string starversion_minor
  starversion_major starversion_patchlevel
  /;

%EXPORT_TAGS = (
		'Funcs' => [ @EXPORT_OK ],
		);

$VERSION = '1.00';
$DEBUG = 0;

# This is the cache used to store the version numbers 
# so that we can be fast with repeat calls

# Is a hash with lower-case keys for each Starlink application.
# Each of the elements contains a hash with keys
#  STRING, MAJOR, MINOR, PATCHLEVEL

my %CACHE;


=head1 METHOD

The following methods are used to determine version number.
In this example, it assumes that the caller is requesting the
version number for an application called "prog".

=over 4

=item 1

Determine the installed directory by looking in the directory pointed
to by the environment variable C<PROG_DIR> for a file called 
C<version.dat>. Some applications such as POLPACK or KAPPA write 
version numbers in this file. This allows the version number to be determined
directly from the C<PROG_DIR> rather than trying to determine it from
related directories (which relies on the application being properly
installed).

=item 2

If the previous method does not work look in directory

  $PROG_DIR/../../dates/

for a file call C<prog_datestamp>. These files are written during
a standard Starlink install and contain package and version information.
The file is searched for a version string.

=item 3

If no environment variable C<PROG_DIR> can be found (or the directory
does not exist), query C<Starlink::Config> for the location of the
standard Starlink directory and look in file

  $STARLINK/dates/prog_datestamp

for a version string. The Starlink directory will not be searched
if a C<PROG_DIR> is found since there is no guarantee that we are running 
an application from the standard Starlink tree (e.g. C</star> might
contain a version of C<PROG> but we are using a version in a different
tree).

=back

If these methds fail undefined values are returned. Note that
this module will not look explicitly in a Starlink install tree
unless it can not work out a directory tree to search as an alternative.

Finally, applications that do not have an application directory
or datestamp file can not have their version determined with this module.

=head2 Special Cases

In some cases the environment variable used to define an application
directory can not be derived directly from the application name
itself. Additionally, some date-stamp files use mixed case.

The following cases are treated specially by the module:

=over 4

=item B<Figaro>

Figaro uses C<FIG_DIR> rather than C<FIGARO_DIR>.

=item B<StarX>

The starx date-stamp file is called C<starX_datestamp>.

=item B<hdstrace>

Hdstrace has an application directory that is not in the
standard location of C</star/bin/app/>. Rather it uses
C</star/bin>. In cases where the application dir ends in
C<bin> the location of the datestamp directory is assumed
to be C<../dates> rather than C<../../dates>.

=back

=head1 FUNCTIONS

The following functions are available:

=head2 starversion

Given a Starlink application name returns the major, minor and
patchlevel version numbers.

  ($major, $minor, $patchlevel) = starversion( 'prog' );

Starting with Perl version 5.6.0, a perl version string is returned
when C<starversion> is called in a scalar context. This allows 
versions to be compared directly using Perl.

  $version = starversion( 'prog' );
  print "yes" if $version gt v0.15.2;

Returns undef if a version number can not be determined.

=cut

sub starversion {
  my %version = _get_version( $_[0] ) or return undef;
  if (wantarray) {
    return ( $version{MAJOR}, $version{MINOR}, $version{PATCHLEVEL} );
  } else {
    return $version{VERSION};
  }
}

=head2 starversion_string

Returns the version string rather than the individual components.

  $string = starversion_string( 'prog' );

The string will be of the form C<Vm.n-p>. C<undef> is returned
if a version can not be determined.

=cut

sub starversion_string {
  my %version = _get_version( $_[0] ) or return undef;
  return $version{ STRING };
}


=head2 starversion_major

Returns the major version number. Returns C<undef> if a version
can not be determined.

 $major = starversion_major( 'prog' );

=cut

sub starversion_major {
  my %version = _get_version( $_[0] ) or return undef;
  return $version{ MAJOR };
}

=head2 starversion_minor

Returns the minor version number. Returns C<undef> if a version
can not be determined.

 $major = starversion_minor( 'prog' );

=cut

sub starversion_minor {
  my %version = _get_version( $_[0] ) or return undef;
  return $version{ MINOR };
}

=head2 starversion_patchlevel

Returns the patchlevel number. Returns C<undef> if a version
can not be determined.

 $patch = starversion_patchlevel( 'prog' );

=cut

sub starversion_patchlevel {
  my %version = _get_version( $_[0] ) or return undef;
  return $version{ PATCHLEVEL };
}


=begin __PRIVATE__

=head1 INTERNAL FUNCTIONS

This section describes the internal functions. They are 
not part of the published interface.

=head2 _get_app_dir

Given a Starlink application name, returns the directory
where the application resides. 

  $dir = _get_app_dir( 'prog' );

This is simply derived from the C<PROG_DIR> environment
variable. C<undef> is returned if the directory could not be
determined (usually because the environment variable was not set).

Special cases 'figaro' since the Figaro environment variable
is C<FIG_DIR> rather than C<FIGARO_DIR>.

=cut

sub _get_app_dir ($) {
  # Construct the environment variable name
  my $app = uc(shift);
  $app = 'FIG' if $app eq "FIGARO";
  my $env = $app . '_DIR';
  return ( exists $ENV{$env} ? $ENV{$env} : undef);
}

=head2 _get_app_datestamp_dir

Return the location of the date stamp directory, given a Starlink
application name.

 $dir = _get_app_datestamp_dir( 'prog' );

This is simply C<$PROG_DIR/../../dates>. Returns C<undef> if
C<_get_app_dir> returns undef. Does not check that the directory
exists.

If the supplied application directory ends in C<bin> it is assumed
that the dates directory is in C<$PROG_DIR/../dates>. This is the
case for Hdstrace.

=cut

sub _get_app_datestamp_dir ($) {
  my $appdir = _get_app_dir( $_[0] );
  if (defined $appdir) {
    # special case bin dirs
    if ($appdir =~ /bin$/) {
      return File::Spec->catdir($appdir,
				File::Spec->updir, 'dates');
    } else {
      return File::Spec->catdir($appdir, File::Spec->updir, 
				File::Spec->updir, 'dates');
    }
  }
  return undef;
}

=head2 _get_standard_datestamp_dir

Return the standard location of the date stamp directory.
This will use the location of the actual Starlink system (as 
supplied by C<Starlink::Config>) rather than the location
of the application that is currently specified by C<PROG_DIR>.

 $dir = _get_standard_datestamp_dir;

This should be used as a last resort. Usually this would 
be C</star/dates>.

=cut

sub _get_standard_datestamp_dir () {
  return File::Spec->catdir( $StarConfig{'Star'} , "dates" );
}


=head2 _get_datestamp_file

Return the datestamp file associated with the supplied application
and directory.

 $file = _get_datestamp_file( $dir, 'prog' );

The directory is usually obtained via the C<_get_standard_datestamp_dir>
or C<_get_app_datestamp_dir> routines.

Special cases 'starx' since the associated date-stamp file
is called C<starX_datestamp>.

Does not check to see if the file exists.

=cut

sub _get_datestamp_file ($$) {
  my $app = lc($_[1]);
  $app = 'starX' if $app eq 'starx';
  return File::Spec->catfile($_[0], $app.'_datestamp');
}


=head2 _parse_version_string

Given a string of the form C<Vm.n-p> (or C<Vm.n.p>) return the
major version, minor version and patchlevel.

  ($major, $minor, $patchlevel) = _parse_version_string( $string );

A zero is returned for patchlevel if the patchlevel is omitted from
the version string.

If neither a string of the form C<Vm.n-p> or C<Vm.n> can be found,
the routine tries to get version information from a string of the
form C<m.n-p> (in some cases the "V" is missing).

Returns C<undef> for all values if the string does not match the
supplied pattern.

=cut

sub _parse_version_string ($) {
  print "CHECKING STRING: $_[0]" if $DEBUG;
  if ($_[0] =~ /V(\d+)\.(\d+)[-\.](\d+)/ ) {
    return ($1, $2, $3);
  } elsif ($_[0] =~ /V(\d+)\.(\d+)/) {
    return ($1, $2, 0);
  } elsif ($_[0] =~ /(\d+)\.(\d+)-(\d+)/ ) {
    return ($1, $2, $3);
  } else {
    return (undef, undef, undef);
  }
}

=head2 _read_datestamp_file

Given a datestamp file, return the major, minor and pathlevel
numbers from it.

 ($major, $minor, $patchlevel) = _read_datestamp_file( $file );

Relies on this file containing a line starting with the string
"C<Version>" and containing a version string that can be parsed
by C<_parse_version_string>.

Returns C<undef> on error.

=cut

sub _read_datestamp_file ($) {
  my $file = shift;
  print "Opening datestamp file $file\n" if $DEBUG;
  my $sym = gensym;
  open( $sym, "< $file" ) || return (undef, undef, undef);

  while (<$sym>) {
    if (/^Version/i) {
      print "FOUND in datestamp: $_\n" if $DEBUG;
      my (@version) = _parse_version_string($_);
      # Return immediately, the file will be closed automatically
      # but close it anyway for clarity
      if (defined $version[0]) {
	close($sym);
	return (@version);
      }
    }
  }
  return (undef, undef, undef);
}


=head2 _get_version_from_datestamp

Retrieve a version number from a datestamp file.

 ($major, $minor, $patchlevel) = 
        _get_version_from_datestamp($useenv, 'prog');

The first argument is a flag describing which datestamp
file to use. If this flag is true, the datestamp file
is derived from the PROG_DIR environment variable. If it
is false the datestamp file is derived from the installed
Starlink directory.

Returns C<undef> on error.

=cut

sub _get_version_from_datestamp ($$) {
  my $dir;
  if ($_[0]) {
    $dir = _get_app_datestamp_dir($_[1]);
  } else {
    $dir = _get_standard_datestamp_dir;
  }
  # Get the filename
  print "Using datestamp dir = $dir\n" if $DEBUG;
  my $file = _get_datestamp_file($dir, $_[1]);

  # Return the version number
  return &_read_datestamp_file($file);
}

=head2 _get_version_from_appdir

Attempt to retrieve a version number from the C<version.dat>
file in the application directory.

 ($major, $minor, $patchlevel) = _get_version_from_appdir( 'prog' );

Returns C<undef> if the version number could not be determined.

=cut

sub _get_version_from_appdir ($) {
  # Determine the directory to use
  my $dir = _get_app_dir ( $_[0] ) or return undef;

  # Construct the filename
  print "Reading version info from app dir version.dat\n" if $DEBUG;
  my $file = File::Spec->catfile($dir, 'version.dat');

  # open the file
  my $fh = gensym;
  open( $fh, "< $file") or return undef;

  # Read the first line and parse it
  my @version = _parse_version_string( <$fh> );

  # Tidy up
  close($fh);

  # return
  return (@version);
}

=head2 _get_version

Retrieve the version number using a variety of techniques.

 %version = _get_version( 'prog' );

The answer is cached.  If the version has already been determined for
this application, the answer is returned directly.

The returned hash contains the following keys:

 MAJOR =>  major version number
 MINOR =>  minor version number
 PATCHLEVEL => patchlevel
 STRING => stringified version in form "Vm.n-p"
 VERSION => perl 5.6.0 version number vm.n.p

The "VERSION" key is only set for perl versions 5.6.0 and newer.
An empty list is returned if version can not be determined.

=cut

sub _get_version ($) {
  my $app = shift;
  $app = lc($app);

  # Check to see if the hash is used and return immediately if it is
  return %{ $CACHE{ $app } } if exists $CACHE{ $app };

  my @version;
  # see if we have a PROG_DIR
  if (defined _get_app_dir( $app ) ) {
    @version = _get_version_from_appdir( $app );

    # Read from datestamp file if could not get from appdir
    @version = _get_version_from_datestamp( 1, $app )
      unless defined $version[0];

  } else {
    # Okay, no PROG_DIR defined so look in /star (or wherever)
    @version = _get_version_from_datestamp( 0, $app );
  }

  # If we have something we need to cache, cache it
  if (defined $version[0]) {
    $CACHE{$app} = {
		    MAJOR => $version[0],
		    MINOR => $version[1],
		    PATCHLEVEL => $version[2],
		    STRING => "V$version[0].$version[1]-$version[2]",
		   };
    if ($] >= 5.006) {
      # Create a perl-style version 'string' if perl 5.6.0 or newer
      $CACHE{$app}{VERSION} = eval "v$version[0].$version[1].$version[2]";
    }
  }

  # Return the version hash if we have a version
  # else we dont want to add the key so return an empty list
  return (defined $version[0] ? %{ $CACHE{ $app } } : () );
}


=end __PRIVATE__

=head1 NOTES

Can be used to determine any Starlink product that installs a datestamp
file. For example, it can be used to determine library versions as well
as applications.

=head1 AUTHOR

Tim Jenness E<lt>t.jenness@jach.hawaii.eduE<gt>

=head1 COPYRIGHT

Copyright (C) 2000 Particle Physics and Astronomy Research Council.
All Rights Reserved.

=cut

1;

