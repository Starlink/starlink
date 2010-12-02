package Starlink::Versions;

=head1 NAME

Starlink::Versions - determine version numbers of Starlink applications

=head1 SYNOPSIS

 use Starlink::Versions qw/ starversion /;

 ($major, $minor, $patchlevel) = starversion('kappa');

 $verstring = starversion_string('kappa');

 print "yes" if starversion_gt('kappa', 'V0.15-2';

 if (starversion('surf') gt v1.5.2) {
   ...
 }

 # Print global version number
 print starversion( 'starlink' );
 print scalar starversion_global();

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

Functions are also provided to simplify comparisons.

=cut

use strict;
use base qw/ Exporter /;
use vars qw/ $VERSION @EXPORT_OK $DEBUG %EXPORT_TAGS/;

use DateTime::Format::Strptime;       # To parse ISO dates
use Symbol;                           # For lexical file handles
use Starlink::Config qw/ :override /; # For location of root starlink dir
use File::Spec;                       # For catfile()
use version;

@EXPORT_OK = qw/
                 starversion starversion_string starversion_minor
                 starversion_major starversion_patchlevel starversion_global
                 starversion_cmp starversion_eq starversion_gt starversion_lt
                 starversion_ge starversion_le starversion_vstring
               /;

%EXPORT_TAGS = (
                'Funcs' => [ @EXPORT_OK ],
               );

$VERSION = '1.06';
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

  $PROG_DIR/../../manifests

and parse the manifest files for a version number.

=item 3

If the previous method does not work look in directory

  $PROG_DIR/../../dates/

for a file call C<prog_datestamp>. These files are written during
a standard Starlink install and contain package and version information.
The file is searched for a version string. This will only work for old
Starlink installations.

=item 4

If no environment variable C<PROG_DIR> can be found (or the directory
does not exist), query C<Starlink::Config> for the location of the
standard Starlink directory and look in file

  $STARLINK/manifests/prog

or

  $STARLINK/dates/prog_datestamp

for a version string. The Starlink directory will not be searched
if a C<PROG_DIR> is found since there is no guarantee that we are running
an application from the standard Starlink tree (e.g. C</star> might
contain a version of C<PROG> but we are using a version in a different
tree).

=back

If these methods fail undefined values are returned. Note that
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

The starx date-stamp file is called C<starX_datestamp>. This is not
an issue for manifest-based installations.

=item B<hdstrace>

Hdstrace has an application directory that is not in the
standard location of C</star/bin/app/>. Rather it uses
C</star/bin>. In cases where the application dir ends in
C<bin> the location of the manifests or datestamp directory is assumed
to be C<../manifests> rather than C<../../manifests>.

=item B<starlink>

This is a special case referring to the global installation version
number found in $STARLINK/manifests/starlink.version. For the global
version it is not possible to obtain major, minor and patchlevel components,
only a single string.

=back

=head1 FUNCTIONS

The following functions are available:

=over 4

=item B<starversion>

Given a Starlink application name returns the major, minor and
patchlevel version numbers.

  ($major, $minor, $patchlevel) = starversion( 'prog' );

In scalar context a perl "version" object is returned. This allows
versions to be compared directly using Perl.

  $version = starversion( 'prog' );
  print "yes" if $version gt version->new("0.15.2");

Returns undef if a version number can not be determined.

=cut

sub starversion {
  my %version = _get_version( $_[0] ) or return undef;
  if (wantarray) {
    return ( $version{MAJOR}, $version{MINOR}, $version{PATCHLEVEL} );
  } else {
    return $version{OBJECT};
  }
}

=item B<starversion_global>

Returns the global Starlink version for the system.

In list context returns the string representation, the
commit ID and the date of that commit.

 ($string, $commit, $commitdate) = starversion_global();

In scalar context returns just the string representation:

  $string = starversion_global();

=cut

sub starversion_global {
  my %version = _get_version( "starlink" ) or return();
  if (wantarray) {
    return ($version{STRING}, $version{COMMIT}, $version{COMMITDATE});
  } else {
    return $version{STRING};
  }
}

=item B<starversion_vstring>

Returns the version as a perl v-string. This approach is deprecated.
It is recommended that perl version objects are used instead (see
C<starversion> function.

  $vstring = starversion_vstring( 'prog' );
  print "yes" if $version gt v0.15.2;

Returns undef if no version can be determined or if your perl is older
than v5.6.0.

=cut

sub starversion_vstring {
  my %version = _get_version( $_[0] ) or return undef;
  return $version{ VERSION };
}

=item B<starversion_string>

Returns the version string rather than the individual components.

  $string = starversion_string( 'prog' );

The string will be of the form C<Vm.n-p>. C<undef> is returned
if a version can not be determined.

=cut

sub starversion_string {
  my %version = _get_version( $_[0] ) or return undef;
  return $version{ STRING };
}


=item B<starversion_major>

Returns the major version number. Returns C<undef> if a version
can not be determined.

 $major = starversion_major( 'prog' );

=cut

sub starversion_major {
  my %version = _get_version( $_[0] ) or return undef;
  return $version{ MAJOR };
}

=item B<starversion_minor>

Returns the minor version number. Returns C<undef> if a version
can not be determined.

 $major = starversion_minor( 'prog' );

=cut

sub starversion_minor {
  my %version = _get_version( $_[0] ) or return undef;
  return $version{ MINOR };
}

=item B<starversion_patchlevel>

Returns the patchlevel number. Returns C<undef> if a version
can not be determined.

 $patch = starversion_patchlevel( 'prog' );

=cut

sub starversion_patchlevel {
  my %version = _get_version( $_[0] ) or return undef;
  return $version{ PATCHLEVEL };
}

=item B<starversion_cmp>

Can be used to compare the version number of a package with a supplied
version number. Returns -1 if the supplied version is greater than
that of the installed package (the installed package is older), 0 if
it is the same, and 1 if it is less than that of the package (the installed
package is newer).

  $cmp = starversion_cmp('prog','V0.15-3');

The version string format should be one of:

  V1.2-3
  V1.2.3
  1.2-3

C<undef> is returned if a comparison could not be made due to
either the application having no version number or if the
supplied version string could not be parsed.

In modern perls version objects (as returned by C<starversion>)
can be used to compare version information directly.

In perl 5.6.0 this affect can be achieved directly using
the C<starversion_vstring> command in a scalar context with a string literal
(but not with a standard Starlink version string):

  $cmp = starversion_vstring('prog') cmp v0.15.4;

This latter approach is not recommended.

=cut

sub starversion_cmp ($$) {
  my %version = _get_version( $_[0] ) or return undef;
  my ($cmaj, $cmin, $cpatch) = _parse_version_string($_[1]);
  return undef unless defined $cmaj;

  # Essentially a switch

  # Compare major version
  return -1 if $cmaj > $version{MAJOR};
  return 1 if $cmaj < $version{MAJOR};

  # Compare minor version
  return -1 if $cmin > $version{MINOR};
  return 1 if $cmin < $version{MINOR};

  # Compare patch level
  return -1 if $cpatch > $version{PATCHLEVEL};
  return 1 if $cpatch < $version{PATCHLEVEL};

  # Must be identical
  return 0;
}

=item B<starversion_lt>

Test whether the version of the installed package is less than
a supplied version number. In other words, whether the installed
package is older than the requested version.

  if ( starversion_lt('kappa', '0.15-2' ) {
    ...
  }

The version string format is described in the description of
C<starversion_cmp>

In perl 5.6.0 this command can be implemented directly using a
string literal:

  if ( starversion('prog') lt v0.15.2 ) {
    ...
  }

but this has been deprecated for v5.8.

=cut

sub starversion_lt ($$) {
  if (starversion_cmp($_[0],$_[1]) == -1) {
    return 1;
  }
  return 0;
}

=item B<starversion_le>

Test whether the version of the installed package is less than
or equal to a supplied version number. In other words, whether the
installed package is older than or is the requested version.

  if ( starversion_le('kappa', '0.15-2' ) {
    ...
  }

The version string format is described in the description of
C<starversion_cmp>

In perl 5.6.0 this command can be implemented directing using a
string literal:

  if ( starversion('prog') le v0.15.2 ) {
    ...
  }

but this has been deprecated for v5.8.

=cut

sub starversion_le ($$) {
  if ( starversion_cmp($_[0],$_[1]) <= 0 ) {
    return 1;
  }
  return 0;
}

=item B<starversion_eq>

Test whether the version of the installed package is equal to
a supplied version number. In other words, whether the installed
package is older the same version as that specified.

  if ( starversion_eq('kappa', '0.15-2' ) {
    ...
  }

The version string format is described in the description of
C<starversion_cmp>

In perl 5.6.0 this command can be implemented directly using a
string literal:

  if ( starversion('prog') eq v0.15.2 ) {
    ...
  }

but this has been deprecated for v5.8.

=cut

sub starversion_eq ($$) {
  if (starversion_cmp($_[0],$_[1]) == 0) {
    return 1;
  }
  return 0;
}

=item B<starversion_gt>

Test whether the version of the installed package is greater than
a supplied version number. In other words, whether the installed
package is newer than the requested version.

  if ( starversion_gt('kappa', '0.15-2' ) {
    ...
  }

The version string format is described in the description of
C<starversion_cmp>

In perl 5.6.0 this command can be implemented directly using a
string literal:

  if ( starversion('prog') gt v0.15.2 ) {
    ...
  }

but this has been deprecated for v5.8.

=cut

sub starversion_gt ($$) {
  if (starversion_cmp($_[0],$_[1]) == 1) {
    return 1;
  }
  return 0;
}

=item B<starversion_ge>

Test whether the version of the installed package is greater than
or equal to a supplied version number. In other words, whether the
installed package is newer than or is the requested version.

  if ( starversion_ge('kappa', '0.15-2' ) {
    ...
  }

The version string format is described in the description of
C<starversion_cmp>

In perl 5.6.0 this command can be implemented directly using a
string literal:

  if ( starversion('prog') ge v0.15.2 ) {
    ...
  }

but this has been deprecated for v5.8.

=cut

sub starversion_ge ($$) {
  if ( starversion_cmp($_[0],$_[1]) >= 0 ) {
    return 1;
  }
  return 0;
}

=back

=begin __PRIVATE__

=head1 INTERNAL FUNCTIONS

This section describes the internal functions. They are
not part of the published interface.

=over 4

=item B<_get_app_dir>

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

=item B<_get_app_datestamp_dir>

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

=item B<_get_app_manifest_dir>

Return the location of the manifests directory, given a Starlink
application name.

 $dir = _get_app_manifest_dir( 'prog' );

This is simply assumed to be C<$PROG_DIR/../../manifests>. Returns
C<undef> if C<_get_app_dir> returns undef. Does not check that the
directory exists.

If the supplied application directory ends in C<bin> it is assumed
that the dates directory is in C<$PROG_DIR/../manifest>. This is the
case for hdstrace.

=cut

sub _get_app_manifest_dir ($) {
  my $appdir = _get_app_dir( $_[0] );
  if (defined $appdir) {
    # special case bin dirs
    if ($appdir =~ /bin$/) {
      return File::Spec->catdir($appdir,
                                File::Spec->updir, 'manifests');
    } else {
      return File::Spec->catdir($appdir, File::Spec->updir,
                                File::Spec->updir, 'manifests');
    }
  }
  return undef;
}

=item B<_get_standard_datestamp_dir>

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

=item B<_get_standard_manifest_dir>

Return the standard location of the manifest directory.
This will use the location of the actual Starlink system (as
supplied by C<Starlink::Config>) rather than the location
of the application that is currently specified by C<PROG_DIR>.

 $dir = _get_standard_manifest_dir;

This should be used as a last resort. Usually this would
be C</star/manifests>.

=cut

sub _get_standard_manifest_dir () {
  return File::Spec->catdir( $StarConfig{'Star'} , "manifests" );
}


=item B<_get_datestamp_file>

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

=item B<_get_manifest_file>

Return the manifest file associated with the supplied application
and directory.

 $file = _get_manifest_file( $dir, 'prog' );

The directory is usually obtained via the C<_get_standard_manifest_dir>
or C<_get_app_manifest_dir> routines.

Does not check to see if the file exists.

=cut

sub _get_manifest_file ($$) {
  my $app = lc($_[1]);
  return File::Spec->catfile($_[0], $app);
}

=item B<_get_git_version>

Given a Starlink version file created for git repositories, return
version information.

  my \%version = _get_git_version( File => $file );

Returns a hash reference with keys STRING, COMMIT, and
COMMITDATE. String is composed of the branch name, commit ID, and
date. COMMIT is the commit ID. COMMITDATE is the date of the commit,
returned as a DateTime object;

The file must have the branch name as the first line. The second line,
if it exists, must be the commit ID. The third line, if it exists,
must be the date in 'YYYY-MM-DD HH:MM:SS TZ' format.

There are two acceptable named arguments. File is a filename, Data is
an array of values. File overrides Data.

=cut

sub _get_git_version {
  my %opts = @_;

  my( $branch, $id, $date );

  if( defined( $opts{'File'} ) ) {

    my $file = $opts{'File'};
    print "Opening git version file $file\n" if $DEBUG;

    open my $fh, "<", $file or return;

    $branch = <$fh>;
    chomp($branch);
    $id = <$fh>;
    $date = <$fh>;
    close $fh;

  } elsif( defined( $opts{'Data'} ) ) {
    $branch = $opts{'Data'}[0];
    $id = $opts{'Data'}[1];
    $date = $opts{'Data'}[2];
  }

  my %info;
  $info{STRING} = $branch;
  if (defined $id) {
    chomp($id);
    $info{STRING} .= " @ $id";
    $info{COMMIT} = $id;
  }
  if (defined $date) {
    chomp($date);
    # Git ISO is YYYY-MM-DD HH:MM:SS -1000
    my $p = DateTime::Format::Strptime->new(pattern => '%Y-%m-%d %H:%M:%S %z',
                                            time_zone=>'UTC');
    my $dt = $p->parse_datetime($date);
    if (defined $dt) {
      $info{COMMITDATE} = $dt;
      $info{STRING} .= " (".$dt->datetime.")";
    } else {
      $info{STRING} .= " (unknown date)";
    }
  }

  return \%info;

}

=item B<_parse_version_string>

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
  if ($_[0] =~ /[Vv](\d+)\.(\d+)[-\.](\d+)/ ) {
    return ($1, $2, $3);
  } elsif ($_[0] =~ /[Vv](\d+)\.(\d+)/) {
    return ($1, $2, 0);
  } elsif ($_[0] =~ /(\d+)\.(\d+)[-\.](\d+)/ ) {
    return ($1, $2, $3);
  } elsif ($_[0] =~ /(\d+)\.(\d+)/ ) {
    return ($1, $2, 0);
  } else {
    return (undef, undef, undef);
  }
}

=item B<_read_datestamp_file>

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
  # use gensym for perl5.005 compatibility
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

=item B<_read_manifest_file>

Given a manifest file, return the major, minor and pathlevel
numbers from it.

 ($major, $minor, $patchlevel) = _read_manifest_file( $file );

Relies on this file containing a version string that can be parsed
by C<_parse_version_string> in a E<lt>versionE<gt> element in the XML.

Returns C<undef> on error.

=cut

sub _read_manifest_file ($) {
  my $file = shift;
  print "Opening manifest file $file\n" if $DEBUG;
  my $sym = gensym;
  open( $sym, "< $file" ) || return (undef, undef, undef);

  while (<$sym>) {
    if (/^<version>/i) {
      print "FOUND in manifest: $_\n" if $DEBUG;
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


=item B<_get_version_from_datestamp>

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

=item B<_get_version_from_manifest>

Retrieve a version number from a manifest file.

 ($major, $minor, $patchlevel) =
        _get_version_from_manifest($useenv, 'prog');

The first argument is a flag describing which datestamp
file to use. If this flag is true, the manifest file
is derived from the PROG_DIR environment variable. If it
is false the manifest file is derived from the installed
Starlink directory.

Returns C<undef> on error.

=cut

sub _get_version_from_manifest ($$) {
  my $dir;
  if ($_[0]) {
    $dir = _get_app_manifest_dir($_[1]);
  } else {
    $dir = _get_standard_manifest_dir;
  }
  # Get the filename
  print "Using manifest dir = $dir\n" if $DEBUG;
  my $file = _get_manifest_file($dir, $_[1]);

  # Return the version number
  return &_read_manifest_file($file);
}

=item B<_get_version_from_appdir>

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

=item B<_get_global_version>

Used to obtain the global version number from the manifests directory.

  $version = _get_global_version();

Return undef if nothing can be found.

=cut

sub _get_global_version {
  my $dir = _get_standard_manifest_dir;
  # Get the filename
  print "Using manifest dir = $dir\n" if $DEBUG;
  my $file = _get_manifest_file($dir, 'starlink.version');

  return _get_git_version( File => $file );
}

=item B<_get_version>

Retrieve the version number using a variety of techniques.

 %version = _get_version( 'prog' );

The answer is cached.  If the version has already been determined for
this application, the answer is returned directly.

The returned hash contains the following keys:

 MAJOR =>  major version number
 MINOR =>  minor version number
 PATCHLEVEL => patchlevel
 STRING => stringified version in form "Vm.n-p"
 OBJECT => version object
 VERSION => perl 5.6.0 version number vm.n.p

The "VERSION" key is only set for perl versions 5.6.0 and newer.
An empty list is returned if version can not be determined.

If the request is for a Starlink version the string will not be
of form "Vm.n-p", but will report the branch name, commit id and
commit date.

 COMMIT => commit identifier (SVN revision number or SHA1 git id)
 COMMITDATE => date of commit (DateTime object)

=cut

sub _get_version ($) {
  my $app = shift;
  $app = lc($app);

  # Check to see if the hash is used and return immediately if it is
  return %{ $CACHE{ $app } } if exists $CACHE{ $app };

  # Special case 'starlink'
  if ($app eq 'starlink' || $app eq 'starlink.version') {
    my $global = _get_global_version();
    if (defined $global) {
      $CACHE{$app} = {
                      MAJOR => undef,
                      MINOR => undef,
                      PATCHLEVEL => undef,
                      VERSION => undef,
                      OBJECT => undef,
                      STRING => $global->{STRING},
                      COMMIT => $global->{COMMIT},
                      COMMITDATE => $global->{COMMITDATE},
                     };
    }
    return (defined $global ? %{ $CACHE{$app} } : () );
  }

  my @version;
  # see if we have a PROG_DIR
  if (defined _get_app_dir( $app ) ) {
    @version = _get_version_from_appdir( $app );

    # Now try a manifest file
    @version = _get_version_from_manifest( 1, $app )
      unless defined $version[0];

    # Read from datestamp file if could not get from appdir
    @version = _get_version_from_datestamp( 1, $app )
      unless defined $version[0];

  }

  # now try normal manifest location
  @version = _get_version_from_manifest( 0, $app )
    unless defined $version[0];

  # Okay, no PROG_DIR defined so look in /star (or wherever)
  @version = _get_version_from_datestamp( 0, $app )
    unless defined $version[0];

  # If we have something we need to cache, cache it
  if (defined $version[0]) {
    $CACHE{$app} = {
                    MAJOR => $version[0],
                    MINOR => $version[1],
                    PATCHLEVEL => $version[2],
                    STRING => "V$version[0].$version[1]-$version[2]",
                    OBJECT => version->new(join(".",@version[0..2])),
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


=back

=end __PRIVATE__

=head1 NOTES

Can be used to determine any Starlink product that installs a
datestamp file or a manifest file. For example, it can be used to
determine library versions as well as applications.

=head1 AUTHORS

Tim Jenness E<lt>t.jenness@jach.hawaii.eduE<gt>
Brad Cavanagh E<lt>b.cavanagh@jach.hawaii.eduE<gt>

=head1 COPYRIGHT

Copyright (C) 2000-2005 Particle Physics and Astronomy Research Council.
Copyright (C) 2007, 2009 Science and Technology Facilities Council.
All Rights Reserved.

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

