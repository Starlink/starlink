use ExtUtils::MakeMaker;

$| =1;

# Calculate the starlink configuration from enviroment
# variables.
# The rule is to look for $STARLINK as the base
# and then derive everything from that. If $STARLINK
# is not defined use $INSTALL. If that is not defined
# use /star as a guess

my $pkg = "Starlink::Config";
my $base;
if (exists $ENV{STARLINK}) {
  $base = $ENV{STARLINK};
  print "$pkg - Reading Starlink location from \$STARLINK\n";
} elsif (exists $ENV{INSTALL}) {
  $base = $ENV{INSTALL};
  print "$pkg - Reading Starlink location from \$INSTALL\n";
} else {
  $base = '/star';
  print "$pkg - Guessing Starlink location to be /star\n";
}

# Once we have decoded $STARLINK check for the existence
# of the directory. Abort if not there

if (-d $base) {
  print "$pkg - Starlink system found at: $base\n";
} else {
  die "Could not find Starlink system at $base\n";
}

# unlink the .pm file in case it is already there
unlink 'Config.pm';

# Now open the pm file
open(PM, ">Config.pm") or die "Could not write Config.pm file";

print "$pkg - Writing Config.pm\n";

# and write the contents
while (defined($line = <DATA>)) {
  print PM $line;
}
# Now print the DATA tag
print PM "__DATA__\n";

# Now generate the subdirectories
# Could use File::Spec to do this in future but wait until next release

# Dont like trailing slashes
$base =~ s,/$,,;

print PM qq|Star =>       $base
Star_Inc   => $base/include
Star_Lib   => $base/lib
Star_Bin   => $base/bin
Star_Share => $base/share
|;


# And close the file
close PM;


# Now Write the makefile
WriteMakefile(
    'NAME'      => 'Starlink::Config',
    'VERSION_FROM'   => 'Config.pm',
    'clean'     => { FILES => 'Config.pm' },
    'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => 'gz',
                    'ZIP'=>'/usr/bin/zip','ZIPFLAGS'=>'-rl'}
);

# This is the actual perl module!!!!
# it gets printed automatically when perl Makefile.PL
# is run
__DATA__
package Starlink::Config;

=head1 NAME

Starlink::Config - Retrieve local Starlink configuration

=head1 SYNOPSIS

  use Starlink::Config;

  $star = $StarConfig{'Star'};
  $inc  = $StarConfig{'Star_Inc'};
  $lib  = $StarConfig{'Star_Lib'};
  $shlibs = $StarConfig{'Star_Share'};

=head1 DESCRIPTION

This package contains the location of the Starlink software
on your system. It is intended to simplify the building
of perl modules based on Starlink libraries and allows
for this information to be placed in a single place
rather than in every single Starlink module.

When used, a hash is imported into the current namespace called
C<%StarConfig>. This hash contains the following keys:

=over 4

=item Star

The root directory of the Starlink software installation.
This usually has a value of F</star>.

=item Star_Bin

The location for Starlink binary files. Usually
in F</star/bin>.

=item Star_Inc

The location of the Starlink include files. Usually
in F</star/include>.

=item Star_Lib

The location of the Starlink static libraries. Usually
in F</star/lib>.

=item Star_Share

The location of Starlink shared libraries. Usually
in F</star/share>.

=back

=head1 IMPLEMENTATION

C<%StarConfig> is implemented as a tied hash primarily to
make sure that the hash is read-only. The module
is automatically generated at build time and reflects
the state of the environment variables at that moment.

=head1 AUTHOR

Tim Jenness (t.jenness@jach.hawaii.edu)

=cut


use strict;
use Exporter ();
use vars qw/ %StarConfig @ISA @EXPORT $VERSION/;
@ISA = qw(Exporter);
@EXPORT = qw(%StarConfig);

$VERSION = '1.00';

# This is populated from <DATA>
my (%localConfig);

# use a tied hash so that we can enforce read-only

sub TIEHASH { bless \%localConfig; }

sub FETCH {
  print $_[0];
  $_[0]->{$_[1]};
}

sub FIRSTKEY { (each %{$_[0]})[0]; }
sub NEXTKEY  { (each( %{$_[0]} ))[0]; }
sub EXISTS   { exists($_[0]->{$_[1]}); }

# Readonly hash
sub STORE  { die "\%Starlink::Config::StarConfig is read-only\n" }
sub DELETE { &STORE }
sub CLEAR  { &STORE }
sub DESTROY { }

# Read the data into a lexical variable
foreach (<DATA>) {
  my ($key, $value) = split('\s*=>\s*');
  $value =~ s/\'//g;
  $localConfig{$key} = $value if defined $key;
}

tie %StarConfig, 'Starlink::Config';

1;

# Store the keys and values after the __DATA__ tag
# Can then simply append the information during
# makefile.PL creation
# Should be of the form Key => Value
