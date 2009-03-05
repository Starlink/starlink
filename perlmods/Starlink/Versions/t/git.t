#!perl

# Fake a git starlink.version file and test the version reading.

use strict;
use Test::More tests => 3;
use File::Spec;
use File::Path qw/ mkpath rmtree /;

BEGIN {
  use_ok( "Starlink::Versions" );
  Starlink::Versions->import( ":Funcs" );
}

my $vers_file = "data/starlink.version";

my $info = Starlink::Versions::_get_git_version( 't/data/starlink.version' );

is( $info->{'COMMIT'}, "3a94952fab707beb4b1484afe3bb9e476eb07e14" );
is( $info->{'COMMITDATE'}->hms, '19:14:48' );
