#!perl

use strict;
use Test::More tests => 8;
use Storable qw/ dclone /;
use Data::Dumper;

require_ok( "Starlink::AST" );

my $specframe = new Starlink::AST::SpecFrame('');
isa_ok( $specframe, "Starlink::AST::SpecFrame" );

my $specframeclone = dclone( $specframe );
isa_ok( $specframeclone, "Starlink::AST::SpecFrame" );

is( $specframeclone->GetC("Title"), 'Wavelength', "Default Title attribute of SpecFrame");
is( $specframeclone->GetC("System"), 'WAVE', "Default System attribute of SpecFrame");
is( $specframeclone->GetC("Unit"), 'Angstrom', "Default Unit attribute of SpecFrame");
is( $specframeclone->GetC("Label"), 'Wavelength', "Default Label attribute of SpecFrame");
is( $specframeclone->GetD("RestFreq"), 100000, "Default RestFreq attribute of SpecFrame");
