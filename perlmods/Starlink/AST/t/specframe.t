#!perl

use strict;
use Test::More tests => 9;

require_ok("Starlink::AST");

my $specframe = new Starlink::AST::SpecFrame('');
isa_ok($specframe, "Starlink::AST::SpecFrame");

is( $specframe->GetC("Title"), 'Wavelength', "Default Title attribute of SpecFrame");
is( $specframe->GetC("System"), 'WAVE', "Default System attribute of SpecFrame");
is( $specframe->GetC("Unit"), 'Angstrom', "Default Unit attribute of SpecFrame");
is( $specframe->GetC("Label"), 'Wavelength', "Default Label attribute of SpecFrame");
is( $specframe->GetD("RestFreq"), 100000, "Default RestFreq attribute of SpecFrame");

$specframe->Set("Unit" => 'micron');
is( $specframe->GetC("Unit"), 'micron', "Unit attribute after setting units to microns");

$specframe->Set("System" => 'FREQ');
is( $specframe->GetC("Unit"), 'GHz', "Unit attribute after setting system to frequency");

