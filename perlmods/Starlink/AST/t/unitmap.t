#!perl

use strict;
use Test::More tests => 28;

require_ok("Starlink::AST");
{
Starlink::AST::Begin();

# Unit map

my $unitmap = new Starlink::AST::UnitMap(2, "");
isa_ok($unitmap, "AstUnitMapPtr");

$unitmap->Show();

ok(! $unitmap->Test( "Report" ), "Report attrib not defined yet");

$unitmap->Set("Report=1");

ok($unitmap->Test( "Report" ), "Report attrib defined");

$unitmap->Clear( "Report" );
ok(! $unitmap->Test( "Report" ), "Report attrib cleared");
$unitmap->SetI("Report", 1);

my $invert = $unitmap->GetI("Invert");
$unitmap->Invert();
ok( $invert != $unitmap->GetI("Invert"), "Inverted mapping");
$unitmap->Invert();
is($unitmap->GetI("Invert"), $invert, "And inverted again");

# Test mapping - Sun211 - Transforming Coordinates
my @xin = ( 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0 );
my @yin = ( 0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 18.0 );

my ($xout, $yout ) = $unitmap->Tran2( \@xin, \@yin, 1 );

is($yout->[9], 18 , "10th element of zoommap");

# and revers the mapping
my ($xin2, $yin2 ) = $unitmap->Tran2( $xout, $yout, 0 );

for my $i ( 0 .. $#xin ) {
  is( $xin2->[$i], $xin[$i], "Compare inverted zoommap X[$i]");
  is( $yin2->[$i], $yin[$i], "Compare inverted zoommap X[$i]");
}

}
