#!perl

use strict;
use Test::More tests => 22;

require_ok("Starlink::AST");
{
Starlink::AST::Begin();

# Test ZoomMap attribute set/get/clear

my $zoommap = new Starlink::AST::ZoomMap(2,5, "");
isa_ok($zoommap, "Starlink::AST::ZoomMap");

$zoommap->Show();

is($zoommap->GetI("Nout"), 2, "Nout attribute of zoommap");
is($zoommap->GetI("Nin"), 2, "Nin attribute of zoommap");
is($zoommap->GetI("Zoom"), 5, "Zoom attribute of zoommap [I]");
is($zoommap->GetD("Zoom"), 5.0, "Zoom attribute of zoommap [D]");

$zoommap->SetI("Zoom", 99);
is($zoommap->GetI("Zoom"), 99, "Test attribute of SetI");

ok(! $zoommap->Test( "Report" ), "Report attrib not defined yet");

$zoommap->Set("Zoom=99.6, Report=1");
is($zoommap->GetD("Zoom"), 99.6, "Test attribute of AST-style Set");

$zoommap->Set("Zoom" => 24,
              "Report" => 0,
             );
is( $zoommap->GetD("Zoom"), 24, "Test attribute of Perl-style Set");

ok($zoommap->Test( "Report" ), "Report attrib defined");

$zoommap->Clear( "Report" );
ok(! $zoommap->Test( "Report" ), "Report attrib cleared");
$zoommap->SetI("Report", 1);

$zoommap->SetC("Ident", "Test ident");
is( $zoommap->GetC("Ident"), "Test ident", "Ident attribute of zoommap");

my $pcdmap = new Starlink::AST::PcdMap( 1, [ 50.0, 50.0 ], '' );
$pcdmap->SetD("PcdCen(1)", 25.0);
is( $pcdmap->GetD("PcdCen(1)"), 25.0, "PcdCen attribute of pcdmap (axis 1)" );

is( $pcdmap->GetD("PcdCen(2)"), 50.0, "PcdCen attribute of pcdmap (axis 2)" );

$pcdmap->SetD("PcdCen", 100.0);
is( $pcdmap->GetD("PcdCen(1)"), 100.0, "PcdCen attribute of pcdmap after setting both axes at once (axis 1)");
is( $pcdmap->GetD("PcdCen(2)"), 100.0, "PcdCen attribute of pcdmap after setting both axes at once (axis 2)");

$pcdmap->Set("PcdCen(1)" => 25, "PcdCen(2)" => 50 );
my %attribs = $pcdmap->Get("PcdCen", "Disco");
is( $attribs{'Disco'}, 1, "Disco attribute of pcdmap through hash");
is( $attribs{'PcdCen'}, 25, "PcdCen(1) attribute of pcdmap through hash");

my $disco = $pcdmap->Get("Disco");
is( $disco, 1, "Disco attribute of pcdmap through scalar context");

my %attribs2 = $pcdmap->Get("PcdCen(1)", "PcdCen(2)");
is( $attribs2{'PcdCen(1)'}, 25, "PcdCen(1) attribute of pcdmap through hash");
is( $attribs2{'PcdCen(2)'}, 50, "PcdCen(2) attribute of pcdmap through hash");

}
