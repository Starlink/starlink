#!perl

use strict;
use Test::More tests => 11;

require_ok( "Starlink::AST" );

Starlink::AST::Begin();

use constant DD2R => 0.017453292519943295769236907684886127134428718885417;
use constant DAS2R => 4.8481368110953599358991410235794797595635330237270e-6;

# Create reference frame
my $sky = new Starlink::AST::SkyFrame("");


# circle simulating an observation area
my $obsArea = new Starlink::AST::Circle( $sky, 1, [0,0], [1*DAS2R], undef, "" );
isa_ok($obsArea, "Starlink::AST::Region");

# create some "survey fields"

my $circle = new Starlink::AST::Circle( $sky, 1, [0,0], [60*DAS2R], undef, "");
my $box = new Starlink::AST::Box($sky, 1,[-0.2,-0.2],[0.4,0.4], undef, "" );
my $int = new Starlink::AST::Interval($sky, [-0.2,-0.2],[0.4,0.4], undef, "" );
my $ellipse = new Starlink::AST::Ellipse( $sky, 1, [0,0],[120*DAS2R,180*DAS2R],
					  [0], undef, "" );

# Test for overlap
is( $circle->Overlap( $obsArea ), 3,"Circular area");
is( $box->Overlap( $obsArea ), 3,"Box area");
is( $int->Overlap( $obsArea ), 3,"Interval area");
is( $ellipse->Overlap( $obsArea ), 3,"Ellipse area");

# something that doesn't overlap
my $obsArea2 = new Starlink::AST::Circle( $sky, 1, [0,0.5],
					  [1*DAS2R], undef,"");
is( $circle->Overlap( $obsArea2 ), 1,"Outside Circular area");
is( $box->Overlap( $obsArea2 ), 1,"Outside Box area");
is( $int->Overlap( $obsArea2 ), 1,"Outside Interval");
is( $ellipse->Overlap( $obsArea2 ), 1,"Outside Ellipse");

# Create a compound region

isa_ok( $circle->CmpRegion( $box, Starlink::AST::Region::AST__AND(), "" ),
	"Starlink::AST::CmpRegion" );
