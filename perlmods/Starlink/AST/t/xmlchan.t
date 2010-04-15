#!perl

# Simple XML Channel

use strict;
use Test::More;

BEGIN {
 use Starlink::AST;
 if ( Starlink::AST::Version() < 3000000 ) {
   plan skip_all => "Not supported. Please upgrade to AST Version > 3.x";
   exit;
 } else {
   plan tests => 2;
 }
};


# Implement astShow
my $obj = new Starlink::AST::UnitMap( 1, "" );

my $ch = new Starlink::AST::XmlChan ( sink => sub {print "# $_[0]\n" } );
$ch->Write( $obj );
ok(1, "Write complete");

# Try again, but storing to an array
my @cards;
{
$ch = new Starlink::AST::XmlChan ( sink => sub {push(@cards, $_[0]) } );
$ch->Write( $obj );
}

for (@cards) {
  print "# $_\n";
}
ok(1, "Write to internal array complete");
