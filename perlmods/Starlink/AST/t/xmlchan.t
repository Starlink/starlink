#!perl

# Simple XML Channel

use strict;
use Test::More tests => 3;
require_ok("Starlink::AST");

# Implement astShow
my $obj = new Starlink::AST::UnitMap( 1, "" );

my $ch = new Starlink::AST::XmlChan ( sink => sub {print "# $_[0]\n" } );
$ch->Write( $obj );
ok(1, "Write complete");

# Try again, but storing to an array
my @cards;
my $ch = new Starlink::AST::XmlChan ( sink => sub {push(@cards, $_[0]) } );
$ch->Write( $obj );

for (@cards) {
  print "# $_\n";
}
ok(1, "Write to internal array complete");
