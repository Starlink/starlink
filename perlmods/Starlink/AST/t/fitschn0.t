#!perl

# Simple Fits Channel

use strict;
use Test::More tests => 4;
require_ok("Starlink::AST");

# Implement astShow
my $obj = new Starlink::AST::UnitMap( 1, "" );

my $rancb;
{
  my $ch = new Starlink::AST::FitsChan ( sink =>
					 sub {
					   $rancb=1;
					   print "# $_[0]\n";
					 } );
  $ch->Write( $obj );
}
is($rancb, 1, "Write callback complete");

# Try again, but storing to an array
my @cards;
{
my $ch = new Starlink::AST::FitsChan ( sink => sub {push(@cards, $_[0]) } );
$ch->Write( $obj );
}

for (@cards) {
  print "# $_\n";
}
ok(@cards, "Got something in internal callback array");
is(scalar(@cards),11, "Write to internal array complete");
