#!perl

use strict;
use Test::More tests => 2;
require_ok("Starlink::AST");

# Implement astShow
my $obj = new Starlink::AST::UnitMap( 1, "" );

my $ch = new Starlink::AST::Channel ( sink => sub {print "# $_[0]\n" } );
$ch->Write( $obj );
ok(1, "Write complete");
