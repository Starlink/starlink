#!perl

use strict;
use Test::More tests => 2;

require_ok("Starlink::Prologue");
require_ok("Starlink::Prologue::Parser");

my $parser = new Starlink::Prologue::Parser();
isa_ok( $parser, "Starlink::Prologue::Parser");

while (my $line = <DATA>) {
  my $status = $parser->push_line( $line );

  if (ref($status)) {
    print $status->stringify;
  }
}

__DATA__
*+
*  Name:
*     test

*  Purpose:
*     test parser

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*-

junk
