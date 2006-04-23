#!perl

use strict;
use Test::More tests => 3;

require_ok("Starlink::Prologue");
require_ok("Starlink::Prologue::Parser");

my $parser = new Starlink::Prologue::Parser();
isa_ok( $parser, "Starlink::Prologue::Parser");

while (defined (my $line = <DATA>)) {
  my $status = $parser->push_line( $line );

  if (ref($status)) {
    print $status->stringify;
  }
}
my $status = $parser->flush();
if (ref($status)) {
  print $status->stringify;
}


__DATA__
/* *+
*  Name:
*     test

*  Purpose:
*     test parser

*  Description:
*     line 1

*     line 2

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     12-APR-2006 (TIMJ):
*        First version
*     {enter_further_changes_here}
*


*- */

junk

#+
#  Name:
#    test csh

#  Language:
#    C-shell

#  Type of Module:
#    Dummy

