#!/bin/perl

# Test FITS header processing

use strict;
use Test;
BEGIN { plan tests => 18 }

use NDF qw/ fits_extract_key_val fits_construct_string /;

# Try lots of different fits header combinations
# Test the combination of fits_extract_key_val and fits_construct_string

# Set up array of arrays for testing
my @parts = (
	     [ "WPLANGLE", 52, ""],
	     [ "TEST", '', "A comment"],
	     [ "NOTHING",undef,"Undefined"],
	     [ "WOW", "   A padded string",'Test pad'],
	     [ "STRING","Can't do this", "Test quoting"],
	     [ 'COMMENT',undef,"A comment"],
	    );


# Go through each array entry
foreach my $hdr (@parts) {

  my $line = &fits_construct_string(@$hdr);
  print "# $line\n";
  my ($key, $val, $com) = fits_extract_key_val($line);

  ok( $key, $hdr->[0]);
  ok( $val, $hdr->[1]);
  ok( $com, $hdr->[2]);

}
