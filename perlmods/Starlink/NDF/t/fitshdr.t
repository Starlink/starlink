#!/bin/perl

# Test FITS header processing

use strict;
use Test;
BEGIN { plan tests => 76 }

use NDF qw/ fits_extract_key_val fits_construct_string /;

# Try lots of different fits header combinations
# Test the combination of fits_extract_key_val and fits_construct_string

# Set up array of arrays for testing
my @parts = (
	     [ "WPLANGLE", 52, undef],
	     [ "NOTHING",undef,"Undefined"],
	     [ "WOW", "   A padded string",'Test pad'],
	     [ "STRING","Can't do this", "Test quoting"],
	     [ 'COMMENT',undef,"A comment"],
	     [ "FLOAT", "52.6e-5", "Floating point number"],
	     [ "NEGFLT", "-1.567e-27","Negative exponent"],
	     [ "FLOAT2", "-52.734567897865", "No exponent"],
	     [ "NUMBER", -527.6, "non stringified number"],
	     [ "INT", 523456,"non stringified integer with ' quote"],
	     [ "EXPON", 2.567e-5,"non stringified exponent"],
	     [ "NULLSTR",'',"A null string"],
	     [ "LONE-E" ,"E", ' '],
	     [ "NUMNOC", -1268964656,undef],
	    );


# Go through each array entry
foreach my $hdr (@parts) {

  my $line = &fits_construct_string(@$hdr);
  ok( length($line), 80);

  print "# $line\n";
  my ($key, $val, $com) = fits_extract_key_val($line);

  ok( $key, $hdr->[0]);
  ok( $val, $hdr->[1]);
  ok( $com, $hdr->[2]); 

}


# These tests are not reversible so we hae to provide the expected
# output.

@parts = (
	  [ "LONGKEYWORD", "string", "comment", "LONGKEYW","string","comment"],
	  [ "DOUBLE", "1.4576892345672345345644D524", "double precision",
	    "DOUBLE", "1.4576892345672345345644e524", "double precision"],
	  [ "BLANK", "      ", "blank string","BLANK"," ","blank string"],
	  [ "WPLANGLE", 52, "", "WPLANGLE", 52,undef],
	  [ "PAD", "  pad  ", "padded string", "PAD", "  pad","padded string"],
	 );


# Go through each array entry
foreach my $hdr (@parts) {

  my $line = &fits_construct_string(@$hdr[0..2]);
  ok( length($line), 80);

  print "# $line\n";
  my ($key, $val, $com) = fits_extract_key_val($line);

  ok( $key, $hdr->[3]);
  ok( $val, $hdr->[4]);
  ok( $com, $hdr->[5]);

}
