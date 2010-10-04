#!perl

use strict;
use warnings;
use constant NTESTS => 70;
use Test::More tests => NTESTS;

require_ok( "Starlink::AST");

Starlink::AST::Begin();

my $vers = Starlink::AST::Version();
print "# Using AST Version $vers\n";

if ($vers < 3_005_000) {
 SKIP: {
    skip "AST version too old for KeyMap", (NTESTS - 1);
  }
  exit;
}

  # create key map
my $map = new Starlink::AST::KeyMap( "" );

# Data type lookup
my %TYPES = (
	     "D" => &Starlink::AST::KeyMap::AST__DOUBLETYPE(),
	     "I" => &Starlink::AST::KeyMap::AST__INTTYPE(),
	     "S" => &Starlink::AST::KeyMap::AST__SINTTYPE(),
	     "C" => &Starlink::AST::KeyMap::AST__STRINGTYPE(),
	     "A" => &Starlink::AST::KeyMap::AST__OBJECTTYPE(),
	    );

# Test hash
my %TESTS = (
	     "DOUBLE" => [ "0D", 5.4, "comment" ],
	     "INTEGER" => [ "0I", 100_000, "" ],
             "SHORT"  => [ "0S", 30000, "" ],
	     "STRING" => [ "0C", "hello again", "comment2" ],
	     "OBJECT" => [ "0A", new Starlink::AST::UnitMap(2,""),
			   "comment3" ],
	     "DARR" => [ "1D", [2.3,-1.3], ""],
	     "IARR" => [ "1I", [22e6,-13], ""],
	     "SARR" => [ "1S", [22,-30000], ""],
	     "STRARR" => [ "1C", ["hello","goodbye","yo"], ""],
	     "DOBJ" => [ "1A", [
				new Starlink::AST::UnitMap(2, ""),
				new Starlink::AST::SpecFrame("")
			       ], "" ],
	    );

# Store
for my $k (keys %TESTS) {
  my $method = "MapPut" . $TESTS{$k}->[0];
  $map->$method( $k, $TESTS{$k}->[1], $TESTS{$k}->[2]);
}

# Size
is( $map->MapSize(), scalar( keys %TESTS ), "Check map size");

# Retrieve
for my $k (keys %TESTS) {
  print "# Processing key $k...\n";
  my $method = "MapGet". $TESTS{$k}->[0];
  my @results = $map->$method( $k );
  my @ori = ( ref( $TESTS{$k}->[1] ) eq 'ARRAY'
	      ? @{ $TESTS{$k}->[1]} : $TESTS{$k}->[1] );
  is( scalar(@results), scalar(@ori) , "Check number of returned elements $k");
  is( $map->MapLength( $k ), scalar(@ori), "Confirm number $k");

  for my $i (0..$#results) {
    if (UNIVERSAL::isa($ori[$i],"Starlink::AST")) {
      # just compare type
      is( ref($results[$i]), ref($ori[$i]),
	  "Compare object element class $i : ". ref($ori[$i]));
    } else {
      is( $results[$i], $ori[$i], "Compare element $i: $ori[$i]");
    }
  }

  # check has the key
  ok( $map->MapHasKey( $k), "Confirm key is present");

  # check data type
  my $type = $map->MapType( $k );
  my $tkey = substr( $TESTS{$k}->[0],1, 1);
  is( $type, $TYPES{$tkey}, "Confirm type" );

}

# For each
for my $i (0 .. ($map->MapSize - 1)) {
  my $key = $map->MapKey( $i );
  ok( $map->MapHasKey( $key ), "Confirm presence of key $i");
}

# Remove a key
$map->MapRemove("DOUBLE");
ok( ! $map->MapHasKey( "DOUBLE"), "DOUBLE no longer in map");

# Test bad type
is( $map->MapType( "gurgle"), Starlink::AST::KeyMap::AST__BADTYPE(),
    "Confirm bad type");
