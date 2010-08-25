#!perl

use strict;
use warnings;
use constant NTESTS => 9;
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

# Test hash
my %TESTS = (
	     DOUBLE => 5.4,
	     INTEGER => 42,,
	     STRING => "hello again",
	     DARR => [2.3,-1.3],
	     IARR => [22,-13],
             UNDEF => undef,
             EMPTY => {},
	     STRARR => ["hello","goodbye","yo"],
             HASH => { key1 => 55,
                       key2 => { a => 1, b=> 24.5, c => [1,2] } },
	    );

# Create a tied hash and copy into it
my %OUT;
tie %OUT, "Starlink::AST::KeyMap";

%OUT = %TESTS;

is_deeply( \%OUT, \%TESTS, "Compare hash copy" );

# Now try to store an AST object (which can't be compared
# above because of the IDs change

$OUT{OBJECT} = new Starlink::AST::UnitMap(2,"");
my $object = $OUT{OBJECT};
isa_ok( $object, "Starlink::AST::UnitMap" );

$OUT{DOBJ} = [
              new Starlink::AST::UnitMap(2, ""),
              new Starlink::AST::SpecFrame("")
             ];

my $objects = $OUT{DOBJ};
isa_ok( $objects->[0], "Starlink::AST::UnitMap" );
isa_ok( $objects->[1], "Starlink::AST::SpecFrame" );

# Store a KeyMap

my $km = Starlink::AST::KeyMap->new( "" );
$km->MapPut0I( "Test", 52, "My comment" );
$OUT{KMOBJ} = $km;

is( $OUT{KMOBJ}->{Test}, 52, "Test store of Keymap object" );

# Remove INTEGER and UNDEF

my @values = delete @OUT{qw/ INTEGER UNDEF / };

is(scalar @values, 2, "Number of deleted entries" );
is($values[0], 42, "Check deleted entry" );

# Clear the hash

%OUT = ();

is( scalar keys %OUT, 0, "Cleared hash count");
