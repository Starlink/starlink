#!perl
use strict;
use Test;
BEGIN { plan tests => 20 }
use Starlink::EMS qw/:Ems/;

# Test EMS -- essentially a combination of the msg.t and err.t
# tests that come with the NDF module

# Also tests the C style calling emsElse etc

# ================================================================
#   Test Token calls
#
# ================================================================

# initialise global status
my $good = &Starlink::EMS::SAI__OK;
my $status = $good;

# Set up some tokens and then return the message

# The keys are the actual msg_set? (set) commands
# The value is the value to set the token
my %tokens = (
	      c => "hello",
	      d => 3.141592654,
              i => -52,
	      r => 162.54,
	      l => 1,
	     );
# The expected result from the token expansion (sometimes different)
my %tokans = (
	      c => "hello",
	      d => 3.141592654,
	      i => -52,
	      r => 162.54,
	      l => "TRUE",
	     );

foreach my $tok (keys %tokens) {
  eval "emsSet$tok('$tok', '$tokens{$tok}');";
  die "Error processing emsSet$tok : $@" if $@;
  my $opstr = emsMload($tok, "^$tok", $status);
  ok($opstr, $tokans{$tok});
}

# Now try the formatted equivalent
# Specify yhre format
my %tokfmt = (
	      c => "a10",
	      d => 'F4.2',   # 3.141592654 => 3.14
              i => 'I5.4',
	      r => 'E10.3E3',
	      l => 'I3',
	     );

%tokans = (
	   c => '     hello',
	   d => '3.14',
	   i => '-0052',
	   r => '0.163E+003',
	   l => '  1',
	  );

foreach my $tok (keys %tokens) {
  eval "emsFmt$tok('$tok', '$tokfmt{$tok}','$tokens{$tok}');";
  die "Error processing emsFmt$tok : $@" if $@;
  my $opstr = emsMload($tok, "^$tok", $status);
  ok($opstr, $tokans{$tok});
}

#--------------
#  Test error reporting
#--------------

emsBegin($status);

# Retrieve a line at a time via err_load
$status = &Starlink::EMS::SAI__WARN;
emsRep("PAR1","Line 1", $status);
emsRep("PAR2","Line 2", $status);

(my $param, my $opstr,$status) = emsEload( );
ok($param, "PAR1");
ok($opstr, "Line 1");
ok($status, &Starlink::EMS::SAI__WARN);
($param, $opstr,$status) = emsEload( );
ok($param, "PAR2");
ok($opstr, "Line 2");
($param, $opstr,$status) = emsEload( );
ok($status, $good);

# Retrieve last status - should be good
my $i = emsStat( );
ok($i, $good);

my $lev = emsLevel();
ok($lev, 2);
emsMark();
$lev = emsLevel();
ok($lev, 3);
emsRlse();
$lev = emsLevel();
ok($lev, 2);

emsEnd($status);
