#!perl -w

use strict;
use Test;
BEGIN { plan tests => 12 }
use NDF;

# ================================================================
#   Test MSG calls
#   
# ================================================================

# initialise global status
my $good = &NDF::SAI__OK;
my $status = $good;

# Make a bell
msg_bell($status);

# Print a blank line
msg_blank($status);

# Set the message level (ADAM only)
msg_ifset(&NDF::MSG__VERB, $status);

msg_setc('TEST', 'This is a test of MSG');

# Test formatting

my $pi = 3.141592654;
msg_fmtr('FMT', 'f4.2', $pi);
msg_out(' ','# Hello: ^TEST with pi = ^FMT',$status);

ok($status, $good);


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
  eval "msg_set$tok('$tok', '$tokens{$tok}');";
  die "Error processing msg_set$tok : $@" if $@;
  msg_load($tok, "^$tok", my $opstr, my $oplen, $status);
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

# On digital unix the formatting of a logical with I3
# actually returns ' -1' rather than '  1'
$tokans{l} = ' -1' if $^O eq 'dec_osf';


foreach my $tok (keys %tokens) {
  eval "msg_fmt$tok('$tok', '$tokfmt{$tok}','$tokens{$tok}');";
  die "Error processing msg_fmt$tok : $@" if $@;
  msg_load($tok, "^$tok", my $opstr, my $oplen, $status);
  ok($opstr, $tokans{$tok});
}

# Tuning
msg_tune('SZOUT', 23, $status);
msg_tune('SZOUT', 0, $status);
ok($status, $good);
