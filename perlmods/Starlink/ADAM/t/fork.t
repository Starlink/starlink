# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'
#

# Test script that forks itself in order to pass a message between
# the child and the parent. This is similar to the message relay
# script implemented in Starlink::AMS::Init

BEGIN { $| = 1; print "1..7\n"; }
END {print "not ok 1\n" unless $loaded;}
use Starlink::ADAM;
$loaded = 1;
print "ok 1\n";

# Okay -- module loaded successfully

# Now need to fork before starting adam


# Set up something to clean up the child
use vars qw/ $waitedpid /;
sub REAPER {
   $waitedpid = wait;
   # loathe sysV: it makes us not only reinstate
   # the handler, but place it after the wait
   $SIG{CHLD} = \&REAPER;
 }
 $SIG{CHLD} = \&REAPER;

# Setup a response to SIGALRM
#  Only happens during timeouts

$SIG{ALRM} = sub { die "Timeout!!"};



if ($pid = fork) {
  # This is the parent
  # PID of child is in $pid
  print "ok 2\n";

  # Start ADAM
  $status = adam_start "PARENT$pid";

  if ($status == &Starlink::ADAM::SAI__OK) {
    print "ok 3\n";
  } else {
    print "not ok 3\n";
    die "Error starting adam messaging\n";
  }



} elsif (defined $pid)  {
  # This is the child ($pid = 0)
  # The child should now start adam messaging itself
  # and send a message to the parent
  $SAI__OK = &Starlink::ADAM::SAI__OK;

  # Initialise the message system in the child
  $status = adam_start "CHILD$$";

  die "Error starting AMS" if $status != $SAI__OK;

  # Pause a bit to let the message system startup
  sleep 1;

  # Send an obey to the parent
  ($path, $messid, $status) = adam_send("PARENT$$", "action", "OBEY", "TEST");

  # Wait for response from parent
  my @reply = adam_getreply(10000, $path, $messid);

  # Exit from the child
  adam_exit;

  exit;
} else {
  # Error forking
  print "not ok 2\n";
  die "Error forking: $!\n";

}

# ADAM messaging is now running
# Need to listen for something from the child
# -- no timeout

# Set up a sigalarm to run in 10 seconds if we havent got any
# response from the child

alarm 10;

# Listen for reply...
@reply = adam_receive;
# Clear alarm
alarm 0;


# Check return status from adam_receive
$ADAM_STATUS = $reply[7];
if ($ADAM_STATUS != &Starlink::ADAM::SAI__OK) {
  print "not ok 4\n";
  die "Bad status from adam_receive: $ADAM_STATUS\n";
}
print "ok 4\n";

# Make sure the test string passed okay
if ($reply[6] eq "TEST") {
  print "ok 5\n";
} else {
  print "not ok 5\n";
}

# Work out the location of the child in the messaging system

$ADAM_STATUS = adam_reply($reply[3], $reply[4], "ACTSTART", $reply[1],
			  "RESPONSE" );
if ($ADAM_STATUS != &Starlink::ADAM::SAI__OK) {
  print "not ok 6\n";
  die "Bad status from adam_receive: $ADAM_STATUS\n";
}
print "ok 6\n";



# Shut down -- this croaks if it fails
adam_exit;
print "ok 7\n";

