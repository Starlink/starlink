#!/bin/perl

use Starlink::ADAM;

use FileHandle;

# Script to relay messages between ADAM and perl
# 
# The go-between waits for adam messages; if a message is received
# from the perl application, the message value is executed and the
# result sent back to perl in an Adam inform message. If a message is 
# received from an adam task its contents are written to standard
# output (and read by perl)

# Unbuffered
$| = 1;

# Good status
$SAI__OK = &Starlink::ADAM::SAI__OK;

# Setup a signal handler.
# We need this in order to shut down the relay cleanly if 
# we are not sent a proper shutdown request

$SIG{'TERM'} = \&shutdown;
$SIG{'INT'}  = \&shutdown;
$SIG{'HUP'}  = \&shutdown;


# Open a log
#open(LOG, '>>junk.log');

# Set unbuffered output
STDOUT->autoflush;
# LOG->autoflush;

# Initialise the Adam message system by using the command-line arguments

$name = shift;
$myname = $name . "_relay";

#print LOG "Starting ADAM\n";
$status = adam_start $myname;
die "Error starting AMS" if $status != $SAI__OK;

# Send an initial OBEY to perl

#print LOG "Sending obey to parent\n";
($path, $messid, $status) = adam_send($name, action, "OBEY", "");

\&shutdown if $status != $SAI__OK;

# Fetch the reply to the obey
#print LOG "Waiting for reply from parent\n";
@reply = adam_receive;

#print LOG join(" ",@reply),"\n";
# Print the reply
#print_reply(@reply);

#print LOG "Looping...\n";
# Loop forever collecting messages

while (1) {
  @reply = ();
  @reply  = adam_receive;

  # Extract the task name
  $task = $reply[1];

  # Compare the task name with the name of the perl app
  if ($task ne $name) {

#    print LOG "*******Message from external task\n";

    # Task did not match so send the message to stdout
    &print_reply(@reply);

#    print LOG "Printed reply from task: ",join("::",@reply),"\n";

  } else {
    
    # Message came from perl so execute the message value as 
    # a command and send the result back to stdout
#    print LOG "*******Message from PERL\n";
    $value = $reply[6];

#    print LOG "Values are $value\n";

    $result = eval $value;

#    print LOG "Result is $@ and $result !!\n";

    # Check return status
    if ($@ ne "") {
      adam_reply($path, $messid, "SYNCREP", &Starlink::ADAM::SAI__ERROR, 
		 $result);
    } else {
      adam_reply($path, $messid, "SYNCREP", &Starlink::ADAM::SAI__OK, $result);
      
    }


  }


}


exit;

# Quick sub to print replies fomr adam_receive
sub print_reply {

#  my ($command,$task,$inmsg_name,$path,$messid,$facerr,$inmsg_value) = @_;

  # Print with separator so that the parent can split up all the values
  # even when they are blank
#  print LOG "Printing to stdout\n";
  print STDOUT join("::", @_),"\n";

}


# This is the nice shutdown subroutine

sub shutdown {
#  print LOG "Exiting via interrupt signal\n";
  adam_exit;

  exit;

}
