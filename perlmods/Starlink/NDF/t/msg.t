#!perl -w

use strict;
no strict "vars";

use NDF;

# ================================================================
#   Test MSG calls
#   
# ================================================================

$n=1; # number of tests
print "1..$n\n";

# initialise global status
$status = &NDF::SAI__OK;


# Make a bell
msg_bell($status);

# Print a blank line
msg_blank($status);

# Set the message level
#msg_ifset(&NDF::MSG__VERB, $status);

msg_setc('TEST', 'This is a test of MSG');


$pi = 3.141592654;

msg_fmtr('FMT', 'f4.2', $pi);
msg_out(' ','Hello: ^TEST with pi = ^FMT',$status);

($status == &NDF::SAI__OK) && 
  (print "ok\n") || (print "not ok\n");

