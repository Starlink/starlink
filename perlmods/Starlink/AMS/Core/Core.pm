package Starlink::AMS::Core;

=head1 NAME

Starlink::AMS::Core - core non-OO interface to the ADAM messaging system

=head1 SYNOPSIS

 use Starlink::AMS::Core;

 adamtask_init;

 adamtask("name","monolith_image");

 adamtask_set("name", "task:param","value");
 adamtask_obeyw("name", "task", "params");
 $value = adamtask_get("name", "task:param");
 adamtask_control("name","default","dir");
 adamtask_control("name","par_reset");

 adamtask_exit;

=head1 DESCRIPTION

This module provides low-level commands for communicating with ADAM tasks via
the ADAM messaging system (AMS). The AMS communications themselves are
handled via a lower level module (C<Starlink::ADAM>).

Commands are provided for starting ADAM monoliths, for controlling them,
and for finding the states of parameters.

A higher level object-oriented interface is provided in
C<Starlink::AMS::Task> and C<Starlink::AMS::Init> and these
should be used if at all possible.

=cut

use strict;
use Carp;
use vars qw($VERSION @ISA @EXPORT %EXPORT_TAGS $AUTOLOAD $DEBUG);

require Exporter;

@ISA = qw(Exporter);

$DEBUG = 0;

# EXTERNAL MODULES

# This module requires the Starlink::ADAM module so that we can access
# the ADAM messaging system

use Starlink::ADAM qw/:adam/;

# Use the IO routines to create the pipe handle
use IO::Pipe;

# Use the Proc module to start the monoliths and keep track of process IDs
use Proc::Simple 1.13;

use File::Basename;

# Global variable definitions
# Global variables

# $RELAY is the filehandle to the pipe communicating with the relay
# $RELAY_NAME is the name of the relay in the messaging system
# $RELAY_PATH is the path to the relay via the messagin system
# $RELAY_MESSID is the message id of the relay in the AMS
# %PIDS is a hash containing the process ids of each monolith
#     eg   $PIDS{KAPPA} is the PID of the kappa monolith
# %TASKS is a hash containing subroutine references to the task commands
#     eg   $TASKS{KAPPA} is the ref to the sub that runs kappa commands
# $ADAM_STATUS is the status returned from the last completed action
# $ERRHAND  file handle to use for error messages
# $MSGHAND  file handle to use for normal messages


use vars qw/$ADAM_STATUS $PARAMREP_SUB $msg_hide $err_hide
  $ERRHAND $MSGHAND $TIMEOUT $RELAY $RELAY_NAME $RELAY_PATH 
  $RELAY_MESSID %PIDS %TASKS $adam_started /;


$RELAY_NAME = undef;

$msg_hide = 0;
$err_hide = 0;

$MSGHAND = *STDOUT;
$ERRHAND = *STDERR;

$adam_started = 0;
$ADAM_STATUS = &Starlink::ADAM::SAI__OK;

# Default timeout is 30 seconds
$TIMEOUT = 30;

# Items to export into callers namespace by default. Use tags
# to be more specific

%EXPORT_TAGS = (
                'Func'=>[qw/
			 adamtask_init       adamtask_exit adamtask
			 adamtask_set
			 adamtask_get        adamtask_obeyw
			 adamtask_control    adamtask_cancel
			 adamtask_forget
                        /]
                );

Exporter::export_tags('Func');

$VERSION = '1.01';


############# Subroutines #############################



=head1 FUNCTIONS

=over 4

=item adamtask_init

Initialises the ADAM messaging system. This routine should always be
called before attempting to control I-tasks.

A relay task is spawned in order to test that the messaging system
is functioning correctly. The relay itself is not necessary for the
non-event loop implementation. If this command hangs then it is
likely that the messaging system is not running correctly (eg
because the system was shutdown uncleanly - try removing named pipes
from the ~/adam directory).

Status is returned.

=cut

sub adamtask_init {

  my ($taskname);

  # See if we have a RELAY running already
  if (defined $RELAY_NAME && $RELAY_NAME =~ /./) { 
    if (adam_path($RELAY_NAME) == 1) {
      print $MSGHAND "Relay task is already running\n" unless $msg_hide;
      return &Starlink::ADAM::SAI__OK;
    }
  }

  return &Starlink::ADAM::SAI__OK if $adam_started;

  # Set the task name
  $taskname = "perl_ams" . $$;

  # Initialise ams using the program name as the task name
  $ADAM_STATUS = adam_start $taskname;

  return $ADAM_STATUS if ($ADAM_STATUS != &Starlink::ADAM::SAI__OK);

  # Set a global variable that tells me I have started ams
  # need this to protect against exit

  $adam_started = 1;

  # Start the relay process
  # Hardwire the location

#  my $relay_dir = "/local/lib/perl5/site_perl/Starlink/";
  my $relay = "MessageRelay.pl";

  # Create pipe
  # Relay should be executable and in the users path.
  $RELAY = new IO::Pipe;
  eval {
    $RELAY->reader("$relay $taskname");
  };
  if ($@) {
    croak "Error starting ADAM relay '$relay': $@";
  }
  $RELAY->autoflush;

  # Wait for a message from the RELAY
#  print "Waiting for relay...(from $RELAY)\n";

  my @reply = adam_receive;
  # Status is the 7 member
  $ADAM_STATUS = $reply[7];
  return $ADAM_STATUS if ($ADAM_STATUS != &Starlink::ADAM::SAI__OK);

#  print join("::",@reply),"\n";

  # Store the path to the relay

  $RELAY_NAME = $reply[1];
  $RELAY_PATH = $reply[3];
  $RELAY_MESSID = $reply[4];

  # print "RELAY NAME is $RELAY_NAME and $reply[1]\n";

  # Reply to the obey
#  print "Replying to OBEY\n";
  $ADAM_STATUS = 
    adam_reply($RELAY_PATH, $RELAY_MESSID, "ACTSTART", $reply[1], "");
  return $ADAM_STATUS if ($ADAM_STATUS != &Starlink::ADAM::SAI__OK);

  # Need to setup an exit handler 


  # Print some info
#  print "Name path messid : $RELAY_NAME $RELAY_PATH $RELAY_MESSID\n";

  return $ADAM_STATUS;
  
}


# adamtask_message
#
#   Routine to process incoming messages
#    Arguments: Reference to array of messages
#   Routine does not read from the PIPE. It relies on another process
#   to pass the text from the pipe to this routine
# Returns immediately if status (arg 7) is not SAI__OK
# Returns a status (usually SAI__OK -- will only change in response
# to a paramrep)

sub adamtask_message {

  my $message = shift;

  my ($command, $path, $messid);

  # Split the message on the separator
  my @message = @$message;

  $command = $message[0];
  $path    = $message[3];
  $messid  = $message[4];
  my $status = $message[7];

  return ($status, undef) if $status != &Starlink::ADAM::SAI__OK;

  # Evaluate the command here (need to think about this)

  # If the command is endmsg or setrepsonse
  # then we have ended a transaction so return
  # Also return if we are using the internal message of 'badstatus'

  return ($status, undef) if $command =~ /endmsg|setresponse|badstatus/;

  # If this is a control response then we should print the directory
  # but only if we are sending a 'default' action. 'par_reset' returns nothing 
  $command eq "controlresponse" && do {
    print $MSGHAND "$message[2] = $message[6]\n" 
      if ($message[2] eq "default" & !$msg_hide);
    return ($status, $message[6]);
  };


  # If this is a getresponse then return the parameter value
  $command eq "getresponse" && do {
    return ($status, $message[6]);
    
  };

  # Just print inform messages
  $command eq "inform" && do {

    # Check for error messages
    if ($message[6] =~ /^!/) {
      print $ERRHAND "$message[6]\n" unless $err_hide;
    } else {
      print $MSGHAND "$message[6]\n" unless $msg_hide;
    }
    return ($status, undef);
  };

  # Need to ask for a parameter if we have a paramreq
  $command eq "paramreq" && do {
    
    # Have to split the paramreq into parts
    # Split on the newline character (that was added by Starlink::ADAM)
  
    my (@bits) = split(/\n/,$message[6]);

    my $value = &$PARAMREP_SUB(@bits);

    # Now need to send this back to the message
    $status = adam_reply($path, $messid, "PARAMREP", "", $value);
  };
  return ($status,undef);
}

=item adamtask($name, $image, {options})

This command is used to start adam tasks. $name is the name of the
task in the messaging system (and should be used for all future
accesses to the task) and $image is the name of the image file to be
spawned (eg /star/bin/kappa/kappa_mon to load the basic KAPPA
monolith).

The options must be supplied in the form of a hash reference.
Allowed options are 'TASKTYPE' (can be either 'I' or 'A').
Default is 'A'.

Example:

  ($obj,$status) = adamtask("kappa_mon",
                            "$ENV{KAPPA_DIR}/kappa_mon",
                            { TASKTYPE => 'A' });

=cut

# adamtask
#
#  Usage: $command = adamtask <name> <file>
#       where <name> is the name of the task and <file> is the
#       name of the image file. If <file> is supplied the adam task
#       is spawned, otherwise it is assumed to be already running.

sub adamtask {

  croak 'Usage: adamtask name, [file], [options]' 
    if (scalar(@_) < 1);

  # Arguments
  my $taskname = shift;


  # Try to find the options
  my ($opthash, $image);
  my %options = ();
  if (@_) {
    my $tmp = shift;
    if (not ref($tmp)) {
      $image = $tmp;
      if (@_) {
	$tmp = shift;
	$opthash = $tmp if ref($tmp) eq 'HASH';
      }
    } else {
      $opthash = $tmp;
    }
  }
  %options = %$opthash if defined $opthash;

  # Variables
  my ($adam_task_type, $adam_task_type_set, $icl_task_name, 
      $icl_task_name_set);

#  print "Image is $image, Options $opthash\n";

  if ($image =~ /./) {

    # Set the type of all adam tasks started to 'I'
    # but taking care that we keep track of what the Env variable
    # was set to
    if (exists $ENV{ADAM_TASK_TYPE}) {
      $adam_task_type = $ENV{ADAM_TASK_TYPE};
      $adam_task_type_set = 1;
    }

    if (exists $options{'TASKTYPE'}) {
      $ENV{ADAM_TASK_TYPE} = $options{'TASKTYPE'};
    } else {
      $ENV{ADAM_TASK_TYPE} = 'A';
    }
    # print STDOUT "ADAM_TASK_TYPE = $ENV{ADAM_TASK_TYPE}\n";

    # Set the message system name as well
    if (exists $ENV{ICL_TASK_NAME}) {
      $icl_task_name = $ENV{ICL_TASK_NAME};
      $icl_task_name_set = 1;
    }
    $ENV{ICL_TASK_NAME} = $taskname;

    # Check for the PERLAMS_VALGRIND_MONS and PERLAMS_VALGRIND_OPTS
    # environment variables.
    if( exists $ENV{PERLAMS_VALGRIND_MONS} ) {
      my @mons;
      if( $ENV{PERLAMS_VALGRIND_MONS} eq '*' ) {
        my( $mon, undef, undef ) = fileparse( $image );
        push @mons, $mon;
      } else {
        @mons = split /,/, $ENV{PERLAMS_VALGRIND_MONS};
      }
      foreach my $mon ( @mons ) {
        if( $image =~ $mon ) {

          my $opts;
          if( exists( $ENV{PERLAMS_VALGRIND_OPTS} ) ) {
            $opts = $ENV{PERLAMS_VALGRIND_OPTS} . " --log-file=valgrind.$mon";
          } else {
            $opts = "--log-file=valgrind.$mon";
          }

          $image = "valgrind $opts $image";
        }
        last;
      }
    }

    # Now execute the task in this new environment
    # and store the object.
    # Cant use a 'system' call since this does not return the PID
    # of the process. Use the Proc::Simple module instead.

     # If I use this method for starting the tasks they 
     # kill themselves when I shutdown.
#    print "Starting $image...\n";
    my $pid = new Proc::Simple;
    $pid->kill_on_destroy(1);
    my $status = $pid->start("$image");
#    print "with status = $status (1 is good))\n";

    if ($status == 0 ) {
      print $ERRHAND "Error starting $image\n" unless $err_hide;
      $ADAM_STATUS = &Starlink::ADAM::SAI__ERROR;

    } else {

      # Set good status
      $ADAM_STATUS = &Starlink::ADAM::SAI__OK;

      # Store this object in the hash array with taskname as the key
      # Mainly do this so that it can be forgotten properly
      # Also have to store it somewhere since the monolith
      # Will automatically be killed on undef of this object
      $PIDS{"$taskname"} = $pid;

    }

    
    # Restore the environment    
    if ($adam_task_type_set) {
      $ENV{ADAM_TASK_TYPE} = $adam_task_type;
    } else {
      delete $ENV{ADAM_TASK_TYPE}
    }
    
    if ($icl_task_name_set) {
      $ENV{ICL_TASK_NAME} = $icl_task_name;
    } else {
      delete $ENV{ICL_TASK_NAME};
    }

  } else {
    # The process ID is not stored since it was not spawned by this
    # subroutine

    $PIDS{"$taskname"} = undef;

  }

  # Return the Process object and the ADAM status
  return $PIDS{"$taskname"}, $ADAM_STATUS;

}

# adamtask_send
#
#  Sends a command to be executed by the relay process and returns the
#  result. The command is made up of a perl command plus arguments
#  and will be evaluated in the relay process.

sub adamtask_send {

  my $command = shift;

  adam_reply($RELAY_PATH, $RELAY_MESSID, "SYNC", "", $command);

  my @reply = adam_getreply(10000, $RELAY_PATH, $RELAY_MESSID);

  $ADAM_STATUS = $reply[5];

  # Status is stored in [7]
  if ($reply[7] == &Starlink::ADAM::SAI__OK) {
    return $reply[6];
  } else {
    croak "adamtask_send: Error in message relay: $reply[6]\n";
  }

}

# adamtask_sendw
#
#  - Send a command and then block until the command is complete
#     This does not use the relay directly from here.
#   - Arguments are those expected for adam_send:
#      task name, command string, Command Context, parameter values

sub adamtask_sendw {

  my $task = shift;
  my $command = shift;
  my $context = shift;
  my $args = shift;

  my ($reply, @reply, $response, $status);

  # First I need to send the command
  # Since we are calling adam_send we should receive the
  # path and messid to the remote task.
  # This allows us to await the return from a specific task and not
  # get confused by other messages

  my (@returns) = eval { adam_send($task, $command, $context, $args); };

  # We had an error sending the command
  if ($@ ne "" || $returns[2] != &Starlink::ADAM::SAI__OK) {

    # Note that I should return a status here.
    # If we are doing a GET I have to return two values
    my $get = 0;
    $get = 1 if ($context =~ /GET|CONTROL/);

    # Set bad status
    my $badstatus = &Starlink::ADAM::SAI__ERROR;
    if ($@ ne "") {
      carp "$@";
      $badstatus = &Starlink::ADAM::SAI__ERROR;
    } else {
      $badstatus = $returns[2];
    }

    # Print error to standard error
    my ($fac, $ident, $text) = adam_appendstatus($badstatus);
    print $ERRHAND "!! $fac"."__$ident: $text\n" unless $err_hide;
    # Now actually return it
    if ($get) {
      return ($badstatus, undef);
    } else {
      return $badstatus;
    }

  }

  # Now I need to wait for a message to come back (should be actstart)
  # From the task that I just contacted.

  while (1) {

#    @reply = adam_receive;

    # Note that the timeout must be given in milliseconds

    @reply = adam_getreply(1000 * $TIMEOUT, $returns[0], $returns[1]);

    # Now need to acknowledge the message if it is a paramreq or something
    # Send the reply off to a generic subroutine

    ($status, $response) = adamtask_message(\@reply);

    last if ($status != &Starlink::ADAM::SAI__OK);

    # Exit loop if we received an 'endmsg' or 'get|setresponse'
    # If there was an error with the getreply itself (ie the monolith
    # died) then the adam_getreply code provides us with an extra message
    # of 'badstatus'

    last if $reply[0] =~ /endmsg|getresponse|setresponse|controlresponse|badstatus/;

  }

  # Check the status of the completed action:

  # An OBEY should finish with DTASK__ACTCOMPLETE
  # A GET should finish with SAI__OK
  # Print bad status to STDERR

  if ($reply[5] != &Starlink::ADAM::DTASK__ACTCOMPLETE &&
     $reply[5] != &Starlink::ADAM::SAI__OK) {
    my ($fac, $ident, $text) = adam_appendstatus($reply[5]);
    print $ERRHAND "!! $fac"."__$ident: $text\n" unless $err_hide;
  }

  # Record the error status (whether it was from the task or from the message
  # system

#  if ($reply[0] eq "badstatus") {
#    $ADAM_STATUS = $reply[7];
#  } else {
      $ADAM_STATUS = $reply[5];
#  }


  # Return the parameter value if this is a 'getresponse'
  $response = " " unless (defined $response);
  return ($ADAM_STATUS,$response) 
    if $reply[0] =~ 'getresponse|controlresponse';


  # Else just return status
  return $ADAM_STATUS;

}



=item adamtask_set($name, $param, $value)

This routine implements the SET command and can be used to set the
values of parameters in I-tasks before sending an OBEY.

The parameter name must be given in terms of the action it is related
to in the form: "action:param". For example, to set the input NDF for
stats to 'test' we can call 'adamtask_set("kappa", "stats:ndf",
"test")'

Status is returned.

=cut


# adamtask_set
#
#   Implements the SET command
#   Usage:  adamtask_set task param val

sub adamtask_set {

  my $task = shift;
  my $param = shift;
  my $val = shift;

  # Reset ADAM_STATUS
  $ADAM_STATUS = &Starlink::ADAM::SAI__OK;

  my $status = 
    adamtask_sendw($task,$param,"SET",$val);

}

=item ($value, $status) = adamtask_get($name, $param)

This routine implements the GET command and can be used to get
the values of parameters in I-tasks.

The parameter name must be given in terms of the action it is related
to in the form: "action:param". For example, to get the input NDF for
stats we can say '$value = adamtask_set("kappa", "stats:ndf")'

The parameter value and status are returned in an array context.
Status is not returned if the routine is called in a scalar context.

This routine can not be used to retrieve parameters from A-tasks
(eg if the monolith was launched as an A-task rather than an I-task).
In that case the par_get routine supplied in the perl NDF module
should be used (where the task name will be 'monolith.task').

=cut


# adamtask_get
#
#   Implements the GET command
#   Usage:  $value = adamtask_get task param

sub adamtask_get {

  my $task = shift;
  my $param = shift;

  # Reset ADAM_STATUS
  $ADAM_STATUS = &Starlink::ADAM::SAI__OK;

  my ($status, $result) = adamtask_sendw($task,$param,"GET","");

  if (wantarray) {
    return ($result, $status);
  } else {
    return $result;
  }


}

# adamtask_obey
#
#   Implements the OBEY command
#   Usage:  adamtask_obey task command params

sub adamtask_obey {

  my $task = shift;
  my $command = shift;
  my $params = shift;

  # Reset ADAM_STATUS
  $ADAM_STATUS = &Starlink::ADAM::SAI__OK;

  my $result = 
    adamtask_send("adam_send(\"$task\",\"$command\",\"OBEY\",\"$params\")");

  return $result;

}

# adamtask_paramreply
#
#   Sends a reply to a PARAMREQ message
#   Usage: adamtask_paramreply task path messid reply

sub adamtask_paramreply {

  my $task = shift;
  my $path = shift;
  my $messid = shift;
  my $reply = shift;

  # Reset ADAM_STATUS
  $ADAM_STATUS = &Starlink::ADAM::SAI__OK;

  my $status = 
    adamtask_send("adam_reply($path,$messid,\"PARAMREP\",\"\",\"$reply\")");

  return $status;

}

# adamtask_syncreply 
#
#  Sends a reply to a sync message

sub adamtask_syncreply {

  my $task = shift;

  my $path = shift;
  my $messid = shift;

  # Reset ADAM_STATUS
  $ADAM_STATUS = &Starlink::ADAM::SAI__OK;

  my $result =
    adamtask_send("adam_reply($path,$messid,\"SYNCREP\",\"\",\"\")");
}


=item adamtask_control()

This routine can be used to send one of two CONTROL messages to 
an ADAM task and can take 3 forms.

=over 4

=item adamtask_control($name, 'par_reset');

This will set all the parameters in $name to their initial values.

=item adamtask_control($name, 'default', $dir);

This will set the current working directory of $name to $dir.

=item $cwd = adamtask_control($name, 'default');

This will return the current working directory and store it in $cwd.

=back

The value and status are returned in an array context (value, status)
even when $value is not required.

=cut


# adamtask_control
#
#  Implements the CONTROL command

sub adamtask_control {

  my $task = shift;
  my $command = shift;

  # There may not be a param (eg for par_reset)
  # So protect from -w
  my $params = " ";
  if (@_) { $params = shift; $params = " " unless defined $params;}

  # Reset ADAM_STATUS
  $ADAM_STATUS = &Starlink::ADAM::SAI__OK;

  my ($status, $result) = 
    adamtask_sendw($task,$command,"CONTROL",$params);

#  return $result if $command eq 'default';
  return ($result, $status);

}

# adamtask_cancel 
#
#  Implements the CANCEL command

sub adamtask_cancel {

  my $task = shift;
  my $command = shift;
  my $params = shift;

  # Reset ADAM_STATUS
  $ADAM_STATUS = &Starlink::ADAM::SAI__OK;

  my $status =
    adamtask_sendw($task,$command,"CANCEL",$params);

  return $status;
}


=item $status = adamtask_obeyw($name, $action, $params)

This routine can be used to send an OBEY to a task. The parameters
can be specified in $params. Note that this command is implemented
as an OBEYW and so only returns when the action is completed.

For example: adamtask_obeyw("kappa", "stats", "ndf=test"); will run
Kappa stats with NDF=test. If the parameter is not specified in here
the current value set with adamtask_set will be taken. If that is
not available then the user will be prompted for a value on the
command line.

Status is returned. The OBEYW has completed successfully if
status has the value of Starlink::ADAM::DTASK__ACTCOMPLETE.

=cut


# adamtask_obeyw
#
#  - implement an obeyw
#     There is no point using the RELAY for this - just send messages
#     directly from here.

sub adamtask_obeyw {

  my ($task, $params, $command);

  $task = shift;
  $command = shift;
  $params = shift || " "; # -w protection

  my ($reply, @reply);

  # Reset ADAM_STATUS
  $ADAM_STATUS = &Starlink::ADAM::SAI__OK;

  # First I need to send the OBEY to sendw
  my $status = adamtask_sendw($task,$command,"OBEY",$params);

  return $status;

}



=item adamtask_exit

This routine shuts down the Adam messaging system and  kills the relay.
ADAM tasks will not be killed by this routine since they will die
automatically when perl exits.
(unless they have been forgotten with adamtask_forget).

This command should always be issued before exiting perl.
Note that if the higher level interface (Starlink::AMS::Init) is used
neither adamtask_init nor adamtask_exit need be called explicitly.

Currently this command is run automatically on exit from the program
(via an END{} block). Note that the exit handler will not be executed
when signals are not caught. In order to trap these signals you may
need

  use sigtrap qw/die normal-signals error-signals/;

at the top of your program.

=cut

# adamtask_exit
#
#  Routine to shut down the relay
#

sub adamtask_exit {

  my ($pid, $task);

  # Ask the relay to kill itself
#  print "Ask the relay to kill itself\n";
  $ADAM_STATUS = adam_reply($RELAY_PATH, $RELAY_MESSID, "SYNC", "", "adam_exit; exit") 
    if defined $RELAY_PATH;

 carp "Error shutting down message relay" 
    if ($ADAM_STATUS != &Starlink::ADAM::SAI__OK);

#  print "Killing the pipe\n";

  # Remove the pipe
  undef $RELAY;

#  print "Exit ams\n";
  # Exit AMS
  adam_exit if $adam_started;

  # Reset adam_started
  $adam_started = 0;

  # Monoliths will kill themselves since they are Proc::Simple
  # objects

}

=item adamtask_forget($name)

This command will remove the specified task from the list of tasks
started by perl. This means that adamtask_exit will not kill the
task if this has been called.

=cut

# adamtask_forget
#
#   - Remove a task from the task list so that adamtask_exit doesnt kill it
#

sub adamtask_forget {

  my $task = shift;

  $PIDS{"$task"}->kill_on_destroy(0);


}

# This is the default subroutine for parameter requests

$PARAMREP_SUB = sub {

  my ($param, $param_prompt, $default) = @_;

  my $prompt = "$param -- $param_prompt / $default / > ";

  print "$prompt";
  chomp(my $value = <>);

  # Accept the default value if needed.
  if ($value !~ /./) {
    $value = $default;
  }

  return $value;

};


# Start up ADAM messaging
#adamtask_init;
#print "Starting ADAM\n";


# Exit handler
END {
  adamtask_exit;
  print "ADAM exited\n" if $DEBUG;
}



1;

__END__

=back

=head1 VARIABLES

=head2 B<Status>

The status of each message is not returned as such but can be accessed
via the variable $Starlink::ADAMTASK::ADAM_STATUS. Note that status is
set to &Starlink::ADAM::DTASK__ACTCOMPLETE for successfull completion
of an OBEY and set to &Starlink::ADAM::SAI__OK for the successful
completion of a GET or SET. The status can be translated to an error
message by using the adam_appendstatus command (in the Starlink::ADAM
module) or the ems1_get_facility_error command (in the Starlink::EMS
module).

=head2 B<Timeouts>

The timeout period (in seconds) is controlled by the
$Starlink::ADAMTASK::TIMEOUT variable. 


=head2 B<Messages>

By default error messages are sent to STDERR and all other messages
are sent to STDOUT.

Two variables are supplied that can be used to control the printing
of messages:

     $msg_hide - if false (default) then all (non-error) inform messages
                are printed to standard output.

     $err_hide - if false (default) then all error messages are
                 printed to standard error.

Two additional variables are supplied to control the filehandle
on which to write these messages. $ERRHAND is used for error messages
(default STDERR) and $MSGHAND used for normal messages (default
STDOUT)

=head2 B<Handling Parameter requests>

When the program receives a request for a parameter value (PARAMREQ)
the programmer must supply the parameter value and return it via a
PARAMREP message. Whilst the message handling for this is performed
automatically a subroutine must be supplied by the programmer that
returns the parameter value to the module.

The reference to this subroutine should be stored in the variable
$Starlink::ADAMTASK::PARAMREP_SUB. A simple subroutine is supplied by
default (that just asks for the parameter value on the command line)
and the variable contains this reference on startup. The subroutine
takes three arguments (the parameter name, prompt string and default
value) and should return the required value.

=head2 B<valgrind>

This module supports running the monoliths through the valgrind
debugger. To do so, two environment variables are supplied:
PERLAMS_VALGRIND_MONS and PERLAMS_VALGRIND_OPTS. PERLAMS_VALGRIND_MONS
is a comma-separated list of monoliths that you wish to run through
valgrind. If you wish to run all monoliths through valgrind, set this
environment variable to '*'. PERLAMS_VALGRIND_OPTS lets you set
valgrind options. Note that the "--log-file" option is already set so
that log files will be of the form "valgrind.<monolith>.<pid>" in the
process's current working directory.

As an example, to trace open filehandles using sixteen levels of
callers in the kappa_mon monolith, one would set the
PERLAMS_VALGRIND_MONS environment variable to "kappa_mon", and the
PERLAMS_VALGRIND_OPTS environment variable to "--track-fds=yes
--num-callers=16". This will result in a log file in the process's
current working directory of the form "valgrind.kappa_mon.<pid>",
where <pid> is the process ID number.

=head1 NOTES

 o Monoliths do take a while to start up so you may have to insert a
   'sleep 1' between loading the monoliths and sending obeys.  (If you
   access the monolith too quickly then a common error message is:
   MSP__NOTFOUND: rendezvous file not found) The adam_path command (in
   Starlink::ADAM) can be used to see whether the monolith is
   accepting messages.

 o get sometimes returns values that contain '@' symbols (eg 
   parameters for devices and NDFs). Make sure these are stripped before
   passing them to obeyw since perl will eval an '@' as an array.

 o Check status after every obey but make sure that the system is exited
   cleanly (via adamtask_exit) if you decide to stop early because of
   bad status. An exit handler is invoked automatically (via an END{}
   block) so that adam messaging will be shut down during normal
   exit of perl.

 o Monoliths can be started by other processes and accessed from the
   current program so long as the identifying name of the monolith
   is known.

=head1 AUTHOR

Tim Jenness E<lt>t.jenness@jach.hawaii.eduE<gt>

=head1 COPYRIGHT

Copyright (C) Particle Physics and Astronomy Research Council 1997-2000.
All Rights Reserved

=head1 ACKNOWLDEGMENTS

This module owes a lot to the Tcl implementation (Dennis Kelly and
Dave Terrett, SUN/186).

=head1 REQUIREMENTS

This module uses  IO::Pipe from the perl distribution and Proc::Simple
(available from CPAN).

The Starlink::ADAM and Starlink::EMS modules must be installed in
order to use this module.

=head1 See Also

L<perl>,
L<Starlink::AMS::Task>,
L<Starlink::AMS::Init>,
L<Starlink::ADAM>,
and L<Starlink::EMS>

=cut
