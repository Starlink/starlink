package Starlink::ADAMTASK;

=head1 NAME

Starlink::ADAMTASK - perl module that provides control of ADAM I-tasks

=head1 SYNOPSIS

 use Starlink::ADAMTASK;
 use Starlink::ADAMTASK qw/:Func/;

 adamtask_init;

 adamtask("name","monolith_image");

 adamtask_set("name", "task:param","value");
 adamtask_obeyw("name", "task", "params");
 $value = adamtask_get("name", "task:param");
 adamtask_control("name","default","dir");
 adamtask_control("name","par_reset");

 $kappa = new Starlink::ADAMTASK("name","monolith_image");
 $kappa->obeyw("task", "params");
 $kappa->set("task:param","value");
 $value = $kappa->get("task:param");
 $dir = $kappa->control("default","dir");
 $kappa->control("par_reset");

 adamtask_exit;

=head1 DESCRIPTION

This module provides commands for communicating with ADAM tasks via
the ADAM messaging system (AMS). The AMS communications themselves are
handled via a lower level module (Starlink::ADAM).

Commands are provided for starting ADAM monoliths, for controlling them,
and for finding the states of parameters.

An object oriented interface is provided as well as function calls.
The function calls are not exported by default. The 'Func' tag
must be specified if the non-OO implementation is required.

=cut

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

my ($RELAY, $RELAY_NAME, $RELAY_PATH, $RELAY_MESSID, %PIDS, %TASKS,
   $adam_started, $TIMEOUT);



use vars qw/$ADAM_STATUS $PARAMREP_SUB $msg_hide $err_hide $RELAY_NAME/;

$RELAY_NAME = undef;

$msg_hide = 0;
$err_hide = 0;

$adam_started = 0;
$ADAM_STATUS = &Starlink::ADAM::SAI__OK;

# Default timeout is 30 seconds
$TIMEOUT = 30;

=head1 METHODS

 For more detailed descriptions of the methods see the corresponding
entries for the function calls.

=over 4

=cut


 
use strict;
use Carp;
use vars qw($VERSION @ISA @EXPORT %EXPORT_TAGS $AUTOLOAD);

require Exporter;
 
@ISA = qw(Exporter);
 
# This module requires the Starlink::ADAM module so that we can access
# the ADAM messaging system
 
use Starlink::ADAM qw/:adam/;

# Use the IO routines to create the pipe handle
use IO::Pipe;

# Use the Proc module to start the monoliths and keep track of process IDs
use Proc::Simple;

# Ask for parameters with readline
#use Term::ReadLine;
 
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
 
$VERSION = '0.01';

############# Methods #################################

# These are the methods for the OO implementation of the
# ADAMTASK module.

# The object contains:
#       $task->{Name}    = name of the monolith in the AMS
#       $task->{PID}     = process ID of spawned monolith
#       $task->{Running} = flag to indicate whether we can talk to a monolith

#                          Currently this is "Running" is not implemented
#                          since it would require a background process
#                          to determine when the monolith is available

=item new 

  Create a new instance of a Starlink::ADAMTASK object.

  $obj = new Starlink::ADAMTASK;
  $obj = new Starlink::ADAMTASK();

=cut

# NEW - create a new instance of Starlink::ADAMTASK

sub new {

  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $task = {};  # Anon hash

  $task->{Name} = undef;  # Name in AMS
  $task->{PID}  = undef;  # Process ID (object) of monolith
  $task->{Running} = 0;   # Is the monolith contactable

  # Bless task into class
  bless($task, $class);

  # If we have arguments then we are trying to do a load
  # as well
  if (@_) { $task->load(@_); };


  return $task;
}

# Methods to access the "instance" data
#
# With args they set the values
# Without args they only retrieve values

sub name {
  my $self = shift;
  if (@_) { $self->{Name} = shift; }
  return $self->{Name};
}

sub pid {
  my $self = shift;
  if (@_) { $self->{PID} = shift; }
  return $self->{PID};
}

sub running {
  my $self = shift;
  if (@_) { $self->{Running} = shift; }
  return $self->{Running};
}



# Methods to actually do things

# Load
#   - Load a monolith and set up the name in the AMS

=item load
 
   Load a monolith and set up the name in the messaging system.
   This task is called by the 'new' method.

     $obj->load("name","monolith_binary");

   If the second argument is omitted it is assumed that the binary
   is already running and can be called by "name".   

   If a path to a binary with name "name" already exists then the monolith
   is not loaded.

=cut


sub load {

  my $self = shift;

  # First argument is the name of the system so set that in the object
  $self->name(shift);

  # A further argument (optional) will be the monolith name
  if (@_) { 

    # If we can already talk to a monolith via this path then
    # we should not start a new monolith

    unless ($self->contact) {
      adamtask($self->name(), shift ); 

    # Store the PID
      $self->pid($PIDS{$self->name()});
   }

  };

}



# OBEYW
#
#  $task->obeyw(action, parameters)

=item obeyw

   Send an obey to a task and wait for a completion message

    $status = $obj->obeyw("action","params");

=cut


sub obeyw {

  my $self = shift;
  my $action = shift;    # This is the action (eg stats)
  my $parameters = shift;

  # Now do the obeyw
  my $status = adamtask_obeyw($self->name(), $action, $parameters);

}

# GET
#
#  $value = $task->get(param)

=item get

  Obtain the value of a parameter.

   ($value, $status) = $obj->get("task:param");

=cut


sub get {

  my $self = shift;
  my $param = shift;

  my $value = undef;
  my $status = undef;

  # Now do the get
  ($value, $status) = adamtask_get($self->name(), $param);

  return $value, $status;
}

# SET
#
#  $task->set(param, value)

=item set

  Set the value of a parameter in a task.

  $status = $obj->set("action:param","value");

=cut


sub set {

  my $self = shift;
  my $param = shift;
  my $value = shift;
  
  # Now do the set
  my $status = adamtask_set($self->name(), $param, $value);

}

# CONTROL
#
#  $task->control(type, dir)
#   where type can be 'par_reset' or 'default'
#   and dir is optionally the new value for 'default'

=item control

  Send control messages to a monolith.

  $obj->control("par_reset");
  ($value, $status)  = $obj->control("default","dir");



=cut



sub control {

  my $self = shift;

  my $type = shift;
  my $dir  = shift;

  my ($value, $status) = adamtask_control($self->name(), $type, $dir);

  return $value, $status;
}


=item forget

  Remove the monolith name from the list of known monoliths.
  This method prevents the monolith from being killed when adam
  exits or when the object is destroyed.

  This routine has no input arguments. 

=cut

sub forget {

  my $self = shift;

  # Remove it from the global PID list
  adamtask_forget($self->name());

  # Undef the process name in the object
  $self->{PID} = undef;

}


=item contact

   This method can be used to determine whether the object can
   contact a monolith. Returns a 1 if we can contact a monolith and
   a zero if we cant.

=cut

sub contact {
  my $self = shift;

  # Try to talk to the monolith
  adam_path $self->name;
}

=item contactw

 This method will not return unless the monolith can be contacted.
It only returns with a timeout. Returns a '1' if we contacted okay
and a '0' if we timed out. Makes use of the Starlink::ADAMTASK::TIMEOUT
variable (in seconds).

=cut

sub contactw {
 my $self = shift;

 # Set the current time
 my $start = time();

 # Check
 while (! $self->contact) {
   if ((time() - $start) > $TIMEOUT) { 
      print STDERR "Timed out whilst trying to contact monolith" 
	unless $err_hide;
      return 0;
   }
 }

 # Must be okay
 return 1;

}



=item DESTROY

  This is the object destructor. When the final 
  reference to the object is removed this routine is run
  to make sure that the messaging system tidies up properly
  (eg by killing the monolith).

=cut

# Cant destroy until I can find out how to kill monoliths without leaving
# the message system in a complete hang up

#sub DESTROY {
#
#  my $self = shift;
#
#  # Kill the process
#  if (defined $self->pid) {
#    $self->pid->kill();
#    print "Shutting down " . $self->name ."\n" unless $msg_hide;
#  }
#
#  }


############# Subroutines #############################



=head1 FUNCTIONS


=cut

=item adamtask_init

Initialises the ADAM messaging system. This routine should always be
called before attempting to control I-tasks.

A relay task is spawned in order to test that the messaging system
is functioning correctly. The relay itself is not necessary for the
non-event loop implementation. If this command hangs then it is
likely that the messaging system is not running correctly (eg
because the system was shutdown uncleanly - try removing named pipes
from the ~/adam directory).

=cut

sub adamtask_init {

  my ($taskname);

  # See if we have a RELAY running already
  if ($RELAY_NAME =~ /./) { 
    if (adam_path($RELAY_NAME) == 1) {
      print "Relay task is already running\n" unless $msg_hide;
      return;
    }
  }

  # Set the task name
  $taskname = "perl_ams" . $$;

  # Initialise ams using the program name as the task name
  adam_start $taskname;

  # Set a global variable that tells me I have started ams
  # need this to protect against exit

  $adam_started = 1;


  # Start the relay process
  # Hardwire the location

  my $relay_dir = "/local/lib/perl5/site_perl/Starlink/";
  my $relay = "MessageRelay.pl";

#  open (RELAY, "$relay_dir/$relay $taskname |");

  # Create pipe
  $RELAY = new IO::Pipe;
  $RELAY->reader("$relay_dir/$relay $taskname");
  $RELAY->autoflush;

  # Wait for a message from the RELAY
#  print "Waiting for relay...(from $RELAY)\n";

  my @reply = adam_receive;
#  print join("::",@reply),"\n";

  # Store the path to the relay

  $RELAY_NAME = $reply[1];
  $RELAY_PATH = $reply[3];
  $RELAY_MESSID = $reply[4];

  # Reply to the obey
#  print "Replying to OBEY\n";
  adam_reply($RELAY_PATH, $RELAY_MESSID, "ACTSTART", $reply[1], "");

  # Need to setup an exit handler 


  # Print some info
#  print "Name path messid : $RELAY_NAME $RELAY_PATH $RELAY_MESSID\n";
  
}


# adamtask_message
#
#   Routine to process incoming messages
#    Arguments: message
#   Routine does not read from the PIPE. It relies on another process
#   to pass the text from the pipe to this routine

sub adamtask_message {

  my $message = shift;

  my ($command, $path, $messid);

  # Split the message on the separator
  my @message = split(/::/, $message);

  $command = $message[0];
  $path    = $message[3];
  $messid  = $message[4];

  # Evaluate the command here (need to think about this)

  # If the command is endmsg or setrepsonse
  # then we have ended a transaction so return
  # Also return if we are using the internal message of 'badstatus'

  return if $command =~ /endmsg|setresponse|badstatus/;

  # If this is a control response then we should print the directory
  # but only if we are sending a 'default' action. 'par_reset' returns nothing 
  $command eq "controlresponse" && do {
    print "$message[2] = $message[6]\n" 
      if ($message[2] eq "default" & !$msg_hide);
    return $message[6];
  };


  # If this is a getresponse then return the parameter value
  $command eq "getresponse" && do {
    return $message[6];
    
  };

  # Just print inform messages
  $command eq "inform" && do {

    # Check for error messages
    if ($message[6] =~ /^!/) {
      print STDERR "$message[6]\n" unless $err_hide;
    } else {
      print "$message[6]\n" unless $msg_hide;
    }
    return;
  };

  # Need to ask for a parameter if we have a paramreq
  $command eq "paramreq" && do {
    
    # Have to split the paramreq into parts
    # Split on the newline character (that was added by Starlink::ADAM)
  
    my (@bits) = split(/\n/,$message[6]);

    my $value = &$PARAMREP_SUB(@bits);

    # Now need to send this back to the message
    adam_reply($path, $messid, "PARAMREP", "", $value);

  };

}

=item adamtask($name, $image)

This command is used to start adam tasks. $name is the name of the
task in the messaging system (and should be used for all future
accesses to the task) and $image is the name of the image file to be
spawned (eg /star/bin/kappa/kappa_mon to load the basic KAPPA
monolith).

=cut

# adamtask
#
#  Usage: $command = adamtask <name> <file>
#       where <name> is the name of the task and <file> is the
#       name of the image file. If <file> is supplied the adam task
#       is spawned, otherwise it is assumed to be already running.

sub adamtask {

  # Arguments
  my ($taskname, $image) = @_;

  # Variables
  my ($adam_task_type, $adam_task_type_set, $icl_task_name, 
      $icl_task_name_set);

  if ($image =~ /./) {

    # Set the type of all adam tasks started to 'I'
    # but taking care that we keep track of what the Env variable
    # was set to
    if (exists $ENV{ADAM_TASK_TYPE}) {
      $adam_task_type = $ENV{ADAM_TASK_TYPE};
      $adam_task_type_set = 1;
    }
    $ENV{ADAM_TASK_TYPE} = 'I';
    
    # Set the message system name as well
    if (exists $ENV{ICL_TASK_NAME}) {
      $icl_task_name = $ENV{ICL_TASK_NAME};
      $icl_task_name_set = 1;
    }
    $ENV{ICL_TASK_NAME} = $taskname;
    
    # Now execute the task in this new environment
    # and store the object.
    # Cant use a 'system' call since this does not return the PID
    # of the process. Use the Proc::Simple module instead.

#    print "Starting $image...";
    my $pid = Proc::Simple->new();
    my $status = $pid->start("$image");
#    print "with status = $status (1 is good))\n";

    if ($status == 0 ) {
      print STDERR "Error starting $image\n" unless $err_hide;
      $ADAM_STATUS = &Starlink::ADAM::SAI__ERROR;

    } else {

      # Set good status and store the PID
      $ADAM_STATUS = &Starlink::ADAM::SAI__OK;

      # Store this object in the hash array with taskname as the key
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

  return $ADAM_STATUS;

}

# adamtask_send
#
#  Sends a command to be executed by the relay process and returns the
#  result. The command is made up of a perl command plus arguments
#

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
#     This does not use the relay
#     directly from here.

sub adamtask_sendw {

  my $command = shift;

  my ($reply, @reply, $response);

  # First I need to send the command
  # Since we are evalling an adam_send we should receive the 
  # path and messid to the remote task.
  # This allows us to await the return from a specific task and not
  # get confused by other messages

  # The assumption is that the command to be evaluated is always a
  # adam_send.

  my (@returns) = eval $command;

  return if ($@ ne "");

  return if $returns[2] != &Starlink::ADAM::SAI__OK;

  # Now I need to wait for a message to come back (should be actstart)
  # From the task that I just contacted.

  while (1) {

#    @reply = adam_receive;

    # Note that the timeout must be given in milliseconds

    @reply = adam_getreply(1000 * $TIMEOUT, $returns[0], $returns[1]);
    $reply = join("::",@reply);

    # Now need to acknowledge the message if it is a paramreq or something
    # Send the reply off to a generic subroutine

    $response = adamtask_message($reply);

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
    print STDERR "!! $fac"."__$ident: $text\n" unless $err_hide;
  }

  # Record the error status (whether it was from the task or from the message
  # system
  
  if ($reply[0] eq "badstatus") {
    $ADAM_STATUS = $reply[1];
  } else {
      $ADAM_STATUS = $reply[5];
  }


  # Return the parameter value if this is a 'getresponse'
  return ($response,$ADAM_STATUS) 
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
    adamtask_sendw("adam_send(\"$task\",\"$param\",\"SET\",\"$val\")");

}

=item $value = adamtask_get($name, $param)

This routine implements the GET command and can be used to get
the values of parameters in I-tasks.

The parameter name must be given in terms of the action it is related
to in the form: "action:param". For example, to get the input NDF for
stats we can say '$value = adamtask_set("kappa", "stats:ndf")'

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

  my ($result,$status) = adamtask_sendw("adam_send(\"$task\",\"$param\",\"GET\",\"\")");

  return ($result, $status);

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

=cut


# adamtask_control
#
#  Implements the CONTROL command

sub adamtask_control {

  my $task = shift;
  my $command = shift;
  my $params = shift;

  # Reset ADAM_STATUS
  $ADAM_STATUS = &Starlink::ADAM::SAI__OK;

  my ($result, $status) = 
    adamtask_sendw("adam_send(\"$task\",\"$command\",\"CONTROL\",\"$params\")");

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
    adamtask_sendw("adam_send(\"$task\",\"$command\",\"CANCEL\",\"$params\")");

  return $status;
}


=item adamtask_obeyw($name, $action, $params)

This routine can be used to send an OBEY to a task. The parameters
can be specified in $params. Note that this command is implemented
as an OBEYW and so only returns when the action is completed.

For example: adamtask_obeyw("kappa", "stats", "ndf=test"); will run
Kappa stats with NDF=test. If the parameter is not specified in here
the current value set with adamtask_set will be taken. If that is
not available then the user will be prompted for a value on the
command line.

=cut


# adamtask_obeyw 
#
#  - implement an obeyw
#     There is no point using the RELAY for this - just send messages
#     directly from here.

sub adamtask_obeyw {

  my $task = shift;
  my $command = shift;
  my $params = shift;

  my ($reply, @reply);

  # Reset ADAM_STATUS
  $ADAM_STATUS = &Starlink::ADAM::SAI__OK;

  # First I need to send the OBEY to sendw
  my $status = adamtask_sendw("adam_send(\"$task\",\"$command\",\"OBEY\",\"$params\")");

  return $status;

}



=item adamtask_exit

This routine shuts down the Adam messaging system, kills the relay
and kills all ADAM tasks that were started by this perl process
(unless they have been forgotten with adamtask_forget).

This command should always be issued before exiting perl.

=cut

# adamtask_exit
#
#  Routine to shut down the relay
#

sub adamtask_exit {

  my ($pid, $task);

  # Ask the relay to kill itself
#  print "Ask the relay to kill itself\n";
  adam_reply($RELAY_PATH, $RELAY_MESSID, "SYNC", "", "adam_exit; exit") 
    if defined $RELAY_PATH;

#  print "Killing the pipe\n";

  # Remove the pipe
  undef $RELAY;

#  print "Exit ams\n";
  # Exit AMS
  adam_exit if $adam_started;

  # Reset adam_started
  $adam_started = 0;

#  print "Kill some monoliths\n";
#  print "Processes = ", %PIDS, "\n";
  # Kill all the monolith processes
  foreach $task (keys %PIDS) {
#    print "Looping through keys\n";
    $pid = $PIDS{$task};

    if (defined $pid) {
      $pid->kill() if defined $pid;

      print "Killing $task...\n" unless $msg_hide;
    }
  }

  # Now undef that hash
  undef %PIDS;

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

  delete $PIDS{"$task"};

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

Error messages are sent to STDERR and all other messages are sent to
STDOUT.

Two variables are supplied that can be used to control the printing
of messages:

     $msg_hide - if false (default) then all (non-error) inform messages
                are printed to standard output.

     $err_hide - if false (default) then all error messages are
                 printed to standard error.

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

=head1 NOTES

  o Monoliths do take a while to start up so you may have to insert
    a 'sleep 1' between loading the monoliths and sending obeys.
    or use the contactw method, that will return once a connection has
    been made.
    (If you access the monolith too quickly then a common error message
    is: MSP__NOTFOUND: rendezvous file not found)

  o get sometimes returns values that contain '@' symbols (eg 
    parameters for devices and NDFs). Make sure these are stripped before
    passing them to obeyw since perl will eval an '@' as an array.

  o Check status after every obey but make sure that the system is exited
    cleanly (via adamtask_exit) if you decide to stop early because of
    bad status.

  o Monoliths can be started by other processes and accessed from the
    current program so long as the identifying name of the monolith
    is known.

=head1 AUTHOR

Tim Jenness (timj@jach.hawaii.edu)
Copyright (C) 1997.

=head1 ACKNOWLDEGMENTS

This module owes a lot to the Tcl implementation (Dennis Kelly and
Dave Terrett, SUN/186).

=head1 REQUIREMENTS

This module uses  IO::Pipe from the perl distribution and Proc::Simple
(available from CPAN).

The Starlink::ADAM and Starlink::EMS modules must be installed in
order to use this module.

=head1 See Also

  L<perl>, L<Starlink::ADAM>, L<Starlink::EMS>

=cut
