package Starlink::AMS::Init;

=head1 NAME

Starlink::AMS::Init - perl module to set up ADAM messaging communications

=head1 SYNOPSIS

  use Starlink::AMS::Init;

  $ams = new Starlink::AMS::Init(1);

  $ams->messages(0);
  $ams->errors(1);
  $ams->timeout(30);

=head1 DESCRIPTION

This module can be used to initialise the ADAM messaging system
and control the behaviour of it. Control of timeout length and
where/whether to print errors and standard output messages can
be controlled. The messaging system is automatically shutdown
when the object is destroyed. Note that only one instance
of a Starlink::AMS::Init object can exist at any one time.

=head1 METHODS

The following methods are available:

=over 4

=cut

use vars qw/$VERSION $AMSRUNNING $debug $NOBJECTS/;

$VERSION = undef;
$VERSION = '0.01';
$debug = 0;

use IO::Pipe;         # Open pipe to RELAY
use Starlink::ADAM;   # We do some communication

# Link with core so that we can set variables in there
# that are global to the messaging system
# The main problem is that we are using this object to control
# global features of the messaging system but these are features
# also required by the ::Task module that does the obeyw.
# This would not be a problem if the task module knew about the
# Init module (ie including ::Task automatically started ::Init).
# Unfortunately there is a requirement for this object to be
# available outside ::Task (ie in main) so that messages
# and errors can be turned on and off, so I am setting it up
# so that the messaging system must still be initialised by
# the user so that they get the object. This means that ::Task
# becomes an OO interface to Core and that ::Init must access 
# variables in the Core namespace.
# I don't like it but I can't think of a way to allow both the user 
# and the tasks to know about the Init object.
# As an aside I have to have ::Init as an object so that it will
# kill itself on destroy.

use Starlink::AMS::Core; 

# Initialise the AMSRUNNING flag
# This variable can be used to make sure that we don't try to start
# up more than one ADAM environment.

$AMSRUNNING = 0;

# Keep track of the number of objects that are created.
# We only want to shut down the message system when this
# count hits 0

$NOBJECTS = 0;

use strict;
use Carp;

=item new

Create a new instance of Starlink::AMS::Init.
If a true argument is supplied the messaging system is also
initialised via the init() method.

=cut



sub new {

  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $task = {};  # Anon hash

  $task->{Running} = undef;       # Just a flag to see if we are runnning
  $task->{Relay}   = undef;       # File handle to the Relay
  $task->{Relay_Name} = undef;    # The name of the relay
  $task->{Relay_Path} = undef;    # The path to the relay
  $task->{Relay_Messid} = undef;  # The message id of the relay
  $task->{Messages} = 1;          # Print all messages
  $task->{Errors} = 1;            # Print all errors
  $task->{ParamRepSub} = undef;   # Code reference for all
  $task->{Timeout} = 30;          # Timeour time
  $task->{StdOut} = *STDOUT;     # Default filehandle for messages
  $task->{StdErr} = *STDERR;     # Default filehandle for Errors

  # Bless this ams into class
  bless($task, $class);

  # Allow for the messaging system to be initiated automatically
  # if a true  argument '1' is passed
  if (@_) {
    my $value = shift;
    if ($value) {
      my $status = $task->init;
      my $errhand = $task->stderr;
      print $errhand "Error initialising messaging system\n"
	unless $status == &Starlink::ADAM::SAI__OK;
    }
  }

  # Increment object count
  $NOBJECTS++;

  # Return to caller
  return $task;
};


# Methods to access instance data

# Set the running flag
sub running {
  my $self = shift;
  if (@_) { $self->{Running} = shift; }
  return $self->{Running};
}

# Get/Set the relay file handle

sub relay {
  my $self = shift;
  if (@_) { $self->{Relay} = shift; }
  return $self->{Relay};
}

# The relay name
sub relay_name {
  my $self = shift;
  if (@_) { $self->{Relay_Name} = shift; }
  return $self->{Relay_Name};
}

# Path to relay
sub relay_path {
  my $self = shift;
  if (@_) { $self->{Relay_Path} = shift; }
  return $self->{Relay_Path};
}

# Messid of relay
sub relay_messid {
  my $self = shift;
  if (@_) { $self->{Relay_Messid} = shift; }
  return $self->{Relay_Messid};
}

# Messages on or off

=item messages

Method to set whether standard messages returned from monoliths
are printed or not. If set to true the messages are printed
else they are ignored.

  $current = $ams->messages;
  $ams->messages(0);

Default is to print all messages.

=cut


sub messages {
  my $self = shift;
  if (@_) { 
    $self->{Messages} = shift; 
    # also need to set the flag in the Core module
    # This is the inverse to Messages since
    # true means I want messages.
    if ($self->{Messages}) {
      $Starlink::AMS::Core::msg_hide = 0;
    } else {
      $Starlink::AMS::Core::msg_hide = 1;
    }

  }
  return $self->{Messages};
}

=item errors

Method to set whether error messages returned from monoliths
are printed or not. If set to true the errors are printed
else they are ignored.

  $current = $ams->errors;
  $ams->errors(0);

Default is to print all messages.

=cut


# Errors on or off

sub errors {
  my $self = shift;
  if (@_) { 
    $self->{Errors} = shift; 
    # also need to set the flag in the Core module
    # This is the inverse to Messages since
    # true means I want messages.
    if ($self->{Errors}) {
      $Starlink::AMS::Core::err_hide = 0;
    } else {
      $Starlink::AMS::Core::err_hide = 1;
    }

  }
  return $self->{Messages};
}

# Set the timeout

=item timeout

Set or retrieve the timeout (in seconds) for some of the ADAM messages.
Default is 30 seconds.

  $ams->timeout(10);
  $current = $ams->timeout;

=cut


sub timeout {
  my $self = shift;
  if (@_) { 
    $self->{Timeout} = shift; 
    # also need to set the flag in the Core module
    $Starlink::AMS::Core::TIMEOUT = $self->{Timeout};
  }
  return $self->{Timeout};
}

# Set the filehandle for error messages. Default is STDERR

=item stderr

Set and retrieve the current filehandle to be used for printing
error messages. Default is to use STDERR.

=cut

sub stderr {
  my $self = shift;

  if (@_) {
    $self->{StdErr} = shift; 
    # also need to set the flag in the Core module
    *Starlink::AMS::Core::ERRHAND = $self->{StdErr};
  }

  return $self->{StdErr};

}

# Set the filehandle for normal messages. Default is STDOUT

=item stderr

Set and retrieve the current filehandle to be used for printing
normal ADAM messages. Default is to use STDOUT.

=cut


sub stdout {
  my $self = shift;

  if (@_) {
    $self->{StdOut} = shift; 
    # also need to set the flag in the Core module
    $Starlink::AMS::Core::MSGHAND = $self->{StdOut};
  }

  return $self->{StdOut};
}

# Parameter reqquests

=item paramrep

Set and retrieve the code reference that will be executed if
the parameter system needs to ask for a parameter.
Default behaviour is to call a routine that simply prompts
the user for the required value. The supplied subroutine
should accept three arguments (the parameter name, prompt string and
default value) and should return the required value.

  $self->paramrep(\&mysub);

A simple check is made to make sure that the supplied argument
is a code reference.

Warning: It is possible to get into an infinite loop if you try
to continually return an unacceptable answer.

=cut


sub paramrep {

  my $self = shift;

  if (@_) {
    my $coderef  = shift;
    my $ref = ref($coderef);
    croak "Supplied argument is not a code reference (is $ref)" 
      unless $ref eq "CODE";

    $self->{ParamRep} = $coderef; 
    # also need to set the value in the Core module
    $Starlink::AMS::Core::PARAMREP_SUB = $self->{ParamRep};
  }

  return $self->{ParamRep};


}


#### METHODS #######

=item init

Initialises the ADAM messaging system. This routine should always be
called before attempting to control I-tasks.

A relay task is spawned in order to test that the messaging system
is functioning correctly. The relay itself is not necessary for the
non-event loop implementation. If this command hangs then it is
likely that the messaging system is not running correctly (eg
because the system was shutdown uncleanly - try removing named pipes
from the ~/adam directory).

Starlink Status is returned.

=cut


sub init {

  my $self = shift;
  my $status = &Starlink::ADAM::SAI__ERROR;

  print "AMSRUNNING is $AMSRUNNING\n" if $debug;

  # Start up ADAM as long as one is not already active
  # Should be able to check by trying to contact the relay
  # for now just keep a state variable
  if (! $AMSRUNNING) {

    $status = $self->__adamtask_init;

    # Set up default options
    # Messages on
    $self->messages(1);

    # Errors on
    $self->errors(1);

    # 30 second timeout
    $self->timeout(30);
    
    # If Status is good; set up the running flags
    if ($status == &Starlink::ADAM::SAI__OK) {

      # Set up the running flag
      $self->running(1);

      # Set the AMSRUNNING flag
      $AMSRUNNING = 1;
      print "AMSRUNNING set to true\n" if $debug;
    }

  } else {
    carp "AMS already running. Can not start more than one instance";
  }
  return $status;

}

=item DESTROY

This method shuts down the Adam messaging system, and kills the relay.
Tasks will also die using their own destructors.

This happens on destruction of the messaging object.  (and should only
happen when perl exits or when the messaging system is no longer
required).

=cut


# This is the crucial bit that shuts ams down at the end of the program
sub DESTROY {   
  my $self = shift;

  $NOBJECTS--;

  # No point killing messaging if it was never initialised
  # Also only kill if $NOBJECTS hits zero

  if ($NOBJECTS < 1) {
    my $status = $self->__adamtask_exit if $self->running;
    carp "Error shutting down AMS: Status = $status\n"
      unless $status == &Starlink::ADAM::SAI__OK;
  }

  print "Returning from adamtask_exit\n" if $debug;
}


########### HIDDEN FUNCTIONS #############

# Start up ADAM
# Maybe we should not be using a private version of adamtask_init
# for this. Probably makes sense to use the version in the
# new Starlink::ADAM::Core module.

sub __adamtask_init {

  my ($taskname);

  my $self = shift;

  # See if we have a RELAY running already
  if (defined $self->relay_name) { 
    if (adam_path($self->relay_name) == 1) {
      my $msghand = $self->stdout;
      print $msghand "Relay task is already running\n" if $self->messages;
      return &Starlink::ADAM::SAI__OK;
    }
  }

  # Set the task name
  $taskname = "perl_ams" . $$;

  # Initialise ams using the program name as the task name
  my $status = adam_start $taskname;
  return $status if ($status != &Starlink::ADAM::SAI__OK);

  # Start the relay process
  # Hardwire the location

#  my $relay_dir = "/local/lib/perl5/site_perl/Starlink";
#  my $relay_dir = "/home/timj/perl/Starlink/AdamTask";
  my $relay = "MessageRelay.pl";

#  open (RELAY, "$relay_dir/$relay $taskname |");

  # Create pipe
  my $RELAY = new IO::Pipe;
  $self->relay($RELAY);

  # Relay should be in the current PATH
  $RELAY->reader("$relay $taskname");
  $RELAY->autoflush;

  # Wait for a message from the RELAY
  print "Waiting for relay...(from $RELAY)\n" if $debug;

  my @reply = adam_receive;

  # Status is the 7 member
  $status = $reply[7];
  return $status if ($status != &Starlink::ADAM::SAI__OK);

  print join("::",@reply),"\n" if $debug;

  # Store the path to the relay
  $self->relay_name($reply[1]);
  $self->relay_path($reply[3]);
  $self->relay_messid($reply[4]);

  # Reply to the obey
  print "Replying to OBEY\n" if $debug;
  $status = adam_reply($self->relay_path, $self->relay_messid, 
		       "ACTSTART", $reply[1], "");


  # Print some info
#  print "Name path messid : $RELAY_NAME $RELAY_PATH $RELAY_MESSID\n";

  print "ID: ",$self->relay, $self->relay_path, $self->relay_name, $self->relay_messid,"\n" if $debug;

  return $status;

}




# adamtask_exit
#
#  Routine to shut down the relay and the messaging system in general
#     - Hidden from outside world

sub __adamtask_exit {

  my $self = shift;

  # Ask the relay to kill itself
  print "Ask the relay to kill itself\n" if $debug;
  print "ID: ",$self->relay, $self->relay_path, $self->relay_name, $self->relay_messid,"\n" if $debug;

  my $status = adam_reply($self->relay_path, $self->relay_messid, "SYNC", 
			  "", "adam_exit; exit") ;

  carp "Error shutting down message relay" 
    if ($status != &Starlink::ADAM::SAI__OK);

  #    if adam_path($self->relay_path);

  print "Killing the pipe\n" if $debug;
  # Remove the pipe
  $self->relay(undef);

  undef $self->{RELAY};

  print "Exit ams\n" if $debug;
  # Exit AMS
  adam_exit if $self->running;

  # Reset adam_started
  $self->running(0);

  print "Resetting AMSRUNNING\n" if $debug;
  $AMSRUNNING = 0;

  return $status;

}



1;

=back

=head1 AUTHOR

Tim Jenness (t.jenness@jach.hawaii.edu).
Copyright (C) 1998.

=head1 REQUIREMENTS

The C<Starlink::AMS::Core> and C<Starlink::AMS::Task>
modules must be installed in order to use this
module.

=head1 See Also

L<perl>, 
L<Starlink::AMS::Task>,
L<Starlink::AMS::Core>,
L<Starlink::ADAM>,
and L<Starlink::EMS>

=cut
