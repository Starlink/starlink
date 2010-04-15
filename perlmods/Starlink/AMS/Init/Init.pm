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

use strict;
use Carp;

use base qw/Exporter/;  # So that version checking is available

use vars qw/$VERSION $AMSRUNNING $debug $NOBJECTS $AMS_OBJECT/;

$VERSION = '1.00';
$debug = 0;
$AMS_OBJECT = undef;

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



=item new

Create a new instance of Starlink::AMS::Init.
If a true argument is supplied the messaging system is also
initialised via the init() method.

=cut



sub new {

  my $proto = shift;
  my $class = ref($proto) || $proto;

  # Check whether we currently have an AMS object
  return $AMS_OBJECT if defined $AMS_OBJECT;

  # Could be done with a scalar object
  my $task = {};  # Anon hash
  $task->{Running}  = 0;
  $task->{Messages} = undef;

  # Bless this ams into class
  bless($task, $class);

  # Allow for the messaging system to be initiated automatically
  # if a true  argument '1' is passed
  if (@_) {
    my $value = shift;
    if ($value) {
      my $status = $task->init;

      unless ($status == &Starlink::ADAM::SAI__OK) {
	my $errhand = $task->stderr;
	print $errhand "Error initialising messaging system\n";
	return undef;
      }
    }
  }

  # Set the object name
  $AMS_OBJECT = $task;

  # Return to caller
  return $task;
};


# Methods to access instance data

sub running {
  my $self = shift;
  if (@_) { $self->{Running} = shift; };
  return $self->{Running};
}


# Get/Set the relay file handle

sub relay {
  my $self = shift;
  if (@_) { $Starlink::AMS::Core::RELAY = shift; }
  return $Starlink::AMS::Core::RELAY;
}

# The relay name
sub relay_name {
  my $self = shift;
  if (@_) { $Starlink::AMS::Core::RELAY_NAME = shift; }
  return $Starlink::AMS::Core::RELAY_NAME;
}

# Path to relay
sub relay_path {
  my $self = shift;
  if (@_) { $Starlink::AMS::Core::RELAY_PATH = shift; }
  return $Starlink::AMS::Core::RELAY_PATH;
}

# Messid of relay
sub relay_messid {
  my $self = shift;
  if (@_) { $Starlink::AMS::Core::RELAY_MESSID = shift; }
  return $Starlink::AMS::Core::RELAY_MESSID;
}

# Messages on or off

=item messages

Method to set whether standard messages returned from monoliths
are printed or not. If set to true the messages are printed
else they are ignored.

  $current = $ams->messages;
  $ams->messages(0);

Default is to print all messages.

If messages() has not been previously set, calling it will return
undef. When messages() is in this state it will act as if it were set
to the default of printing all messages.

=cut


sub messages {
  my $self = shift;
  if (@_) {
    my $arg = shift;
    # also need to set the flag in the Core module
    # This is the inverse to Messages since
    # true means I want messages.
    if ($arg) {
      $Starlink::AMS::Core::msg_hide = 0;
      $self->{Messages} = $arg;
    } else {
      $Starlink::AMS::Core::msg_hide = 1;
      $self->{Messages} = 0;
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
    my $arg = shift;
    # also need to set the flag in the Core module
    # This is the inverse to Messages since
    # true means I want messages.
    if ($arg) {
      $Starlink::AMS::Core::err_hide = 0;
    } else {
      $Starlink::AMS::Core::err_hide = 1;
    }

  }
  return $Starlink::AMS::Core::err_hide;
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
    # also need to set the flag in the Core module
    $Starlink::AMS::Core::TIMEOUT = shift();
  }
  return $Starlink::AMS::Core::TIMEOUT;
}

# Set the filehandle for error messages. Default is STDERR

=item stderr

Set and retrieve the current filehandle to be used for printing
error messages. Default is to use STDERR.

=cut

sub stderr {
  my $self = shift;

  if (@_) {
    # also need to set the flag in the Core module
    $Starlink::AMS::Core::ERRHAND = shift;
  }

  return $Starlink::AMS::Core::ERRHAND;

}

# Set the filehandle for normal messages. Default is STDOUT

=item stdout

Set and retrieve the current filehandle to be used for printing
normal ADAM messages. Default is to use STDOUT.

=cut


sub stdout {
  my $self = shift;

  if (@_) {
    # also need to set the flag in the Core module
    $Starlink::AMS::Core::MSGHAND = shift;
  }

  return $Starlink::AMS::Core::MSGHAND;
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

    $Starlink::AMS::Core::PARAMREP_SUB = $coderef;
  }

  return $Starlink::AMS::Core::PARAMREP_SUB;


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

  # Start up ADAM as long as one is not already active
  # Should be able to check by trying to contact the relay
  # for now just keep a state variable
  unless ($Starlink::AMS::Core::adam_started) {
    print "Running init adam\n" if $debug;

    # Run the init routine
    $status = &Starlink::AMS::Core::adamtask_init;

    # If Status is good; set up the running flags
    if ($status == &Starlink::ADAM::SAI__OK) {

      # Set up the running flag
      $self->running(1);

      # Set up default options
      # Messages on
      $self->messages(1);

      # Errors on
      $self->errors(1);

      # 30 second timeout
      $self->timeout(30);

    }
    return $status;
  }
  return &Starlink::ADAM::SAI__OK;
}

=item shutdown

This method forces the Adam messaging system to be shutdown.
(It runs the adamtask_exit routine in Starlink::AMS::Core).

Returns the status.

=cut

sub shutdown {
  my $self = shift;

  my $status = &Starlink::AMS::Core::adamtask_exit;

  # Reset adam_started
  $self->running(0);

  return $status;
}

=back

=head1 AUTHOR

Tim Jenness (t.jenness@jach.hawaii.edu).

=head1 COPYRIGHT

Copyright (C) 2009 Science and Technology Facilities Council.
Copyright (C) Particle Physics and Astronomy Research Council 1998, 1999.
All Rights Reserved.

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

1;
