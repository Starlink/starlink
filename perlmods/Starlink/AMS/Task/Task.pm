package Starlink::AMS::Task;

=head1 NAME

Starlink::AMS::Task - perl module to load and communicate with ADAM monoliths

=head1 SYNOPSIS

  use Starlink::AMS::Task;

  $kappa = new Starlink::ADAMTASK("name","monolith_image");
  $status = $kappa->obeyw("task", "params");
  $status = $kappa->set("task:param","value");
  ($value, $status) = $kappa->get("task:param");
  $dir = $kappa->control("default","dir");
  $kappa->control("par_reset");

=head1 DESCRIPTION

This module provides commands for communicating with ADAM tasks via
the ADAM messaging system (AMS). The AMS communications themselves are
handled via lower level modules (C<Starlink::AMS::Core> and 
C<Starlink::ADAM>).

It is presumed that the messaging system has already been initialised
by a call to C<Starlink::AMS::Init>. This is necessary before communications
can proceed.

=head1 METHODS

The following methods are available:

=over 4

=cut

use strict;
use Carp;

use vars qw/$VERSION/;

# Access the Core functions
use Starlink::AMS::Core qw/:Func/;
use Starlink::ADAM qw/:adam/;

$VERSION = undef;
$VERSION = '0.01';



=item new()

Create a new instance of a Starlink::AMS::Task object.

  $obj = new Starlink::AMS::Task;
  $obj = new Starlink::AMS::Task("name_in_message_system","monolith");

If supplied with arguments (matching those expected by load() ) the
specified task will be loaded upon creating the object. If the load()
fails then undef is returned (which will not be an object reference).

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
  if (@_) { 
    my $status = $task->load(@_); 
    # If the load failed I think we should return undef rather
    # than an object
    if ($status != 0) {
      return undef;
    }

  };
 
 
  return $task;
}
 

# Methods to access the "instance" data
#
# With args they set the values
# Without args they only retrieve values

=item name

The name used by the messaging system to contact the monolith.
Can be used to set or retrieve the value of name (do not set
this value from outside unless you know that a path to the 
new name exists in the messaging system).

  $name = $obj->name;
  $name = $obj->name("name")

=cut
 
sub name {
  my $self = shift;
  if (@_) { $self->{Name} = shift; }
  return $self->{Name};
}

=item pid

Returns the process object (derived from a Proc::Simple object)
of the monolith if the monolith was started by this object.
This can be used to query the process ID and to check that
the process associated with the monolith still exists.

  $proc = $obj->pid;

=cut
 
sub pid {
  my $self = shift;
  if (@_) { $self->{PID} = shift; }
  return $self->{PID};
}
 
# Not actually used

sub running {
  my $self = shift;
  if (@_) { $self->{Running} = shift; }
  return $self->{Running};
}
 

# I could set up a desctructor here to make the monolith
# die before perl exits.
# Will try it this way
# Especially now that ::Core does not try to kill the monolith
# itself.

=item DESTROY

Destructor for this object. When the last reference to this
object is removed the destructor is exectued automatically.
In this case the monolith attached to object is killed 
unless it is already dead(!) or it has been forgotten
via the forget() method.

=cut


sub DESTROY {
  my $self = shift;
  if (defined $self->pid) {
    # Cant use the auto destructor at this point since other
    # references to the object may exist
    # must use my knowledge of the objects methods
    if ($self->pid->kill_on_destroy) {
#      print "Killling ".$self->name ."\n";
      $self->pid->kill;
    }
  }

}


# Methods to actually do things
 
# Load
#   - Load a monolith and set up the name in the AMS

=item load

Load a monolith and set up the name in the messaging system.
This task is called by the 'new' method.

  $status = $obj->load("name","monolith_binary");

If the second argument is omitted it is assumed that the binary
is already running and can be called by "name".   

If a path to a binary with name "name" already exists then the monolith
is not loaded.

=cut


sub load {
 
  my $self = shift;
  my $status = 0;  # A good status
 
  # First argument is the name of the system so set that in the object
  $self->name(shift);
 
  # A further argument (optional) will be the monolith name
  if (@_) { 
 
    # If we can already talk to a monolith via this path then
    # we should not start a new monolith
 
    unless ($self->contact) {
      my ($process, $status) = adamtask($self->name(), shift ); 
 
      # Store the PID object (may be undefined if not spawned by us)
      # if good status?
      $self->pid($process);
   }
 
  };
 
  return $status;

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
  my $parameters = shift | " ";
 
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
 
  # Set the flag on the object so that it doesnt die on
  # undef
  $self->pid->kill_on_destroy(0);
 
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
and a '0' if we timed out. It will timeout if it takes longer than
specified in Starlink::AMS::Init->timeout.

=cut

sub contactw {
 my $self = shift;
 
 # Set the current time
 my $start = time();

 # Don't like accessing variables directly in another namespace
 # Would be nice if I knew the object associated with the messaging
 # system(::Init).
 my $timeout = $Starlink::AMS::Core::TIMEOUT;
 my $err_hide = $Starlink::AMS::Core::err_hide;
 my $stderr   = *Starlink::AMS::Core::ERRHAND;

 # Check
 while (! $self->contact) {
   if ((time() - $start) > $timeout) { 
      print $stderr "Timed out whilst trying to contact monolith ".
	$self->name."\n" 
	  unless $err_hide;
      return 0;
   }
   sleep 0.2;
 }
 
 # Must be okay
 return 1;
 
}
 

=back

=head1 AUTHOR

Tim Jenness (t.jenness@jach.hawaii.edu).
Copyright (C) 1998.

=head1 REQUIREMENTS

The C<Starlink::AMS::Core> and C<Starlink::AMS::Init>
modules must be installed in order to use this
module.

=head1 See Also

L<perl>, 
L<Starlink::AMS::Core>,
L<Starlink::AMS::Init>,
L<Starlink::ADAM>,
L<Starlink::EMS>, and L<Proc::Simple>.

=cut



1; 
 
