package Starlink::AMS::Task;

=head1 NAME

Starlink::AMS::Task - perl module to load and communicate with ADAM monoliths

=head1 SYNOPSIS

  use Starlink::AMS::Task;

  $kappa = new Starlink::AMS::Task("name","monolith_image");
  $status = $kappa->obeyw("task", "params");
  $status = $kappa->set("task","param","value");
  ($status, $value) = $kappa->get("task","param");
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

use base qw/Exporter/;  # So that version checking is available

use vars qw/$VERSION/;

# NDF required for A-task parameter retrieval
# Only 'use' when we need to
use autouse NDF => qw/ par_get($$$) /;

# Access the Core functions
use Starlink::AMS::Core qw/:Func/;
use Starlink::ADAM qw/:adam/;

$VERSION = '1.00';



=item new()

Create a new instance of a Starlink::AMS::Task object.

  $obj = new Starlink::AMS::Task;
  $obj = new Starlink::AMS::Task("name_in_message_system","monolith");

If supplied with arguments (matching those expected by load() ) the
specified task will be loaded upon creating the object. If the load()
fails then undef is returned (which will not be an object reference).

=cut

# NEW - create a new instance of Starlink::AMS::Task

sub new {

  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $task = {};  # Anon hash

  $task->{Name} = undef;  # Name in AMS
  $task->{PID}  = undef;  # Process ID (object) of monolith
  $task->{Running} = 0;   # Is the monolith contactable
  $task->{TaskType} = 'A'; # Default to A-task
  $task->{Monolith} = undef; # Monolith name
  $task->{AdamDir} = undef; # Location of $ADAM_USER

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


=item tasktype

Return or set the type of task associated with the object.
Should be either 'A' or 'I'.

  $task->tasktype('A');

=cut

sub tasktype {
  my $self = shift;
  if (@_) {
    my $type = uc(shift());
    croak "Adam task of type '$type' is not recognised"
      unless ($type eq 'A' || $type eq 'I');
    $self->{TaskType} = $type;
  }
  return $self->{TaskType};
}


=item monolith

Returns or sets the name of the monolith (not including path).
This is obtained by looking at the second argument to the
load method and is required for A-tasks when performing a parameter
get.

=cut

sub monolith {
  my $self = shift;
  if (@_) { $self->{Monolith} = shift(); }
  return $self->{Monolith};
}

=item adamdir

Retrieve the current value of $ENV{ADAM_USER}. If not defined the
value of $ADAM_USER is stored -- this method is used to store the
value of $ADAM_USER at load time so that the location of any
adam parameter file can be found (for A-tasks).

=cut

sub adamdir {
  my $self = shift;
  unless (defined $self->{AdamDir}) {
    # ADAM_USER can be either $ADAM_USER or $HOME/adam
    if (exists $ENV{'ADAM_USER'}) {
      $self->{AdamDir} = $ENV{'ADAM_USER'};
    } elsif (exists $ENV{HOME}) {
      $self->{AdamDir} = $ENV{'HOME'} .'/adam';
    } else {
      croak 'Neither $ADAM_USER not $HOME environment variables are defined';
    }
  }
  return $self->{AdamDir};
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

  $status = $obj->load("name","monolith_binary",{ TASKTYPE => 'A' });

If the second argument is omitted it is assumed that the binary
is already running and can be called by "name".   

The last argument, if needed, is a reference to a hash containing
the load options. 

Options currently supported are:

  TASKTYPE => 'A' for A-task, 'I' for I-task.
  MONOLITH => monolith name to be used for A-task parameter retrieval

Default is to launch an A-task. Default task type when no monolith
or options are specified is 'I'.

The MONOLITH option can be used to configure A-tasks such that 
they can retrieve parameters from monoliths that were not started by this 
object. (It is identical to creating the object and then setting the
monolith name via the monolith() method)

If a path to a binary with name "name" already exists then the monolith
is not loaded.

=cut


sub load {

  my $self = shift;
  my $status = &Starlink::ADAM::SAI__OK;  # A good status

  # First argument is the name of the system so set that in the object
  $self->name(shift);

  # Store ADAM_USER
  $self->adamdir;

  # A further argument (optional) will be the monolith name
  # and optional hash.
  if (@_) {

    # Set default behaviour of task
    $self->tasktype('A');

    # Check arguments for a hash

    # If the last argument is a hash reference then this may
    # contain the task type

    # Construct our own version of the input arguments
    my @args = ();

    my $options = {};  # Options hash

    if (ref($_[-1]) eq 'HASH') {
      # Okay so there is an options hash so use it
      $options = pop @_;

      # Check tasktype
      if (exists $options->{TASKTYPE}) {
	$self->tasktype($options->{TASKTYPE});
      } else {
	# Okay so TASKTYPE was not defined so we set it
	# to our default value.
	$options->{TASKTYPE} = $self->tasktype;
      }

      # Check monolith name
      $self->monolith($options->{MONOLITH})
	if exists $options->{MONOLITH};

      push(@args, $options); # Store the options

    }


    # Check for any more arguments (the monolith name)

    if (@_) {

      # If we can already talk to a monolith via this path then
      # we should not start a new monolith

      unless ($self->contact) {

	# Get the monolith name (if present)
	if (not ref $_[0]) {
	  # Need to extract the monolith name
	  my $monolith = (split(/\//, $_[0]))[-1];
	  # Store monolith name unless it has already been
	  # stored - eg by the MONOLITH options
	  $self->monolith( $monolith )
	    unless defined $self->monolith;

	  # Store the monolith path and name on arg list
	  unshift @args, $_[0];	
	}

	# Launch the monolith
	my ($process, $status) = adamtask($self->name(), @args ); 

	# Store the PID object (may be undefined if not spawned by us)
	# if good status?
	$self->pid($process);
      }

    }

  } else {
    # Assume I-task behaviour if no arguments
    $self->tasktype('I');
  }

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

  my $parameters = " ";
  if (@_) { $parameters = shift; } 

  # Now do the obeyw
  my $status = adamtask_obeyw($self->name(), $action, $parameters);

}

# GET
#
#  $value = $task->get(task, param)

=item get

Obtain the value of a parameter from a task. 
When called from an array context the status and values are returned
(multiple values are returned in an array). When called from a scalar
context the values are returned as a single string joined by commas.

 ($status, @values) = $obj->get("task","param");

or

  $value = $obj->get("task","param");

If the monolith has been started as an A-task the adam messaging
system can not be used to retrieve the parameter value. Instead,
the par_get routine from the perl NDF module is invoked.

=cut


sub get {

  my $self = shift;

  croak 'Usage: get(task, param)' unless scalar(@_) == 2;

  my $task  = shift;
  my $param = shift;

  my @values = ();
  my $status = &Starlink::ADAM::SAI__OK;

  # Now do the get
  if ($self->tasktype eq 'I') {
    # This is for an I-task

    my $value;

    my $par = $task .':' . $param;
    ($value, $status) = adamtask_get($self->name(), $par);

    # Now need to check whether we have an array return value
    # so that we can disentanlge adam array syntax
    # ADAM returns arrays as [a,b,c] format
    if ($value =~ /^\s*\[.*\]\s*$/) {
      # Remove the brackets
      $value =~ s/^\s*\[(.*)]\s*/$1/;

      # Now split on comma
      @values = split(/,/, $value);

    } else {
      push(@values, $value);
    }

    # DOUBLE precision values are not handled by perl as numbers
    # We need to change all parameters values of form "numberD+-number"
    # to "numberE+-number"
    # not clear whether I should be doing this in the ADAM module
    # itself or here. For now do it in the ORAC interface

    map { s/(\d)D((?:\+|-)?\d)/${1}E$2/g; } @values;

  } else {
    # This is for an A-task

    # Use NDF - need to search in file $monolith for task 
    my $monolith = $self->monolith;

    if (defined $monolith) {

      my $file = $monolith . ".$task";

      # Store and set ADAM_USER
      my $old_adam;
      $old_adam = $ENV{ADAM_USER} if exists $ENV{ADAM_USER};
      $ENV{ADAM_USER} = $self->adamdir;

      @values = par_get($param, $file, \$status);

      # Reset ADAM_USER
      if (defined $old_adam) {
	$ENV{ADAM_USER} = $old_adam;
      } else {
	delete $ENV{ADAM_USER};
      }

    } else {
      # Monolith name is not defined. Suggests that we did not
      # start the monolith.
      carp 'Error determining name of monolith. ' . 
	'Parameters cannot be retrieved from monoliths that were not started '.
	  'by this object.';
      $status = &Starlink::ADAM::SAI__ERROR;
    }

  }

  # Return the values if in an array context else return the
  # comma separated values (no status) in a scalar context
  if (wantarray) {
    return $status, @values;
  } else {
    my $scalar;
    if ($status == &Starlink::ADAM::SAI__OK) {
      $scalar = join(",",@values);
    } else {
      return undef;
    }
  }

}

# SET
#
#  $task->set(param, value)

=item set

Set the value of a parameter in an I-task.

  $status = $obj->set("action","param","value");

This routine has no effect in A-tasks.

=cut

# Probably should support multiple values by automatically joining
# with commas

sub set {

  my $self = shift;

  croak 'Usage: set(action, param, value)'
    unless scalar(@_) == 3;

  my $action = shift;
  my $param = shift;
  my $value = shift;

  # Construct the adam parameter string from action and param
  my $string = $action .":$param";

  # Now do the set

  my $status;
  if ($self->tasktype eq 'I') {
    $status = adamtask_set($self->name(), $string, $value);
  } else {
    carp 'Can not use AMS set for non I-tasks' if $^W;
    $status = &Starlink::ADAM::SAI__ERROR;
  }

  return $status;

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
specified in Starlink::AMS::Init-E<gt>timeout.

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
 my $stderr   = $Starlink::AMS::Core::ERRHAND;

 # Check
 while (! $self->contact) {
   if ((time() - $start) > $timeout) {
      print $stderr "Timed out whilst trying to contact monolith ".
	$self->name."\n"
	  unless $err_hide;
      return 0;
   }
   select undef,undef,undef, 0.2;
 }

 # Must be okay
 return 1;

}


=back

=head1 AUTHOR

Tim Jenness (t.jenness@jach.hawaii.edu).

=head1 COPYRIGHT

Copyright (C) Particle Physics and Astronomy Research Council 1998, 1999.
All Rights Reserved.

=head1 REQUIREMENTS

The C<Starlink::AMS::Core> and C<Starlink::AMS::Init>
modules must be installed in order to use this
module.

The C<NDF> module is required in order to retrieve parameters
from A-tasks.

=head1 SEE ALSO

L<perl>, 
L<Starlink::AMS::Core>,
L<Starlink::AMS::Init>,
L<Starlink::ADAM>,
L<Starlink::EMS>, L<Proc::Simple>,
and L<NDF>.

=cut



1;

