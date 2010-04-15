package Starlink::ADAM;

=head1 NAME

Starlink::ADAM - Perl extension for the ADAM messaging system

=head1 SYNOPSIS

  use Starlink::ADAM;

  adam_start($status);
  adam_exit();
  @reply = adam_receive;

=head1 DESCRIPTION

This module provides an interface to the ADAM messaging system.
The AMS C library routines are accessible directly or they can be
accessed via wrapper routines that handle the most commonly used
combinations.

The Starlink::AMS::Task module can be used to provide higher level
access to AMS.

=head1 Exported functions

The AMS library routines are:

 ams_astint() ams_astmsg() ams_exit() ams_extint() ams_getreply()
 ams_init() ams_path() ams_plookup() ams_receive() ams_reply()
 ams_send()

The wrapper routines are:

 adam_getreply() adam_receive() adam_reply() adam_send() adam_start()
 adam_path() adam_exit() adam_strtocont() adam_strtostatus()
 adam_appendstatus()

Note that the C interface is retained except that it is not necessary
to specify the values of C<message_name_s> or C<message_value_s>
since these are set by perl to a suitable size to receive
the message. Routines affected are

 ams_getreply() ams_recieve()

=head1 Function Descriptions

=over 4

=cut


use strict;
use Carp;
use vars qw($VERSION @ISA @EXPORT %EXPORT_TAGS $AUTOLOAD);

# This module requires the Starlink::EMS module to translate
# the facility error status.

use Starlink::EMS qw/ ems1_get_facility_error /;

require Exporter;
require DynaLoader;

@ISA = qw(Exporter DynaLoader);
# Items to export into callers namespace by default. Use tags
# to reduce to be more specific

%EXPORT_TAGS = (
		'ams'=>[qw/
			ams_astint ams_astmsg ams_exit ams_extint ams_getreply
			ams_init ams_path ams_plookup ams_receive ams_reply
			ams_send
			/],

		'adam'=>[qw/
			 adam_getreply adam_receive adam_reply adam_send
			 adam_start adam_path adam_exit
			 adam_strtocont adam_strtostatus adam_appendstatus
			/]
		);

Exporter::export_tags('ams','adam');

$VERSION = '1.14';

sub AUTOLOAD {
    # This AUTOLOAD is used to 'autoload' constants from the constant()
    # XS function.  If a constant is not found then control is passed
    # to the AUTOLOAD in AutoLoader.

    my $constname;
    ($constname = $AUTOLOAD) =~ s/.*:://;
#    my $val = constant($constname, @_ ? $_[0] : 0);
    # Note that the default autoloader expects integer argument
    # if @_ contains something (this can be @_ from the calling routine!)
    # Since these routines only expect a single argument just pass a 0.
    my $val = constant($constname);

    if ($! != 0) {
	if ($! =~ /Invalid/) {
	    $AutoLoader::AUTOLOAD = $AUTOLOAD;
	    goto &AutoLoader::AUTOLOAD;
	}
	else {
		croak "Your vendor has not defined Starlink::ADAM constant $constname";
	}
    }
    eval "sub $AUTOLOAD { $val }";
    goto &$AUTOLOAD;
}

bootstrap Starlink::ADAM $VERSION;

# Install optimised access method for compiler constant
# This is only slightly better than the AUTOLOAD since the
# AUTOLOAD also sets up a new command for each AUTOLOAD via eval...
{
  my $SAI__OK = &pSAI__OK();
  eval "sub SAI__OK { $SAI__OK }";
}

# Preloaded methods go here.
# These are wrappers to the AMS routines

=item adam_exit()

Exit the adam messaging system. This simply calls C<ams_exit()>.

=cut


# adam_exit
#
#  Exit this process invoking the adam exit handler

sub adam_exit {

  croak "Wrong # args: should be adam_exit()\n"
    if (@_);

  # Exit AMS
  ams_exit();

}



# adam_process_message
#
#  Determine the command name of a message and set the return array
#  for the user
#
#  Arguments:  $path, $messid, $inmsg_status, $inmsg_context,
#              $inmsg_name, $inmsg_length, $inmsg_value, $status
#
#   Returns an array containing:
#     $command, $taks, $facerr, $status

# facerr is the status number and so is identical to inmsg_status
# at the present time.

sub adam_process_message {

  # Local variables
  my ($task, $prompt, $facility, $text, $command, $ident, $facerr, $value);

  # Arguments
  my ($path,$messid, $inmsg_status, $inmsg_context, $inmsg_name,
      $inmsg_length, $inmsg_value, $status) = @_;

  croak 'Usage: adam_process_message(path, messid, msg_status, msg_context, msg_name, msg_length, msg_value, status)'
    if (scalar(@_)!=8);

  # Return if bad status
  return if ($status != &SAI__OK);

  # Lookup the task name for this path
  ams_plookup($path, $task, $status);

  # Check status and find out the command that was issued
  if ($status != &SAI__OK) {
    carp "adam_process_message: Error looking up path name\n";
  } else {
    if ($inmsg_context == &OBEY || $inmsg_context == &CANCEL) {

      # Translate message status from a number to a string
      if ($inmsg_status == &DTASK__ACTSTART) {
	$command = "actstart";
      } elsif ($inmsg_status == &MESSYS__PARAMREQ) {
	$command = "paramreq";
	$prompt  = $inmsg_value;
      } elsif ($inmsg_status == &MESSYS__PARAMREP) {
	$command = "paramrep";
      } elsif ($inmsg_status == &MESSYS__INFORM) {
	$command = "inform";
      } elsif ($inmsg_status == &MESSYS__SYNC) {
	$command = "sync";
      } elsif ($inmsg_status == &MESSYS__TRIGGER) {
	$command = "trigger";
      } elsif ($inmsg_status == &SAI__OK) {
	$command = "startmsg";
      } else {
	$command = "endmsg";
      }

    } elsif ($inmsg_context == &GET) {
      $command = "getresponse";

    } elsif ($inmsg_context == &SET) {
      $command = "setresponse";

    } elsif ($inmsg_context == &CONTROL) {
      $command = "controlresponse";

    } else {

      if ($inmsg_status == &MESSYS__PARAMREQ) {
	$command = "paramreq";
	$prompt  = $inmsg_value;

      } elsif ($inmsg_status == &MESSYS__PARAMREP) {
	$command = "paramrep";

      } elsif ($inmsg_status == &MESSYS__INFORM) {
	$command = "inform";

      } elsif ($inmsg_status == &MESSYS__SYNC) {
	$command = "sync";

      } else {
	$status = &SAI__ERROR;
      }
    }

    if ($status == &SAI__OK) {

      # The message status is just inmsg_status
      # we should not do anything to this (eg translating it to text)
      # Leave that to the caller
      $facerr = $inmsg_status;

      # Now look at $prompt (dont know why since prompt CAN only
      # equal $inmsg_value!)
      # Need to work this out

      if ($prompt) {
	$value = $prompt;
      } else {
	$value = $inmsg_value;
      }


    } else {
      # Bad status from all that
      carp "adam_process_message: Bad message context: message name was ".
	" $inmsg_name and message values was $inmsg_value\n";

    }
  }

  # Now return everything to the calling routine if status is good

  if ($status == &SAI__OK) {

    # No point in returning input arguments
    return($command, $task, $facerr, $status);

  } else {
    return(undef,undef, undef, $status);
  }


}

######################################

=item adam_getreply(timeout, path, messid)

Wait for an adam message to arrive on the given path and messid.
This routine returns an array containing:

   ($command, $task, $inmsg_name, $path, $messid, $facerr,
    $inmsg_value, $status)

where $command is the message type (eg endmsg, inform, paramreq),
$task is the name of the task (eg kappa), $inmsg_name is the action
name (eg gdnames, linplot), $path and $messid are the path and message
ID of the transaction, $facerr is the error status of the transaction,
$inmsg_value is the contents of the message (eg inform or error
message or parameter name) and $status is the status from this
subroutine (not necessarily the status of the transaction).

=cut

# adam_getreply
#  Wait for an adam message to arrive on the given path and messid
#
#  Usage: adam_getreply timeout path messid
#
#  Returns:
#    ($command, $task, $inmsg_name, $path, $messid, $facerr, $inmsg_value,
#     $status)


sub adam_getreply {

  my ($status, $inmsg_status, $inmsg_context, $inmsg_name, $inmsg_length,
      $inmsg_value, $command, $task, $facerr);
  my ($facility, $ident, $text);

  croak 'Usage: adam_getreply timeout path messid'
    if (scalar(@_)!=3);

  my ($timeout, $path, $messid) = @_;

  # set good status
  $status = &SAI__OK;

  # Run the ams command
  ams_getreply($timeout, $path, $messid,
	      $inmsg_status, $inmsg_context, $inmsg_name, $inmsg_length,
	      $inmsg_value, $status);


   # Check return status
  if ($status != &SAI__OK) {
    ($facility, $ident, $text) = adam_appendstatus($status);
    carp "adam_getreply: Error reading adam message: $status \n"
          . $facility."__$ident: $text\n";
    my $facerr = $facility."__$ident: $text";
    return ("badstatus",undef,undef,undef,undef,$status,$inmsg_value,$status);
  }

  # Now examine this message to see what we can found out about it

  ($command, $task, $facerr, $status) =
    adam_process_message($path, $messid, $inmsg_status, $inmsg_context,
			 $inmsg_name, $inmsg_length, $inmsg_value, $status);

  # Strip trailing spaces from inmsg_value
  # Should null terminate the strings in the XS module
  # This is done in the XS module but currently there is a bug when the
  # returned string length is zero (ie a blank line)

  $inmsg_value =~ s/\s+$//;
  $inmsg_value = substr($inmsg_value, 0, $inmsg_length);

  # Now we can return all the info if we have good status

  return ($command, $task, $inmsg_name, $path, $messid, $facerr, $inmsg_value,
	 $status);
}


######################################

=item adam_receive

Wait for any adam message to arrive - the routine will wait forever if
necessary. This routine returns an array containing:

   ($command, $task, $inmsg_name, $path, $messid, $facerr,
    $inmsg_value, $status)

where $command is the message type (eg endmsg, inform, paramreq),
$task is the name of the task (eg kappa), $inmsg_name is the action
name (eg gdnames, linplot), $path and $messid are the path and message
ID of the transaction, $facerr is the error status of the transaction,
$inmsg_value is the contents of the message (eg inform or error
message or parameter name) and $status is the status from this
subroutine (not necessarily the status of the transaction).

=cut


# adam_receive
#  Wait for any adam message to arrive
#   Returns an array containing:
#     ($command, $task, $inmsg_name, $path, $messid, $facerr, $inmsg_value)

sub adam_receive {

  my ($inmsg_status, $inmsg_context, $inmsg_name, $inmsg_length,
      $inmsg_value, $path, $messid, $status, $task, $command, $facerr);
  my ($fac, $text, $ident);

  # Set status to good
  $status = &SAI__OK;

  # Receive an adam message
  ams_receive(&MESSYS__INFINITE,
	     $inmsg_status, $inmsg_context, $inmsg_name, $inmsg_length,
	     $inmsg_value, $path, $messid, $status);

  # Check return status
  if ($status != &SAI__OK) {
    ($fac, $ident, $text) = adam_appendstatus($status);
    print $fac."__$ident: $text\n";

    carp "adam_receive: Error reading adam message: $status\n";

    # Return such that the status is stored in the 7th component
    # to match a normal return
    return (undef,undef,undef,undef,undef,undef,undef,$status);
  }

  # Now examine this message to see what we can found out about it

  ($command, $task, $facerr, $status) =
    adam_process_message($path,$messid, $inmsg_status, $inmsg_context,
			 $inmsg_name, $inmsg_length, $inmsg_value, $status);

  # Strip trailing spaces from inmsg_value
  # We have to strip to length rather than white space because the
  # strings are not null terminated when they come back from the XS module
  # Either I put the Null in myself or I just make this the right length.
  # This is done in the XS module but currently there is a bug when the
  # returned string length is zero (ie a blank line)

  $inmsg_value =~ s/\s+$//;
  $inmsg_value = substr($inmsg_value, 0, $inmsg_length);

  # Now we can return all the info

  return ($command, $task, $inmsg_name, $path, $messid, $facerr, $inmsg_value,
	 $status);

}

###############################

=item adam_reply(path, messid, status, msg_name, value)

Construct an adam message from the supplied arguements and send it.
Returns status.

=cut

# adam_reply
#
#  Construct an adam message from the arguments to the perl command
#  and send it.
#
#  Usage:  adam_reply path messid status msg_name value

sub adam_reply {

  my ($status, $msg_status, $fac, $ident, $text);

  croak 'Usage: adam_reply path messid mstatus msg_name value'
    if (scalar(@_)!=5);

  my ($path, $messid, $mstatus, $msg_name, $value) = @_;

  # Set good status
  $status = &SAI__OK;

  # Convert the status to a number
  $msg_status = adam_strtostatus($mstatus, $status);


  # Call the ams routine
  ams_reply($path, $messid, &MESSYS__MESSAGE, $msg_status, &OBEY,
	   $msg_name, length($value), "$value", $status);

  # Check status

  if ($status != &SAI__OK) {
    ($fac, $ident, $text) = adam_appendstatus($status);
    print $fac."__$ident: $text\n";
    carp "adam_reply: failed to send adam message\n";

  }
  return $status;
}

##############################

=item adam_send(task, msg_name, context, value)

Send an adam message. This routine returns the path and messid of
the task.


=cut

# adam_send
#
#    Send an adam message
#  Usage:  adam_send task msg_name context value
#
#  Returns: path, messid, $status
#

sub adam_send {

  my ($status, $path, $context, $messid);
  my ($fac, $ident, $text);

  croak 'Usage: adam_send task msg_name context value'
    if (scalar(@_)!=4);

  my ($task, $msg_name, $strcontext, $value) = @_;

  # Good status
  $status = &SAI__OK;

  # Get the communications path
  ams_path($task, $path, $status);

  if ($status == &SAI__OK) {

    # Get the numeric context
    $context = adam_strtocont($strcontext, $status);
    $| = 1;
    if ($status == &SAI__OK) {

      # Send the message
      ams_send($path, &MESSYS__MESSAGE, &SAI__OK, $context, $msg_name,
	       length($value), $value, $messid, $status);

      if ($status != &SAI__OK) {
	($fac, $ident, $text) = adam_appendstatus($status);
	carp $fac."__$ident: $text\n" .
	  "adam_send: Failed to send ADAM message to $task\n";
      }


    } else {
      ($fac, $ident, $text) = adam_appendstatus($status);
      carp $fac."__$ident: $text\n" .
           "adam_send:Bad context $strcontext, should be GET,SET, OBEY or CANCEL\n";
    }

  } else {
    ($fac, $ident, $text) = adam_appendstatus($status);
    carp $fac."__$ident: $text\n" . "adam_send: Failed to get path to task $task\n";
  }

  return ($path, $messid, $status);

}

#############################

=item adam_start(name)

Initialise the adam system and identify it in the messaging system by
'name'

=cut

# adam_start
#
#  initialise the adam system
#
#  Usage: adam_start my_name
#    where my_name is the name of this perl process

sub adam_start {

  my ($fac, $ident, $text);

  croak "Usage: adam_start task_name"
      if (scalar(@_)!=1);

  my $task_name = shift;

  # Good status
  my $status = &SAI__OK;

  # Start AMS
  ams_init($task_name, $status);

  if ($status != &SAI__OK) {
    ($fac, $ident, $text) = adam_appendstatus($status);
    print $fac."__$ident: $text\n";
    carp "adam_start: Failed to open Adam message system\n";
  }

  return $status;

}

#############################

=item adam_path(name)

Get a path to an adam task. Returns a 1 if the path exists and 0 if it
does not.

=cut

# adam_path
#
#   - get a path to an adam task
#
#   Usage: adam_path name
#
#   Returns 1 if a path exists and 0 if it does not

sub adam_path {

  my ($status, $path);

  croak 'Usage: adam_path name'
    if (scalar(@_)!=1);

  my $name = shift;

  # Good status
  $status = &SAI__OK;

  # Get the path
  ams_path($name, $path, $status);

  if ($status == &SAI__OK) {
    return 1
  } else {
    return 0
  }

}




#############################

=item adam_strtocont(string, status)

Convert a string to a context number. The context is returned.

=cut

# adam_strtocont
#
#  convert a string to a context number
#
#  Usage: $context = adam_strtocont($string, $status)


sub adam_strtocont {

  croak 'Usage: adam_strtocont string status'
    if (scalar(@_)!=2);

  my ($string, $status) = @_;

  return if $status != &SAI__OK;

  # Upper case
  $string = uc($string);

  # Now compare

  ($string eq 'GET') && (return &GET);
  ($string eq 'SET') && (return &SET);
  ($string eq 'OBEY') && (return &OBEY);
  ($string eq 'CANCEL') && (return &CANCEL);
  ($string eq 'CONTROL') && (return &CONTROL);

  # Error
  carp "adam_strtocont: String is not a recognized context\n";

}



##############################

=item adam_strtostatus(string, status)

Convert a string to a status number. String must be one of
ACTSTART, ACTCOMPLETE, PARAMREP, PARAMREQ, INFORM, SYNC, SYNCREP,
TRIGGER. Note that these values are available as compiler constants
directly from the autoloader.

=cut

# adam_strtostatus
#   Convert a string to a status number
#
#   Usage: adam_strtostatus string status
#
#   Returns  msg_status

sub adam_strtostatus {

  croak 'Usage: adam_strtostatus string status'
    if (scalar(@_)!=2);

  my ($string, $status) = @_;

  return if $status != &SAI__OK;

  $string = uc($string);

  ($string eq 'ACTSTART') && (return &DTASK__ACTSTART);
  ($string eq 'ACTCOMPLETE') && (return &DTASK__ACTCOMPLETE);
  ($string eq 'PARAMREP') && (return &MESSYS__PARAMREP);
  ($string eq 'PARAMREQ') && (return &MESSYS__PARAMREQ);
  ($string eq 'INFORM') && (return &MESSYS__INFORM);
  ($string eq 'SYNC') && (return &MESSYS__SYNC);
  ($string eq 'SYNCREP') && (return &MESSYS__SYNCREP);
  ($string eq 'TRIGGER') && (return &MESSYS__TRIGGER);

  # Error
  carp "adam_strtostatus: $string is not a recognised status\n";

}


##############################

=item adam_appendstatus(status)

The adam status is converted to a text string and the message is
returned. Three arguments are returned in an array context:

$facility is the facility error name (eg NDF)

$ident is the error identifier.

$text is the text of the error message.

=cut


# adam_appendstatus
#
#   The adam status is converted to a text string and the message
#   part is returned

sub adam_appendstatus {

  my ($facility, $ident, $text);

  croak 'Usage: adam_appendstatus istat'
    if (scalar(@_)!=1);

  my $istat = shift;

  ems1_get_facility_error($istat, $facility, $ident, $text);

  return ($facility, $ident, $text);

}





# Autoload methods go after =cut, and are processed by the autosplit program.

1;
__END__
# Below is the documentation for the module

=back

=head1 Compiler Constants

The following constants are available via the autoloader:

CANCEL, CONTROL, DTASK__ACTSTART, DTASK__ACTCOMPLETE,
GET, MESSYS__INFINITE, MESSYS__PARAMREQ, MESSYS__PARAMREP,
MESSYS__INFORM, MESSYS__SYNC, MESSYS__SYNCREP,
MESSYS__TRIGGER, MESSYS__MESSAGE, MSG_NAME_LEN, MSG_VAL_LEN,
OBEY, SAI__OK, SAI__WARN, SAI__ERROR, SET

They can be accessed as subroutines eg

  $status = &Starlink::ADAM::SAI__OK;

=head1 Other requirements

The Starlink::EMS module is required.

=head1 SEE ALSO

perl(1). L<Starlink::EMS>, L<Starlink::AMS::Task>


=head1 AUTHOR

Tim Jenness E<lt>t.jenness@jach.hawaii.eduE<gt>,
Copyright (c) Particle Physics and Research Council
and Tim Jenness 1997-2000. All Rights Reserved.

=cut
