package Starlink::NBS;

use strict;
use Carp;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK $AUTOLOAD %EXPORT_TAGS);

require Exporter;
require DynaLoader;
require AutoLoader;

@ISA = qw(Exporter DynaLoader);
# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.
@EXPORT = qw(
	
);
$VERSION = '0.5';


# Set up export tags just in case somebody doesn't want to use
# the OO interface

%EXPORT_TAGS = (
		'nbslib' =>[ qw /
			     nbs_find_item
			     nbs_find_noticeboard
			     nbs_find_nth_item
			     nbs_get_children
			     nbs_get_info
			     nbs_get_name
			     nbs_get_primitive
			     nbs_get_shape
			     nbs_get_size
			     nbs_get_type
			     nbs_get_updated
			     nbs_get_value_c
			     nbs_get_value_d
			     nbs_get_value_f
			     nbs_get_value_i
			     nbs_get_value_l
			     nbs_lose_item
			     nbs_lose_noticeboard
			     nbs_put_value_c
			     nbs_put_value_d
			     nbs_put_value_f
			     nbs_put_value_i
			     nbs_put_value_l
			     nbs_tune
			     nbs_tune_noticeboard
			     /
			   ]
	       );

Exporter::export_tags('nbslib');



sub AUTOLOAD {
    # This AUTOLOAD is used to 'autoload' constants from the constant()
    # XS function.  If a constant is not found then control is passed
    # to the AUTOLOAD in AutoLoader.

    my $constname;
    ($constname = $AUTOLOAD) =~ s/.*:://;
    my $val = constant($constname, @_ ? $_[0] : 0);
    if ($! != 0) {
	if ($! =~ /Invalid/) {
	    $AutoLoader::AUTOLOAD = $AUTOLOAD;
	    goto &AutoLoader::AUTOLOAD;
	}
	else {
		croak "Your vendor has not defined Starlink::NBS macro $constname";
	}
    }
    eval "sub $AUTOLOAD { $val }";
    goto &$AUTOLOAD;
}

bootstrap Starlink::NBS $VERSION;

# Preloaded methods go here.



=head1 NAME

Starlink::NBS - Perl extension for accessing NBS noticeboards

=head1 SYNOPSIS

  use Starlink::NBS;
  
  $id = new Starlink::NBS($nbs_name);


=head1 DESCRIPTION

This module provides an OO interface to the NBS system.


=cut

=head1 METHODS

The available methods are

=over 4

=cut


# This is the new method

=item new

  Create a new instance of a Starlink::NBS object.
  Only used directly to access the top level of the noticeboard

   $nbs_id  = new Starlink::NBS("nbsname");

=cut

sub new {

  my $proto = shift;
  my $class = ref($proto) || $proto;
 
  my $nbs = {};  # Anon hash

  $nbs->{Path} = undef;  # Path to item (incl name)
  $nbs->{ID}   = undef;  # ID of shared memory area
  $nbs->{Top}  = undef;  # Is this a top level structure
  $nbs->{Status} = &SAI__OK; # The status of this object
  $nbs->{RootID} = undef; # ID of the actual noticeboard
   
  # Bless task into class
  bless($nbs, $class);
 
  # If we have arguments then assume we are trying
  # to load a new top level notice board

  if (@_) { $nbs->loadnbs(@_);};
 
  return $nbs;

}

=item General access methods

  These methods are for accessing the "instance" data:
    name, path, id, top, status

  With args they set the values.
  Without args they retrieve the values

=cut


# Methods to access "instance" data"
sub path {
  my $self = shift;
  if (@_) { $self->{Path} = shift; }
  return $self->{Path};
}

sub id {
  my $self = shift;
  if (@_) { $self->{ID} = shift; }
  return $self->{ID};
}

sub rootid {
  my $self = shift;
  if (@_) { $self->{RootID} = shift; }
  return $self->{RootID};
}

sub top {
  my $self = shift;
  if (@_) { 
    my $val = shift;
    if ($val) { 
      $self->{Top} = 1;
    } else {
      $self->{Top} = 0;
    }
  }
  return $self->{Top};
}

sub status {
  my $self = shift;
  if (@_) { $self->{Status} = shift; }
  return $self->{Status};
}

=item isokay

  Method that simply returns whether status is acceptable (SAI__OK)
  or not. If everything is okay return 1, else return 2.

=cut

sub isokay {
  my $self = shift;

  if ($self->status == &SAI__OK) {
    return 1;
  } else {
    return 0;
  }
}


=item loadnbs

  Method to load a top level noticeboard

=cut

sub loadnbs {

  my $self = shift;
  my $nbs_name = shift;

  my ($id, $status);

  $status = $self->status; # can either do it via abstraction
                           # or simply pass in $self->{Status}
  nbs_find_noticeboard($nbs_name, $id, $status);

  # Populate the object
  $self->status($status);
  if ($self->isokay) {
    $self->id($id);  
    $self->path($self->name);
    $self->top(1);
    $self->rootid($id);
  }
}

=item type

  Find the storage type of an object

    Arguments: None (but uses the current status of the object)
    Returns:  ($type, $status)

=cut

sub type {

  my $self = shift;
  my ($type, $status);

  $status = $self->status;
  nbs_get_type($self->id, $type, $status);

  return ($type, $status);
}

=item primitive

  Determines whether the object is a primitive

  Arguments: None (but uses the current status of the object)
  Returns:  ($prim, $status)
  $prim is 1 if it is a primitive, 0 otherwise.

=cut

sub primitive {

  my $self = shift;
  my ($status, $primitive);

  $status = $self->status;

  nbs_get_primitive($self->id, $primitive, $status);

  return ($primitive, $status);

}

=item name

  Find the name of the object

   Arguments:  None (but uses the current status of the object)
   Returns:  ($name, $status)

=cut

sub name {

  my $self = shift;
  my ($status, $name);

  $status = $self->status;

  nbs_get_name($self->id, $name, $status);

  return ($name, $status);
}


=item size

  Find the size of the item

    Arguments: None (but uses the current status of the object)
    Returns: ($size, $maxsize, $status)

  Returns status = NBS__NOPRIMITIVE if the object is not a primitive.
  Returns status = NBS__NILID  if the object is not defined

  The returned size is the number of entries that can be contained.
  (ie not bytes unless a _CHAR) and depends on the type of the object.

=cut

sub size {

  my $self = shift;
  my ($size, $maxsize, $primitive,  $status);
  my ($bsize, $bmaxsize, $type, $bytes_per_unit); 
 
  $status = $self->status;
  
  nbs_get_primitive($self->id, $primitive, $status);

  # If we have a primitive then we can proceed
  if ($primitive) {

    print "It is a primitive\n";
    # Now get the size in bytes
    nbs_get_size($self->id, $bmaxsize, $bsize, $status);

    # Now get the type (should I go through the method call here?
    nbs_get_type($self->id, $type, $status);
    
    # From the type work out the number of bytes per unit
    
    # First need strip leading _
    $type =~ s/^_//;

    $bytes_per_unit = &nbs_byte_size($type);

    if ($bytes_per_unit > 0) {
      $size = $bsize / $bytes_per_unit;
      $maxsize = $bmaxsize / $bytes_per_unit;
    } else {
      $size = $bsize;
      $maxsize = $bmaxsize;
      print "Error determining size of primitive type\n";
    }


  } else {
   print "It is not a primitive\n";
   $status = &NBS__NOTPRIMITIVE;
 
  }

  return ($size, $maxsize, $status);

}



=item find

  Find an item in a noticeboard. The full path name must be given.
  The item must exist below the current object.

  Arguments: Full name of object (separated by dots)
             An object relative to the current object can be
             given if it starts with a '.'
   
  Returns an object blessed into Starlink::NBS


=cut


sub find {

  my $self = shift;

  my $new = shift;

  my ($start_id, $path, $lump, $parent_id, $child_id, $status);

  # Now need to parse the input name.  
  # Split on the '.'

  my @bits = split(/\./, $new);

  # now work out the start ID
  if ($bits[0] =~ /./) {
     # Has something in first bin so we are starting from the top
     $start_id = $self->rootid;
     $path = $new;
     shift @bits; # Shift off the parent
  } else {
     # Starting from current object
     $start_id = $self->id;
     $path = $self->path . $new;
     shift @bits;  # Shift off the blank entry (parent)   
  }

  # Loop through the names until we find the one we are looking
  # for

  $status = $self->status;

  $parent_id = $start_id;  # This is the first parent

  foreach $lump (@bits) {
#    print "Searching for $lump...\n";

    nbs_find_item($parent_id, $lump, $child_id, $status);

    # Child is now the parent next time round
    $parent_id = $child_id;
  }

  # Create ourselves a new object
  # and store some things whether the status is good or bad

  my $new_obj = new $self;
  $new_obj->status($status);
  $new_obj->rootid($self->rootid);
  $new_obj->path($path);
  $new_obj->top(0);

  # We have now looped round
  # Status should be good if we have a valid id
  if ($status == &SAI__OK) {
 #   print "Status is okay\n";
    # Store the new object
    $new_obj->id($child_id);

  } 

  return $new_obj;


}


=item get

  Get the item corresponding to the current object.
  Must be a primitive object

  Arguments: None (uses current status of object)
  Returns:   Status and The values (in array context)
               ($status, @values)

=cut

sub get {

  my $self = shift;

  my ($status, @values, $value, $type, $primitive);

  $status = $self->status;
  
  nbs_get_primitive($self->id, $primitive, $status);

  # If we have a primitive then we can proceed
  if ($primitive) {

    # Now get the type (should I go through the method call here?
    nbs_get_type($self->id, $type, $status);
    

    # First need strip leading
    if ($status == &SAI__OK) {
      if ($type eq '_INTEGER') {

        nbs_get_value_i($self->id, \@values, $status);

      } elsif ($type eq '_REAL') {
  
        nbs_get_value_f($self->id, \@values, $status);

      } elsif ($type eq '_DOUBLE') {
  
        nbs_get_value_d($self->id, \@values, $status);

      } elsif ($type eq '_LOGICAL') {
  
        nbs_get_value_l($self->id, \@values, $status);

      } elsif ($type eq '_CHAR') {
  
        nbs_get_value_c($self->id, $value, $status);
        push(@values, $value);

      } else {

        # Type not found
        $status = &NBS__IMPOSSIBLE;
        print "Type $type is not supported\n";

      }
    }

  } else {

     $status = &NBS__NOTPRIMITIVE;

  }

  return($status, @values);

}


=item put

  Put values into the object

   Arguments: Values (however many values are supported)
   Returns:  Status

=cut

sub put {

  my $self = shift;

  my (@values) = @_;

  my ($status, $type, $old, $string, $primitive);

  $status = $self->status;
  
  nbs_get_primitive($self->id, $primitive, $status);

  # If we have a primitive then we can proceed
  if ($primitive) {

    # Now get the type (should I go through the method call here?
    nbs_get_type($self->id, $type, $status);
    
    # Now need to get permission to work on this noticeboard
    nbs_tune_noticeboard($self->rootid, "WORLD WRITE", 1, $old, $status);

    if ($status == &SAI__OK) {

      # First need strip leading
      if ($type eq '_INTEGER') {

        nbs_put_value_i($self->id, $#values+1, \@values, $status);

      } elsif ($type eq '_REAL') {
  
        nbs_put_value_f($self->id, $#values+1, \@values, $status);

      } elsif ($type eq '_DOUBLE') {
  
        nbs_put_value_d($self->id, $#values+1, \@values, $status);

      } elsif ($type eq '_LOGICAL') {
  
        nbs_put_value_l($self->id, $#values+1, \@values, $status);

      } elsif ($type eq '_CHAR') {
  
        $string = $values[0]; # This routine needs a string
        nbs_put_value_c($self->id, $string, $status);

      } else {

        # Type not found
        $status = &NBS__IMPOSSIBLE;
        print "Type $type is not supported\n";

      }
    } 

  } else {

     $status = &NBS__NOTPRIMITIVE;

  }

  return $status;

}


# Autoload methods go after =cut, and are processed by the autosplit program.
1;
__END__




=back

=head1 EXAMPLES

  use Starlink::NBS;
  $nbs_name = "p57321_plotnb";

  $nbs = new Starlink::NBS("$nbs_name");
  $fmax_item = $nbs->find(".port_0.fmax");

  ($status, $value) = $fmax_item->get;

  $status = $fmax_item->put(3500);




=head1 NON-METHODS

  In addition to the OO implementation. Direct hooks to the NBS library
  can be accessed. They can be imported into your global namespace by
  using:

      use Starlink::NBS qw/:nbslib/;

  The available commands are:

    nbs_find_item(envid, name, id, status)
    nbs_find_noticeboard( noticeboard, topid, status)
    nbs_find_nth_item(envid, posn, id, status)
    nbs_get_children(id, children, status)
    nbs_get_info(id, name, value, status)
    nbs_get_name(id, name, status)
    nbs_get_primitive(id, primitive, status)
    nbs_get_shape(id, maxdims, dims, actdims, status)
    nbs_get_size(id, maxbytes, actbytes, status)
    nbs_get_type(id, type, status)
    nbs_get_updated(id, updated, status)
    nbs_get_value_c(id, $cvalue, status)
    nbs_get_value_d(id, \@values, status)
    nbs_get_value_f(id, \@values, status)
    nbs_get_value_i(id, \@values, status)
    nbs_get_value_l(id, \@values, status)
    nbs_lose_item(id, option, status)
    nbs_lose_noticeboard(id, option, status)
    nbs_put_value_c(id, $string, status)
    nbs_put_value_d(id, nvals, \@values, status)
    nbs_put_value_f(id, nvals, \@values, status)
    nbs_put_value_i(id, nvals, \@values, status)
    nbs_put_value_l(id, nvals, \@values, status)
    nbs_tune(name, value, oldvalue, status)
    nbs_tune_noticeboard(id, name, value, oldvalue, status)

  Note that nbs_get_ and nbs_put_ require a separate routine for each
  type. Additionally, the get and set routines expect array arguments.


=head1 AUTHOR

T. Jenness (timj@jach.hawaii.edu)

=head1 SEE ALSO

perl(1). Starlink::EMS(3)

=cut
