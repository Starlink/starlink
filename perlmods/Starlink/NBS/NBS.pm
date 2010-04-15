package Starlink::NBS;

use Starlink::EMS qw/ ems1_get_facility_error /;
use 5.004;
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


# VERSION number
$VERSION = '1.01';


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
			     nbs_begin_definition
			     nbs_define_structure
			     nbs_define_primitive
			     nbs_define_shape
			     nbs_end_definition
			     nbs_restore_definition
			     nbs_restore_noticeboard
			     nbs_save_noticeboard
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

  $nbs_name = "p57321_plotnb";

  $nbs = new Starlink::NBS($nbs_name);
  $fmax_item = $nbs->find(".port_0.fmax");

  ($status, $value) = $fmax_item->get;

  $status = $fmax_item->put(3500);

  tie %top, ref($nbs), $nbs;

  use Data::Dumper;
  print Dumper(\%top);

=head1 DESCRIPTION

This module provides an interface to the NBS system. Object-oriented
and function interfaces are provided. Additionally, it is possible
to tie the noticeboard to a perl variable allowing the notice board
to be manipulated using normal perl commands.

=head1 METHODS

=head2 Constructors

=over 4

=cut


# This is the new method

=item new

Create a new instance of a Starlink::NBS object.
Only used directly to access the top level of the noticeboard

   $nbs_id  = new Starlink::NBS("nbsname");

The loadnbs() method is invoked with the supplied argument.

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
  $nbs->{Nchilds} = undef;    # Number of children
  $nbs->{Pos}   = 0;      # Position in structure
  $nbs->{Debug} = 0;

  # Bless task into class
  bless($nbs, $class);

  # If we have arguments then assume we are trying
  # to load a new top level notice board

  if (@_) { $nbs->loadnbs(@_);};

  return $nbs;

}

=back

=head2 Accessor Methods

These methods are for accessing the "instance" data.
With arguments they set the value, without arguments they
retrieve the current value.

=over 4

=item path

Full path (name) to the item associated with this object
including the name of the noticeboard.

=cut

# Methods to access "instance" data"
sub path {
  my $self = shift;
  if (@_) { $self->{Path} = shift; }
  return $self->{Path};
}

=item id

ID of the shared memory area.

=cut

sub id {
  my $self = shift;
  if (@_) { $self->{ID} = shift; }
  return $self->{ID};
}

=item pos

Position in structure.

=cut

sub pos {
  my $self = shift;
  if (@_) { $self->{Pos} = shift; }
  return $self->{Pos};
}

=item rootid

ID of the actual noticeboard.

=cut

sub rootid {
  my $self = shift;
  if (@_) { $self->{RootID} = shift; }
  return $self->{RootID};
}

=item top

True if this is a top-level object. False otherwise.

=cut

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

=item status

Contains the current status associated with the object. Status from
each method call is placed in here (and usually returned with each
command). Methods will not do anything if the stored status is not
good. This behaves as an inherited status without having to pass
the status into the methods explicitly.

=cut

sub status {
  my $self = shift;
  if (@_) { $self->{Status} = shift; }
  return $self->{Status};
}

=item debug

Turns debugging on or off. Default is off.

=cut

sub debug {
  my $self = shift;
  if (@_) { $self->{Debug} = shift; }
  return $self->{Debug};
}

=item nchilds

Returns the number of children associated with the current object.
Can not be set externally.

=cut

sub nchilds {
  # This routine calculates the number of children
  # associated with an object
  # The value can not be set externally.
  # If undef the value is recalculated

  my $self = shift;

  unless (defined $self->{Nchilds}) {
    my $status = $self->status;
    my $num;
    nbs_get_children($self->id, $num, $status);
    if ($status == &SAI__OK) {
      $self->{Nchilds} = $num;
    }
  }

  return $self->{Nchilds};
}

=back

=head2 General Methods

=over 4

=item isokay

Method that simply returns whether status is acceptable (SAI__OK)
or not. If everything is okay return 1, else return 0.

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

    # Work out the number of children
    $self->nchilds;
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
  $prim is 1 if it is a primitive, 0 otherwise (including if status
  is bad).

=cut

sub primitive {

  my $self = shift;
  my ($status, $primitive);

  $status = $self->status;

  nbs_get_primitive($self->id, $primitive, $status);

  $primitive = 0 if ($status != &SAI__OK);

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

Returns status = NBS__NOTPRIMITIVE if the object is not a primitive.
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

    print "primitive Name = ". ($self->name)[0] ."\n" if $self->debug;
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
      print STDERR "Error determining size of primitive type\n";
    }


  } else {
   print "It is not a primitive\n" if $self->debug;
   $status = &NBS__NOTPRIMITIVE;

  }

  return ($size, $maxsize, $status);

}


=item nth_name(num)

Return the name of the nth component in the objects structure.

 Arguments: number
 Return:    name

=cut


sub nth_name {

   my $self = shift;
   my $num = shift;

   my $id = $self->id;
   my ($child, $name);
   my $status = $self->status;

   # Check num
   return undef if ($num < 0 || $num > $self->nchilds);

   nbs_find_nth_item($id, $num, $child, $status);

   if ($status == &SAI__OK) {
     # Retrieve the child name
     nbs_get_name($child, $name, $status);
     if ($status == &SAI__OK) {
       return $name;
     }

   }
   return undef;

}

=item find

Find an item in a noticeboard. The full path name must be given.
The item must exist below the current object.

  Arguments: Full name of object (separated by dots)
             An object relative to the current object can be
             given if it starts with a '.'

Returns an object blessed into C<Starlink::NBS> class.

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

    nbs_find_item($parent_id, $lump, $child_id, $status);

    last if $status != &SAI__OK;

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
	my $name = ($self->name)[0];
        print "Cannot get data for $name from item of type $type\n";

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

        nbs_put_value_c($self->id, $values[0], $status);

      } else {

        # Type not found
        $status = &NBS__IMPOSSIBLE;
	my $name = ($self->name)[0];
        print "Cannot put data for $name into item of type $type\n";

      }
    }

  } else {

     $status = &NBS__NOTPRIMITIVE;

  }

  return $status;

}

=item poke(item, val)

Set the value of item to 'val'.
This is the equivalent of a find() followed by a put().

  $nbs->poke(".port_0.display_data", 'IMAGE');

Status of the put() is returned.

=cut

sub poke {
  my $self = shift;
  my $item = shift;
  my $value = shift;

  my $new = $self->find($item);
  $new->put($value);

}

=item peek(item)

Return the value stored in item. This is the equivalent of a find()
followed by a get()

  ($value) = $nbs->peek(".port_0.display_data");

undef is returned if bad status is encountered.
The values are returned in an array context.

=cut

sub peek {
  my $self = shift;
  my $item = shift;

  my $new = $self->find($item);
  my ($status, @values) = $new->get;

  if ($status == &SAI__OK) {
    return @values
  } else {
    return undef;
  }
}


=back

=head1 TIE

Scalar values in the noticeboard can be tied to scalar perl variables
using the perl tie() function.

   $what = $Nbs->find("primitive.object");
   tie ($object, ref($what), $what);

Now $object will automatically reflect the value in the notice board
associated with primitive.object. Note that this only works for
primitives (not structures).

Noticeboard structures can be tied to perl hashes also:

   $what = $Nbs->find("structure");
   tie (%hash, ref($what), $what);

Now %hash can be used to update the entire noticeboard structure.
Note that keys are always assumed to be relative to the
tied object -- in effect this means that a '.' is automatically
prepended to all keys if one is not found. (see find()
for more information on relative addressing).

Note that ties can be broken by using simple copies.
For example, for

     %new = %hash

%new will not be tied even though %hash was.

It is possible to tie a hash to the top-level noticeboard structure
and to access lower levels of the noticeboard entirely using the
tie. For example, a simple way to list the noticeboard contents
is to tie a hash to the top level and then use Data::Dumper to
list the hash.

  use Starlink::NBS;
  use Data::Dumper;

  $nbs = new Starlink::NBS($nbsname);
  tie %hash, ref($nbs), $nbs;

  print Dumper(\%hash);

A method is supplied for tieing NBS objects to variables:

=over 4

=item tienbs

Tie a Starlink::NBS object to a perl variable. No arguments.  If the
object points to a structure a reference to a perl hash is returned.
If the object points to a primitive a reference to a perl scalar is
returned.

=back

=cut

sub tienbs {
  my $self = shift;
  my ($tie, %tie);

  # Check if it is a primitive
  my ($prim,$status) = $self->primitive;
  if ($status == &SAI__OK) {

    if ($prim) {
      # Tie to a scalar
      tie($tie, ref($self), $self);
      return \$tie;
    } else {
      # Tie to a hash;
      tie(%tie, ref($self), $self);
      return \%tie;
    }
  }
  return undef;
}



# Method to tie a scalar to a notice board item
# Expects a  Starlink::NBS object that is pointing to
# a primitive noticeboard entry

sub TIESCALAR {
  my $class = shift;
  my $obj = shift;

  # Check that we have been supplied an object
  unless (UNIVERSAL::isa($obj, "Starlink::NBS")) {
    carp "NBS:Tiescalar can not tie a non-NBS object";
    return undef;
  }

  # Check that we are pointing to a primitive
  my ($prim, $status) = $obj->primitive;
  unless ($prim) {
    carp "NBS::Tiescalar given non-primitive noticeboard item";
    return undef;
  }

  return $obj;
}

# Method to tie hash to notice board.
# Almost identical to TIESCALAR.

sub TIEHASH {
  my $class = shift;
  my $obj = shift;

  # Check that we have been supplied an object
  unless (UNIVERSAL::isa($obj, "Starlink::NBS")) {
    carp "NBS:Tiehash can not tie a non-NBS object";
    return undef;
  }

  # Check that we are pointing to a non primitive item
  my ($prim, $status) = $obj->primitive;
  if ($prim) {
    carp "NBS::Tiehash given primitive noticeboard item";
    return undef;
  }

  # Return undef if bad status
  if ($status != &SAI__OK) {
    ems1_get_facility_error($status, my $facility, my $ident, my $text);
    carp "NBS::Tiehash: Error checking whether object is primitive\n".
      "$status: $text\n";
    return undef;
  }

  return $obj;
}



# Method to retrieve the value of the tied object
# Now we need to distinguish between a tied scalar and a
# tied hash (since we are allowing both to be tied in this
# module)

# Do it by counting arguments. If there is only one argument
# to fetch, then assume it is

# Returns an array reference if there is more than one value

sub FETCH {
  my $self = shift;
  my ($obj, $status, @values, %newtie, $prim);

  if (@_) {
    print "TIED hash\n" if $self->debug;
    # Must be a hash since we are being asked to retrieve a key
    my $key = shift;

    $key = "." . $key unless $key =~ /^\./;

    $obj = $self->find($key);

    # Now need to check if we have a primitive or
    # a structure
    ($prim, $status) = $obj->primitive;
    if ($status == &SAI__OK) {
      if ($prim) {
	($status, @values) = $obj->get;
	if ($status == &SAI__OK) {
	  if ($#values == 0) {
	    return $values[0];
	  } else {
	    return \@values;
	  }
	} else {
	  return undef;
	}

      } else {
	# We have been given a strucuture
	# So open try tieing a hash to it!
	tie(%newtie, ref($obj), $obj);
	return \%newtie;
      }
    } else {
      return undef;
    }


  } else {
    # We are a tied scalar
    # We know we are a primitive
    print "TIED scalar\n" if $self->debug;

    ($status, my $value) = $self->get;
    if ($status == &SAI__OK) {
      return $value;
    } else {
      return undef;
    }

  }

}

sub STORE {
  my $self = shift;
  my ($value,$status,$prim);
  my $key = ''; # for error message

  # If we have two args left then we are tieing a hash
  if (scalar(@_) == 2) {
    $key = shift;
    $value = shift;

    $key = "." . $key unless $key =~ /^\./;

    # Need to get reference to key structure
    my $obj = $self->find($key);

    # Check whether a primitive
    ($prim, $status) = $obj->primitive;

    if ($status == &SAI__OK) {
      if ($prim) {

	# If we have been given a scalar
	if (not ref($value)) {
	  $status = $obj->put($value);
	} elsif (ref($value) eq 'ARRAY'){
	  # An array is okay (I dont like expanding the array
	  $status = $obj->put(@$value);
	} else {
	  # Trying to write a reference to a primitive
	  $status = &NBS__PRIMITIVE;
	}

      } else {
	# Not a primitive so check to see whether we have been given
	# a hash

	if (ref($value) eq 'HASH') {

	  # Need to loop over the keys and put each object into
	  # shared memory (note this does not mean that we create
	  # the shared memory portion)

	  foreach my $nkey (keys %$value) {
	    # Recursion
	    $obj->STORE($nkey, $$value{$nkey});
	  }

	}

      }
    }

  } else {
    # Tied scalar
    $value = shift;
    $status = $self->put($value);

  }

  if ($status != &SAI__OK) {
    my $name = ($self->name)[0];
    ems1_get_facility_error($status, my $facility, my $ident, my $text);
    carp "NBS::STORE: Error storing $value in $name$key\n".
      "$status: $text\n";
  }


}


# Delete entries from a hash
# This method does nothing (since I don't want to delete
# shared memory

sub DELETE {
  my $self = shift;

}

# Method to see whether a key exists
# Returns 1 if it does and 0 otherwise.
# Note that the find() method prints an error message to the
# screen - very annoying - cant stop nbs_find_item from doing this.

sub EXISTS {
  my $self = shift;
  my $key = shift;

  $key = "." . $key unless $key =~ /^\./;

  # Get the new object
  my $obj = $self->find($key);

  if ($obj->isokay) {
    return 1;
  } else {
    return 0;
  }

}


# Method to return the keys

# First key


sub FIRSTKEY {

   my $self = shift;

   $self->pos(1);
   return $self->nth_name(1);

}

# Generate the next key

sub NEXTKEY {
  my $self = shift;

  my $lastkey = shift;
  my $curr = $self->pos;

  # Increment counter
  $curr++;
  $self->pos($curr);

  return $self->nth_name($curr);
}


# Autoload methods go after =cut, and are processed by the autosplit program.
1;
__END__

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
    nbs_begin_definition(id, status)
    nbs_define_structure(envsid, name, type, sid, status)
    nbs_define_primitive(envsid, name, type, maxdims, maxbytes, sid, status)
    nbs_define_shape(sid, ndims, dims, status)
    nbs_end_definition(name, option, status)
    nbs_restore_definition(name, save_name, status)
    nbs_restore_noticeboard(name, save_name, status)
    nbs_save_noticeboard(id, status)

Note that nbs_get_ and nbs_put_ require a separate routine for each
type. Additionally, the get and set routines expect array arguments.

=head1 REQUIREMENTS

This module requires the Starlink::EMS module for error reporting
facilities.

=head1 MORE INFORMATION

More information on NBS systems can be found in Starlink User
Note 77 (D.J. Allan, 1995).
Starlink can be contacted at http://www.starlink.ac.uk/

=head1 AUTHOR

T. Jenness (t.jenness@jach.hawaii.edu). Copyright T. Jenness and
Particle Physics and Astronomy Research Council 1997, 1998.
All Rights Reserved.

=head1 SEE ALSO

perl(1). L<perltie>, L<Starlink::EMS>

=cut
