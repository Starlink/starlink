package GSD;

=head1 NAME

GSD - A module to allow read access to JCMT GSD data.

=head1 SYNOPSIS

  use GSD;

  $status = gsdOpenRead($file,$version,$label,$no_items,$fptr,$file_dsc,
			$item_dsc,$data_ptr);
  $status = gsdGet0c($file_dsc, $item_dsc, $data_ptr, $item, $data);
  $status = gsdClose($fptr, $file_dsc, $item_dsc, $data_ptr);

  $gsd = new GSD($filename);
  @data = $gsd->GetByName('C13DAT');

  tie %hash, 'GSD', $filename;
  foreach (keys %hash) { print "$_\n";}

  tie @array, 'GSD', $gsdobject;
  foreach (0.. $#array) { print $array[$_],"\n";}

=head1 DESCRIPTION

This module add the ability to read JCMT GSD data from perl. Since the
GSD library is read only, GSD files can not be written by this module.
An interface using the C API is provided but in general it is recommended
that either the object-oriented or tied interfaces are used since these
are much simpler and hide the C API.

=head1 LIBRARY INTERFACE

The following library calls are implemented:

=over 4

=item File inquiry

gsdOpenRead gsdClose gsdFind gsdItem gsdInqSize

=item Scalar access

gsdGet0d gsdGet0r gsdGet0i gsdGet0l gsdGet0c gsdGet0b gsdGet0w

=item Array access

gsdGet1d gsdGet1r gsdGet1i gsdGet1c

=item Array access via string

gsdGet1dp gsdGet1rp gsdGet1ip

=back

Commands that return or require arrays must pass in a reference
to an array:

  gsdInqSize($file_dsc, $item_dsc, $data_ptr, $i, \@dimnm, \@dimunt,
	     \@dimvals, $actdims, $size);

Note also that the C<gsdInqSize> subroutine does not require
a value for MAXDIMS since the perl interface allows for the maximum
size automatically. The C<gsdGet1x> functions return a vectorised
array rather than an N-D array. Note that the C<fptr> argument
to C<gsdOpenRead> and C<gsdClose> is an opaque object and not
a filehandle.

=cut

require 5.005;

use strict;
use Carp;

use Exporter;
use DynaLoader;

use vars qw/$VERSION @ISA @EXPORT/;


@ISA = qw(Exporter DynaLoader);

@EXPORT = qw( gsdOpenRead gsdClose gsdFind gsdItem gsdInqSize
              gsdGet0d gsdGet0r gsdGet0i gsdGet0l gsdGet0c gsdGet0b
	      gsdGet0w
	      gsdGet1d gsdGet1r gsdGet1i gsdGet1c
	      gsdGet1dp gsdGet1rp gsdGet1ip
	    );

$VERSION = '1.13';

bootstrap GSD  $VERSION;

# OO interface

=head1 OBJECT ORIENTED INTERFACE

An OO interface is provided. This can be used to hide
the GSD layer from the user.

=head2 Constructors

=over 4

=item B<new>

Constructor. Takes a GSD filename as argument. Returns undef
on error.

 $gsd = new GSD("obs_das_0015.dat");

=cut

sub new ($$) {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  # Try to open the argument
  my $file = shift;

  my ($version, $label, $no_items, $fptr, $file_dsc, $item_dsc, $data_ptr);
  my $status = gsdOpenRead(
			   $file, $version, $label, $no_items,
			   $fptr, $file_dsc, $item_dsc, $data_ptr
			  );
  # Return undef on error
  return undef if $status != 0;

  # Strip trsiling space
  $label =~ s/\s+$//;

  # Create the object (an anon hash) and populate it
  my $gsd = {
	     FILENAME => $file,
	     VERSION  => $version,
	     LABEL    => $label,
	     NITEMS   => $no_items,
	     FPTR     => $fptr,
	     FILE_DSC => $file_dsc,
	     ITEM_DSC => $item_dsc,
	     DATA_PTR => $data_ptr
	    };

  # Bless the object into the class
  bless( $gsd, $class);

}

=back

=head2 Accessors

Accessor methods are provided for some of the GSD file
information. Public access to the file descriptor, item descriptor
and data pointer are not provided.

=over 4

=item B<label>

Returns the label of the GSD file.

  $version = $gsd->label;

=cut

sub label {
  return $_[0]->{LABEL};
}

=item B<nitems>

Returns the number of items stored in the GSD file.

  $version = $gsd->nitems;

=cut

sub nitems {
  return $_[0]->{NITEMS};
}

=item B<version>

Returns the version number of the GSD file.

  $version = $gsd->version;

=cut

sub version {
  return $_[0]->{VERSION};
}

=back

=head2 Simplified Library Methods

This section describes simplified forms of the standard library
functions since you are meant to be writing Perl and not knowing
how C works. These methods hide the use of the item and file
descriptors and provide explicit return values.

=over 4

=item B<Find>

Find the GSD item number by name.

  ($itemno, $units, $type, $array) = $gsd->Find($name);

Returns undef on error (technically 4 undefs).
In a scalar context, simply returns the item number.

  $itemno = $gsd->Find($name);

Spaces are stripped from the end of the units string.

=cut

sub Find ($$) {
  my $status = gsdFind($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
		       $_[1], my $itemno, my $units, my $type, my $array);
  if (wantarray) {
    if ($status == 0) {
      $units =~ s/\s+$//;
      return ($itemno, $units, $type, $array);
    } else {
      return (undef, undef, undef, undef);
    }
  } else {
    return ( $status == 0 ? $itemno : undef );
  }
}

=item B<Get0x>

All the C<gsdGet0x> commands are available as methods of the following
form:

  $data = $gsd->Get0x($itemno);

Returns undef if an error occurred.

=cut

sub Get0d ($$) {
  my $status = gsdGet0d($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			$_[0]->{DATA_PTR}, $_[1], my $value);
  return ( $status == 0 ? $value : undef );
}

sub Get0r ($$) {
  my $status = gsdGet0r($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			$_[0]->{DATA_PTR}, $_[1], my $value);
  return ( $status == 0 ? $value : undef );
}

sub Get0i ($$) {
  my $status = gsdGet0i($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			$_[0]->{DATA_PTR}, $_[1], my $value);
  return ( $status == 0 ? $value : undef );
}

sub Get0l ($$) {
  my $status = gsdGet0l($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			$_[0]->{DATA_PTR}, $_[1], my $value);
  return ( $status == 0 ? $value : undef );
}

sub Get0c ($$) {
  my $status = gsdGet0c($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			$_[0]->{DATA_PTR}, $_[1], my $value);
  return ( $status == 0 ? $value : undef );
}

sub Get0b ($$) {
  my $status = gsdGet0b($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			$_[0]->{DATA_PTR}, $_[1], my $value);
  return ( $status == 0 ? $value : undef );
}

sub Get0w ($$) {
  my $status = gsdGet0w($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			$_[0]->{DATA_PTR}, $_[1], my $value);
  return ( $status == 0 ? $value : undef );
}

=item B<Get1x>

All the C<gsdGet0x> commands are available as methods of the following
form:

  @data = $gsd->Get1x($itemno, \@dimvals, \@start, \@end);

Note that this routine does not require that the number of dimensions
for dimvals is given (this is known to perl) or the number of
values returned to the user (you can simply find the size of the array).
C<@dimvals> is an array reflecting the required shape of the array
which is to be sliced by C<@start> and C<@end>. This does not necessarily
have to match the actual shape of the array item, allowing you to
extract arbritrary slices by modifying the dimensionality. It is assumed
that the product of C<@dimvals> is not greater than the size
of the GSD item (although this is not checked!).

In addition, if only a single start and end position are required
they can be supplied as scalars

  @data = $gsd->Get1x($itemno, $size, 1, $size);

Returns undef if an error occurred.

=cut

sub Get1d ($$$$$) {
  my $ndims = ( ref($_[2]) ? scalar(@{$_[2]}) : 1);
  my @values;
  my $status = gsdGet1d($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			$_[0]->{DATA_PTR}, $_[1], $ndims, $_[2],
		        $_[3], $_[4], \@values, my $actvals);
  return ( $status == 0 ? @values : undef );
}

sub Get1r ($$$$$) {
  my $ndims = ( ref($_[2]) ? scalar(@{$_[2]}) : 1);
  my @values;
  my $status = gsdGet1r($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			$_[0]->{DATA_PTR}, $_[1], $ndims, $_[2],
		        $_[3], $_[4], \@values, my $actvals);
  return ( $status == 0 ? @values : undef );
}

sub Get1i ($$$$$) {
  my $ndims = ( ref($_[2]) ? scalar(@{$_[2]}) : 1);
  my @values;
  my $status = gsdGet1i($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			$_[0]->{DATA_PTR}, $_[1], $ndims, $_[2],
		        $_[3], $_[4], \@values, my $actvals);
  return ( $status == 0 ? @values : undef );
}

sub Get1c ($$$$$) {
  my $ndims = ( ref($_[2]) ? scalar(@{$_[2]}) : 1);
  my @values;
  my $status = gsdGet1c($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			$_[0]->{DATA_PTR}, $_[1], $ndims, $_[2],
		        $_[3], $_[4], \@values, my $actvals);
  return ( $status == 0 ? @values : undef );
}

=item B<Item>

Return the GSD item by number.

  ($name, $units, $type, $array) = $gsd->Find($itemno);

Returns undef on error (technically 4 undefs).
In a scalar context, simply returns the item name.

  $name = $gsd->Find($itemno);

Trailing spaces are stripped from the end of the strings (since the
GSD library itself pads the string).

=cut

sub Item ($$) {
  my $status = gsdItem($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
		       $_[1], my $name, my $units, my $type, my $array);
  if (wantarray) {
    if ($status == 0) {
      $name =~ s/\s+$//;
      $units =~ s/\s+$//;
      return ($name, $units, $type, $array);
    } else {
      return (undef, undef, undef, undef);
    }
  } else {
    $name =~ s/\s+$//;
    return ( $status == 0 ? $name : undef );
  }
}

=item B<InqSize>

Inquire the array size. Does not work on scalar items.

   ($dimnm, $dimunt, $dimvals, $size) = $gsd->InqSize($itemno);

This methods returns references to 3 arrays containing the
names of each dimension, the units of each dimension and the
size of each dimensions. The number of dimensions can be
retrieved simply by counting the number of entries in the
arrays. The total size of the array is returned for convenience
even though that is simply the product of C<dimvals>.

Trailing spaces are stripped from strings.

undef (times 4) is returned on error.

=cut

sub InqSize ($$) {
  my (@dimnm, @dimunt, @dimvals, $size, $actdims);
  my $status = gsdInqSize($_[0]->{FILE_DSC}, $_[0]->{ITEM_DSC},
			  $_[0]->{DATA_PTR}, $_[1],
			  \@dimnm, \@dimunt, \@dimvals, $actdims, $size);
  if ($status == 0) {
    foreach (@dimnm) { s/\s+$//; }
    foreach (@dimunt) { s/\s+$//; }
    return (\@dimnm, \@dimunt, \@dimvals, $size);
  } else {
    return (undef, undef, undef, undef);
  }
}

=item B<DESTROY>

Automatic destructor. This tidies up the object and frees
memory when the object goes out of scope. It is not
necessary to run this method explicitly.

This methods runs C<gsdClose> so that the user does not
need to.

=cut

sub DESTROY {
  # Since this is called when the GLOB is tidied up
  # we need check that we have an argument on the stack
  # from an object
  if (scalar(@_) && UNIVERSAL::isa($_[0],'HASH') ) {
    gsdClose($_[0]->{FPTR},
	     $_[0]->{FILE_DSC},
	     $_[0]->{ITEM_DSC},
	     $_[0]->{DATA_PTR}
	    );
  }
}


=back

=head2 Wrapper Methods

This section describes commands that are used to simplify GSD
access such that the data can be retrieved by name or number without
having to know what type the item is or whether it is an array item
or not. They hide the actual GSD library calls.

=over 4

=item B<GetByName>

Returns the contents of the GSD item from the named entry.

  $data = $gsd->GetByName($name);
  @data = $gsd->GetByName($name);

Works for array and scalar types. The data are returned in their
native precision (float, double etc).  Trailing spaces are removed
when character values are returned.

In a scalar context, array items are returned as references to
arrays (unless they happen to be PDL objects (see L<GSD::PDL>)
in which case the PDL is returned).

Returns undef on error.

=cut

sub GetByName ($$) {
  my $self = shift;
  my $name = shift;

  # Find the item in the GSD file
  my ($itemno, $units, $type, $array) = $self->Find($name);

  # Return if bad status
  return undef unless defined $itemno;

  # Now get the data
  return $self->_getTypedData($itemno, $type, $array);
}

=item B<GetByNum>

Returns the contents of a GSD item from position.

  @data = $gsd->GetByNum($number);
  $data = $gsd->GetByNum($number);

Works for array and scalar types. The data are returned in their
native precision (float, double etc).  Trailing spaces are removed
when character values are returned.

In a scalar context, array items are returned as references to
arrays (unless they happen to be PDL objects (see L<GSD::PDL>
in which case the PDL is returned).

Returns undef on error.

=cut

sub GetByNum ($$) {
  my $self = shift;
  my $itemno = shift;

  # Find the item in the GSD file
  my ($name, $units, $type, $array) = $self->Item($itemno);

  # Return if bad status
  return undef unless defined $name;

  # Now get the data
  return $self->_getTypedData($itemno, $type, $array);
}

# Internal routine to decide on the correct get method
# to use given a item number, type and array flag
# Not a public method

sub _getTypedData {
  my $self = shift;
  my ($itemno, $type, $array) = @_;

  # Construct the method name
  my $method = "Get$array" . lc($type);

  # If the method is there, run it
  if ($self->can($method) ) {
    if ($array) {
      my ($dimnm, $dimunt, $dimvals, $size) = $self->InqSize($itemno);
      if (defined $size) {
	# We need to specify the correct dimensions here so that
	# the PDL subclass will get the shape correct
	my (@start, @end);
	foreach (@$dimvals) {
	  push(@start, 1);
	  push(@end, $_);
	}
	my @data = $self->$method($itemno, $dimvals, \@start, \@end);
	if ($type eq 'C') { # Strip trailing spaces
	  foreach (@data) { s/\s+$//; }
	}
	# Check context for return arguments
	if (wantarray) {
	  return @data;
	} else {
	  # Check to see if we have a PDL as the first entry
	  if (UNIVERSAL::isa($data[0], 'PDL')) {
	    return $data[0];
	  } else {
	    return \@data;
	  }
	}
      }
    } else {
      my $data = $self->$method($itemno);
      $data =~ s/\s+$// if (defined $data && $type eq 'C');
      return $data;
    }

  } else {
    return undef;
  }
}

=back

=head1 TIED INTERFACE

It is also possible to tie a perl array or hash to a GSD file
using the C<tie> command:

   tie %hash, 'GSD', $filename;
   tie @array, 'GSD', $filename;

   tie %hash, 'GSD', $gsdobj;
   tie @array, ref($gsdobj), $gsdobj;

   $data = $hash{'NAME'};
   $data = $array[5];

   foreach ( keys %hash ) { print $_,"\n" }

These provide a simplified interface to the C<GetByNum> and
C<GetByName> methods. Note that the data is vectorised.

If a GSD object is provided to the tie rather than a filename it is
assumed that the array or hash should be tied to the supplied object.
This allows the tie to be used when a file or tie has already been
setup (eg setting up a tie to an array and a hash for the same GSD
file is inefficient if the GSD file has to be opened separately for
each tie).

  tie %hash, 'GSD';

The object associated with the tie can be retrieved either
by storing the return value directly or by using the C<tied>
function. This is useful if further information is required such
as the dimensions and the units.

Note that following Perl convention, the first element in the array
(position 0) is GSD item 1.

Also, array items are returned in an array reference since hashes
and arrays can only return scalar items. The L<GSD::PDL|GSD::PDL>
module can be used for the tie in order to return PDL objects
rather than perl arrays.

The array and hash are readonly.

=cut

# READ-ONLY ARRAY INTERFACE

sub TIEARRAY {
  # Given a class and filename (or object), return an object
  return ( ref($_[1]) ?  $_[1] : $_[0]->new( $_[1] ) );
}

sub FETCHSIZE {
  return $_[0]->nitems;
}

sub STORESIZE { }

sub EXTEND {  }

sub PUSH {
  carp "GSD files are read-only. Can add item $_[1]";
}

sub SHIFT {
  carp "GSD files are read-only. Can shift items off the array";
}

sub UNSHIFT {
  carp "GSD files are read-only. Can not UNSHIFT";
}

sub SPLICE {
  carp "GSD files are read-only. Can not SPLICE";
}

# READ-ONLY HASH INTERFACE

sub TIEHASH {
  # Given a class and filename (or object), return an object
  return ( ref($_[1]) ?  $_[1] : $_[0]->new( $_[1] )  );
}

sub EXISTS {
  my $itemno = $_[0]->Find($_[1]);
  return ( defined $itemno ? 1 : 0 );
}

sub FIRSTKEY {
  return $_[0]->Item(1);
}

sub NEXTKEY {
  # Need to get the position from the last key
  my $last = $_[0]->Find($_[1]);
  return $_[0]->Item(++$last);
}

# SHARED TIE METHODS

sub FETCH {
  # Need to see whether we are fetching a number (ARRAY) or string
  my $key = $_[1];

  my @data;
  if ($key =~ /^-?\d+$/) {
    # We have an ARRAY
    # Need to take care of negative indices and also
    # that counting starts at 0 for a perl array
    if ($key >= 0) {
      $key++;  # increment offset
    } else {
      $key += $_[0]->nitems();
    }
    @data = $_[0]->GetByNum($key);
  } else {
    # HASH
    @data = $_[0]->GetByName($key);
  }

  # Now we have to return an array ref if we have more than one
  # item
  if ($#data == 0) {
    return $data[0];
  } elsif ($#data > 0) {
    return [ @data ];
  } else {
    return undef;
  }

}


sub STORE {
  carp "GSD files are read-only. Unable to store new value for key/position $_[1]";
}

sub DELETE {
  carp "GSD files are read-only. Can not delete item $_[1]";
}

sub CLEAR {
  carp "GSD files are read-only. Can not clear all entries. Use unlink() instead";
}

=head1 NOTES

The C<gsdGet1xp> routines return a packed string rather than an array.
This is far more convenient for packages such as C<PDL> which need
to manipulate the array without a perl array overhead. The PDL::IO::GSD
module hides this from the casual user.

This library is read-only.

=head1 AUTHOR

This module was written by Tim Jenness, E<lt>t.jenness@jach.hawaii.eduE<gt>

Copyright (C) 1995-2000,2003 Tim Jenness and the UK Particle Physics and
Astronomy Research Council. All Rights Reserved.

=head1 CVS REVISION

 $Id$

=head1 ACKNOWLEDGMENTS

This module would not have been possible without the help
of Karl Glazebrook and Frossie Economou (frossie@jach.hawaii.edu).

=cut

1;
