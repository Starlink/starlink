package Starlink::Prologue::Parser;

=head1 NAME

Starlink::Prologue::Parser - Parse a source code file and extract prologue

=head1 SYNOPSIS

  use Starlink::Prologue::Parser;

  $p = new Starlink::Prologue::Parser();

  for my $l (@lines) {
    my $status = $p->push_line( $l );
  }


=head1 DESCRIPTION

This class is responsible for parsing source code and creating
C<Starlink::Prologue> objects. This class is made more complex since
it is not clear which form of Starlink prologue will be encountered
until lines appear.

=cut

use 5.006;
use strict;
use warnings;
use Carp;

use Starlink::Prologue;
use Starlink::Prologue::Parser::STARLSE;

=head1 METHODS

=head2 Constructor

=over 4

=item B<new>

Create a new base parser

  $p = new Starlink::Prologue::Parser;

=cut

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $p = bless {
		 WORKER => undef,
		}, $class;

  return $p;
}


=back

=head2 Accessors

=over 4

=back

=head2 General

=over 4

=item B<push_line>

Add a new line of content to the parser. Returns the line
itself if the line is not part of a prologue. Returns undef if the
line was part of a prologue and returns a C<Starlink::Prologue>
object if the line ends a prologue.

=cut

sub push_line {
  my $self = shift;
  my $line = shift;

  # if we are in a prologue then we simply pass this to the worker
  # if we are not in a prologue and this is the start of one, we need
  # to create the specific parser
  # if this is an end we should destroy the worker parser

  my $worker = $self->_worker();

  if (defined $worker) {
    # currently in a prologue
    my $status = $worker->push_line($line);

    # we should only be getting undef or a prologue object
    # unless we are at the top of an old style prolog.
    if (defined $status) {
      if (ref($status) ) {
	# finished this prologue
	$self->_worker( undef );
      }
    }
    return $status;

  } else {
    # not in a prologue, so this might be a start
    my $class;
    if (Starlink::Prologue::Parser::STARLSE->prolog_start( $line ) ) {
      # looks like a standard header
      $class = "STARLSE";
    } elsif ( $line =~ /^\s*\/\*\+\s+\*\\$/ ) {
      # looks like a C file with protective commenting
      $class = "CSTARLSE";
    } elsif ($line =~ /^\*\+\s+[\w_]+\s+\-\s+\w+/) {
      # ADAM/SSE prolog
      $class = "ADAMSSE";
    } else {
      # line looks normal so return it
      return $line;
    }

    # load the class
    my $fclass = "Starlink::Prologue::Parser::$class";
    eval "require $fclass;";
    if ($@) {
      croak "Prologue looked to be of type $class but could not load: $@\n";
    }

    # create a object
    my $w = $fclass->new();
    $self->_worker( $w );
    return $w->push_line( $line );
  }

}

=item B<flush>

Indicate that no more content is arriving for the current
parser. This should be called after a set of C<push_line>
calls when no more lines are expected.

 $prl = $parser->flush();

Returns undef if a prologue was not in progress, else returns
the current prologue (in as complete a state as was available).

The internal state is reset after this is called, any new lines
added to C<push_line> will be treated as a new prologue.

=cut

sub flush {
  my $self = shift;

  my $worker = $self->_worker;
  if (!defined $worker) {
    # no active worker so either no prologue or it was 
    # already complete.
    return;
  } else {
    my $prl = $worker->flush;
    $self->_worker( undef );
    return $prl;
  }

}


=back

=begin __INTERNAL__

=head2 Internal

=over 4

=item B<_worker>

Underlying parser object responsible for this particular style
of header.

=cut

sub _worker {
  my $self = shift;
  if (@_) {
    $self->{WORKER} = shift;
  }
  return $self->{WORKER};
}

=back

=end __INTERNAL__


=head1 SEE ALSO

C<Starlink::Prologue>

=head1 AUTHOR

Tim Jenness E<lt>t.jenness@jach.hawaii.eduE<gt>

Copyright 2006 Particle Physics and Astronomy Research Council.
All Rights Reserved.

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful,but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 59 Temple
Place,Suite 330, Boston, MA  02111-1307, USA

=cut

1;
