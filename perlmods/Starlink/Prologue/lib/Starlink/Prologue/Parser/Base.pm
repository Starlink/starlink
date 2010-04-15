package Starlink::Prologue::Parser::Base;

=head1 NAME

Starlink::Prologue::Parser::Base - Base class for Prologue parsers

=head1 SYNOPSIS

  use base qw/ Starlink::Prologue::Parser::Base /;

=head1 DESCRIPTION

This class provides a base class for subclasses that implement
real parsers. It does not know how to parse any prologues itself.

=cut

use 5.006;
use strict;
use warnings;
use Carp;

use Starlink::Prologue;

=head1 METHODS

=head2 Constructor

=over 4

=item B<new>

Create a new base parser

  $p = new Starlink::Prologue::Parser::Base();

=cut

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $p = bless {
		 PROLOGUE => undef,
		 SECTION => undef,
		 CONTENT => [],
		}, $class;

  return $p;
}


=back

=head2 Accessors

=over 4

=item B<prologue>

Prologue that is currently being populated. It will be deleted when the
current prolog completes.

  $prl = $parser->prologue;
  $parser->prologue( $prl );

=cut

sub prologue {
  my $self = shift;
  if (@_) {
    $self->{PROLOGUE} = shift;
  }
  return $self->{PROLOGUE};
}

=item B<section>

Title of currently active section.

  $title = $parser->section;
  $parser->section( $title );

=cut

sub section {
  my $self = shift;
  if (@_) {
    my $s = shift;
    if (defined $s) {
      $s =~ s/\s+$//; # trim
      $s =~ s/^\s+//;
    }
    $self->{SECTION} = $s;
  }
  return $self->{SECTION};
}

=item B<content>

Contents of currently active section.

 @{$parser->content} = ();
 @lines = $parser->content;

Returns the lines in list context, the reference to the internal
array in scalar context.

=cut

sub content {
  my $self = shift;
  if (wantarray) {
    return @{$self->{CONTENT}};
  } else {
    return $self->{CONTENT};
  }
}

=item B<tidy_content>

Tidy the content for the current section. The base class trims off blank
lines from the start and end of the section.

 $parser->tidy_content();

Called from flush_section() and can be sub-classed.

=cut

sub tidy_content {
  my $self = shift;

  # local copy for editing
  my @content = $self->content;

  # remove blank lines from the front
  my $nonblank;
  for my $pos (0..$#content) {
    # loop until we
    if (defined $content[$pos] && $content[$pos] =~ /\S/) {
      $nonblank = $pos;
      last;
    }
  }
  if (defined $nonblank) {
    @content = @content[$nonblank..$#content];
  }

  # remove any blank lines from the end of the content
  my $pos = $#content;
  if ($pos > -1) {
    while ($pos >=0 && $content[$pos] =~ /^\s*$/) {
      $pos--;
    }
    @content = @content[0..$pos];
  }

  # Store it back
  @{$self->content()} = @content;
}

=item B<flush_section>

Migrate the current section information into the C<Starlink::Prologue>
object.

  $parser->flush_section();

=cut

sub flush_section {
  my $self = shift;
  my $section = $self->section;
  return unless defined $section;

  # tidy the content before storing
  $self->tidy_content();

  # store the content if we had content
  my $prl = $self->prologue;
  my @content = $self->content();
  $prl->content( $section, @content) if @content;

  # reset internal state
  $self->section( undef );
  @{ $self->content } = ();

}

=back

=head2 General

=over 4

=item B<push_line>

Add a new line of content to the parser. Returns the line
itself if the line is not part of a prologue. Returns undef if the
line was part of a prologue and returns a C<Starlink::Prologue>
object if the line ends a prologue.

In the base implementation, always returns the input line.

=cut

sub push_line {
  my $self = shift;
  my $line = shift;
  return $line;
}

=item B<flush>

Flush the current prologue and return it. Resets the object.

  $prologue = $parser->flush();

Returns undef if no active prologue.

This should be called when no more lines are to be parsed to
make sure that unterminated prologues are returned.

=cut

sub flush {
  my $self = shift;
  my $prl = $self->prologue;
  return () unless defined $prl;

  # flush internal content to prologue
  $self->flush_section();

  # reset internal content
  $self->prologue(undef);

  # return the prologue as-is
  return $prl;
}

=begin __INTERNAL__

=head2 Internal Methods

=over 4

=item B<_set_prologue_type>

=cut

sub _set_prologue_type {
  my $self = shift;
  my $prl = $self->prologue;
  return unless defined $prl;

  # get the class name and chop off everything before the last ::
  my $class = ref($self);
  $class =~ s/.*:://;
  $prl->prologue_type( $class )
}


=back

=end __INTERNAL__

=head2 Class Methods

=over 4

=item B<prolog_start>

Returns true if the line supplied could be interpreted as a
start of a prolog. Otherwise returns false.

 $is_start = $p->prolog_start( $line );

Always returns false in the base class.

=cut

sub prolog_start {
  return ();
}

=back

=head1 SEE ALSO

C<Starlink::Prologue::Parser>,
C<Starlink::Prologue::Parser::STARLSE>,
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
