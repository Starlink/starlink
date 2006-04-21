package Starlink::Prologue::Parser::STARLSE;

=head1 NAME

Starlink::Prologue::Parser::STARLSE - Standard Starlink prologue parser

=head1 SYNOPSIS

  use Starlink::Prologue::Parser::STARLSE;

  $p = new Starlink::Prologue::Parser::STARLSE();

  for my $l (@lines) {
    my $status = $p->push_line( $l );
  }

=head1 DESCRIPTION

This class is responsible for parsing source code and creating
C<Starlink::Prologue> objects.  It only knows how to parse standard
STARLSE-style prologues.

=cut

use 5.006;
use strict;
use warnings;
use Carp;

use Starlink::Prologue;

=head1 METHODS

=head1 METHODS

=head2 Constructor

=over 4

=item B<new>

Create a new STARLSE parser

  $p = new Starlink::Prologue::Parser::STARLSE;

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

=cut

=item B<prologue>

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

=cut

sub section {
  my $self = shift;
  if (@_) {
    $self->{SECTION} = shift;
  }
  return $self->{SECTION};
}

=item B<content>

Contents of currently active section.

 @{$self->content} = ();

=cut

sub content {
  my $self = shift;
  return $self->{CONTENT};
}

=back

=head2 General

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

  my $prl = $self->prologue;
  if (defined $prl) {
    # currently active prologue
    my $cchar = $prl->comment_char;

    # now need to parse the line. We have the following cases:
    # - end of prologue (*-)
    # - start of section (* Section:)
    # - content (*  XXXX)
    # - blank line (*   )
    # - blank line (    )
    # - programmatic (    XXXX)
    # - C comment end  */ (sometimes "*- */")
    #
    # blank lines are treated as content unless they are
    #   before a section.
    # programmatic content in a modern prologue indicates
    #   that the prologue finished prematurely.
    # C comment end usually indicates end of prologue in modern
    #   implementation

    if ( $line =~ /^\s*$cchar\-/  #  *-
	 || $line =~ /\*\/$/      #  */
	 || ($line !~ /\s*$cchar/ && $line =~ /\w/)  # real code
       ) {
      # end of prologue so flush the current section
      $self->flush_section();

      # end of prologue and end of C comment
      $prl->end_c_comment(1) if ($line =~ /\*\/$/);

      # undefine the internal copy
      $self->prologue( undef );

      # return the newly minted version
      return $prl;

    } elsif ( $line =~ /^\s*$cchar\s+(.*)$/ ) {
      # prologue content (or *-)

    }


  } else {
    # looking for one to start
    my ($cchar, $title) = $self->prolog_start( $line );
    if (defined $cchar) {
      # a new prologue is starting
      $prl = new Starlink::Prologue();
      $self->prologue( $prl );
      $prl->comment_char( $char );

      # are we in a section already?
      if (defined $title) {
	$self->section( $section );
	@{$self->content} = ();
      }

      # inside a prolog so return nothing
      return undef;

    } else {
      # not interested
      return $line;
    }

  }

}

=item B<prolog_start>

Returns true if the line supplied could be interpreted as a
start of a prolog. Otherwise returns false.

 $is_start = $p->prolog_start( $line );

In list context returns the comment character and optional section
heading (in case we are starting with a "Name".

 ($cchar, $title) = $p->prolog_start( $line );

=cut

sub prolog_start {
  my $self = shift;
  my $line = shift;
  if ($line =~ /^\s*([\*\#])\+\s*$/               #  *+
     || $line =~ /^\s*\/([\*\#])\+\s*$/           #  /*+
     ||  $line =~ /^\s*([\*\#])\s+(Name)\s*:\s*$/ #  * Name:
     ) {
    my $cchar = $1;
    my $title = $2;
    if (wantarray) {
      return ($cchar, $title);
    } else {
      return 1;
    }
  }
  return ();
}

=back

=head1 SEE ALSO

C<Starlink::Prologue::Parser>

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
