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
use base qw/ Starlink::Prologue::Parser::Base /;

=head1 METHODS

=head2 Constructor

=over 4

=item B<new>

Create a new STARLSE parser

  $p = new Starlink::Prologue::Parser::STARLSE();

=back

=head2 Accessors

For a list of generic accessors see C<Starlink::Prologue::Parser::Base>.

=head2 General

Also see C<Starlink::Prologue::Parser::Base>.

=over 4

=item B<push_line>

Add a new line of content to the parser. Since some prologues are only
terminated when the first line of code is discovered, it is sometimes
necessary to return both a line of code and the relevant
C<Starlink::Prologue> prologue object. The C<push_line> method returns
the line itself if the line is not part of a prologue, and/or a
prologue object if the prologue is terminated by this line.

  ($line, $prologue) = $parser->push_line( $input );

The returned line will not have a new line.

=cut

sub push_line {
  my $self = shift;
  my $line = shift;
  chomp($line);

  my $prl = $self->prologue;
  if (defined $prl) {
    # currently active prologue
    my $cchar = $prl->comment_char;
    my $r = quotemeta( $cchar );

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

    if ( $line =~ /^\s*$r\-/  #  *-
	 || $line =~ /^\s*\*\/\s*$/      #  */
	 || ($line !~ /\s*$r/ && $line =~ /\w/)  # real code
       ) {
      # print "End of prologue detected\n";
      # end of prologue so flush the current section
      $self->flush_section();

      # end of prologue and end of C comment
      $prl->end_c_comment(1) if ($line =~ /^\s*\*\/\s*$/);

      # undefine the internal copy
      $self->prologue( undef );

      # must return the line itself if it was real code
      my $retval;
      $retval = $line if ($line !~ /\s*$r/ && $line =~ /\w/);

      # return the newly minted version
      return ($retval, $prl);

    } elsif ( $line =~ /^\s*$r\s{1,4}([A-Z][A-Za-z\s]*)\s*:\s*$/ ) {
      # section - but only allow up to 4 spaces between comment
      # char and title. This prevents History entries being included.
      $self->flush_section();
      $self->section($1);
    } elsif ( $line =~ /^\s*$r(\s+.*)\s*$/ ) {
      # prologue content (or *- but that is dealt with above)
      # Include leading spaces since we want to retain indenting
      # strip off the first 5 spaces (standard formatting)
      # Replace tabs with 8 characters (not clever)
      my $content = $1;
      $content =~ s/\t/        /;
      $content =~ s/^\s{2,5}//;
      push(@{$self->content()}, $content);
    } else {
      # if nothing matches we have a blank line
      # we should store it in case it is within a section
      my $content = $line;
      $content =~ s/^\s+//;
      $content =~ s/\s+$//;
      $content = '' if $content =~ /^$r$/;
      push(@{$self->content()}, $content);

    }
    # indicate that we are in a prologue but not complete
    return ();

  } else {
    # looking for one to start
    my ($cchar, $title, $startc) = $self->prolog_start( $line );
    if (defined $cchar) {
      # print "Starting prologue with comment char '$cchar' ($line)\n";
      # a new prologue is starting
      $prl = new Starlink::Prologue();
      $self->prologue( $prl );
      $self->_set_prologue_type();
      $prl->comment_char( $cchar );
      $prl->start_c_comment( $startc );

      # are we in a section already?
      if (defined $title) {
	$self->section( $title );
	@{$self->content} = ();
      }

      # inside a prolog so return nothing
      return ();

    } else {
      # not interested
      return ($line, undef);
    }

  }

}

=item B<tidy_content>

Subclass variant of tidy_content(). The base class trims blank lines from
the start and end of the section, this version also removes section terminators
of the form

     {enter_changes_here}

etc.

  $parts->tidy_content;

=cut

sub tidy_content {
  my $self =  shift;
  $self->SUPER::tidy_content();

  # look for terminator. Only has lower case and underscore characters.
  my @content = $self->content;
  if ($content[-1] =~ /^\{[a-z_]+\}$/) {
    pop(@{$self->content});
  }

  # call base class again just in case we have a blank line left
  $self->SUPER::tidy_content();

}

=back

=head2 Class Methods

=over 4

=item B<prolog_start>

Returns true if the line supplied could be interpreted as a
start of a prolog. Otherwise returns false.

 $is_start = $p->prolog_start( $line );

In list context returns the comment character and optional section
heading (in case we are starting with a "Name") and flag indicating
whether a C comment started at the same time.

 ($cchar, $title, $startc) = $p->prolog_start( $line );

=cut

sub prolog_start {
  my $self = shift;
  my $line = shift;
  my $cchar;
  my $title;
  my $startc;
  if ($line =~ /^\s*([\*\#])\+\s*$/) {
     # Normal prologue start :  *+ or #+
     $cchar = $1;
  } elsif ($line =~ /^\s*\/\*\+\s*$/        #  /*+
           || $line =~ /^\s*\/\*\s*\*\+\s*$/ ) {  #  /* *+
     # C comment
     $cchar = ($1 || "*");
     $startc = 1;
  } elsif ( $line =~ /^\s*([\*\#])\s+(Name)\s*:\s*$/ ) {
     # Name: (implicit start)
     $cchar = $1;
     $title = $2;
  }

  if (defined $cchar) {
    if (wantarray) {
      return ($cchar, $title, $startc);
    } else {
      return 1;
    }
  }
  return ();
}

=back

=head1 NOTES

This class is a subclass of C<Starlink::Prologue::Parser::Base>

=head1 SEE ALSO

C<Starlink::Prologue::Parser::Base>

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
