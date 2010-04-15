package Starlink::Prologue::Parser::ADAMSSEC;

=head1 NAME

Starlink::Prologue::Parser::ADAMSSEC - Old ADAM/SSE C prologue parser

=head1 SYNOPSIS

  use Starlink::Prologue::Parser::ADAMSSEC;

  $p = new Starlink::Prologue::Parser::ADAMSSEC();

  for my $l (@lines) {
    my $status = $p->push_line( $l );
  }

=head1 DESCRIPTION

This class is responsible for parsing source code and creating
C<Starlink::Prologue> objects.  It only knows how to parse old-style
ADAM/SSE prologues, as written in old ADAM C code.

=cut

use 5.006;
use strict;
use warnings;
use Carp;

use Starlink::Prologue;
use base qw/ Starlink::Prologue::Parser::ADAMSSE /;

=head1 METHODS

=head2 Constructor

=over 4

=item B<new>

Create a new ADAM/SSE (BDK) parser

  $p = new Starlink::Prologue::Parser::ADAMBDK();

=back

=head2 Accessors

For a list of generic accessors see C<Starlink::Prologue::Parser::Base>.

=over 4

=item B<in_body>

True if we are parsing the body of the content (indicated by a Method:
section). False if we are still listing the function start.

=cut

sub in_body {
  my $self = shift;
  if (@_) {
    $self->{IN_BODY} = shift;
  }
  return $self->{IN_BODY};
}

=back

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

The returned line will not have a newline.

=cut

sub push_line {
  my $self = shift;
  my $line = shift;
  chomp($line);

  my $prl = $self->prologue;
  if (defined $prl) {
    # currently active prologue

    # Terminates with a */

    # Sections are normal ADAMSSE  XXX :

    if ( $line =~ /^\s*\*\/\s*$/) {
      # terminator

      # end of prologue so flush the current section
      $self->flush_section();

      # undefine the internal copy
      $self->prologue( undef );
      $self->in_body( 0 );

      # return the newly minted version
      return (undef, $prl);

    } elsif ( $line =~ /\s*\/\*\s+Method/) {
      # Start of real prologue body
      $self->in_body( 1 );
      $self->flush_section();
      $self->section( "Algorithm" );

    } elsif ( $line =~ /^\s+([A-Z][A-Za-z\s]*)\s*:\s*$/ ) {
      # section start
      if ($self->in_body) {
	$self->flush_section();
	my $section = $1;
	$section =~ s/\s+$//;
	$self->section( $section );
      }
    } elsif ( $line =~ /^(\s+.*)\s*$/ ) {
      # prologue content
      # Include leading spaces since we want to retain indenting
      # strip off the first 5 spaces (standard formatting)
      if ($self->in_body) {
	my $content = $1;
	$content =~ s/^\s{2,6}//;

	push(@{$self->content()}, $content);
      }
    } else {
      # if nothing matches we have a blank line
      # we should store it in case it is within a section
      if ($self->in_body) {
	my $content = $line;
	$content =~ s/^\s+//;
	$content =~ s/\s+$//;
	push(@{$self->content()}, $content);
      }
    }
    if ($self->in_body) {
      # indicate that we are in a prologue but not complete
      return (undef, undef );
    } else {
      return ($line, undef );
    }
  } else {

    # looking for one to start
    my ($title, $purpose) = $self->prolog_start( $line );
    if (defined $title) {

      # a new prologue is starting
      $prl = new Starlink::Prologue();
      $self->prologue( $prl );
      $self->_set_prologue_type();

      # Force C mode
      $prl->comment_char( '*' );
      $prl->start_c_comment( 1 );
      $prl->end_c_comment( 1 );

      # we should always have this
      $prl->name( $title );

      if (defined $purpose && $purpose =~ /\w/) {
	$prl->purpose( $purpose );
      }

      # inside a prolog so return nothing
      return ();

    } else {
      # not interested
      return ($line, undef);
    }
  }
  return ();
}

=item B<tidy_content>

Tweak dates in History and pass the result to the parent class for
further translation.

=cut

sub tidy_content {
  my $self = shift;

  my $section = $self->section();
  my @content = $self->content;

  if ($section eq 'History' ) {
    for my $hline (@content) {
      if ($hline =~ /\d{1,2}\s?\w{3,}\s?\d\d\d\d:/) {
	$hline =~ s/(\d{1,2})\s?(\w{3,})\s?(\d\d\d\d:)/$1$2$3/;

	# fix up author
	if ($hline =~ /(.*):(.*):\s(.*)$/) {
	  my $date = $1;
	  my $content = $2;
	  my $author = uc($3);
	  $author =~ s/\s//g;
	  $hline = "$date: $content ($author)";
	}

      } elsif ($hline =~ /^(.*):(.*)(\s\d{1,2}\/\d{1,2}\/\d{2,4})$/) {
	my $date = $3;
	my $content = $1;
	my $author = uc($2);
	$author =~ s/\s//g;
	$date =~ s/^\s+//;
	$content =~ s/^\s+//; $content =~ s/\s+$//;
	$hline = "$date: $content ($author)";
      }

    }
    @{$self->content} = @content;
  }

  # call ADAMSSE tidier
  $self->SUPER::tidy_content();
}

=back

=head2 Class Methods

=over 4

=item B<prolog_start>

Returns true if the line supplied could be interpreted as a
start of a prolog. Otherwise returns false.

 $is_start = $p->prolog_start( $line );

In list context returns the name of the routine and the purpose (which
should all be on a single line).

 ($title, $purpose) = $p->prolog_start( $line );

An ADAM C prologue (BDK-style) looks either like:

 /*= NAME - Purpose */
or
 /*+ NAME - Purpose */
or
 /*= NAME */

=cut

sub prolog_start {
  my $self = shift;
  my $line = shift;
  my $title;
  my $purpose;
  if ($line =~ /^\s*\/\*[\+=]\s*([\w_\d]+)\s*-?\s*(.*)\s*\*\/\s*$/) {
     # Normal prologue start :  /*+ title - purpose */
     $title = $1;
     $purpose = ucfirst($2) if defined $2;
  }

  # title is mandatory
  if (defined $title ) {
    if (wantarray) {
      return ($title, $purpose);
    } else {
      return 1;
    }
  }
  return ();
}

=back

=head1 NOTES

This class is a subclass of C<Starlink::Prologue::Parser::Base>

=head1 EXAMPLE

This is an ADAM C prologue used in PCS:

 /*=  AMS_ACCEPT - accept a request to open a path */

 static void ams_accept
 (
 struct a_loc_init_in * loc_init_in,  /* the message requesting
                                         initialisation (given) */
 sendq_type ackq,                     /* queue for returning
                                         acknowledgement (given) */
 int *status                          /* global status (given and
                                         returned) */
 )

 /*   Method :
       An "init" message has been received from another task requesting a
       connecting path to be set up. Allocate a data structure to the path
       and return an acceptance message.
      Authors :

      History :
       15Jun 1992: Created: irj
       18Jun 1992: Tidied : irj, skr
       12Apr 1994: make function static (BDK)
       27Jun 1994: use msp_mkcomq to get command queue of other task (BDK)
 */

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
