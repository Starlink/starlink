package Starlink::Prologue::Parser::GNS;

=head1 NAME

Starlink::Prologue::Parser::GNS - Parse old GNS prologues

=head1 SYNOPSIS

  use Starlink::Prologue::Parser::GNS;

  $p = new Starlink::Prologue::Parser::GNS();

  for my $l (@lines) {
    my $status = $p->push_line( $l );
  }

=head1 DESCRIPTION

This class is responsible for parsing source code and creating
C<Starlink::Prologue> objects.  It only knows how to parse old prologues
favoured by the original GNS source code.

Note that since the GNS prologue starts with the same initial
indicator (*+) as STARLSE prologues, STARLSE recgonition must
be disabled in C<Starlink::Prologue::Parser> before an GNS prologue
will be understood.

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

Create a new ADAM/SSE parser

  $p = new Starlink::Prologue::Parser::ADAMSSE();

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

The returned line will not have a newline.

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
    # - start of section (* Section :)
    # - content (*  XXXX)
    # - blank line (*   )
    # - terminator (*++) if we are too late

    if ( $line =~ /^\s*$r\+\+\s*$/  || #   *++
	 $line =~ /^\s*$r\+\s*$/       #   *+
       ) {
      # print "End of prologue detected\n";
      # end of prologue so flush the current section
      $self->flush_section();

      # undefine the internal copy
      $self->prologue( undef );

      # For GNS we must combine input/output args into Arguments:
      my @given = $prl->content( "Input Arguments" );
      my @returned = $prl->content( "Output Arguments" );
      $prl->content("Arguments", @given, @returned);
      $prl->del_section( "Input Arguments" );
      $prl->del_section( "Output Arguments" );

      # return the newly minted version
      return (undef, $prl);

    } elsif ( $line =~ /^\s*$r\s+([A-Z][A-Za-z\s_]*)\s*:\s*$/ ) {
      # section start (must start with upper case).
      $self->flush_section();

      my $section = $1;
      $section =~ s/\s+$//;
      if ($section eq 'Remarks') {
	$section = "Notes";
      } elsif ($section eq 'Inputs') {
	$section = 'Input Arguments';
      } elsif ($section eq 'Outputs') {
	$section = 'Output Arguments';
      }
      $self->section( $section );

    } elsif (defined $self->section() && $self->section eq 'Name'
	     && $line =~ /\s*$r\s+(.*)$/) {
      # first non-blank line is
      #   NAME Purpose
      my $title = $1;
      $title =~ s/^\s+//;
      $title =~ s/\s+$//;

      my ($name, @purpose) = split /\s+/, $title;
      push(@{$self->content}, $name);
      $self->flush_section;
      $self->section( "Purpose" );
      push(@{$self->content}, join(" ", @purpose));
      $self->flush_section;

    } elsif ( $line =~ /^\s*$r(\s+.*)\s*$/ ) {
      # prologue content
      # Remove all leading spaces
      # Does not match a line with just a comment character
      my $content = $1;
      $content =~ s/^\s+//;
      $content =~ s/\s*$//; # trailing spaces

      # need current section name for some decisions
      my $section = $self->section;

      # Look for an Author. Indicated by -1991 date
      if ($line =~ /^\s*$r\s+(.*)\s+(\d{1,2}\-\w\w\w\-\d\d\d\d)/) {

	my $author = $1;
	my $date = $2;
	$author =~ s/\s+$//;

	my $comment;
	if ($line =~ /\-\d\d\d\d\s+(.*)$/) {
	  $comment = $1;
	  $comment =~ s/\s+$//;
	  $comment =~ s/^\s+//;
	}

	# initials
	my @names = split /\s+/,$author;
	my $initials;
	for my $n (@names) {
	  $initials .= substr($n,0,1);
	}

	# Assume it is possible that there can be another line
	# so retrieve earlier information
	$self->flush_section();
	$self->section( "Authors" );
	push(@{$self->content}, $prl->content( "Authors" ), $initials . ": ". $author . " (Starlink)");
	$self->flush_section();

	# history
	my $hist = $date . " ($initials):";
	$self->section("History");
	push(@{$self->content},
	     $prl->content("History"),
	     $hist, "   ". ($comment ? $comment : "Modified") .".");
	$self->flush_section();
	return();
      } elsif (defined $section && ($section =~ /^Input/ || $section =~ /Output/)) {
	my ($var, $type, $desc) = split(/\s+/, $content, 3);
                 my $arr = '';
                 if ($type =~ /^\w\(\)$/) {
                    $arr = '()';
                    $type =~ s/\(\)//;
                 }
	my $io;
	if ($section =~ /Input/) {
	  $io = "Given";
	} elsif ($section =~ /Output/) {
	  $io = "Returned";
	} else {
	  croak "Error parsing Input/Output section: $section\n";
	}
	my %types = (
		     routine => 'SUBROUTINE',
		     c => 'CHAR',
		     'c*(*)' => 'CHAR',
		     'c(*)' => 'CHAR',
		     'c*2' => 'CHAR*2',
		     i => 'INTEGER',
		     r => 'REAL',
		     d => 'DOUBLE',
		     l => 'LOGICAL');
             if (!exists $types{$type}) {
               warn "Possible Bad type in argument list: $content\n";
                # Assume a continuation of a previous line description
                $content =~ s/^\s+//;
                push(@{$self->content()}, "    ". $content);
             } else {
                 push(@{$self->content()}, "$var$arr = $types{$type} ($io)",
                                                        "    ". ucfirst($desc) );
             }
            return ();
      }

      # store the content if we have some left
      push(@{$self->content()}, $content);

    } elsif ($line =~ /^\s*$r\s*$/) {
      # if nothing matches we have a blank line
      # ignore them
    } else {
      croak "Error in GNS parse: $line\n";
    }
    # indicate that we are in a prologue but not complete
    return (undef, undef );

  } else {

    # looking for one to start
    my ($cchar) = $self->prolog_start( $line );
    if (defined $cchar) {

      # a new prologue is starting
      $prl = new Starlink::Prologue();
      $self->prologue( $prl );
      $self->_set_prologue_type();
      $prl->comment_char( $cchar );
      $prl->start_c_comment( 0 );

      # indicate that we are in a Name section (the next line)
      $self->section( "Name" );

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

Remove duplicate content from Authors.

=cut

sub tidy_content {
  my $self = shift;
  my $section = $self->section;
  return unless defined $section;
  if ($section eq 'Authors' ) {
    my @content = $self->content;
    my @out;
    my %uniq;
    for my $e (@content) {
      if (!exists $uniq{$e}) {
	$uniq{$e} = $e;
	push(@out, $e);
      }
    }
    @{$self->content} = @out;
  }
}

=back

=head2 Class Methods

=over 4

=item B<prolog_start>

Returns true if the line supplied could be interpreted as a
start of a prolog. Otherwise returns false.

 $is_start = $p->prolog_start( $line );

In list context returns the comment character (usually a '*').

 ($cchar) = $p->prolog_start( $line );

Note that prolog is also ended by *++.

=cut

sub prolog_start {
  my $self = shift;
  my $line = shift;
  my $cchar;
  my $title;
  my $purpose;
  if ($line =~ /^\s*(\*)\+\+\s*$/) {
     # Normal prologue start :  *++
     $cchar = $1;
  }

  if (defined $cchar ) {
    if (wantarray) {
      return ($cchar);
    } else {
      return 1;
    }
  }
  return ();
}

=back

=head1 NOTES

This class is a subclass of C<Starlink::Prologue::Parser::Base>

Note that this class will not be at all useful as soon as all the GNS
source code is converted to STARLSE style.

=head1 EXAMPLE

An example GNS prologue:

 *++
 *   gns_1HOSTN  returns the name of the system
 *
 *   Description:
 *       Gets the system name; eg the DECnet node name or IP host name
 *
 *   Input arguments:
 *      STATUS  i                 Inherited status
 *
 *   Output arguments:
 *      NAME    c                 The system name
 *      LNAME   i                 The length of the name
 *      STATUS  i                 Status
 *
 *   D L Terrett 06-JUN-1991
 *++
       IMPLICIT NONE


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
