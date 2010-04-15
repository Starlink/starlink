package Starlink::Prologue::Parser::SGS;

=head1 NAME

Starlink::Prologue::Parser::SGS - Parse old SGS prologues

=head1 SYNOPSIS

  use Starlink::Prologue::Parser::SGS;

  $p = new Starlink::Prologue::Parser::SGS();

  for my $l (@lines) {
    my $status = $p->push_line( $l );
  }

=head1 DESCRIPTION

This class is responsible for parsing source code and creating
C<Starlink::Prologue> objects.  It only knows how to parse old prologues
favoured by the original SGS source code.

Note that since the SGS prologue starts with the same initial
indicator (*+) as STARLSE prologues, STARLSE recgonition must
be disabled in C<Starlink::Prologue::Parser> before an SGS prologue
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
    # - terminator (*-) if we are too late

    # Totally blank lines are a terminator

    if ( $line =~ /^\s*$/i  # blank line
	 || $line =~ /^\s*$r[\-\+]\s*$/  #   *- or *+
       ) {
      # print "End of prologue detected\n";
      # end of prologue so flush the current section
      $self->flush_section();

      # undefine the internal copy
      $self->prologue( undef );

      # we must return the line unless it was the *-
      my $retval;
      $retval = $line unless $line =~ /^\s*$r[\+\-]\s*$/;

      # For SGS we must combine Given: and Returned: into Arguments:
      my @given = $prl->content( "Given" );
      my @returned = $prl->content( "Returned" );
      $prl->content("Arguments", @given, @returned);
      $prl->del_section( "Given" );
      $prl->del_section( "Returned" );

      # return the newly minted version
      return ($retval, $prl);

    } elsif ( $line =~ /^\s*$r\s+([A-Z][A-Za-z\s_]*)\s*:\s*$/ ) {
      # section start (must start with upper case).
      $self->flush_section();

      my $section = $1;
      $section =~ s/\s+$//;
      if ($section eq 'Remarks') {
	$section = "Notes";
      }
      $self->section( $section );

    } elsif ( $line =~ /^\s*$r(\s+.*)\s*$/ ) {
      # prologue content (or *- but that is dealt with above)
      # Remove all leading spaces
      # Does not match a line with just a comment character
      my $content = $1;
      $content =~ s/^\s+//;
      $content =~ s/\s*$//; # trailing spaces

      # need current section name for some decisions
      my $section = $self->section;

      if ($line =~ /\- \-/) {
	# either about to indicate Name or about to end name
	# whatever happens we have end of previous section
	$self->flush_section();

	if (!defined $section) {
	  # entering Name section
	  $self->section( "Name" );
	} else {
	  # entering Purpose
	  $self->section( "Purpose" );
	}
	return ();
      } elsif ($section eq 'Name' && $content =~ /\w/) {
         #    F U N C (internal routine)
         if ($content =~ /^\s*([\w\s_\d]+)\s\((.*)\)\s*$/) {
           # some comment at the end
           $content = $1;
           my $comment = $2;
           $comment =~ s/^\s+//;
           $comment =~ s/\s+$//;
           $prl->type_of_module( $comment );
         }
         # remove spaces from function name
         $content =~ s/\s+//g;

      } elsif ($line =~ /Wallace|Terrett/) {
	# assume all authors on one line
	$self->flush_section();
	$self->section( "Authors" );

	# this is a bit of a hack!
	my @authors;
	if ($line =~ /Wallace/) {
	  push(@{$self->content()}, "PTW: P. T. Wallace (Starlink)" );
	  push(@authors, "PTW");
	}
	if ($line =~ /Terrett/) {
	  push(@{$self->content()}, "DLT: D. L. Terrett (Starlink)" );
	  push(@authors, "DLT");
	}
	$self->flush_section();

	# history
	my $initials = join("/",@authors);

	$self->section("History");
	my $hist;
	if ($line =~ /(\d+)\s+(\w+)\s+(\d\d\d\d)/) {
	  $hist = sprintf("%02d-%s-%04d ($initials):",
			  $1,substr(uc($2), 0,3),$3)
	} elsif ($line =~ /(\d+\-\w+\-\d\d\d\d)/) {
	  $hist = uc($1) . " ($initials):";
	}
	push(@{$self->content},$hist, "   Modified.");
	$self->flush_section();
	return();
      } elsif (defined $section && ($section eq 'Returned' || $section eq 'Given')) {
	my ($var, $type, $desc) = split(/\s+/, $content, 3);
                 my $arr = '';
                 if ($type =~ /^\w\(\)$/) {
                    $arr = '()';
                    $type =~ s/\(\)//;
                 }

	my %types = (
                               routine => 'SUBROUTINE',
                               c => 'CHAR',
                               'c*(*)' => 'CHAR',
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
                 push(@{$self->content()}, "$var$arr = $types{$type} ($section)",
                                                        "    ". ucfirst($desc) );
             }
            return ();
      }

      # store the content if we have some left
      push(@{$self->content()}, $content);

    } elsif ($line =~ /^\s*$r\s*$/) {
      # if nothing matches we have a blank line
      # we should store it in case it is within a section
      # unless we are in a Purpose and need to switch to Description
      my $s = $self->section;
      if (defined $s && $s eq 'Purpose' &&
	 @{$self->content}) {
	$self->flush_section;
	$self->section("Description");
      } else {
	push(@{$self->content()}, "");
      }
    } else {
      croak "Error in SGS parse: $line\n";
    }
    # indicate that we are in a prologue but not complete
    return (undef, undef );

  } else {

    # looking for one to start
    my ($cchar, $title, $purpose) = $self->prolog_start( $line );
    if (defined $cchar) {

      # a new prologue is starting
      $prl = new Starlink::Prologue();
      $self->prologue( $prl );
      $self->_set_prologue_type();
      $prl->comment_char( $cchar );
      $prl->start_c_comment( 0 );

      # inside a prolog so return nothing
      return ();

    } else {
      # not interested
      return ($line, undef);
    }
  }
  return ();
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

=cut

sub prolog_start {
  my $self = shift;
  my $line = shift;
  my $cchar;
  my $title;
  my $purpose;
  if ($line =~ /^\s*(\*)\+\s*$/) {
     # Normal prologue start :  *+
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

Note that this class will not be at all useful as soon as all the SGS
source code is converted to STARLSE style.

=head1 EXAMPLE

An example SGS prologue:

 *+
 *   - - - - -
 *    N O R M     (Internal routine)
 *   - - - - -
 *
 *   Normalize a pair of numbers which specify the extent of a rectangle.
 *
 *   The routine returns a pair of numbers with the same quotient but
 *   neither of which is greater than unity.
 *
 *   Given:
 *        X,Y       r     number pair to be normalised
 *
 *   Returned:
 *        XN,YN     r     normalised number pair
 *
 *   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
 *-


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
