package Starlink::Prolgue;

=head1 NAME

Starlink::Prologue - Object representing a source code prologue

=head1 SYNOPSIS

  use Starlink::Prologue;

  $prl = new Starlink::Prologue();

  $desc = $prl->description;
  $inv  = $prl->invocation;

  $cchar = $prl->comment_char;

  $text = $prl->stringify;

=head1 DESCRIPTION

An object representation of a Starlink source code prologue.
The simplest way to create a C<Starlink::Prologue> object is to
use C<Starlink::Prologue::Parser>.

=cut

use 5.006;
use strict;
use warnings;
use Carp;

my @STANDARD_HEADERS = (qw/
			   Name
			   Purpose
			   Language
			   Invocation
			   Description
			   Arguments
			   Copyright
			   Licence
			   Authors
			   History
			   Notes
			   Bugs
			   /);

# create accessors
for my $h (@STANDARD_HEADERS) {
  my $method = lc($h);
  my $code = q{
sub METHOD {
  my $self = shift;
  my $key = "KEY";
  if (@_) {
    $self->content( $key, @_ );
  }
  if (wantarray) {
    return ( $key, [$self->_content( $key ) ] );
  } else {
    return [$self->_content( $key )];
  }
}
};

  # replace the placeholders
  $code =~ s/METHOD/$method/g;
  $code =~ s/KEY/$h/g;

  # Create the method
  print "Method $method\n";
  eval $code;
  croak "Error creating accessor method: $@\n Code: $code\n" if $@;
}



=head1 METHODS

=head2 Constructors

=over 4

=item B<new>

Create a C<Starlink::Prologue> object.

  $prl = new Starlink::Prologue();

Currently takes no arguments.

=cut

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $prl = bless {
		   CONTENT => {},
		   INITIALIZERS => {},
		   COMMENT_CHAR => '*',
                   START_C_COMMENT => 0,
                   END_C_COMMENT => 0,
		  }, $class;

  return $prl;
}

=back

=head2 Accessors

=over 4

=item B<comment_char>

Returns the comment character used for this prologue.
Usually "*" or "#".

  $cchar = $prl->comment_char;
  $prl->comment_char( $cchar );

=cut

sub comment_char {
  my $self = shift;
  if (@_) {
    $self->{COMMENT_CHAR} = shift;
  }
  return $self->{COMMENT_CHAR};
}

=item B<start_c_comment>

True if the prologue started at the same time as the C comment was begun.
False if the prologue started after the C comment was begun.

=cut

sub start_c_comment {
  my $self = shift;
  if (@_) {
    $self->{START_C_COMMENT} = shift;
  }
  return $self->{START_C_COMMENT};
}

=item B<end_c_comment>

True if the prologue ended at the same time as the C comment was closed.
False if the prologue ended before the C comment was closed.

=cut

sub end_c_comment {
  my $self = shift;
  if (@_) {
    $self->{END_C_COMMENT} = shift;
  }
  return $self->{END_C_COMMENT};
}

=back

=head2 Section Accessors

Each of the standard prologue section accessors behaves in the same manner:

  $prl->SECTION( @lines );

to store a section (no comment characters)

  $lines = $prl->SECTION();

Retrieve reference to array of content.

  ($title, $lines) = $prl->SECTION();

Return section title and reference to array of content.

The following methods are available by default:

   Name
   Purpose
   Language
   Invocation
   Description
   Arguments
   Copyright
   Licence
   Authors
   History
   Notes
   Bugs

=over 4

=item B<content>

General purpose accessor for textual content.  Takes the section name
(or key) as argument and a list containing the content (without
comment characters):

  $prl->content( "Description", @lines );

Returns a list containing all the content.

  @lines = $prl->content( "Description" );

For standard sections, use the accessors directly.

=cut

sub content {
  my $self = shift;
  my $tag = shift;
  my @content = @_;

  # Normalise the tag into standard form
  $tag =~ s/^\s+//;
  $tag =~ s/\s+$//;
  my @parts = split(/\s+/, $tag);
  for  (@parts) {
    $_ = lc($_);
    $_ = ucfirst($_); 
  }
  $tag = join(" ", @parts );

  croak "Must supply an argument to content()" unless defined $tag;

  if (@content) {
    $self->{CONTENT}->{$tag} = \@content;
  } else {
    if (exists $self->{CONTENT}->{$tag}) {
      return @{$self->{CONTENT}->{$tag}};
    } else {
      return ();
    }
  }
}

=item B<sections>

Returns all the section headings present in the prologue in alphabetical order.

 @sections = $prl->sections;

=cut

sub sections {
  my $self = shift;
  return sort keys %{$self->{CONTENT}};
}

=item B<misc_sections>

Return all the section headings (in alphabetical order) not present in the
standard list but present in the prologue.

 @misc = $prl->misc_sections;

=cut

sub misc_sections {
  my $self = shift;
  my @present = $self->sections;
  my %standard = map { $_, undef } @STANDARD_HEADERS;

  my @misc;
  for my $p (@present) {
    push(@misc, $p) unless exists $standard{$p};
  }
  return @misc;
}

=back

=head2 General

=over 4

=item B<stringify>

Convert the prologue into a valid source prologue.

  $text = $prl->stringify;

=cut

sub stringify {
  my $self = shift;
  my $cchar = $self->comment_char;

  my $code = '';

  # Do we need to open a C-style comment first?
  $code .= "/*\n" if $self->start_c_comment;

  # Open the prologue, using the proper comment character
  $code .= $cchar ."+\n";

  for my $h (@STANDARD_HEADERS, $self->misc_sections) {
    my $meth = lc($h);
    my ($title, $content) = $self->$meth();

    $code .= $cchar . "  $title:\n";
    for my $l (@$content) {
      $code .= $cchar . "     " . $l ."\n";
    }
  }

  # end the prologue
  $code .= "\n$cchar" . "-\n";

  # close the comment block if required
  $code .= "*/\n" if $self->end_c_comment;

  return $code;
}

=back

=begin __INTERNAL__

=head2 Internal

=over 4


=back

=end __INTERNAL__

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
