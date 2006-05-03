package Starlink::Prologue;

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

# Standard headers in the standard order
my @STANDARD_HEADERS = (qw/
			   Name
			   Purpose
			   Language
			   /,
			"Type Of Module",
			qw/
			   Invocation
			   Description
			   Arguments
                           /,
                          "Returned Value",
                         qw/
			   Algorithm
			   /,
			"Implementation Deficiencies",
			qw/
			   Notes
			   Copyright
			   Licence
			   Authors
			   History
			   Bugs
			   /);

# Lists any defaults for the standard headers that are used if no content
# is available
my %DEFAULTS = (
		Bugs => '{note_any_bugs_here}',
		History => '{enter_changes_here}',
		Authors => '{original_author_entry}',
		Language => '{routine_language}',
	       );

# Aliases that can be used in place of primary key
my %ALIASES = (
	       Authors => 'Author',
	       Licence => 'License',
	      );

# Lists any terminators of sections (if not-terminated)
my %TERMINATORS = (
		   Bugs => '{note_new_bugs_here}',
		   History => '{enter_further_changes_here}',
		   Authors => '{enter_new_authors_here}',
		  );

# create accessors
for my $h (@STANDARD_HEADERS) {
  my $method = lc($h);
  $method =~ s/\s+/_/g;
  my $code = q{
sub METHOD {
  my $self = shift;
  my $key = "KEY";
  if (@_) {
    $self->content( $key, @_ );
  }
  if (wantarray) {
    return ( $key, [$self->content( $key ) ] );
  } else {
    return [$self->content( $key )];
  }
}
};

  # replace the placeholders
  $code =~ s/METHOD/$method/g;
  $code =~ s/KEY/$h/g;

  # Create the method
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
		   PROLOGUE_TYPE => undef,
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

=item B<prologue_type>

Type of prologue that was parsed to construct this object. Can be undef if the
object was constructed manually.

  $type = $prl->prologue_type;

A standard Starlink prologue would be "STARLSE". Other types are possible,
see C<Starlink::Prologue::Parser>. This method can be used to decide whether
the prologue for a source file should be modernised.

=cut

sub prologue_type {
  my $self = shift;
  if (@_) {
    $self->{PROLOGUE_TYPE} = shift;
  }
  return $self->{PROLOGUE_TYPE};
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

   name
   purpose
   language
   invocation
   description
   arguments
   copyright
   licence
   authors
   history
   notes
   bugs
   implementation_deficiencies
   type_of_module

=over 4

=item B<content>

General purpose accessor for textual content.  Takes the section name
(or key) as argument and a list containing the content (without
comment characters):

  $prl->content( "Description", @lines );

Returns a list containing all the content.

  @lines = $prl->content( "Description" );

For standard sections, the accessors can be used directly.

Aliases are not resolved automatically.

=cut

sub content {
  my $self = shift;
  my $tag = shift;
  Carp::confess("Supplied tag is not defined!") unless defined $tag;

  # copy in content and remove newlines
  my @content = @_;

  # Normalise the tag into standard form
  $tag = $self->_normalise_section_name( $tag );
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

=item B<years>

Parse the history information and extract all the relevant years.

 @years = $prl->years();

=cut

sub years {
  my $self = shift;
  my ($text, $histlines) = $self->history;

  # Extract history information
  my %history;
  for my $line (@$histlines) {
    if ($line =~ /(\d\d\d\d)/) {  # yyyy
      $history{$1}++;
    } elsif ( $line =~ /\d+[\/\.]\d+[\/\.](\d\d)/ ) { # dd.mm.yy dd/mm/yy
      my $yr = $1;
      if ($yr > 50) {
	$yr += 1900;
      } else {
	$yr += 2000;
      }
      $history{$yr}++;
    }
  }

  return (sort keys %history);
}

=item B<sections>

Returns all the section headings present in the prologue in alphabetical order.

 @sections = $prl->sections;

=cut

sub sections {
  my $self = shift;
  return sort keys %{$self->{CONTENT}};
}

=item B<del_section>

Delete the named section (if it exists).

=cut

sub del_section {
  my $self = shift;
  my $tag = shift;
  $tag = $self->_normalise_section_name( $tag );
  delete $self->{CONTENT}->{$tag};
  return;
}

=item B<has_section>

Returns the section name if the section name exists. Can be used to
translate aliases.

  $section = $prl->has_section( "Authors" );

may return "Author" if that section is defined.

=cut

sub has_section {
  my $self = shift;
  my $key = shift;
  my $alias;
  $alias = $ALIASES{$key} if exists $ALIASES{$key};

  my %keys = map { $_, undef } $self->sections;

  my $hasprim = (exists $keys{$key});
  my $hasalias = (defined $alias && exists $keys{$alias});

  if ($hasprim && $hasalias) {
    warn "Both section ($key) and alias ($alias) exist\n";
    return $key;
  } elsif ($hasprim) {
    return $key;
  } elsif ($hasalias) {
    return $alias;
  }
  return ();
}

=item B<misc_sections>

Return all the section headings (in alphabetical order) not present in the
standard list but present in the prologue.

 @misc = $prl->misc_sections;

=cut

sub misc_sections {
  my $self = shift;
  my @present = $self->sections;
  my %standard = map { $_, undef } @STANDARD_HEADERS, values %ALIASES;

  my @misc;
  for my $p (@present) {
    next unless $p; # blank keys are a bug
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

#  use Data::Dumper;
#  print Dumper($self);

  my $code = '';

  # Do we need to open a C-style comment first?
  $code .= "/*\n" if $self->start_c_comment;

  # Open the prologue, using the proper comment character
  $code .= $cchar ."+\n";

  # Go through each of the standard headers in order and then
  # finish with the unknown sections
  for my $h (@STANDARD_HEADERS, $self->misc_sections) {

    my $section = $self->has_section( $h );

    # get the content for this section
    my @content;
    @content = $self->content( $section ) if defined $section;

    # if no content we may have a default
    if (!@content) {
      if (exists $DEFAULTS{$h}) {
	$section = $h;
	@content = ( $DEFAULTS{$h} );
      }
    }

    # process content
    if (@content) {
      $code .= $cchar . "  $section:\n";

      # do we have to terminate content?
      if (exists $TERMINATORS{$h} &&
	  $content[$#content] !~ /^\s*\{[\w_]+\}\s*$/) {
	push(@content, $TERMINATORS{$h} );
      }

      # write out the content
      for my $l (@content) {
	if (defined $l) {
	  if ($l =~ /\w/) {
	    # not just a blank line
	    $code .= $cchar . "     " . $l ."\n";
	  } else {
	    # blank line
	    $code .= $cchar . "\n";
	  }
	}
      }
      # blank line between each section
      $code .= "\n";
    }
  }

  # end the prologue
  $code .= "$cchar" . "-\n";

  # close the comment block if required
  $code .= "*/\n" if $self->end_c_comment;

  return $code;
}

=item B<guess_defaults>

Try to fill in some default values by using hint information.

  $prl->guess_defaults( File => $file );

Recognized hints are:

  File => The filename

=cut

sub guess_defaults {
  my $self = shift;
  my %hints = @_;

  # fix up language if not defined
  if (defined $hints{File} && !@{$self->language}) {
    my $file = $hints{File};
    if ($file =~ /\./) {
      # File has a suffix
      my @parts = split /\./,$file;

      my $lang;
      my $suffix = $parts[-1];
      if ($suffix =~ /^(for|f)$/i) {
	$lang = "Starlink Fortran 77" ;
      } elsif ($suffix =~ /^(pl|pm)$/) {
	$lang = "Perl";
      } elsif ($suffix =~ /^(t)$/) {
	$lang = "Perl Test";
      } elsif ($suffix eq 'c') {
	$lang = "Starlink C";
      } elsif ($suffix eq 'C' || $suffix eq 'cc') {
	$lang = "Starlink C++";
      } elsif ($suffix eq 'sh' ) {
	$lang = "Bourne shell";
      } elsif ($suffix eq 'csh' ) {
	$lang = "C-shell";
      }
      $self->language( $lang ) if defined $lang;

    } else {
      # now things are getting hard so assume Starlink conventions
      if ($file =~ /_(ERR|SYS|CMN|PAR)$/i ) {
	$self->language( "Starlink Fortran 77" );
      }
    }
  }

  # fix up type of module
  my @type_of_module = @{$self->type_of_module};
  if (!@type_of_module && defined $hints{File}) {
    my $file = $hints{File};
    if ($file =~ /_CMN$/i) {
      $self->type_of_module( "COMMON BLOCK" );
    } elsif ($file =~ /_(ERR|PAR|SYS)$/i ) {
      $self->type_of_module( "FORTRAN INCLUDE" );
    }
  }
  return;
}


=back

=begin __INTERNAL__

=head2 Internal

=over 4

=item B<_normalise_section_name>

Internal routine to normalise a section name to a standard form for storing
internally.

  $norm = $prl->_normalise_section_name();

=cut

sub _normalise_section_name {
  my $self = shift;
  my $tag = shift;
  $tag =~ s/^\s+//;
  $tag =~ s/\s+$//;
  my @parts = split(/\s+/, $tag);
  for  (@parts) {
    # leave unchanged if all upper case
    if ( $_ !~ /^[A-Z_]$/) {
      $_ = lc($_);
      $_ = ucfirst($_);
    } 
  }
  $tag = join(" ", @parts );
  return $tag;
}

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
