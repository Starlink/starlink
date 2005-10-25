# -*- perl -*-
#
# Test::AutoBuild::Publisher by Daniel Berrange <dan@berrange.com>
#
# Copyright (C) 2002-2004 Daniel Berrange <dan@berrange.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# $Id$

=pod

=head1 NAME

Test::AutoBuild::Publisher - Simple publishering of modules

=head1 SYNOPSIS

  use Test::AutoBuild::Publisher

  my $publisher = Test::AutoBuild::Publisher->new(name => $name, 
                                                  label => $label,
                                                  options => \%options);

  my $name = $publisher->name([$newname]);
  my $label = $publisher->label([$newlabel]);
  my $value = $publisher->option($name[, $newvalue]);

=head1 DESCRIPTION

The Test::AutoBuild::Publisher module provides an API for
copying artifacts from the module build root to a destination
directory.

=head1 CONFIGURATION

The valid configuration options for the C<publishers> block are

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Publisher;

use strict;
use Carp qw(confess);

=pod

=item my $publisher = Test::AutoBuild::Publisher->new(name => $name, 
                                                      label => $label,
                                                      [options => \%options]);

Creates a new publisher object. C<modules> is an array ref of Test::AutoBUild::Module
objects representing the members of the publisher. C<name> is a short
alphanumeric token for the name of the publisher. C<label> is a free
text title for the publisher. C<admin> is the name/contact details
of the publisher administrator. C<options> is a hash ref of arbitrary
options for the publisher.

=cut

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = {};
    my %params = @_;

    $self->{name} = exists $params{name} ? $params{name} : confess "name parameter is required";
    $self->{label} = exists $params{label} ? $params{label} : confess "label parameter is required";
    $self->{options} = exists $params{options} ? $params{options} : {};

    bless $self, $class;

    return $self;
}

=pod

=item my $name = $publisher->name([$newname]);

Gets the name of the publisher. The name is a short alphanumeric
token. If the C<newname> parameter is supplied then the name
is updated.

=cut

sub name {
    my $self = shift;
    $self->{name} = shift if @_;
    return $self->{name};
}

=pod

=item my $label = $publisher->label([$newlabel]);

Gets the label of the publisher. The label is a free text title for
the publisher. If the C<newlabel> parameter is supplied then the label
is updated.

=cut

sub label {
    my $self = shift;
    $self->{label} = shift if @_;
    return $self->{label};
}

=pod

=item my $value = $publisher->option($name, [$newvalue]);

Gets the value corresponding to the option C<name>. If the
second C<newvalue> parameter is specified then the value
for the option is updated. 

=cut

sub option {
   my $self = shift;
   my $name = shift;

   $self->{options}->{$name} = shift if @_;

   return $self->{options}->{$name};
}


sub publish {
    my $self = shift;
    my $src = shift;
    my $dst = shift;
    
    confess "module " . ref($self) . " forgot to implement the publish method";
}

1 # So that the require or use succeeds.

__END__

=back 4

=head1 AUTHORS

Daniel Berrange <dan@berrange.com>

=head1 COPYRIGHT

Copyright (C) 2002-2004 Daniel Berrange <dan@berrange.com>

=head1 SEE ALSO

L<perl(1)>

=cut
