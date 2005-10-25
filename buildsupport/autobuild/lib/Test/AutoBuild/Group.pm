# -*- perl -*-
#
# Test::AutoBuild::Group by Daniel Berrange <dan@berrange.com>
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

Test::AutoBuild::Group - Simple grouping of modules

=head1 SYNOPSIS

  use Test::AutoBuild::Group

  my $group = Test::AutoBuild::Group->new(name => $name, 
                                          label => $label,
                                          modules => \@modules,
                                          options => \%options);

  my $name = $group->name([$newname]);
  my $label = $group->label([$newlabel]);
  my $admin = $group->admin([$newadmin]);
  my \@modules = $group->modules([\@modules]);
  my $value = $group->option($name[, $newvalue]);

=head1 DESCRIPTION

The Test::AutoBuild::Group module provides for simple 
grouping of modules. Its primary purpose is to allow
the list of modules in HTML status pages to be split
up into groups for easy browsing.

=head1 CONFIGURATION

The valid configuration options for the C<groups> block are

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Group;

use strict;
use Carp qw(confess);


use Digest::MD5;

=pod

=item my $group = Test::AutoBuild::Group->new(name => $name, 
                                              label => $label,
                                              [modules => \@modules,]
                                              [admin => $admin,]
                                              [options => \%options]);

Creates a new group object. C<modules> is an array ref of Test::AutoBUild::Module
objects representing the members of the group. C<name> is a short
alphanumeric token for the name of the group. C<label> is a free
text title for the group. C<admin> is the name/contact details
of the group administrator. C<options> is a hash ref of arbitrary
options for the group.

=cut

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = {};
    my %params = @_;

    $self->{name} = exists $params{name} ? $params{name} : confess "name parameter is required";
    $self->{label} = exists $params{label} ? $params{label} : confess "label parameter is required";
    $self->{admin} = exists $params{admin} ? $params{admin} : undef;
    $self->{modules} = exists $params{modules} ? $params{modules} : [];
    $self->{options} = exists $params{options} ? $params{options} : {};

    bless $self, $class;

    return $self;
}

=pod

=item my $name = $group->name([$newname]);

Gets the name of the group. The name is a short alphanumeric
token. If the C<newname> parameter is supplied then the name
is updated.

=cut

sub name {
    my $self = shift;
    $self->{name} = shift if @_;
    return $self->{name};
}

=pod

=item my $label = $group->label([$newlabel]);

Gets the label of the group. The label is a free text title for
the group. If the C<newlabel> parameter is supplied then the label
is updated.

=cut

sub label {
    my $self = shift;
    $self->{label} = shift if @_;
    return $self->{label};
}

=pod

=item my $admin = $group->admin([$newadmin]);

Gets the admin of the group. The admin property is free text
representing the group admin name and contact details. If the 
C<newadmin> parameter is supplied then the admin property is 
updated.

=cut

sub admin {
    my $self = shift;
    $self->{admin} = shift if @_;
    return $self->{admin};
}

=pod

=item my \@modules = $group->modules([\@newmodules]);

Gets an array ref representing the members of the
group. Each element in the array is an instance of
the Test::AutoBuild::Module class. If the C<newmodules>
array ref is supplied, then the members of the group
are updated.

=cut

sub modules {
    my $self = shift;
    $self->{modules} = shift if @_;
    return $self->{modules};
}

=pod

=item my $value = $group->option($name, [$newvalue]);

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
