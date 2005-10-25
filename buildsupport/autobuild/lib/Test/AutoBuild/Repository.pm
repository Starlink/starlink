# -*- perl -*-
#
# Test::AutoBuild::Repository by Daniel Berrange <dan@berrange.com>
#
# Copyright (C) 2002 Daniel Berrange <dan@berrange.com>
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

Test::AutoBuild::Repository - Source control repository access

=head1 SYNOPSIS

  use Test::AutoBuild::Repository

  my $rep = Test::AutoBuild::Repository->new(
               name => $name,
               options => \%options,
               env => \%env,
               label => $label);

  # Add a module to the repository
  $rep->module($module);

  # Initialize the repository
  $rep->init();
 
  # Checkout / update the module
  my $changed = $rep->export($name, $module);

=head1 DESCRIPTION

This module provides the API for interacting with the source
control repositories. A repository implementation has to be
able to do two main things

 * Get a checkout of a new module
 * Update an existing checkout, determining if any
   changes where made

=head1 CONFIGURATION

The valid configuration options for the C<repositories> block are

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Repository;

use strict;
use Test::AutoBuild::Lib;
use Carp qw(confess);


=pod

=item my $rep = Test::AutoBuild::Repository->new(name => $name,
           label => $label,
           options => \%options,
           env => \%env);

This method creates a new repository. The C<name> argument is an
alphanumeric token representing the name of the repository. The
C<label> argument is a human friendly name of the repository. The
optional C<options> argument is a hashref of implementation
specific options. The optional C<env> argument is a hashref of
environment variables to set when running the commands to access
the repository.

=cut

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = {};
    my %params = @_;

    $self->{name} = exists $params{name} ? $params{name} : confess "name parameter is required";
    $self->{options} = exists $params{options} ? $params{options} : {};
    $self->{env} = exists $params{env} ? $params{env} : {};
    $self->{label} = exists $params{label} ? $params{label} : confess "label parameter is required";
    $self->{modules} = {};

    bless $self, $class;

    return $self;
}

=pod

=item my $name = $rep->name([$name]);

When run without any arguments, returns the alphanumeric token representing
the name of the repository. If a single argument is supplied, this is use to
update the name.

=cut

sub name {
    my $self = shift;
    $self->{name} = shift if @_;
    return $self->{name};
}

=pod

=item my $value = $rep->option($name[, $value]);

When run with a single argument, retuns the option value corresponding to
the name specified in the first argument. If a second argument is supplied,
then the option value is updated.

=cut

sub option {
   my $self = shift;
   my $name = shift;

   $self->{options}->{$name} = shift if @_;

   return $self->{options}->{$name};
}

=pod

=item my $value = $rep->env($name[, $value]);

When run with a single argument, retuns the environment variable corresponding 
to the name specified in the first argument. If a second argument is supplied,
then the environment variable is updated.

=cut

sub env {
   my $self = shift;
   my $name = shift;

   $self->{env}->{$name} = shift if @_;
   return $self->{env}->{$name};
}

=pod

=item my $label = $rep->label([$label]);

When run without any arguments, returns the human friendly string representing
the label of the repository. If a single argument is supplied, this is use to
update the label.

=cut

sub label {
    my $self = shift;
    $self->{label} = shift if @_;
    return $self->{label};
}

=item my $module = $rep->module($name[, $module]);

When run with a single argument, returns the module object corresponding
to the name specified as the first argument. If the second argument is
supplied, then the module object is updated.

=cut

sub module {
    my $self = shift;
    my $name = shift;
    $self->{modules}->{$name} = shift if @_;
    return $self->{modules}->{$name};
}

=item my @modules = $rep->modules();

Returns the list of modules in this repository

=cut

sub modules {
    my $self = shift;
    return keys %{$self->{modules}};
}


=item $rep->init();

Performs any one time initialization required prior to
exporting modules from the repoitory.

=cut

sub init {
    my $self = shift;
}

=item my $changed = $rep->export($name, $module);

Exports the module specified in the second argument to a directory
specified by the first argument. Returns zero if there were no changes
to export; non-zero if the module was new or changed. This is a virtual
method which must be implemented by all subclasses.

=cut

sub export {
    my $self = shift;
    my $module = shift;
    confess "class " . ref($self) . " forgot to implement the export method";
}

=item my $output = $rep->_run($cmd);

Runs the command specified in the first argument, having first
setup the environment variables specified when the repository
was created. It returns any text written to standard out by the
command

=cut

sub _run {
    my $self = shift;
    my $cmd = shift;

    return Test::AutoBuild::Lib::run($cmd, $self->{env});
}

1 # So that the require or use succeeds.

__END__

=back 4

=head1 AUTHORS

Daniel Berrange <dan@berrange.com>

=head1 COPYRIGHT

Copyright (C) 2002 Daniel Berrange <dan@berrange.com>

=head1 SEE ALSO

L<perl(1)>

=cut
