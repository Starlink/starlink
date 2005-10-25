# -*- perl -*-
#
# Test::AutoBuild::Output by Daniel Berrange <dan@berrange.com>
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

Test::AutoBuild::Output - Generates post build output

=head1 SYNOPSIS

  use Test::AutoBuild::Output

  my $output = Test::AutoBuild::Output->new(name => $name,
                                            label => $label,
                                            [options => \%options,]
                                            [start_time => $time]);

  my $name = $output->name([$newname]);
  my $label = $output->label([$newlabel]);
  my $time = $output->startTime([$newtime]);
  my $value = $output->option([$newvalue]);

  $output->process();

=head1 DESCRIPTION

This module is used to generate post-build output such
as HTML pages, email alerts. Subclasses must implement
the C<process> method to perform whatever processing
they require.

=head1 CONFIGURATION

The valid configuration options for the C<output> block are

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Output;

use strict;
use Carp qw(confess);


=pod

=item my $output = Test::AutoBuild::Output->new(name => $name,
                                                label => $label,
                                                [options => \%options,]
                                                [start_time => $time]);

Creates a new output module. This constructor is only really useful
to subclasses, since the C<process> method on this base class is
virtual. C<name> is the alphanumeric token for the module name. C<label>
is the free text human friendly title of the module. C<options> is a
hash ref containing module specific configuration options. C<start_time>
is the time at which the build cycle began.

=cut

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = {};
    my %params = @_;

    $self->{name} = exists $params{name} ? $params{name} : confess "name parameter is required";
    $self->{label} = exists $params{label} ? $params{label} : confess "label parameter is required";
    $self->{options} = exists $params{options} ? $params{options} : {};
    $self->{start_time} = exists $params{start_time} ? $params{start_time} : time;

    bless $self, $class;

    return $self;
}

=pod

=item my $name = $output->new([$newname]);

Retrieves the name of the output module, which is an
alphanumeric token. If the C<newname> parameter is
supplied the name is updated.

=cut

sub name {
    my $self = shift;
    $self->{name} = shift if @_;
    return $self->{name};
}

=pod

=item my $value = $output->option($name[, $newvalue]);

Retrieves the option value corresponding to the C<name>
parameter. If the C<newvalue> parameter is supplied, then
the option value is updated.

=cut

sub option {
   my $self = shift;
   my $name = shift;

   $self->{options}->{$name} = shift if @_;

   return $self->{options}->{$name};
}

=pod

=item my $label = $output->label([$newlabel]);

Retrieves the label for this output module, a free
text, human friendly title for the module. If the
C<newlabel> parameter is supplied the label is updated.

=cut

sub label {
    my $self = shift;
    $self->{label} = shift if @_;
    return $self->{label};
}

=pod

=item my $time = $output->start_time([$newtime]);

Retrieves the time at which the build cycle was 
started. If the C<newtime> parameter is supplied
the start time is updated.

=cut

sub start_time {
    my $self = shift;
    $self->{start_time} = shift if @_;
    return $self->{start_time};
}

=pod

=item $output->process(\%modules, \%groups, \%repositories, \%packageTypes);

Runs the output module. C<modules> is a hash ref of all code modules,
whose keys are module names & values are instances of Test::AutoBuild::Module.
C<groups> is a hash ref of all groups whose keys are group names & values are
instances of Test::AutoBuild::Group. C<repositories> is a hash ref of
code repositories whose keys are repository names and values are instances
of Test::AutoBuild::Repository. C<pacakgeTypes> is a hash ref of package
types whose keys are names and values are instances of Test::AutoBuild::PackageType

=cut

sub process {
    my $self = shift;
    confess "class " . ref($self) . " forgot to implement the process method";
}

sub _expand_macro {
    my $self = shift;
    my $in = shift;
    my $macro = shift;
    my $name = shift;
    my @values = @_;

    my @out;
    foreach my $entry (@{$in}) {
        my $src = $entry->[0];
        my $dst = $entry->[1];        

        if ($dst =~ /$macro/) {
            foreach my $value (@values) {
                my $file = $dst;
                $file =~ s/$macro/$value/;

                my $vars = {};
                map { $vars->{$_} = $entry->[2]->{$_} } keys %{$entry->[2]};
                $vars->{$name} = $value;
                
                push @out, [$src, $file, $vars];
            }
        } else {
            push @out, $entry;
        }
    }

    return \@out;
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
