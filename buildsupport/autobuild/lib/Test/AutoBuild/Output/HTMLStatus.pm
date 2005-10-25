# -*- perl -*-
#
# Test::AutoBuild::Output::HTMLStatus by Daniel Berrange <dan@berrange.com>
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

Test::AutoBuild::Output::HTMLStatus - Generates HTML status pages

=head1 SYNOPSIS

  use Test::AutoBuild::Output::HTMLStatus


=head1 DESCRIPTION

This module generates the primary HTML status pages

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Output::HTMLStatus;

use Carp qw(confess);
use Test::AutoBuild::Output::TemplateGenerator;
use Test::AutoBuild::Lib;
use strict;
use vars qw(@ISA);

@ISA = qw(Test::AutoBuild::Output::TemplateGenerator);


=pod

=item my $mod = Test::AutoBuild::Output::HTMLStatus->new(  );

=cut

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = $class->SUPER::new(@_);

    bless $self, $class;

    return $self;
}


sub process {
    my $self = shift;
    my $modules = shift;
    my $groups = shift;
    my $repositories = shift;
    my $package_types = shift;
    my $publishers = shift;

    my @modules;

    foreach my $name (sort { $modules->{$a}->label cmp $modules->{$b}->label } keys %{$modules}) {

        my @packs = ();
        my $packages = $modules->{$name}->packages();

        foreach my $filename (keys %{$packages}) {
            my $fn = $packages->{$filename}->name;
            $fn =~ s,.*/,,;

            my $size = $packages->{$filename}->size();

            my $p = {
                'filename' => $fn,
                'size' => $size,
                'prettysize' => Test::AutoBuild::Lib::pretty_size($size),
                'md5sum' => $packages->{$filename}->md5sum,
                'type' => $packages->{$filename}->type,
            };
            push @packs, $p;
        }
        @packs = sort { $a->{type}->name() cmp $b->{type}->name() or $a->{filename} cmp $b->{filename} } @packs;

        my $logfile = $modules->{$name}->build_log_filename;

	my $links = $modules->{$name}->links();
	my $artifacts = $modules->{$name}->artifacts();
	
	my @artifacts;
	foreach my $artifact (@{$artifacts}) {
	    next unless $artifact->{label};

	    push @artifacts, {
		label => $artifact->{label},
		path => $artifact->{dst},
	    };
	}

        my $mod = {
            'name' => $name,
            'label' => $modules->{$name}->label,
            'status' => $modules->{$name}->build_status,
            'group' => $modules->{$name}->group,
            'repository' => $modules->{$name}->repository,
            'buildTime' => Test::AutoBuild::Lib::pretty_time($modules->{$name}->build_time),
            'buildDate' => Test::AutoBuild::Lib::pretty_date($modules->{$name}->build_date),
            'logFilename' => $logfile,
            'packages' => \@packs,
	    'links' => $links,
	    'artifacts' => \@artifacts
        };

        push @modules, $mod;
    }

    my @groups;
    foreach my $name (sort keys %{$groups}) {
        my $group = $groups->{$name};
        
        my @groupmods = grep { grep { $_ eq $name } split (',', $_->{group}) } @modules;

        my $entry = {
            name => $name,
            label => $group->label,
            modules => \@groupmods,
        };
        
        push @groups, $entry;
    }

    my @repositories;
    foreach my $name (sort keys %{$repositories}) {
        my $repository = $repositories->{$name};
        
        my @repositorymods = grep { $_->{repository} eq $name } @modules;

        my $entry = {
            name => $name,
            label => $repository->label,
            modules => \@repositorymods,
        };
        
        push @repositories, $entry;
    }
    
    my @package_types;
    foreach my $name (sort keys %{$package_types}) {
        my $package_type = $package_types->{$name};
        
        my $entry = {
            name => $name,
            label => $package_type->label,
        };
        
        push @package_types, $entry;
    }
    
    my %vars = (
                'modules' => \@modules,
                'groups' => \@groups,
                'repositories' => \@repositories,
                'package_types' => \@package_types,
                );
    $self->_generate_templates($modules, $groups, $repositories, $package_types, \%vars);
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
