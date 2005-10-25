# -*- perl -*-
#
# Test::AutoBuild::Repository::Perforce by Daniel Berrange <dan@berrange.com>
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

Test::AutoBuild::Repository::Perforce - A repository for Perforce

=head1 SYNOPSIS

  use Test::AutoBuild::Repository::Perforce


=head1 DESCRIPTION

This module provides access to source stored in a Perforce 
repository.

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Repository::Perforce;

use strict;
use Carp qw(confess);

use Test::AutoBuild::Repository;

use vars qw(@ISA);

@ISA = qw(Test::AutoBuild::Repository);

=pod

=item my $mod = Test::AutoBuild::Repository::Perforce->new(  );

=cut

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = $class->SUPER::new(@_);

    bless $self, $class;

    return $self;
}


sub init {
    my $self = shift;

    # Get the existing client
    my $client = $self->_run("p4 client -o");
    my $orig_client = "$client";

    # Strip out the View: section
    $client =~ s/\n\s*View:(.*)$//s;

    my $view = "";
    my %views;
    my @views;
    # Compose the new View: section
    foreach my $name ($self->modules) {
        my $module = $self->module($name);

        my $paths = $module->paths();
        foreach my $path (@{$paths}) {
            unless ($path =~ /^\s*(\S+)\s*->\s*(\S+)\s*$/) {
                die "malformed perforce path $path\n";
            }
            my $src = $1;
            my $dst = $2;

            if ( (exists $views{$src}) && ($views{$src} ne $dst) ) {
                die "Trying to set path ($src,$dst) but source is already set to '$dst'";
            }

            $views{$src} = $dst;
            push @views, $src;
        }
    }

    foreach my $src (@views) {
        $view .= "\n\t/$src /$views{$src}";
    }

    $client .= "\n\nView:$view\n\n";

    if ($client ne $orig_client) {
        # The client has changes, so now update it
        {
            local %ENV = %ENV;
            foreach (keys %{$self->{env}}) {
                $ENV{$_} = $self->{env}->{$_};
            }

            my $cmd = "p4 client -i";
            open P4CLIENT, "| $cmd 2>&1" or die "$cmd: $!";
            print P4CLIENT $client;
            close P4CLIENT;
        }
    }
}


sub export {
    my $self = shift;
    my $name = shift;       # Name of the module to export.
    my $module = shift;     # Module object.
    my $groups = shift;

    my $f_opt = uc ($self->_option("force-refresh")) eq "TRUE" ? "-f" : "";
    my $changelist = $self->_option("changelist", $module, $groups);

    if (defined $changelist) {
        $changelist = "\@$changelist";
    } else {
        $changelist = "";
    }

    my $changed = 0;
    my $paths = $module->paths();
    foreach my $path (@{$paths}) {
        unless ($path =~ /^\s*(\S+)\s*->\s*(\S+)\s*$/) {
            die "malformed perforce path $path\n";
        }
        my $src = $1;
        my $dst = $2;

        $_ = $self->_run("p4 sync $f_opt /$dst$changelist");

        if (!/file\(s\) up-to-date/i) {
            $changed = 1;
        }
    }

    return $changed;
}


sub _option () {
    my $self = shift;
    my $option = shift;
    my $module = shift;
    my $groups = shift;

    if (defined $module) {
        if (defined $module->option($option)) {
            return $module->option($option);
        }

        foreach(split ',', $module->group()) {
	    if ( defined &&
		 defined $groups &&
		 exists $groups->{$_} &&
		 defined $groups->{$_}->option($option) ) {
		return $groups->{$_}->option($option);
	    }
	}
    }

    return $self->option($option);
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
