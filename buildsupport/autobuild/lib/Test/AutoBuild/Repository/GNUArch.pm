# -*- perl -*-
#
# Test::AutoBuild::Repository::GNUArch by Daniel Berrange
#
# Copyright (C) 2004 Daniel Berrange
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

Test::AutoBuild::Repository::GNUArch - A repository for GNU Arch

=head1 SYNOPSIS

  use Test::AutoBuild::Repository::GNUArch


=head1 DESCRIPTION

This module provides a repository implementation for the
GNU Arch revision control system. It requires that the
'tla' command version 1.1 or higher be installed. It has
full support for detecting updates to an existing checkout.

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Repository::GNUArch;

use strict;
use Carp qw(confess);

use Test::AutoBuild::Repository;

use vars qw(@ISA);

@ISA = qw(Test::AutoBuild::Repository);


=pod

=item my $repository = Test::AutoBuild::Repository::GNUArch->new(  );

=cut
    
sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = $class->SUPER::new(@_);
    
    bless $self, $class;
    
    return $self;
}


sub export {
    my $self = shift;
    my $name = shift;		# Name of the module to export.
    my $module = shift;		# Module object.
    
    # Make sure all archives are up2date
    $self->_register_archives;

    # Don't support using multiple paths yet
    my $path = $module->paths->[0]; #Path (from the configuration file).
    
    if (-d $name) {
	my $missing = $self->_run("tla missing -d $name --summary");
	if ($missing =~ /^\s*$/) {
	    return 0;
	}
	$self->_run("tla update -d $name");
	return 1;
    } else {
	$self->_run("tla get $path $name");
	return 1;
    }
}


sub _register_archives {
    my $self = shift;
    
    my $arch_name = $self->option("archive-name");
    my $arch_uri = $self->option("archive-uri");
    
    my $existing = $self->_run("tla archives -n -R");
    my %existing;
    map { $existing{$_} = 1 } split /\n/, $existing;

    if (! exists $existing{$arch_name}) {
      $self->_run("tla register-archive $arch_name $arch_uri");
    }
}


1 # So that the require or use succeeds.

__END__

=back 4

=head1 AUTHORS

Daniel Berrange

=head1 COPYRIGHT

Copyright (C) 2004 Daniel Berrange

=head1 SEE ALSO

L<perl(1)>

=cut
