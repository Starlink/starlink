# -*- perl -*-
#
# Test::AutoBuild::Repository::CVS by Daniel Berrange <dan@berrange.com>
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

Test::AutoBuild::Repository::CVS - A repository for CVS

=head1 SYNOPSIS

  use Test::AutoBuild::Repository::CVS


=head1 DESCRIPTION

This module provides access to source within a CVS repository.

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Repository::CVS;

use strict;
use Carp qw(confess);

use Test::AutoBuild::Repository;

use vars qw(@ISA);

@ISA = qw(Test::AutoBuild::Repository);


=pod

=item my $???? = Test::AutoBuild::Repository::CVS->new(  );

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

    # Don't support using multiple paths yet
    my $path = $module->paths->[0];	# Path (from the configuration file).
    
    my $branch = "HEAD";

    if ($path =~ /(.*):((?:\w|-)+)$/) {
	$branch = $2;
	$path = $1;
    }
        
    my $output = $self->_run("cvs checkout -d $name -r $branch -P $path");
    
    # Crude change checking - any line which doesn't
    # look like a directrory traversal message treated
    # as indicating a change
    my $changed = 0;
    foreach (split /\n/, $output) {
	next if /^cvs server:/;
	$changed = 1;
    }

    return $changed;
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
