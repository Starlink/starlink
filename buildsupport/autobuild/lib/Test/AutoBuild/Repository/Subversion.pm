# -*- perl -*-
#
# Test::AutoBuild::Repository::Subversion by Daniel Berrange <dan@berrange.com>
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

Test::AutoBuild::Repository::Subversion - A repository for Subversion

=head1 SYNOPSIS

  use Test::AutoBuild::Repository::Subversion


=head1 DESCRIPTION

Description

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Repository::Subversion;

use strict;
use Carp qw(confess);

use base qw(Test::AutoBuild::Repository);


=pod

=item my $???? = Test::AutoBuild::Repository::Subversion->new(  );

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

    my $url = $self->option("url");
    $url =~ s,/$,,;

    my $changed = 0;
    my $default = 0;
    foreach my $path (@{$module->paths}) {
        my ($src, $dst, $rev);
        if ($path =~ /^\s*(.*?)(?::(\d+))?\s*->\s*(.*?)\s*$/) {
            $rev = $2;
            $src = $1;
            $dst = $3;
        } elsif ($path =~ /^\s*(.*?)(?::(\d+))?\s*$/) {
	    if ($default) {
		die "only one default path allowed, use 'src -> dst' syntax instead\n";
	    }
	    $default = 1;
            $rev = $2;
            $src = $1;
            $dst = "";
        } else {
            die "cannot parse subversion path $path\n";
        }
	
	$src =~ s,^/,,;
	$dst =~ s,^/,,;

	my $path = $url . "/" . $src;

        my $output = $rev ? 
	    $self->_run("svn checkout -r $rev $path $name/$dst") :
	    $self->_run("svn checkout $path $name/$dst");

        # Simple change checking - look for any magic
        # file update status codes.
        foreach (split /\n/, $output) {
	    next if /^Checked out revision (\d+).$/;
	    $changed = 1;
        }
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
