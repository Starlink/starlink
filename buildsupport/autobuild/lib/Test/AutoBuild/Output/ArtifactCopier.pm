# -*- perl -*-
#
# Test::AutoBuild::Output::ArtifactCopier by Daniel Berrange <dan@berrange.com>
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

Test::AutoBuild::Output::ArtifactCopier - Copies build artifacts to a directory

=head1 SYNOPSIS

  use Test::AutoBuild::Output::ArtifactCopier


=head1 DESCRIPTION

This module copies the artifacts of each module to an HTTP or FTP
site.

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Output::ArtifactCopier;

use strict;
use Carp qw(confess);
use Test::AutoBuild::Output;
use File::Path;
use vars qw(@ISA);

@ISA = qw(Test::AutoBuild::Output);


=pod

=item my $mod = Test::AutoBuild::Output::ArtifactCopier->new(  );

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

    my $directory = $self->option("directory");
    confess "directory parameter is required" unless $directory;

    # By default, remove the old contents of the directory.  This can be overridden by setting
    # the 'clean-directory' parameter to 0
    my $clean = $self->option("clean-directory");
    $clean = 1 unless ( defined ($clean) && $clean == 0 );
    if ( $clean ) {
        if ( $directory =~ m,\%m, ) {
            foreach my $name (keys %{$modules}) {
                $directory = $self->option("directory");
                $directory =~ s,\%m,$name,g;
                rmtree($directory);
            }
        } else {
            rmtree($directory);
        }
    }

    #print "Running in " . getcwd . "\n";

    foreach my $name (keys %{$modules}) {
        my $dst_base = $self->option("directory");
        $dst_base =~ s,\%m,$name,g;
	$dst_base =~ s,/$,,;
	
	my $src_base = $modules->{$name}->dir;
	$src_base =~ s,/$,,;

        mkpath($dst_base);

	foreach my $artifact (@{$modules->{$name}->artifacts}) {
	    my $src = $src_base . "/" . $artifact->{src};
	    my $dst = $dst_base . "/" . $artifact->{dst};
	    
	    my $publisher = $publishers->{$artifact->{publisher}};
	    confess "cannot find publisher $artifact->{publisher}\n"
		unless $publisher;
	    
	    if (-e $src) {
		#print "Publishing $src to $dst\n";
		
		$publisher->publish($src, $dst);
	    } else {
		warn "Skipping $src because it does not exist\n";
	    }
	}
    }
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
