# -*- perl -*-
#
# Test::AutoBuild::PackageType by Daniel Berrange <dan@berrange.com>
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

Test::AutoBuild::PackageType - Package type handling

=head1 SYNOPSIS

  use Test::AutoBuild::PackageType


=head1 DESCRIPTION

This module provides handling for package types (ie RPMs,
Debs, Tar.gz, etc).

=head1 CONFIGURATION

The valid configuration options for the C<package-types> block are

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::PackageType;

use strict;
use Carp qw(confess);
use Test::AutoBuild::Package;
use File::Find;
use File::Path;

=pod

=item my $mod = Test::AutoBuild::PackageType->new(  );

=cut

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = {};
    my %params = @_;

    bless $self, $class;

    $self->{name} = exists $params{name} ? $params{name} : confess "name parameter is required";
    $self->{label} = exists $params{label} ? $params{label} : confess "label parameter is required";
    $self->{extension} = exists $params{extension} ? $params{extension} : confess "extension parameter is required";
    $self->{arch} = exists $params{arch} ? $params{arch} : undef;
    $self->{filetype} = exists $params{filetype} ? $params{filetype} : "file";

    if (exists $params{spool}) {
        $self->spool ($params{spool});
    } else {
        $self->{spool} = [];
    }

    if (exists $params{clean}) {
        $self->clean ($params{clean});
    } else {
        $self->{clean} = undef;
    }

    return $self;
}

sub name {
    my $self = shift;
    $self->{name} = shift if @_;
    return $self->{name};
}

sub label {
    my $self = shift;
    $self->{label} = shift if @_;
    return $self->{label};
}


sub spool {
    my $self = shift;
    if (@_) {
        local $_ = shift;
        if (ref ($_) eq "ARRAY") {
            $self->{spool} = $_;
        } else {
            $self->{spool} = [ $_ ];
        }
    }
    return $self->{spool};
}


sub extension {
    my $self = shift;
    $self->{extension} = shift if @_;
    return $self->{extension};
}

sub arch {
    my $self = shift;
    $self->{arch} = shift if @_;
    return $self->{arch};
}

sub filetype {
    my $self = shift;
    $self->{filetype} = shift if @_;
    return $self->{filetype};
}


sub clean {
    my $self = shift;
    if (@_) {
        # clean option has the format like "7d" for 7 days, "4h" for 4 hours,
        # etc. Convert this into minutes for the -mmin option of find.
        my $clean = shift;
        my $mins;

        if ($clean =~ /^(\d+)d$/) {
            $mins = $1 * 24 * 60;
        } elsif ($clean =~ /^(\d+)h$/) {
            $mins = $1 * 60;
        } elsif ($clean =~ /^(\d+)m$/) {
            $mins = $1;
        } else {
            confess "clean option, if it exists, must have form NNd (days), NNh (hours) or NNm (mins)";
        }

        $self->{clean} = $mins;
    }
    return $self->{clean};
}

# If $self->{clean} is defined, remove packages in the spool which
# are older than $self->{clean} minutes. The main code calls this
# just before the build (to ensure that it is called periodically).
sub do_clean {
    my $self = shift;

    return unless $self->{clean};

    my @spooldirs = grep { -d $_ } @{$self->{spool}};
    my $ext = $self->{extension};
    my $mins = $self->{clean};

    if (@spooldirs == 0) {
        warn "warning: no spool directories for $self->{name}\n";
    } else {
        if ($self->{filetype} eq "directory") {
            foreach (@spooldirs) {
                opendir(DIR, $_) or die("can't opendir $_: $!");
                foreach my $dir (grep { m/$ext$/ && -d && ((-M $_) * 60 * 24) > $mins } readdir(DIR)) {
                    rmtree($dir);
                }
                closedir DIR;
            }
        } else {
            find ( { wanted => sub {
                if ( m/$ext$/ && -f && ((-M $_) * 60 * 24 > $mins)) {
                    unlink
                    }
            }, no_chdir => 1 }, @spooldirs );
        }
    }
}

sub snapshot {
    my $self = shift;

    my @spooldirs = grep { -d $_ } @{$self->{spool}};
    my $ext = $self->{extension};
    my $ext_re = "$ext";
    $ext_re =~ s/\./\\./g;
    my $cmd = "";

    my $packages = {};

    if (@spooldirs) {
        if ($self->{filetype} eq "directory") {
            foreach my $dir (@spooldirs) {
                opendir(DIR, $dir) or next;
                foreach my $match (map { File::Spec->catdir($dir,$_) }
                                   grep { -d File::Spec->catdir($dir,$_) && !m/^\.$/ && !m/^\.\.$/ && m/.*$ext_re/ } readdir(DIR)) {
                    $packages->{$match} =
                        new Test::AutoBuild::Package (name => $match, type => $self);
                }
                closedir DIR;
            }
        } else {
            find ( { wanted => sub {
                if ( ( -f || -l ) && m/.*$ext_re/ ) {
                    $packages->{$File::Find::name} =
                        new Test::AutoBuild::Package (name => $File::Find::name,
                                                          type => $self);
                }
            }, no_chdir => 1 }, @spooldirs);
        }
    }

    return $packages;
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
