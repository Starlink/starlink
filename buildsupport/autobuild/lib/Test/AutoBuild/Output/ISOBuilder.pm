# -*- perl -*-
#
# Test::AutoBuild::Output::ISOBuilder by Daniel Berrange <dan@berrange.com>
#
# Copyright (C) 2004 Daniel Berrange <dan@berrange.com>
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

Test::AutoBuild::Output::ISOBuilder - Create CD ISO images

=head1 SYNOPSIS

  use Test::AutoBuild::Output::ISOBuilder


=head1 DESCRIPTION

This module creates CD ISO images containing packages for
a number of modules

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Output::ISOBuilder;

use Carp qw(confess);
use Test::AutoBuild::Output::TemplateGenerator;
use Test::AutoBuild::Lib;
use POSIX qw(strftime);
use Digest::MD5;

use strict;
use vars qw(@ISA);

@ISA = qw(Test::AutoBuild::Output::TemplateGenerator);


=pod

=item my $mod = Test::AutoBuild::Output::ISOBuilder->new(  );

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
    
    my $destdir = $self->option("iso-dest-dir");
    if (! -e $destdir) {
        mkdir $destdir 
            or die "cannot create dir $destdir: $!";
    }
    
    my $scratchdir = $self->option("scratch-dir");
    $scratchdir = "/var/tmp" unless defined $scratchdir;
    
    my @isos;
    
    my %images = %{$self->option("images")};
    foreach my $key (sort keys %images) {
        my $image = $images{$key};
        my $name = $image->{name};
        
        my $vroot = "$scratchdir/$$-$name";
        mkdir $vroot 
            or die "cannot create virtual root directory $vroot: $!";
        
        my $cd_package_types = $image->{'package-types'};
        my $cd_modules = $image->{'modules'};
        
        $cd_package_types = keys %{$package_types}
        unless $cd_package_types;
        $cd_modules = [] unless $cd_modules;
        
        my %types;
        foreach my $type (@{$cd_package_types}) {
            mkdir "$vroot/$type"
                or die "cannot create dir $vroot/$type: $!";
            $types{$type} = 1;
        }
        
        
        foreach my $mod (@{$cd_modules}) {
            my $module = $modules->{$mod};
            warn "Process ISO $mod";
            die "cannot find module $mod" unless defined $module;
            
            my $packages = $module->packages;
            
            foreach my $filename (keys %{$packages}) {
                my $pkg = $packages->{$filename};
                
                if (exists $types{$pkg->type->name}) {
                    my $file = $filename;
                    $file =~ s,^.*/,,;
                    my $dst = $vroot . "/" . $pkg->type->name . "/" . $file;
                    
                    warn "Copy $filename -> $dst";
                    next if $file =~ /.md5$/;
                    
                    $self->_copy_file($filename, $dst);
                    $self->_create_file($pkg->md5sum, $dst . ".md5");
                } else {
                    warn "Skip $filename because " . $pkg->type->name . " is not wanted";
                }
            }
        }
        
        
        my $isofile = $destdir . "/" . $name;
        
        my $label = $image->{"label"} || "Untitled-Auto-Build-CD";
        
        system ("mkisofs -A '$label' -J --hide-joliet '*.md5' -r -o $isofile $vroot") == 0
            or die "cannot create iso $isofile: $?";	
        
        # cleanup
        foreach my $mod (@{$cd_modules}) {
            my $module = $modules->{$mod};
            my $packages = $module->packages;
            foreach my $filename (keys %{$packages}) {
                my $pkg = $packages->{$filename};
                $filename =~ s,^.*/,,;
                
                if (exists $types{$pkg->type->name}) {
                    my $file = $filename;
                    $file =~ s,^.*/,,;
                    my $dst = $vroot . "/" . $pkg->type->name . "/" . $file;
                    
                    next if $file =~ /.md5$/;
                    
                    unlink $dst
                        or die "cannot delete $dst: $!";
                    unlink "$dst.md5"
                        or die "cannot delete $dst.md5: $!";
                }
            }
        }
        foreach my $type (@{$cd_package_types}) {
            rmdir "$vroot/$type"
                or die "cannot delete dir $vroot/$type: $!";
        }
        rmdir $vroot
            or die "cannot delete $vroot: $!";
        
        
        my $md5 = Digest::MD5->new();
        open FILE, $isofile or die "cannot open $isofile: $!";
        $md5->addfile(\*FILE);
        
        my @stat = stat $isofile;
        
        push @isos, {
            label => $label,
            filename => $name,
            md5sum => $md5->hexdigest,
            size => Test::AutoBuild::Lib::pretty_size($stat[7])
            };
    }
    
    my %vars = (
		'isos', \@isos
                );
    $self->_generate_templates($modules, $groups, $repositories, $package_types, \%vars);
}

sub _copy_file {
    my $self = shift;
    my $src = shift;
    my $dst = shift;
    
    open SRC, "<$src"
	or die "cannot read $src: $!";
    open DST, ">$dst"
	or die "cannot create $dst: $!";
    
    # Memory is practically free!
    # ...but we should fix this to be efficient.
    local $/ = undef;
    print DST <SRC>;

    close SRC;
    close DST;
    
}

sub _create_file {
    my $self = shift;
    my $data = shift;
    my $dst = shift;
    
    open DST, ">$dst"
	or die "cannot create $dst: $!";
    
    print DST $data;
    
    close DST;
}

1 # So that the require or use succeeds.

__END__

=back 4

=head1 AUTHORS

Daniel Berrange <dan@berrange.com>

=head1 COPYRIGHT

Copyright (C) 2004 Daniel Berrange <dan@berrange.com>

=head1 SEE ALSO

L<perl(1)>

=cut
