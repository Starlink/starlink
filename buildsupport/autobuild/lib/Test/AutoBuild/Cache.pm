# -*- perl -*-
#
# Test::AutoBuild::Cache by Dennis Gregorovic <dgregorovic@alum.mit.edu>
#
# Copyright (C) 2003 Dennis Gregorovic <dgregorovic@alum.mit.edu>
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

Test::AutoBuild::Cache - Caching of build results

=head1 SYNOPSIS

  use Test::AutoBuild::Cache;

  my $cache = Test::AutoBuild::Cache->new(cache_root => $directory,
                                          timestamp => $boolean);

  my $up2date = $cache->test_cache($module);

=head1 DESCRIPTION

This module provides caching for module builds. It caches
installed files in the virtual root, generated packages,
and status metadata.

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Cache;

use strict;
use Carp qw(confess);
use File::Copy;
use File::Find;
use File::Path;
use File::Spec;
use Test::AutoBuild::Lib;
use Test::AutoBuild::PackageType;

=pod

=item my $cache = Test::AutoBuild::Cache->new(cache_root=> $directory,
                                              timestamp => $boolean);

Creates a new module cache in the directory specified by C<cache_root>
parameter. If the C<timestamp> parameter evaluates to true, then when
checking if a module cache is uptodate, it will compare the timestamps
on every single file. This is overkill for most situations, since the
respository module informs the build whether there were any changes in
source control since previous build. In particular for CVS, this causes
caching to fail altogether since 'cvs update' always touchs files in
the 'CVS' directory.

=cut

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = {};
    my %params = @_;

    $self->{cache_root} = exists $params{cache_root} ? $params{cache_root} : confess "cache_root parameter is required";
    $self->{timestamp} = exists $params{timestamp} ? $params{timestamp} : 0;
    $self->{field_delimiter} = "!!";
    $self->{item_delimiter} = ",,";
    bless $self, $class;

    return $self;
}

sub test_cache {
    my $self = shift;
    my $module = shift;
    
    if (! -f File::Spec->catfile($self->{cache_root}, $module->name(), "timestamp")) {
	return 0;
    }
    
    if (! $self->{timestamp}) {
	print "Not checking timestamp\n";
	return 1;
    }
    
    print "Cache timestamp is " . Test::AutoBuild::Lib::pretty_date($self->read_timestamp($module->name()));

    if ( newer_than_timestamp(File::Spec->catfile($self->{cache_root},$module->name(),"timestamp"), $module->dir())) {
        print " code is newer\n";
        return 0;
    }
    print " timestamp is newer\n";
    return 1;
}

sub create_package_list {
    my $self = shift;
    my $packages = shift;

    my %package_types;
    my @items;

    foreach my $key (keys %{$packages}) {
        $package_types{$packages->{$key}->type()->name()} = $packages->{$key}->type();
        push (@items, join ($self->{field_delimiter}, "package", $key, $packages->{$key}->type()->name()));
    }

    foreach my $key (keys %package_types) {
        push (@items, join ($self->{field_delimiter}, "package_type", $key, $package_types{$key}->filetype()));
    }

    my $package_list = join ($self->{item_delimiter}, @items);

    return $package_list;
}

sub save_packages {
    my $self = shift;
    my $module = shift;
    my $packages = shift;

    $self->clean_package_dir($module);

    my $package_dir = $self->get_package_dir($module);
    my $new_packages = ();

    foreach my $fileordir (keys %{$packages}) {
        print "Caching package $fileordir\n";

        my $src_dir = (File::Spec->splitpath($fileordir))[1];
        my $newfileordir = cp_minus_root ($src_dir, $fileordir, $package_dir);

        $packages->{$fileordir}->name($newfileordir);
        $new_packages->{$newfileordir} = $packages->{$fileordir};
    }

    my $package_list = $self->create_package_list ($new_packages);
    $self->write_file($module, "package.list", $package_list);

    return $new_packages;
}

sub get_packages {
    my $self = shift;
    my $module = shift;
    my $package_types = shift;

    my $packages;
    my $package_list = $self->read_file($module, "package.list");

    if (! defined ($package_list)) {
        # This happens if the build failed
        return {};
    }

    my @items = split ($self->{item_delimiter}, $package_list);
    foreach (@items) {
        my @item_fields = split ($self->{field_delimiter});
        if ($item_fields[0] eq "package_type") {
            my $name = $item_fields[1];
            my $filetype = $item_fields[2];
            unless (exists $package_types->{$name}) {
                $package_types->{$name} = new Test::AutoBuild::PackageType (name => $name,
                                                                                label => "",
                                                                                extension => "",
                                                                                filetype => $filetype);
            }
        }
    }
    foreach (@items) {
        my @item_fields = split ($self->{field_delimiter});
        if ($item_fields[0] eq "package") {
            my $name = $item_fields[1];
            my $packagetype = $item_fields[2];
            $packages->{$name} = new Test::AutoBuild::Package (name => $name,
                                                                   type => $package_types->{$packagetype});
        }
    }

    return $packages;
}

sub packages {
    my $self = shift;
    my $module = shift;
    my $pkgs = shift;
    my $package_types = shift;

    if (defined $pkgs) {
        return $self->save_packages($module, $pkgs, $package_types);
    } else {
        return $self->get_packages($module, $package_types);
    }
}

sub build_log {
    my $self = shift;
    my $module = shift;
    if (@_) {
        $self->write_file($module, "build.log", shift);
    }
    return $self->read_file($module, "build.log");
}

sub save_install {
    my $self = shift;
    my $module = shift;
    my $src_dir = shift;
    my $new_files = shift;

    mkpath(File::Spec->catdir($self->{cache_root},$module,"inst"));

    foreach my $file ( keys %{$new_files} ) {
        cp_minus_root ($src_dir, $file, File::Spec->catdir($self->{cache_root},$module,"inst"));
    }

    $self->write_timestamp($module, time());
}

sub load_install {
    my $self = shift;
    my $module = shift;
    my $target_dir = shift;

    mkdir "$target_dir";

    my $install_dir = File::Spec->catdir($self->{cache_root},$module,"inst");
    if (-d $install_dir) {
	    opendir(DIR, $install_dir) or die("can't opendir $install_dir: $!");
	    foreach my $file_or_dir (grep { !m/^\.$/ && !m/^\.\.$/ } readdir(DIR)) {
            Test::AutoBuild::Lib::_copy(File::Spec->catfile($install_dir, $file_or_dir), $target_dir);
	    }
	    closedir DIR;
    }
}

sub clear {
    my $self = shift;
    my $module = shift;

    print "Clearing cache...";
    rmtree(File::Spec->catdir($self->{cache_root},$module));
    print "done\n";
}

sub get_build_date {
    my $self = shift;
    my $module = shift;

    my $timestamp = File::Spec->catfile($self->{cache_root},$module,"timestamp");
    if (-f $timestamp) {
        return (stat($timestamp))[9];
    } else {
        return undef;
    }
}

sub write_timestamp {
    my $self = shift;
    my $module = shift;
    my $timestamp = shift;

    $self->write_file($module, "timestamp", $timestamp);
}

sub read_timestamp {
    my $self = shift;
    my $module = shift;

    my $timestamp = $self->read_file($module, "timestamp");
    $timestamp = 0 unless ( defined $timestamp );

    return (int $timestamp);
}

sub write_file {
    my $self = shift;
    my $module = shift;
    my $filename = shift;
    my $value = shift;

    my $dir = File::Spec->catdir($self->{cache_root},$module);
    my $file = File::Spec->catdir($dir,$filename);
    mkpath($dir);

    open (OUT, "> $file" ) || die "could not open $file: $!";
    print OUT $value;
    close OUT;
}

sub read_file {
    my $self = shift;
    my $module = shift;
    my $filename = shift;

    my $file = File::Spec->catfile($self->{cache_root},$module,$filename);
    open (IN, "< $file") || return undef;

    local $/ = undef;
    my $contents = <IN>;
    close IN;

    return $contents;
}

sub newer_than_timestamp {
    my $file = shift;
    my $dir = shift;

    if (! -f $file) {
        return 1;
    }

    my $newer = 0;
    find ( { wanted => sub {
        if ( $newer == 0 ) {
            if (-f && (-M $_ < -M $file)) {
                $newer = 1;
            }
        }
    }, no_chdir => 1 }, $dir );

    return $newer;
}

sub get_package_dir {
    my $self = shift;
    my $module =shift;

    return File::Spec->catdir($self->{cache_root},$module,"packages");
}

sub clean_package_dir {
    my $self = shift;
    my $module = shift;

    rmtree($self->get_package_dir($module));
}

sub cp_minus_root {
    my $src_dir = shift;
    my $src_file = shift;
    my $target_dir = shift;

    chomp ($src_dir);

    my $copy_fragment = $src_file;
    $copy_fragment =~ s/^$src_dir[\/]?//;

    my $target_file = File::Spec->catfile($target_dir,$copy_fragment);
    mkpath((File::Spec->splitpath($target_file))[1]);

    Test::AutoBuild::Lib::_copy ($src_file, $target_file);

    return $target_file;
}

1 # So that the require or use succeeds.

__END__

=back 4

=head1 AUTHORS

Dennis Gregorovic <dgregorovic@alum.mit.edu>

=head1 COPYRIGHT

Copyright (C) 2003-2004 Dennis Gregorovic <dgregorovic@alum.mit.edu>

=head1 SEE ALSO

L<perl(1)>

=cut
