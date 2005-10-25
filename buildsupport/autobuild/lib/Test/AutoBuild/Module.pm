# -*- perl -*-
#
# Test::AutoBuild::Module by Daniel Berrange <dan@berrange.com>
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

Test::AutoBuild::Module - represents a code module to be built

=head1 SYNOPSIS

  use Test::AutoBuild::Module;

  my $module = Test::AutoBuild::Module->new(name => $name,
                                            label => $label,
                                            paths => \@paths,
                                            repository => $repository,
                                            [depends => \@modules,]
                                            [env => \%env,]
                                            [group => $group,]
                                            [dir => $directory],
                                            [buildroot => $directory],
                                            [controlfile => $controlfile]);
 
  $module->build($cache, \%modules);
  $module->install($cache, \%modules);


=head1 DESCRIPTION

The Test::AutoBuild::Module manages a single code module
in the build.

=head1 CONFIGURATION

The valid configuration options for the C<modules> block are

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Module;

use strict;
use Carp qw(confess);


=pod

=item my $module = Test::AutoBuild::Module->new(name => $name,
                                            label => $label,
                                            paths => \@paths,
                                            repository => $repository,
                                            [depends => \@modules,]
                                            [env => \%env,]
                                            [group => $group,]
                                            [dir => $directory],
                                            [buildroot => $directory],
                                            [controlfile => $controlfile]);

Creates a new code module object. C<name> is a alphanumeric
token for the name of the module. C<labe> is a short human
friendly title for the module. The C<paths> array ref is a 
list of paths to checkout of the source repository, whose
name is specified by C<repository>. C<depends> is an array
ref containing a list of dependant module names. C<env> is
a hash ref of environment variables to define when building
the module. C<group> is the optional name of the group to 
which the module belongs. C<dir> is the directory in which
the module was checked out, if different from C<name>. 
C<buildroot> is the path of the virtual root directory for
this module if different from the global default. The
C<controlfile> parameter is the name of the build control
file to run if different from the global default.

=cut

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = {};
    my %params = @_;

    $self->{name} = exists $params{name} ? $params{name} : confess "name parameter is required";
    $self->{label} = exists $params{label} ? $params{label} : confess "label parameter is required";
    $self->{paths} = exists $params{paths} ? $params{paths} : confess "paths parameter is required";
    $self->{repository} = exists $params{repository} ? $params{repository} : confess "repository parameter is required";
    $self->{links} = exists $params{links} ? $params{links} : [];
    $self->{artifacts} = exists $params{artifacts} ? $params{artifacts} : [];
    $self->{packages} = {};
    $self->{depends} = exists $params{depends} ? $params{depends} : [];
    $self->{env} = exists $params{env} ? $params{env} : {};
    $self->{group} = exists $params{group} ? $params{group} : "global";
    $self->{dir} = exists $params{dir} ? $params{dir} : $self->{name};
    $self->{build_root} = exists $params{buildroot} ? $params{buildroot} : undef;
    $self->{build_log} = '';
    $self->{build_date} = undef;
    $self->{build_time} = undef;
    $self->{build_log_filename} = "$self->{name}." . time . ".log";
    $self->{build_status} = 'pending';
    $self->{control_file} = exists $params{controlfile} ? $params{controlfile} : './rollingbuild.sh';

    $self->{use_cache} = exists $params{cache} ? $params{cache} : 1;
    $self->{load_cache} = 0;
    $self->{cache} = undef;
    $self->{options} = exists $params{options} ? $params{options} : {};
    $self->{installed} = 0;

    bless $self, $class;

    return $self;
}

=pod

=item my $value = $module->option($name[, $newvalue]);

Returns the value of the option referenced by C<name>.
If the C<newvalue> parameter is supplied, then the
option is also updated.

=cut

sub option {
   my $self = shift;
   my $name = shift;

   $self->{options}->{$name} = shift if @_;

   return $self->{options}->{$name};
}

=pod

=item my $label = $module->label([$newlabel]);

Returns the label of this module, a short
human friendly title. If the C<newlabel>
parameter is supplied the label is also updated.

=cut

sub label {
    my $self = shift;
    $self->{label} = shift if @_;
    return $self->{label};
}


=pod

=item my $name = $module->name([$newname]);

Returns the name of this module, a short
alphanumeric token. If the C<newname>
parameter is supplied the name is also updated.

=cut

sub name {
    my $self = shift;
    $self->{name} = shift if @_;
    return $self->{name};
}

=pod

=item my \@paths = $module->paths([@newpaths]);

Returns an array reference of paths to check
out of the source control repository. If the
C<newpaths> parameter is supplied, the paths
are also updated.

=cut

sub paths {
    my $self = shift;
    $self->{paths} = shift if @_;
    return $self->{paths};
}


=pod

=item my $repository = $module->repository($newrep)

Returns the name of the source repository storing
this module. If the C<newrep> parameter is supplied
the name is also updated.

=cut

sub repository {
    my $self = shift;
    $self->{repository} = shift if @_;
    return $self->{repository};
}


=pod

=item my $path = $module->dir($newpath);

Returns the path for the directory checked
out of source control. Typically this is
the same as the module name. If the C<newpath>
parameter is supplied the dir is updated.

=cut

sub dir {
    my $self = shift;
    $self->{dir} = shift if @_;
    return $self->{dir};
}

=pod

=item my $dir = $module->build_root([$newpath]);

Returns the path to the virtual root directory
in which this module will install files. If the
C<newpath> parameter is supplied the build root
will be updated.

=cut

sub build_root {
    my $self = shift;
    $self->{build_root} = shift if @_;
    if (defined $self->{build_root}) {
        return $self->{build_root};
    } else {
        return $ENV{AUTO_BUILD_ROOT};
    }
}

sub real_build_root {
    my $self = shift;
    return $self->{build_root};
}

=pod

=item $module->install($cache, \%modules);

Installs all this module's files from a previously
populated build cache. If any dependant
modules have not yet been processed, they will
be installed first.

=cut

sub install {
    my $self = shift;
    my $cache = shift;
    my $modules = shift;

    if ($self->{installed}) {
	return;
    }
    foreach my $depend (@{$self->{depends}}) {
	$modules->{$depend}->install($cache, $modules);
    }
    defined $cache && $cache->load_install($self->{name}, $self->build_root());
    $self->{installed} = 1;
}


=pod

=item my \%packages = $module->packages($pkgs, $package_types);

Not quite sure what this does yet.

=cut

sub packages {
    my $self = shift;
    my $pkgs = shift;
    my $package_types = shift;

    if ($self->{use_cache} && defined($self->{cache})) {
        # Using cache; need to coordinate
        if (defined $pkgs) {
            if ($self->{load_cache}) {
                # Set the packages
                $self->{packages} = $self->{cache}->packages($self->{name}, $pkgs, $package_types);
            } else {
                # We ignore passed in packages unless load_cache == 1
                $self->{packages} = $self->{cache}->packages($self->{name}, undef, $package_types);
            }
        } else {
            # Get the packages
            $self->{packages} = $self->{cache}->packages($self->{name}, undef, $package_types);
        }
    } else {
        if (defined $pkgs) {
            $self->{packages} = $pkgs;
        }
    }
    return $self->{packages};
}


=pod

=item my \@modules = $module->dependancies([\@modules]);

Returns an array ref of dependant module names. If the
C<modules> parameter is supplied then the list of 
dependants is updated.

=cut

sub dependencies {
    my $self = shift;
    $self->{depends} = shift if @_;
    return $self->{depends};
}


sub env {
    my $self = shift;
    $self->{env} = shift if @_;
    return $self->{env};
}

sub group {
    my $self = shift;
    $self->{group} = shift if @_;
    return $self->{group};
}


=pod

=item my \@links = $module->links([\@links]);

Returns an array ref of links associated with this
module. Each element in the array is a hash reference.
The keys in the hash reference are, C<title>, C<description>
and C<href>.

=cut

sub links {
    my $self = shift;
    $self->{links} = shift if @_;
    return $self->{links};
}

=pod

=item my \@artifacts = $module->artifacts([\@artifacts]);

Returns an array ref of artifacts to publish at the end
of each build cycle. Each element in the array is a hash
reference. The keys in the hash reference are, C<title>,
C<destionation>, C<src>, C<dst>, and C<publisher>. 

=cut

sub artifacts {
    my $self = shift;
    $self->{artifacts} = shift if @_;
    return $self->{artifacts};
}

sub build_log {
    my $self = shift;
    $self->{build_log} = shift if @_;
    return $self->{build_log};
}

sub build_time {
    my $self = shift;
    $self->{build_time} = shift if @_;
    return $self->{build_time};
}

sub build_date {
    my $self = shift;
    $self->{build_date} = shift if @_;
    return $self->{build_date};
}

sub build_log_filename {
    my $self = shift;
    $self->{build_log_filename} = shift if @_;
    return $self->{build_log_filename};
}

sub build_status {
    my $self = shift;
    $self->{build_status} = shift if @_;
    return $self->{build_status};
}

sub do_build {
    my $self = shift;
    my $cache = shift;
    my $modules = shift;

    local %ENV = %ENV;
    foreach (keys %{$self->{env}}) {
        $ENV{$_} = $self->{env}->{$_};
    }

    my $cmd = "cd $self->{dir} && " . $self->{control_file};


    my $cmd_output = `$cmd 2>&1`;

    $self->build_log($cmd_output);

    if ($? == 0) {
        $self->build_status_from_log($cmd_output);
    } else {
        $self->build_status('failed');
    }

    $self->{installed} = 1;
}

sub build {
    my $self = shift;
    my $cache = shift;
    my $modules = shift;

    $self->build_date(time());

    if ( $self->{use_cache} && defined ($cache) ) {

        $self->{cache} = $cache;

        my $cache_good = $cache->test_cache($self);
        my $all_deps_ok = 1;
        foreach my $depend (@{$self->{depends}}) {
            if ($modules->{$depend}->build_status() ne 'cache') {
                $all_deps_ok = 0;
            }
        }

        if ( ! $all_deps_ok ) {
            #print "Not using cache because dependencies were not cached\n";
        }

        if ( $cache_good && $all_deps_ok) {
            $self->build_log ("CACHED\n" . $cache->build_log($self->{name}));
            $self->build_status('cache');
            $self->build_date($cache->get_build_date($self->{name}));
            return;
        }

        $cache->clear($self->{name});

        my $package_type = undef;
        my $before = undef;
        my $after = undef;
        my $new_files = {};

        $package_type =
            Test::AutoBuild::PackageType->new(name => $self->{name},
                                                  label => '',
                                                  extension => '',
                                                  spool => $self->build_root());

        foreach my $depend (@{$self->{depends}}) {
            $modules->{$depend}->install($cache, $modules);
        }

        $before = $package_type->snapshot();
        $self->do_build($cache, $modules);
        $after = $package_type->snapshot();

        if ( $self->build_status() eq 'success') {
            $new_files = Test::AutoBuild::Lib::new_packages ($before, $after);
            $cache->save_install($self->{name}, $self->build_root(), $new_files);
            $cache->build_log($self->{name}, $self->build_log());
            $self->{load_cache} = 1;
        }

        return;
    }

    foreach my $depend (@{$self->{depends}}) {
        $modules->{$depend}->install($cache, $modules);
    }

    $self->do_build($cache, $modules);
}

sub build_status_from_log {
    my $self = shift;
    my $log = shift;

    # This is a pretty grotty hack. In fact, this whole function is
    # a nasty piece of work. Scripts should fail by dieing. It's only
    # a strangeness of ant that they don't.

    if ($log =~ m/\s+FAILED\s+/ || $log =~ m/\s+ERROR\s+/) {
        $self->build_status('failed');
        return;
    }

    $self->build_status('success');
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
