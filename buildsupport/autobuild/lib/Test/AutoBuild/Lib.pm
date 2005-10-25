# -*- perl -*-
#
# Test::AutoBuild::Lib by Daniel Berrange <dan@berrange.com>
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

Test::AutoBuild::Lib - A library of useful routines

=head1 SYNOPSIS

  use Test::AutoBuild::Lib;

  my \@groups = Test::AutoBuild::Lib::load_groups($config);
  my \@repositories = Test::AutoBuild::Lib::load_repositories($config);
  my \@outputs = Test::AutoBuild::Lib::load_outputs($config);
  my \@modules = Test::AutoBuild::Lib::load_modules($config);
  my \@package_types = Test::AutoBuild::Lib::load_package_types($config);

  my \@sorted_modules = Test::AutoBuild::Lib::sort_modules(\@modules);

  my \%packages = Test::AutoBuild::Lib::package_snapshot($package_types);
  my \%newpackages = Test::AutoBuild::Lib::new_packages(\%before, \%after);

  my $string = Test::AutoBuild::Lib::pretty_size($bytes);
  my $string = Test::AutoBuild::Lib::pretty_date($seconds);
  my $string = Test::AutoBuild::Lib::pretty_time($seconds);

  my 

=head1 DESCRIPTION

The Test::AutoBuild::Lib module provides a library of routines
that are shared across many different modules.

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Lib;

use strict;
use Carp qw(confess);
use Test::AutoBuild::Group;
use Test::AutoBuild::Module;
use Test::AutoBuild::PackageType;
use File::Copy;
use File::Path;
use File::stat;
use POSIX qw(strftime);


=pod

=item my \@groups = Test::AutoBuild::Lib::load_groups($config);

Loads the groups defined in the configuration object C<config>. The
config object is an instance of the C<Config::Record> class. The
elements in the returned array reference are instances of the 
Test::AutoBuild::Group class.

=cut

sub load_groups {
    my $config = shift;

    my $data = $config->get("groups", {
        global => {
            label => "Global",
            }
    });
    my $groups = {};

    foreach my $name (keys %{$data}) {
        my $params = $data->{$name};
        confess "no label for $name group" unless exists $params->{label};

        my $group = Test::AutoBuild::Group->new(name => $name, %{$params});
        $groups->{$name} = $group;
    }

    $groups->{global} = Test::AutoBuild::Group->new(name => "global", label => "Global")
        unless exists $groups->{global};

    return $groups;
}

=pod

=item my \@publishers = Test::AutoBuild::Lib::load_publishers($config);

Loads the artifact publishing modules defined in the configuration 
object C<config>. The config object is an instance of the 
C<Config::Record> class. The elements in the returned array reference 
are instances of a subclass of Test::AutoBuild::Publisher.

=cut

sub load_publishers {
    my $config = shift;

    my $data = $config->get("publishers", {
      copy => {
        label => "File Copier",
        module => "Test::AutoBuild::Publisher::Copy"
      }
    });
    my $publishers = {};

    foreach my $name (keys %{$data}) {
        my $params = $data->{$name};
        confess "no label for $name group" unless exists $params->{label};

        my $module = $data->{$name}->{module};
        confess "no module for $name publisher" unless defined $module;

        eval "use $module;";
        die $@ if $@;
        my $publisher = $module->new(name => $name, %{$params});
        $publishers->{$name} = $publisher;
    }

    return $publishers;
}

=pod

=item my \@repositories = Test::AutoBuild::Lib::load_repositories($config);

Loads the repositories defined in the configuration object C<config>. The
config object is an instance of the C<Config::Record> class. The
elements in the returned array reference are instances of the 
Test::AutoBuild::Repository class.

=cut

sub load_repositories {
    my $config = shift;

    my $data = $config->get("repositories", {
        cvs => {
            module => "Test::AutoBuild::Repository::CVS",
            label => "CVS Repository"
            }
    });
    my $reps = {};

    foreach my $name (keys %{$data}) {
        my $label = $data->{$name}->{label};
        confess "no label for $name repository" unless defined $label;

        my $opts = $data->{$name}->{options};
        $opts = {} unless defined $opts;

        my $env = $data->{$name}->{env};
        $env = {} unless defined $env;

        my $module = $data->{$name}->{module};
        confess "no module for $name repository" unless defined $module;

        eval "use $module;";
        die $@ if $@;

        my $rep = $module->new(label => $label,
                               name => $name,
                               options => $opts,
                               env => $env);
        $reps->{$name} = $rep;
    }

    return $reps;
}


=pod

=item my \@outputs = Test::AutoBuild::Lib::load_outputs($config);

Loads the outputs defined in the configuration object C<config>. The
config object is an instance of the C<Config::Record> class. The
elements in the returned array reference are instances of the 
Test::AutoBuild::Output class.

=cut

sub load_outputs {
    my $config = shift;

    my $start_time = time;

    my $data = $config->get("output", {
        http => {
            module => 'Test::AutoBuild::Output::PackageCopier',
            label => 'Web Distribution Site',
            options => {
                directory => '/var/www/autobuild'
                }
        }
    });

    my $outputs = {};

    foreach my $name (keys %{$data}) {

        my $label = $data->{$name}->{label};
        confess "no label for $name output" unless defined $label;

        my $module = $data->{$name}->{module};
        confess "no module for $name output" unless defined $module;

        my $opts = $data->{$name}->{options};
        $opts = {} unless defined $opts;

        eval "use $module;";
        die $@ if $@;

        my $output = $module->new(label => $label,
                                  name => $name,
                                  options => $opts,
                                  start_time => $start_time);
        $outputs->{$name} = $output;
    }

    return $outputs;
}

=pod

=item my \@package_types = Test::AutoBuild::Lib::load_package_types($config);

Loads the package_types defined in the configuration object C<config>. The
config object is an instance of the C<Config::Record> class. The
elements in the returned array reference are instances of the 
Test::AutoBuild::PackageType class.

=cut


sub load_package_types {
    my $config = shift;

    my $data = $config->get("package-types", {
        rpm => {
            label => 'Linux RPM',
            extension => '.rpm',
            spool => '~/rpm'
            }
    });

    my $package_types = {};

    foreach my $name (keys %{$data}) {
        my $params = $data->{$name};

        $package_types->{$name} = Test::AutoBuild::PackageType->new(name => $name, %{$params});
    }

    return $package_types;
}

=pod

=item my \@modules = Test::AutoBuild::Lib::load_modules($config);

Loads the modules defined in the configuration object C<config>. The
config object is an instance of the C<Config::Record> class. The
elements in the returned array reference are instances of the 
Test::AutoBuild::Module class.

=cut


sub load_modules {
    my $config = shift;

    my $data = $config->get("modules", {
        test => {
            label => 'Test Application',
            paths => 'test/test-app:HEAD',
            repository => 'cvs',
            env => {
                X_TOOLS_HOME => '/usr/local/foo'
                }
        }
    });

    my $modules = {};

    foreach my $name (keys %{$data}) {
        my $params = $data->{$name};

        $modules->{$name} = Test::AutoBuild::Module->new(name => $name, %{$params});
    }

    return $modules;
}


=pod

=item my \@sorted_modules = Test::AutoBuild::Lib::sort_modules(\@modules);

Performs a topological sort of modules based on their declared
build dependancies. The elements in the C<modules> parameter 
should be instances of Test::AutoBuild::Module class. The returned
array ref will be in sorted order.

=cut

sub sort_modules {
    my $modules = shift;

    my $order = [];

    my %pairs;	# all pairs ($l, $r)
    my %npred;	# number of predecessors
    my %succ;	# list of successors

    # tsort code by Jeffrey S. Haemer, <jsh@boulder.qms.com>
    # SEE ALSO tsort(1), tcsh(1), tchrist(1)
    # Algorithm stolen from Jon Bentley (I<More Programming Pearls>, pp. 20-23),
    # Who, in turn, stole it from Don Knuth
    # (I<Art of Computer Programming, volume 1: Fundamental Algorithms>,
    # Section 2.2.3)

    foreach my $name (keys %{$modules}) {
        my $depends = $modules->{$name}->dependencies();
        if ($#{$depends} > -1) {
            foreach (@{$depends}) {
                die "module $name depends on non-existent module $_"
                    unless exists $modules->{$_};
                next if defined $pairs{$_}{$name};
                $pairs{$_}{$name}++;
                $npred{$_} += 0;
                $npred{$name}++;
                push @{$succ{$_}}, $name;
            }
        } else {
            $pairs{$name}{$name}++;
            $npred{$name} += 0;
            push @{$succ{$name}}, $name;
        }
    }
    # create a list of nodes without predecessors
    my @list = &_rand_order(grep {!$npred{$_}} keys %npred);
    while (@list) {
        $_ = pop @list;
        push @{$order}, $_;
        foreach my $child (@{$succ{$_}}) {
            # depth-first (default)
            push @list, $child unless --$npred{$child};
        }
    }
    return $order;
}

sub _rand_order { 
    my @new_array;
    while (@_ > 0) {
	push @new_array, splice(@_,int(rand @_),1);
    }
    return @new_array;
}

=pod

=item my \%packages = Test::AutoBuild::Lib::package_snapshot(\@pacakge_types);

Takes a snapshot all packages on disk for each package
type. The elements in the C<package_types> parameter 
should be instances of Test::AutoBuild::PackageType.
The keys in the returned hash ref will be the fully
qualified filenames of the packages, while the values
will be instances of Test::AutoBuild::Package class.

=cut

sub package_snapshot {
    my $package_types = shift;

    my $packages = {};
    foreach my $name (keys %{$package_types}) {
        my $packs = $package_types->{$name}->snapshot();

        map { $packages->{$_} = $packs->{$_} } keys %{$packs};
    }
    return $packages;
}


=pod

=item my %packages = Test::AutoBuild::Lib::new_packages(\%before, \%after);

Compares the sets of packages defined by the C<before> and C<after>
package snapshots. The returned hash ref will have entries for any
files in C<after>, but not in C<before>, or any files which were 
modified between C<before> and C<after> snapshots.

=cut

sub new_packages {
    my $before = shift;
    my $after = shift;

    my $packages = {};
    foreach my $file (keys %{$after}) {
        if (!exists $before->{$file} ||
            $before->{$file}->last_modified() != $after->{$file}->last_modified()) {
            $packages->{$file} = $after->{$file};
        }
    }

    return $packages;
}

=pod

=item my $string = Test::AutoBuild::Lib::pretty_date($seconds);

Formats the time specified in the C<seconds> parameter to 
follow the style "Wed Jan 14 2004 21:45:23 UTC".

=cut

sub pretty_date {
    my $time = shift;

    if (defined $time) {
        return strftime "%a %b %e %Y %H:%M:%S UTC", gmtime($time);
    } else {
        return "";
    }
}

=pod

=item my $string = Test::AutoBuild::Lib::pretty_time($seconds);

Formats an interval in seconds for friendly viewing according
to the style "2h 27m 12s" - ie 2 hours, 27 minutes and 12 
seconds.

=cut

sub pretty_time {
    my $time = shift;

    if (defined $time) {
        my $time_hours;
        my $time_minutes;
        my $time_seconds;
        {
            use integer;
            $time_hours = $time / 3600;
            $time_minutes = ($time - ($time_hours * 3600)) / 60;
            $time_seconds = $time - ($time_hours * 3600) - ($time_minutes * 60);
        };

        return sprintf ("%02dh %02dm %02ds",
                        $time_hours,
                        $time_minutes,
                        $time_seconds);
    } else {
        return "";
    }
}

=pod

=item my $string = Test::AutoBuild::Lib::pretty_size($bytes);

Formats the size specified in the C<bytes> parameter for
friendly viewing. If the number of bytes is > 1024x1024
then it formats in MB, with 2 decimal places. Else if
the number of bytes is > 1024 it formats in kb with 2
decimal places. Otherwise it just formats as bytes.

=cut

sub pretty_size {
    my $size = shift;

    if ($size > (1024 * 1024)) {
        return sprintf("%.2f MB", ($size / (1024 * 1024)));
    } elsif ($size > 1024) {
        return sprintf("%.2f KB", ($size / 1024));
    } else {
        return $size . " b";
    }
}

=pod

=item my $output = Test::AutoBuild::Lib::run($command, \%env);

Executes the program specified in the C<command> argument.
The returned value is the output of the commands standard
output stream. Prior to running the command, the environment
variables specified in the C<env> parameter are set. This
environment is modified locally, so the changes are only
in effect for the duration of this method.

=cut

sub run {
    my $command = shift;
    my $env = shift;

    my $output = "";

    # print "running: $command\n";

    local %ENV = %ENV;
    foreach (keys %{$env}) {
        $ENV{$_} = $env->{$_};
    }

    $output = `$command`;

    if (! defined ($output) || $?) {
        die "error running '$command': $!\n";
    }

    # print "output: $output\n";

    return $output;
}

sub _copy {
    my @source = @_;
    my $target = pop @source;

    if (@source < 1) {
        if (defined $target) {
            die "no target specified";
        } else {
            die "no source or target specified";
        }
    }
    if (@source > 1 && -e $target && ! -d $target) {
        die "multiple sources specified but '$target' exists and is not a directory";
    }
    my $cat_dirs = 0;
    if (-d $target) {
	$cat_dirs = 1;
    }
    foreach (@source) {
        $_ = File::Spec->canonpath($_);
	if (!-e) {
	    confess "Source file $_ to copy does not exist\n";
	}

        if (-d && !-l) {
            my $dir = $_;
            my @dirs = File::Spec->splitdir($dir);
            my $new_target = $cat_dirs ? File::Spec->catdir($target, $dirs[$#dirs]) : $target;
            my @files;
            opendir(DIR, $dir) or die("can't opendir $dir: $!");
            push @files, grep { !m/^\.$/ && !m/^\.\.$/ } readdir(DIR);
            closedir DIR;
            foreach (@files) { $_ = File::Spec->catfile($dir, $_) };
            mkpath($new_target);
            @files > 0 && _copy (@files, $new_target);
        } else {
	    my @dirs = File::Spec->splitdir($target);
	    if (@dirs > 1) {
		pop @dirs;
		mkpath(File::Spec->catfile(@dirs));
	    }
            my $newfile = -d $target ? File::Spec->catfile($target,(reverse File::Spec->splitpath($_))[0]) : $target;
            if (-l $_) {
                symlink (readlink, $newfile);
                &setStats($newfile, lstat($_));
            } else {
                if (!copy($_, $target)) {
		    confess "cannot copy $_ to $target: $!";
		}
                &setStats($newfile, stat($_));
            }
        }
    }
}

sub setStats {
    my $file = shift;
    my $sb = shift;
    chmod ($sb->mode, $file);
    chown ($sb->uid, $sb->gid, $file);
}

1 # So that the require or use succeeds.

__END__

=back 4

=head1 AUTHORS

Daniel Berrange <dan@berrange.com>, Dennis Gregorovic <dgregorovic@alum.mit.edu>

=head1 COPYRIGHT

Copyright (C) 2002-2004 Daniel Berrange <dan@berrange.com>

=head1 SEE ALSO

L<perl(1)>

=cut
