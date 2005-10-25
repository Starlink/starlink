# -*- perl -*-
#
# Test::AutoBuild by Dan Berrange, Richard Jones
#
# Copyright (C) 2002 Dan Berrange, Richard Jones
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

Test::AutoBuild - Automated build engine

=head1 SYNOPSIS

  use Test::AutoBuild;
  use Config::Record;

  my $config = new Config::Record (file => $filename);
  my $builder = new Test::AutoBuild (config => $config [, verbose => 1]);

  my $status = $builder->run;

  exit $status;

=head1 DESCRIPTION

This module provides the engine of the automated build system.
It hooks together all the other modules to provide a structured
workflow process for running the build. In a future release of
autobuild, this module will be re-written to allow the workflow
to be defined through the configuration file.

=head1 SETUP

After installing the modules, the first setup step is to create
an unprivileged user to run the build as. By convention the 
user is called 'builder', in a similarly named group and a home 
directory of /var/builder. So as root, execute the following 
commands:

 $ groupadd builder
 $ useradd -g builder -m -d /var/builder builder

NB, with the combined contents of the source checkout, the cache 
and the virtual installed root, and HTTP site, the disk space
requirements can be pretty large for any non-trivial software.
Based on the applications being built, anywhere between 100MB
and many GB of disk space make be neccessary. For Linux, making
/var/builder a dedicated partition with LVM (Logical Volume
Manager) will enable additional space to be easily grafted on
without requiring a re-build.

The next step is to create the basic directory structure within
the user's home directory for storing the various files. This
should all be done while logged in as the build user:

  # Where source code will be checked out to  
  $ mkdir /var/builder/build-home

  # Where packages will install themselves
  $ mkdir /var/builder/build-root

  # Where the builder will cache installed files
  $ mkdir /var/builder/build-cache

  # Where HTML status pages will be generated
  # and packages / logs copied
  $ mkdir /var/builder/public_html

  # Where FTPable packages will be copied
  $ mkdir /var/builder/public_ftp

  # Required output dirs when building various 
  # different types of packages
  $ mkdir /var/builder/packages
  $ mkdir /var/builder/packages/rpm
  $ mkdir /var/builder/packages/rpm/BUILD
  $ mkdir /var/builder/packages/rpm/RPMS
  $ mkdir /var/builder/packages/rpm/RPMS/noarch
  $ mkdir /var/builder/packages/rpm/RPMS/i386
  $ mkdir /var/builder/packages/rpm/RPMS/i486
  $ mkdir /var/builder/packages/rpm/RPMS/i586
  $ mkdir /var/builder/packages/rpm/RPMS/i686
  $ mkdir /var/builder/packages/rpm/RPMS/i786
  $ mkdir /var/builder/packages/rpm/RPMS/sparc
  $ mkdir /var/builder/packages/rpm/SPECS
  $ mkdir /var/builder/packages/rpm/SOURCES
  $ mkdir /var/builder/packages/rpm/SRPMS
  $ mkdir /var/builder/packages/zips
  $ mkdir /var/builder/packages/tars
  $ mkdir /var/builder/packages/debian

=head1 CONFIGURATION

The first level of parameters in the configuration file
are processed by this module. The currently supported
parameters are:

=over 4

=item debug = 0

Boolean value to turns on/off debugging output
of build workflow engine

=item checkout-source = 0

Boolean value to turn on/off checkout of source
code. When debugging build configuration, it is
sometimes useful to turn off checkout of the
module source files. By setting this value to 1,
checkout will be disabled, letting the build run
against the previously checked out source.

=item nice-level = 20

Sets the schedular 'nice' priority. Software builds can
impose a considerable load on a machine, so by setting
the nice-level to +20, the operating system schedular
is informed that the automatic build should be run with
lowest priority on the system. This often helps interactivity
of command line shells on the build server. Since builds
should never be run as root, the values for this setting
can range from 0 (normal) to 20 (low).

=item control-file = rollingbuild.sh

Sets name of the shell file to execute to perform the
build on a module. There is generally no need to change
this value.

=item abort-on-fail = 0

Boolean value determining whether the build cycle terminates
immediately when a module fails to build. Since the build
engine will automatically skip any dependant modules upon
failure of their pre-requisite, there is generally no need to 
set this value to 1. 

=item tmp-dir = /var/tmp

Occassionally the build engine needs to maintain temporary
files. This setting determines which directory the files
will be saved in.

=back

=head2 Build sub-configuration

The C<build> sub-configuration block defines options specifically
related to the build process. These are all defined within a 
block:

  build = {
     .. options ...
  }

=over 4

=item home = /var/builder/build-home

Build home refers to the directory into which modules are
checked out from source control repository.

=item root = /var/builder/build-root

Build root refers to the virtual root directory into which
module builds will install files. When the module build is
invoked, this parameter will be provided in the AUTO_BUILD_ROOT
environment variable, allowing it to be used when running
make install (or equivalent action). eg 'make install DESTDIR=$AUTO_BUILD_ROOT'

=item cache-dir = /var/builder/build-cache

Build cache refers to the directory in which the build
engine caches files between cycles. There are currently 
two sets of files which are cached, generated packages
(ie RPMs, ZIPs, etc), and installed files from the virtual
root directory.

=item cache = 1

Boolean flag to determine whether to use the build cache.
The build engine detects whether there have been any changes 
in source control for a module & if none were made, then it
will skip build of the module & install files from the cache
into the build root & use previously detected packages. If 
you want to force all modules to be re-built even when there
were no changes, then set this to 0.

=item cache-timestamp = 0

As an alternative to having the build engine use the source
control tools to detect any changes, it is possible to have
it scan the local build home directory, comparing last
modification timestamp to the last build cycle time. If any
files are newer, then it will assume there were changes to
the module. This option is only needed when the source control
repository module does not support detection of changes. At
this time all source control modules support change detection
so there is no need to set this parameter to 1.

=back

=head2 Lock sub-configuration

The C<lock> sub-configuration block defines options specifically
related to the locking between build instances. These are all 
defined within a block:

    lock = {
        ....options....
    }

=over 4

=item file = /var/builder/.build.mutex

The file to use to prevent multiple instances of the builder
running concurrently. The typical cron configuration starts
the builder every 5 minutes. The first action the builder will
take is to try and lock the files, if it fails it will exit
immediately.

=item use-flock = 1

The flock() system call is not safe on NFS partitions. If
the lock file is on an NFS partition then set this value to
zero to have the builder use an alternative lock mechanism,
albeit one with a small race condition. This race condition
generally does not matter, unless two instances are started
at *exactly* the same time.

=back

=head2 Environment sub-configuration

The C<env> configuration block allows arbitrary environment
variales to be defined for the duration of the build
process. The options should all be within a block

    env = {
        ...options...
    }

=over 4

=item key = value

The C<key> is the name of the environment variable (eg C<PATH>)
while the value is an arbitrary string (eg C</usr/bin:/bin>).

=back

=head2 Group sub-configuration

The C<group> configuration block defines a set of module
groups. These groups are typically used in the build status
HTML pages for splitting up the display of a large number
of modules. Refer to the L<Test::AutoBuild::Group> module
for details of the configuration options. The options should
all be within a block

    groups = {
	....options...
    }

=head2 Package type sub-configuration

The C<package-types> configuration block defines a set
of package types to handle. The directory defined for
each package type will be scanned before & after a module
build to pick up any packages which were generated during
the build. Refer to the L<Test::AutoBuild::PackageType>
module for details of the configuration options.The options should
all be within a block

    package-types = {
	....options...
    }

=head2 Repository sub-configuration

The C<repositories> configuration block defines a set
of source control repositories from which modules will
be checked out. Refer to the L<Test::AutoBuild::Repository>
module for details of the configuration options. The options should
all be within a block

    repositories = {
	....options...
    }

=head2 Publisher sub-configuration

The C<publisher> configuration block defines a set of
publishers to use for copying module build artifacts to
an output directory, typically within the HTTP site.
Refer to the L<Test::AutoBuild::Publisher> module for
details of the configuration options. The options should
all be within a block

    publishers = {
	....options...
    }

=head2 Module sub-configuration

The C<module> configuration block defines a set of modules
to build on each cycle. Refer to the L<Test::AutoBuild::Module>
module for details of the configuration options.The options should
all be within a block

    modules = {
	....options...
    }

=head2 Output module sub-configuration

The C<output> configuration block defines a set of output
modules to run at the end of each build cycle.Refer to
the L<Test::AutoBuild::Output> module for details of the
configuration options.The options should
all be within a block

    output = {
	....options...
    }

=head1 METHODS

=over 4

=cut

package Test::AutoBuild;

use strict;
use BSD::Resource;
use Carp qw(confess);
use Test::AutoBuild::Cache;
use Test::AutoBuild::Lib;
use Fcntl ':flock';
use File::Path;
use File::Spec;
use POSIX qw(strftime);

use vars qw($VERSION);
$VERSION = '1.0.2';

=pod

=item $builder = Test::AutoBuild->(config => $config [, verbose => 1]);

Creates a new autobuild runtime object. C<$config> is a configuration 
file (instance of C<Config::Record>).

=cut

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = {};
    my %params = @_;

    $self->{config} = exists $params{config} ? $params{config} :
        confess "config parameter is required";
    $self->{verbose} = exists $params{verbose} ? $params{verbose} : 0;

    bless $self, $class;

    return $self;
}

=pod

=item $config = $builder->config([$name, [$default]]);

If invoked with no arguments returns the Config::Record object
storing the builder configuration. If invoked with a single
argument, returns the configuration value with the matching
name. An optional default value can be provided in the second
argument

=cut

sub config
{
    my $self = shift;

    if (@_) {
        my $name = shift;
        return $self->{config}->get($name, @_);
    }
    return $self->{config};
}

=pod

=item $builder-run();

Executes the build process. This is the heart of the auto build
engine. It performs the following actions:

 * Reads the list of modules, source control repositories,
   package types and output modules from the configuration
   file
 * Initializes the build cache
 * Takes out an exclusive file lock to prevent > 1 builder 
   running at the same time.
 * Changes the (nice) priority of the AutoBuild process
 * Checks the code for each module out of its respective 
   source control repository.
 * Does a topological sort to determine the build order
   for all modules
 * For each module to be built:
    - Take a snapshot of the package & virtual root install 
      directories
    - Change to the top level source directory of the module
    - Run the rollingbuild.sh script
    - Take another snapshot & compare to determine which
      files were install in the virtual root & which packages
      were generated
    - Save the intsalled files and packages in the cache.
 * Invoke each requested output module, for example, HTML
   status generator, package & log file copiers, email
   alerts

=cut

sub run
{
    my $self = shift;

    my $debug = $self->config("debug", 0);
    my $checkout = $self->config("checkout-source", 1);
    my $nice_level = $self->config("nice-level", 20);

    my $abort_on_fail = $self->config("abort-on-fail", 0);

    my $lockfile = $self->config("lock.file", "$ENV{HOME}/.build.mutex");
    my $flocking = $self->config("lock.use-flock", "0");

    my $use_cache = $self->config("build.cache", "0");
    my $cache_timestamp = $self->config("build.cache-timestamp", "0");
    my $cache_dir = $self->config("build.cache-dir", "$ENV{HOME}/.build-cache");
    my $cache = undef;

    if ($use_cache) {
        $cache = Test::AutoBuild::Cache->new(cache_root => $cache_dir,
					     timestamp => $cache_timestamp);
    }

    # $build_home is where we check out the source to (for CVS). Pretty
    # much unused for Perforce builds.
    my $build_home = $self->config("build.home", "$ENV{HOME}/build_home");

    # $build_root can be used as the fake "root" directory for
    # configure --prefix $AUTO_BUILD_ROOT/usr (if you wish).
    my $build_root = $self->config("build.root", "$ENV{HOME}/.build");

    my $package_dir = $self->config("package.dir", "$ENV{HOME}/public_html");

    my $repositories
        = Test::AutoBuild::Lib::load_repositories($self->{config});
    my $outputs
        = Test::AutoBuild::Lib::load_outputs($self->{config});
    my $groups
        = Test::AutoBuild::Lib::load_groups($self->{config});
    my $publishers
        = Test::AutoBuild::Lib::load_publishers($self->{config});

    # %$package_types maps "name" => PackageType objects, where "name"
    # is a string like "rpm" or "pkg".
    my $package_types
        = Test::AutoBuild::Lib::load_package_types($self->{config});

    # %$modules maps "name" => Module objects, where "name" is the name
    # of a module (eg. "auto-ccm-core-trunk").
    my $modules
        = Test::AutoBuild::Lib::load_modules($self->{config});

    my $tmpdir = $self->config("tmp-dir", "/var/tmp");

    my $tsort_input_file = "$tmpdir/tsort.in";

    #----------------------------------------------------------------------
    # Grab the global build lock.

    # print "Getting exclusive lock $flocking\n" if $debug;
    if ($flocking) {
        open LOCKFILE, ">$lockfile" or die "cannot open $lockfile: $!";

        flock (LOCKFILE, LOCK_EX | LOCK_NB) or exit 1;
    } else {
        # Note: There really isn't a race condition here.
        # since this script is only invoked every 5 mins
        if ( -f $lockfile ) {
            exit 1;
        }

        open LOCKFILE, ">$lockfile" or die "cannot open $lockfile: $!";
        close LOCKFILE;
    }
    print "Got exclusive lock\n" if $debug;

    #----------------------------------------------------------------------
    # Initialize a couple of random things

    chdir $build_home or die "chdir: $build_home: $!";

    my $start_time = time;              # NB: Also used for epoch number.

    #----------------------------------------------------------------------
    # Global environment overrides
    my $env = $self->config("env");
    local %ENV = %ENV;
    if (defined $env) {
        foreach (keys %{$env}) {
            $ENV{$_} = $env->{$_};
        }
    }

    #----------------------------------------------------------------------
    # Renice ourselves so we don't monopolise the machine
    print "Renicing to level $nice_level\n" if $debug;
    setpriority PRIO_PROCESS, $$, $nice_level
        or die "cannot renice to $nice_level: $!";

    #----------------------------------------------------------------------
    # Export code from source repository

    $0 = "Exporting code from source repositories";

    if ($checkout) {
        foreach my $name (keys %{$modules}) {
            my $module = $modules->{$name};

            my $repository = $repositories->{$module->repository()};
            die "cannot find repository definition for module " . $module->label
                unless defined $repository;

            print "Adding module $name to repository " . $repository->label() . "\n" if $debug;
            $repository->module($name, $module);
        }

        foreach my $name (keys %{$repositories}) {
            my $repo = $repositories->{$name};
            print "Initializing repository " . $repo->label() . "\n" if $debug;
            $repo->init();
        }

        foreach my $name (sort keys %{$modules}) {
            my $module = $modules->{$name};

            if ($debug) {
                print "Checking out $name\n";
            }

            my $repository = $repositories->{$module->repository()};
            die "cannot find repository definition for module " . $module->label
                unless defined $repository;

            my $changed = $repository->export ($name, $module, $groups);
	    if ($changed) {
		print "Module " . $module->name() . " changed, so clearing cache\n" if $debug;
		$cache->clear($module->name());
	    }
        }
    } else {
        if ($debug) {
            print "Skipping checkout of source code\n";
        }
    }

    my $order = Test::AutoBuild::Lib::sort_modules $modules;

    #----------------------------------------------------------------------
    # Clean up old packages directories (if specified).

    foreach my $name (keys %$package_types) {
        $package_types->{$name}->do_clean;
    }

    #----------------------------------------------------------------------
    # Do the build.

    # Set up the environment for the build.

    rmtree($build_root);
    mkdir $build_root, 0775 or die "mkdir $build_root: $!";

    foreach my $name (@$order) {
        my $module = $modules->{$name};
        my $module_build_root = $module->real_build_root();
        if (defined $module_build_root) {
            rmtree($module_build_root);
            mkdir $module_build_root, 0775 or die "mkdir $module_build_root: $!";
        } else {
	    # We need to explicitly set each module's build root so
	    # that in the case that it has to load its files from
	    # cache, they don't get added to the calling module's
	    # build root
	    $module->build_root($build_root);
	}
    }

    if ($debug) {
        print "build roots cleaned\n";
    }

    # A unique integer counter for this build,
    # allowing person calling us to override its value
    unless (exists $ENV{AUTO_BUILD_COUNTER}) {
        $ENV{AUTO_BUILD_COUNTER} = time;
    }

    # @$order contains a suitable build ordering. Now go and build it.

    if ($debug) {
        print "Build order:\n";
        foreach my $module (@$order) {
            print "  $module\n";
        }
        print "End\n";
    }

    foreach my $name (@$order) {
        print "Building $name (" . Test::AutoBuild::Lib::pretty_date(time()) . ")\n" if $debug;
        my $module = $modules->{$name};
        my $before =
            Test::AutoBuild::Lib::package_snapshot ($package_types);

        $0 = "Building: $name"; # Change our name to reflect what we're doing.

        my $depends = $module->dependencies();
        my $skip = 0;
        foreach my $depend (@{$depends}) {
            if ($modules->{$depend}->build_status() ne 'success' &&
                $modules->{$depend}->build_status() ne 'cache' ) {
                if ($debug) {
                    print "Skipping " . $module->label() . " because " .
                        $modules->{$depend}->label() . " failed\n";
                }
                $skip = 1;
            }
        }
        if ($skip) {
            $module->build_status("skipped");
            next;
        }

        if (defined $module->real_build_root()) {
            $ENV{AUTO_BUILD_ROOT} = $module->real_build_root();
        } else {
            $ENV{AUTO_BUILD_ROOT} = $build_root;
        }

        my $start = time;
        $module->build($cache, $modules);
        my $end = time;
        $module->build_time($end - $start);

        if ($module->build_status() eq 'failed' && $abort_on_fail) {
            last;
        }

        print "Done building $name (" . Test::AutoBuild::Lib::pretty_date(time()) . ")\n" if $debug;

        my $after
            = Test::AutoBuild::Lib::package_snapshot ($package_types);

        # %$packages maps "filename" => Package objects, where "filename"
        # is the absolute path of each package built during this run
        # (eg. the filename of an RPM file).
        my $packages
            = $module->packages
            (Test::AutoBuild::Lib::new_packages ($before, $after), $package_types);
        if ($debug) {
            foreach (keys %{$packages}) {
                print "Found package: $_\n";
            }
        }
    }

    #----------------------------------------------------------------------
    # Run the output modules

    foreach my $name (sort keys %{$outputs}) {
        my $output = $outputs->{$name};
        print "Running output module " . ref($output) . "\n" if $debug;
        $0 = "Running: " . ref($output); # Change our name to reflect what we're doing.
        $output->process ($modules, $groups, $repositories, $package_types, $publishers);
    }

    #----------------------------------------------------------------------
    # Release the lock.

    unless ($flocking) {
        print "Removing lockfile\n" if $debug;
        unlink $lockfile or die "unlink: $lockfile: $!";
    }
}


1 # So that the require or use succeeds.

__END__

=back 4

=head1 AUTHORS

Daniel P. Berrange, Dennis Gregorovic

=head1 COPYRIGHT

Copyright (C) 2002 Daniel Berrange <dan@berrange.com>

=head1 SEE ALSO

L<perl(1)>, L<http://www.autobuild.org>

=cut
