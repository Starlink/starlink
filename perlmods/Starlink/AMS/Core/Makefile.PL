use ExtUtils::MakeMaker;

$| = 1;

# Check that we have the relevant modules

print "Checking for related modules...\n";


# Sub to check for the presence of a module
sub check_module {
  my $mod = shift;
  print "$mod...";
  eval "use $mod";
  die "missing.\n This is required before installing this module" if $@;
  print "ok\n";
}

check_module "IO::Pipe";
check_module "Proc::Simple";
check_module "Proc::SimpleKill";
print "Everything seems to be present\n";

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'      => 'Starlink::AMS::Core',
    'VERSION_FROM' => 'Core.pm', # finds $VERSION
    'linkext'   => {LINKTYPE=>'' }, # no link needed
    'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => 'gz',
                    'ZIP'=>'/usr/bin/zip','ZIPFLAGS'=>'-rl'}
);

