use ExtUtils::MakeMaker;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'      => 'Starlink::AMS::Core',
    'PREREQ_PM' => { 'Proc::Simple' => 1.13,
                     'IO::Pipe' => 0.01 },
    'VERSION_FROM' => 'Core.pm', # finds $VERSION
    'linkext'   => {LINKTYPE=>'' }, # no link needed
    'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => 'gz',
                    'ZIP'=>'/usr/bin/zip','ZIPFLAGS'=>'-rl'}
);

