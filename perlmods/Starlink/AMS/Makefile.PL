use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'EXE_FILES' => [ 'MessageRelay.pl' ],
    'INSTALLSCRIPT' => '/star/bin',
    'NAME'      => 'Starlink::AMS',
    'VERSION'   => '1.00', # finds $VERSION
    'DIR'       => [ qw/ Core Init Task /],
    'linkext'   => {LINKTYPE=>'' }, # no link needed
    'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => 'gz',
                    'ZIP'=>'/usr/bin/zip','ZIPFLAGS'=>'-rl'}
);

