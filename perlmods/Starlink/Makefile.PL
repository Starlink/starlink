use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'      => 'Starlink',
    'VERSION'   => '1.12', # finds $VERSION
    'DIR'       => [ qw/ Versions EMS ADAM AMS NBS HDSPACK /],
    'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => 'gz',
                    'ZIP'=>'/usr/bin/zip','ZIPFLAGS'=>'-rl'},
 ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
     (ABSTRACT => 'StarPerl modules',
      AUTHOR     => 'Tim Jenness <t.jenness@jach.hawaii.edu>') : ()),
);

