use ExtUtils::MakeMaker;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'      => 'Starlink::Versions',
    'PREREQ_PM' => { 'File::Spec' => 0,
	'Starlink::Config' => 0,
        'DateTime::Format::Strptime' => 0,
	},
    'EXE_FILES' => [ qw/ starversion / ],
    'VERSION_FROM' => 'Versions.pm', # finds $VERSION
    'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => 'gz',
                    'ZIP'=>'/usr/bin/zip','ZIPFLAGS'=>'-rl'},
  ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
     (ABSTRACT_FROM => 'Versions.pm',
      AUTHOR     => 'Tim Jenness <t.jenness@jach.hawaii.edu>') : ()),

);

