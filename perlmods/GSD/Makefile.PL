use ExtUtils::MakeMaker;

# This makefile tries to find the libgsd library in
# a Starlink environment

# For location of Starlink libraries and incs
eval "use Starlink::Config;";
if ($@) {
  print "Could not find Starlink::Config - using /star\n";
  %StarConfig = (
                 Star_Inc => '/star/include',
                 Star_Lib => '/star/lib',
                );
}

# Define your math libraries here
# Only need sun math if we are on a sun
if ($^O eq 'solaris') {
  $mathlibs = '-L/opt/SUNWspro/lib -lsunmath -lm';
} else {
  $mathlibs = '-lm'
}

# Uncomment this if you are explicitly linking with libarrays
# $arrays = "-Larrays -larrays";

$libs = "$arrays $mathlibs";

WriteMakefile(
    'NAME'      => 'GSD',
    'VERSION_FROM' => 'GSD.pm',
    'LIBS'      => "-L$StarConfig{Star_Lib} -lgsd $libs",
    'PREREQ_PM' => {
                              'Test' => undef,
                             },
    'EXE_FILES' => ['gsdprint'],
    'DEFINE'    => '',
    'INC'       => "-I$StarConfig{Star_Inc}",
    dist          => { COMPRESS => "gzip -9f" },
   ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
     (ABSTRACT_FROM => 'GSD.pm',
      AUTHOR     => 'Tim Jenness <t.jenness@jach.hawaii.edu>') : ()),
    );

