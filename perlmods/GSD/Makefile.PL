use ExtUtils::MakeMaker;


# Define your math libraries here
$mathlibs = '-L/opt/SUNWspro/lib -lsunmath -lm';

# Uncomment this if you are explicitly linking with libarrays
# $arrays = "-Larrays -larrays";
 
$libs = "$arrays $mathlibs";

WriteMakefile(
    'NAME'      => 'GSD',
    'VERSION_FROM' => 'GSD.pm',
    'LIBS'      => "-L./gsd -lgsd $libs",
    'DEFINE'    => '',
    'INC'       => '-I./gsd',
    dist          => { COMPRESS => "gzip -9f" },
    );

