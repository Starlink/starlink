use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

use ExtUtils::F77;
use Config;

$osname = $Config{osname};

 
#---- INCLUDES ----
 
# This is the location of the Starlink include files
$starinc = "-I/star/include";
 
# This is the location of the standard C includes
$inc = "-I/usr/include";
 
# ---- LIBS -------
 
# This is the list of libraries required to use ems
$nbslibs = qx/nbs_link/;
 
die "No NDF libraries were found.\n This may imply that the
nbs_link command is not in your path." if $nbslibs !~ /./;

$fortranlibs = ExtUtils::F77->runtime;

# Need to add -lgcc on my linux system
$fortranlibs .= " -lgcc" if ($osname eq 'linux');

# This is the location of the Starlink NDF libraries
$starlibs = "-L/star/lib";
 
$libs = "$starlibs $nbslibs $fortranlibs ";
 
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'      => 'Starlink::NBS',
    'VERSION_FROM' => 'NBS.pm', # finds $VERSION
    'LIBS'      => [ "$libs"],   # e.g., '-lm' 
    'DEFINE'    => '',     # e.g., '-DHAVE_SOMETHING' 
    'INC'       => "$inc $starinc",     # e.g., '-I/usr/include/other' 
    'dist'      => { COMPRESS => "gzip -9f" },
);

