use ExtUtils::MakeMaker;
use ExtUtils::F77;
use Config;

#---- INCLUDES ----
 
# This is the location of the Starlink include files
$starinc = "-I/star/include";
 
# This is the location of the standard C includes
$inc = "-I/usr/include";

# ---- LIBS -------

# This is the list of libraries required to use ams
$amslibs = qx/ams_link_adam/;

die "No AMS libraries were found.\n This may imply that the
ams_link_adam command is not in your path." if $amslibs !~ /./;

# Firstly I need to remove the link directives from the library list
# since MakeMaker doesn't understand them
 
undef $linkopts;
 
if ($osname eq 'dec_osf') {
 
  print "This is a $osname machine\n";
  @temp = split /\s+/, $amslibs;
  undef $amslibs;
 
 
  foreach (@temp) {
    if (/^-Wl/) {
#      $linkopts .= (split(/,/))[1] . " ";
      $linkopts .= $_ . " ";
    } else {
      $amslibs .= $_ . " ";
    }
  }
  print "Link options are $linkopts\n";
 
}
 
# This is a kludge but I need to pass the linker options through
# to the compiler. The only way I can see to do this is via the 
# Optimize flag. Just append the link options to the optimize
 
$optimize = $Config{optimize} . " $linkopts";

# Fortran libraries depends on the architecture
# Use the ExtUtils::F77 module for this
 
$fortranlibs = ExtUtils::F77->runtime;


# This is the location of the Starlink NDF libraries
$starlibs = "-L/star/lib";

$libs = "$starlibs $amslibs $fortranlibs";



# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'Starlink::ADAM',
    'VERSION_FROM' => 'ADAM.pm', # finds $VERSION
    'OPTIMIZE'  => $optimize,
    'LIBS'	=> [ "$libs"],   # e.g., '-lm' 
    'DEFINE'	=> '',     # e.g., '-DHAVE_SOMETHING' 
    'INC'	=> "$inc $starinc",     # e.g., '-I/usr/include/other' 
);
