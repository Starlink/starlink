use ExtUtils::MakeMaker;
use Config;

# For location of Starlink libraries and incs
eval "use Starlink::Config qw/ :override :verbose /;";
if ($@) {
  print "Could not find Starlink::Config - using /star\n";
  %StarConfig = (
                 Star_Inc => '/star/include',
                 Star_Lib => '/star/lib',
                );
}

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

# This is the location of the Starlink NDF libraries
$starlibs = "-L$StarConfig{Star_Lib}";

$libs = "$starlibs $amslibs";



# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'Starlink::ADAM',
    'VERSION_FROM' => 'ADAM.pm', # finds $VERSION
    'PREREQ_PM' => { 'Starlink::EMS' => 0 },
    'OPTIMIZE'  => $optimize,
    'LIBS'	=> [ "$libs"],   # e.g., '-lm'
    'DEFINE'	=> '',     # e.g., '-DHAVE_SOMETHING'
    'INC'	=> "-I$StarConfig{Star_Inc}",     # e.g., '-I/usr/include/other'
    'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => 'gz',
                    'ZIP'=>'/usr/bin/zip','ZIPFLAGS'=>'-rl'},
  ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
     (ABSTRACT_FROM => 'ADAM.pm',
      AUTHOR     => 'Tim Jenness <t.jenness@jach.hawaii.edu>') : ()),
);
