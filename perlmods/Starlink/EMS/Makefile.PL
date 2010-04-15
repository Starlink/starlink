use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

use Config;

# For location of Starlink libraries and incs
eval "use Starlink::Config qw/ :override :verbose/;";
if ($@) {
  print "Could not find Starlink::Config - using /star\n";
  %StarConfig = (
		 Star_Inc => '/star/include',
		 Star_Lib => '/star/lib',
		);
}

# This is the list of libraries required to use ems
$emslibs = qx/ems_link/;

die "No EMS libraries were found.\n This may imply that the
ems_link command is not in your path." if $emslibs !~ /./;

# Libs requires the location of the Starlink libraries
$libs = "-L$StarConfig{Star_Lib} $emslibs ";

# Write the makefile....
WriteMakefile(
    'NAME'      => 'Starlink::EMS',
    'VERSION_FROM' => 'EMS.pm', # finds $VERSION
    'LIBS'      => [ $libs ],   # e.g., '-lm'
    'INC'       => "-I$StarConfig{Star_Inc}", # e.g., '-I/usr/include/other'
    'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => 'gz',
                    'ZIP'=>'/usr/bin/zip','ZIPFLAGS'=>'-rl'},
  ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
     (ABSTRACT_FROM => 'EMS.pm',
      AUTHOR     => 'Tim Jenness <t.jenness@jach.hawaii.edu>') : ()),
);

