use ExtUtils::MakeMaker;
use ExtUtils::F77;
use Config;

$osname = $Config{osname};

# For location of Starlink libraries and incs
eval "use Starlink::Config;";
if ($@) {
  print "Could not find Starlink::Config - using /star\n";
  %StarConfig = (
                 Star_Inc => '/star/include',
                 Star_Lib => '/star/lib',
		 Star_Bin => '/star/bin',
                );
}


# ------- Install directory
# This is where the new perl binary will be placed if you 'make perl'
# and make install.

$install = $StarConfig{Star_Bin};

#---- LIBS ----
# The Starlink libraries are picked up via the 'ndf_link' command

# This is the list of libraries required to use NDFs
# $ndflibs = -lndf -lpsx -lary -lhds -lcnf -lerr -lerr_standalone -lems -lchr -lprm -lprm_a 
$ndflibs = qx/ndf_link/;

die "No NDF libraries were found.\n This may imply that the ndf_link command is not in your path." if $ndflibs !~ /./;

# If you are using a 64bit machine then we need to do some special
# fiddling. Note that starlink libraries do not work if malloc returns
# 64 bit memory addresses. The ndf_link command contains linker directives
# to force malloc to return 32 bit addresses.
# Note also that you must build your perl binary with 32 bit malloc
# if you wish to use a sharable version of the NDF library.

# In most cases you must build the NDF module as a static ndfperl binary
# to fix these problems

# Firstly I need to remove the link directives from the library list
# since MakeMaker doesn't understand them


undef $linkopts;

if ($osname eq 'dec_osf') {

  print "This is a $osname machine\n";
  @temp = split /\s+/, $ndflibs;
  undef $ndflibs;


  foreach (@temp) {
    if (/^-Wl/) {
#      $linkopts .= (split(/,/))[1] . " ";
      $linkopts .= $_ . " ";
    } else {
      $ndflibs .= $_ . " ";
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

# Also need to remove -lUfor from the library list if I am
# using my own version of getarg

$fortranlibs =~ s/-lUfor// if ($osname eq 'dec_osf');

# This is the location of the Starlink NDF libraries
$starlibs = "-L$StarConfig{Star_Lib}";

# Uncomment this if you are explicitly linking with libarrays
# $arrays = "-Larrays -larrays";

$libs = "$starlibs $ndflibs $arrays $fortranlibs";

WriteMakefile(
    'INSTALLBIN'=> $install,
    'MAP_TARGET'=> "ndfperl",
    'NAME'      => 'NDF',
    'OPTIMIZE'  => $optimize,
    'DISTNAME'  => 'NDF',
    'VERSION_FROM' => 'NDF.pm', # finds $VERSION
    'LIBS'      =>  [ "$libs" ],
    'DEFINE'    => "$defines",
    'INC'       => "-I$StarConfig{Star_Inc}",
    'dist'      => { COMPRESS => "gzip -9f" },
    'INCLUDE_EXT'=> [ ],      # Only build NDF in the static perl
  ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
     (ABSTRACT_FROM => 'NDF.pm',
      AUTHOR     => 'Tim Jenness <t.jenness@jach.hawaii.edu>') : ()),
    );

