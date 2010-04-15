use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

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

$osname = $Config{osname};


# ---- LIBS -------

# This is the list of libraries required to use NBS
$nbslibs = qx/nbs_link/;

die "No NDF libraries were found.\n This may imply that the
nbs_link command is not in your path." if $nbslibs !~ /./;


undef $linkopts;

if ($osname eq 'dec_osf') {

  print "This is a $osname machine\n";
  @temp = split /\s+/, $nbslibs;
  undef $nbslibs;


  foreach my $entry (@temp) {
    if ($entry =~ /^-Wl/) {
#      $linkopts .= (split(/,/))[1] . " ";
      $linkopts .= $entry . " ";
    } else {
      $nbslibs .= $entry . " ";
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

$libs = "$starlibs $nbslibs ";

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'      => 'Starlink::NBS',
    'PREREQ_PM' => { 'Starlink::EMS' => 0 },
    'OPTIMIZE'  => $optimize,
    'VERSION_FROM' => 'NBS.pm', # finds $VERSION
    'LIBS'      => [ "$libs"],   # e.g., '-lm'
    'DEFINE'    => '',     # e.g., '-DHAVE_SOMETHING'
    'INC'       => "-I$StarConfig{Star_Inc}",  # e.g., '-I/usr/include/other'
    'dist'      => { COMPRESS => "gzip -9f" },
  ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
     (ABSTRACT_FROM => 'NBS.pm',
      AUTHOR     => 'Tim Jenness <t.jenness@jach.hawaii.edu>') : ()),);

