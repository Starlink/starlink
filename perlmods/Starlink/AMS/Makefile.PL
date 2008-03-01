use ExtUtils::MakeMaker;

# For location of Starlink libraries and incs
eval "use Starlink::Config qw/ :override :verbose /;";
if ($@) {
  print "Could not find Starlink::Config - using /star\n";
  %StarConfig = (
                 Star_Bin => '/star/bin',
                );
}


# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'INSTALLSCRIPT' => "$StarConfig{Star_Bin}",
    'NAME'      => 'Starlink::AMS',
    'VERSION'   => '1.00', # finds $VERSION
    'DIR'       => [ qw/ Core Init Task /],
    'linkext'   => {LINKTYPE=>'' }, # no link needed
    'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => 'gz',
                    'ZIP'=>'/usr/bin/zip','ZIPFLAGS'=>'-rl'},
  ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
     (ABSTRACT => 'High level OO interface to ADAM messaging',
      AUTHOR     => 'Tim Jenness <t.jenness@jach.hawaii.edu>') : ()),

);

