use strict;
use ExtUtils::MakeMaker;
use vars qw/ %StarConfig /;

# For location of Starlink libraries and incs
eval "use Starlink::Config qw/ :override :verbose /;";
if ( $@ ) {
  print "Could not find Starlink::Config - using /star\n";
  %StarConfig = (
                 Star_Bin => '/star/bin',
                );
}

WriteMakefile(
              'EXE_FILES' => [ 'autoastrom' ],
              'INSTALLSCRIPT' => "$StarConfig{Star_Bin}/autoastrom",
              'NAME' => 'Starlink::Autoastrom',
              'VERSION_FROM' => 'Autoastrom.pm',
              'PREREQ_PM' => {
                              'Starlink::AST' => 0,
                              'Starlink::Astrom' => 0,
                              'Starlink::Extractor' => 0,
                              'Astro::Coords' => 0.1,
                              'Astro::Correlate' => 0,
                              'Astro::FITS::HdrTrans' => 0,
                              'Astro::FITS::Header' => 0,
                              'Astro::WaveBand' => 0,
                              'NDF' => 0,
                             },
              ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
               (ABSTRACT_FROM => 'Autoastrom.pm',
                AUTHOR => 'Brad Cavanagh <b.cavanagh@jach.hawaii.edu>') : ()),
              'dist' => {COMPRESS=>'gzip -9f', SUFFIX=>'gz'},
             );
