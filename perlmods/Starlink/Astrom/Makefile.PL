use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
              'NAME' => 'Starlink::Astrom',
              'VERSION_FROM' => 'Astrom.pm',
              'PREREQ_PM' => {
                              'Starlink::AMS::Init' => 0,
                              'Starlink::AMS::Task' => 0,
                              'Astro::Catalog' => 0,
                              'File::Spec' => 0,
                             },
              ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
               (ABSTRACT_FROM => 'Astrom.pm',
                AUTHOR => 'Brad Cavanagh <b.cavanagh@jach.hawaii.edu>') : ()),
              'dist' => {COMPRESS=>'gzip -9f', SUFFIX=>'gz'},
             );
