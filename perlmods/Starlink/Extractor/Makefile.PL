use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
              'NAME' => 'Starlink::Extractor',
              'VERSION_FROM' => 'Extractor.pm',
              'PREREQ_PM' => {
                              'Starlink::AMS::Init' => 0,
                              'Starlink::AMS::Task' => 0,
                              'Starlink::EMS' => 0,
                              'Astro::Catalog' => 0,
                              'File::Spec' => 0,
                              'File::Temp' => 0.14,
                             },
              ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
               (ABSTRACT_FROM => 'Extractor.pm',
                AUTHOR => 'Brad Cavanagh <b.cavanagh@jach.hawaii.edu>') : ()),
              'dist' => {COMPRESS=>'gzip -9f', SUFFIX=>'gz'},
             );
