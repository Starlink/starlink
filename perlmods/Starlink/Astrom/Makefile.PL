use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
              'NAME' => 'Starlink::Astrom',
              'VERSION_FROM' => 'Astrom.pm',
              'PREREQ_PM' => {
                              'Starlink::AST' => 0,
                              'Astro::FITS::Header::CFITSIO' => 0,
                              'Astro::Catalog' => 0,
                              'File::Spec' => 0,
                             },
              ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
               (ABSTRACT_FROM => 'Astrom.pm',
                AUTHOR => 'Brad Cavanagh <b.cavanagh@jach.hawaii.edu>') : ()),
              'dist' => {COMPRESS=>'gzip -9f', SUFFIX=>'gz'},
             );
