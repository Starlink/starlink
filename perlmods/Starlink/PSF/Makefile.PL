use ExtUtils::MakeMaker;
use strict;

WriteMakefile(
               'NAME'           => 'Starlink::PSF',
               'VERSION'        => '0.01',
               'PREREQ_PM'      => {
				    'Math::Gsl::Sf' => '0',
                                   },
               'dist'           => { COMPRESS => "gzip -9f"},
               ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
               ( ABSTRACT  => 'Integrate PSF',
                 AUTHOR         => 'Andy Gibb E<lt>agg@astro.ubc.caE<gt>') : ()),
             );
