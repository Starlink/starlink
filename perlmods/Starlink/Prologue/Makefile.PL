use ExtUtils::MakeMaker;
use strict;

WriteMakefile(
               'NAME'           => 'Starlink::Prologue',
               'VERSION'        => '0.01',
               'PREREQ_PM'      => {
                                    'Test::More' => 0,
                                   },
               EXE_FILES        => [qw/ procvt prolis prolic /],
               'dist'           => { COMPRESS => "gzip -9f"},
               ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
               ( ABSTRACT  => 'Starlink Prologue parsing',
                 AUTHOR         => 'Tim Jenness E<lt>t.jenness@jach.hawaii.eduE<gt>') : ()),
             );
