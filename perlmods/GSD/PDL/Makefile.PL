use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'      => 'GSD::PDL',
    'VERSION_FROM' => 'PDL.pm', # finds $VERSION
    dist          => { COMPRESS => "gzip -9f" },
    );

