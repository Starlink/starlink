use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'      => 'PDL::IO::GSD',
    'VERSION_FROM' => 'GSD.pm', # finds $VERSION
    dist          => { COMPRESS => "gzip -9f" },
    );

