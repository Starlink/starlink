use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'      => 'PDL::Io::NDF',
    'VERSION_FROM' => 'NDF.pm', # finds $VERSION
    dist          => { COMPRESS => "gzip -9f" },
    );

