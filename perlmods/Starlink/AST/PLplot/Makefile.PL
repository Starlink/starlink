use ExtUtils::MakeMaker;

WriteMakefile( 
               
               'NAME'           => 'Starlink::AST::PLplot',

               # Version of distribution not any given module
	       'VERSION_FROM'   => '../AST.pm',
               'PREREQ_PM'      => { },
	       'dist'           => { COMPRESS => "gzip -9f"},
	       ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
	       ( ABSTRACT       => 'PLplot wrapper class',
		 AUTHOR         => 'Alasdair Allan <aa@astro.ex.ac.uk>') : ()),
             );

