use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

use Config;
use strict;
use vars qw/ %StarConfig /;

# For location of Starlink libraries and incs - unless $ENV{STARLINK}
if (exists $ENV{STARLINK}) {
    print "Using \$STARLINK override of $ENV{STARLINK}\n";
    %StarConfig = (
		   Star_Inc => $ENV{STARLINK}.'/include',
		   Star_Lib => $ENV{STARLINK}.'/lib',
		  );
} else {
  # Try looking up the config system
  eval "use Starlink::Config;";
  if ($@) {
    print "Could not find Starlink::Config - using /star\n";
    %StarConfig = (
		   Star_Inc => '/star/include',
		   Star_Lib => '/star/lib',
		  );
  } else {
    print "Found Starlink in $StarConfig{Star}\n";
  }
}

# This is the list of libraries required to use ems
my $astlibs = qx/ast_link/;

die "No AST libraries were found.\n This may imply that the
ast_link command is not in your path." if $astlibs !~ /./;

# Libs requires the location of the Starlink libraries 
my $libs = "-L$StarConfig{Star_Lib} $astlibs ";

# Write the makefile....
WriteMakefile(
    'NAME'      => 'Starlink::AST',
    'VERSION_FROM' => 'AST.pm', # finds $VERSION
    'LIBS'      => [ $libs ],   # e.g., '-lm' 
    'INC'       => "-I$StarConfig{Star_Inc}", # e.g., '-I/usr/include/other' 
    'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => 'gz',
                    'ZIP'=>'/usr/bin/zip','ZIPFLAGS'=>'-rl'},
  ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
     (ABSTRACT_FROM => 'AST.pm',
      AUTHOR     => 'Tim Jenness <t.jenness@jach.hawaii.edu>') : ()),
);

