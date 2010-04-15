use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

use Config;
use strict;
use vars qw/ %StarConfig /;
use File::Spec;

# Try looking up the config system
eval "use Starlink::Config qw/ :override :verbose/;";
if ($@) {
  print "Could not find Starlink::Config - using /star\n";
  %StarConfig = (
		 Star_Inc => '/star/include',
		 Star_Lib => '/star/lib',
		);
}

# This is the list of libraries required to use ems
my $astlibs = qx/ast_link -myerr -mygrf /;

die "No AST libraries were found.\n This may imply that the
ast_link command is not in your path." if $astlibs !~ /./;

# Libs requires the location of the Starlink libraries
my $libs = "-L$StarConfig{Star_Lib} $astlibs ";

# Fink requires an extra path on OS X Panther
if ($^O eq 'darwin') {
  my $osver = $Config{osvers}; #  eg 8.2.0
  $osver =~ s/\..*//;
  $libs = "-L/sw/lib $libs -lcc_dynamic" if $osver < 8;
}

# Sun compiler needs special path to fortran libraries
$libs = "-L/opt/SUNWspro/lib $libs" if $^O eq 'solaris' && $astlibs =~ /lM77/;


# Typemap files. We have a typemap to use by default.
# For perl 5.6.1 and older we need to provide a typemap for
# T_AVREF and T_HVREF. Only use that if required.
my @typemaps = ( "typemap" );
unshift( @typemaps, "typemap.561") if $] < 5.008;

# Location of AST.pm
my $ast_pm = File::Spec->catfile( "lib", "Starlink", "AST.pm" );

# Write the makefile....
WriteMakefile(
    'NAME'      => 'Starlink::AST',
    'VERSION_FROM'   => $ast_pm,
    'LIBS'      => [ $libs ],   # e.g., '-lm'
    'INC'       => "-I$StarConfig{Star_Inc}", # e.g., '-I/usr/include/other'
    'TYPEMAPS'  => \@typemaps,
    'OBJECT'    => 'AST.o arrays.o grf_perl.o astTypemap.o',
    'depend'    => { 'AST.o' => "AST_ERR.xsh AST_WCSMAP.xsh AST_GRF.xsh"},
    'dist'      => {'COMPRESS'=>'gzip -9f', 'SUFFIX' => 'gz',
                    'ZIP'=>'/usr/bin/zip','ZIPFLAGS'=>'-rl'},
  ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
     (ABSTRACT_FROM => $ast_pm,
      AUTHOR     => 'Tim Jenness <tjenness@cpan.org>') : ()),
);

