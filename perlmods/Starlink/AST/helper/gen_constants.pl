#!perl

use strict;

# Extract constants from ast.h
# call as
#  grep 'define AST__' $STARLINK/include/ast.h | perl gen_constants.pl
# or
#  grep 'define GRF__' grf.h | perl gen_constants.pl
# 
my $switch;
$switch = 'STATUS'; # Also allow WCSMAP
$switch = 'WCSMAP';
$switch = 'ASTGRF';

if ($switch eq 'STATUS') {
  print "MODULE = Starlink::AST  PACKAGE = Starlink::AST::Status\n\n";
} elsif ($switch eq 'WCSMAP') {
  print "MODULE = Starlink::AST  PACKAGE = Starlink::AST::WcsMap\n\n";
} elsif ($switch eq 'ASTGRF' ) {
  print "MODULE = Starlink::AST  PACKAGE = Starlink::AST::Grf\n\n";
}

my $keep = 0;
while (<>) {
  my $line = $_;
  chomp($line);
  my (@x) = split(/\s+/,$line);

  if ($switch eq 'STATUS' ) {
    next unless $x[2] =~ /^2339/;
  } elsif ($switch eq 'WCSMAP' ) {
    next if $x[2] < 1 || $x[2] > 31;
    $keep = 1 if $x[1] eq 'AST__AZP';
    $keep = 0 if $x[1] eq 'AST__NPID';
    next unless $keep;
  } elsif ($switch eq 'ASTGRF' ) {
    $keep = 1;
    next unless $keep; 
  } else {
    next;
  }

  print "StatusType
$x[1]()
 CODE:
#ifdef $x[1]
    RETVAL = $x[1];
#else
    Perl_croak(aTHX_ \"Constant $x[1] not defined\\n\");
#endif
 OUTPUT:
  RETVAL

";

}
