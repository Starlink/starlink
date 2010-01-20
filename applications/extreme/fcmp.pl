#!/usr/bin/env starperl

#  Open.
      my( $f1, $f2 ) = @ARGV[ 0, 1 ];
      open( F1, "<$f1" ) or die( "Failed to open $f1\n" );
      open( F2, "<$f2" ) or die( "Failed to open $f2\n" );

#  Get normalised text.
      my( $s1 ) = normalise( join( "", <F1> ) );
      my( $s2 ) = normalise( join( "", <F2> ) );

#  Close.
      close( F1 );
      close( F2 );

#  Compare.
      if ( $s1 eq $s2 ) {
         exit( 0 );
      }
      else {
         print( " *** Edit error?\n" );
         exit( 1 );
      }

   exit;


#  Fortran normalisation routine.  Gets rid of continuation lines, 
#  spaces, and (most) CNF_PVAL invocations.

   sub normalise {
      local( $_ ) = @_;
      s/^ *include *'cnf_par' *(!.*)?\n//mig;
      s/\n[^*cCdD!]....[^ 0]//g;
      s/ *//g;
      s/cnf_pval\(([^)]*)\)/$1/ig;
      return( $_ );
   }
      
