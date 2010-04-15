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


#  C normalisation routine.  Changes INT_BIG back to int and collapses
#  spaces.

   sub normalise {
      local( $_ ) = @_;
      s/INT_BIG/int/g;
      s/\s+/ /g;
      return( $_ );
   }

