#include <stdio.h>
#include "cnf.h"
#include "f77.h"

FILE *fd;   /*  A pointer to the FILE structure identifying the open file */

F77_INTEGER_FUNCTION(con_cwri2)( CHARACTER(data) TRAIL(data) ){
/*
 *  Name:
 *    con_cwri2
 
 *  Purpose:
 *    Provides access to the C "fwrite" function from Fortran
 *    for a single CHARACTER.

 *  Invocation:
 *    ISTAT = CON_CWRI2( DATA )

 *  Arguments:
 *    DATA = CHARACTER *( * ) (Given)
 *       The data to write out.

 *  Function Value:
 *    ISTAT = INTEGER (Returned)
 *        Status: 0 = failure, 1 = success

 */   
      GENPTR_CHARACTER(data)

      unsigned int n=0;
      int ret=0;
      char *c_data;


/*  Convert the supplied Fortran "DATA" string to a C-style string.  First
 *  get sufficient storage to hold the C string (including a trailing
 *  null). */

      c_data = (char *) malloc( sizeof( char )*( data_length ) );

      if( c_data )
         {
/*     Get the string. */
         cnf_imprt( data, data_length, c_data );

/*     Write out the C string. */
         if( fwrite( c_data, sizeof( char ), data_length, fd ) ==
                    data_length ) ret = 1;

/*     Free the storage used to hold the copy of the DATA string. */
         free( c_data );
      }

      return( ret );

}
 
