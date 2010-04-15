#include <stdio.h>
#include "cnf.h"
#include "f77.h"

extern FILE *fd;   /*  A pointer to the FILE structure identifying the open file */


F77_INTEGER_FUNCTION(con_cwrit)( CHARACTER(data) TRAIL(data) ){
/*
 *  Name:
 *    con_cwrit

 *  Purpose:
 *    Provides access to the C "fwrite" function from Fortran
 *    for CHARACTER data.

 *  Notes:
 *    This routine forces the length of any string printed to be a
 *    multiple of 4.

 *  Invocation:
 *    ISTAT = FWRITEC( DATA )

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

/*     Work out how much space needs to be added to make the string
       length a multiple of 4. */
         n=4*(unsigned int)(1+data_length/4)-data_length;

/*     Write out the appropriate C string terminator. */
         switch (n)
           {
           case (1):
             fprintf( fd, "\n");
             break;
           case (2):
             fprintf( fd, " \n");
             break;
           case (3):
             fprintf( fd, "  \n");
             break;
           case (4):
             fprintf( fd, "   \n");
             break;
           }

/*     Free the storage used to hold the copy of the DATA string. */
         free( c_data );
      }

      return( ret );

}

