#include <stdio.h>
#include "cnf.h"
#include "f77.h"

extern FILE *fd;   /*  A pointer to the FILE structure identifying the open file */

F77_INTEGER_FUNCTION(con_cwri2)( BYTE_ARRAY(data), INTEGER(data_length) ){
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

 *  History:
 *    19-DEC-2000 (AJC):
 *       Use a byte array as argument
 */

      GENPTR_BYTE_ARRAY(data)
      GENPTR_INTEGER(data_length)

      int ret=0;


/*     Write out the C string. */
       if( fwrite( data, sizeof(char), *data_length, fd ) ==
                    *data_length ) ret = 1;


      return( ret );

}

