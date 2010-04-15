#include <stdio.h>
#include "cnf.h"
#include "f77.h"

extern FILE *fd;   /*  A pointer to the FILE structure identifying the open file */



F77_INTEGER_FUNCTION(con_cclos)( void  ){
/*
 *  Name:
 *    con_cclose

 *  Purpose:
 *    Provides access to the C "fclose" function from Fortran.

 *  Invocation:
 *    ISTAT = CON_CCLOS()

 *  Function Value:
 *    ISTAT = INTEGER (Returned)
 *       Status: 0 = failure, 1 = success


 */

      if( fclose( fd ) ){
         return( 0 );

      } else {
         return( 1 );

      }

}
