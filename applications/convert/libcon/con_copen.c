#include <stdio.h>
#include "cnf.h"
#include "f77.h"

extern FILE *fd;   /*  A pointer to the FILE structure identifying the open file */



F77_INTEGER_FUNCTION(con_copen)( CHARACTER(name), CHARACTER(access) TRAIL(name)
                       TRAIL(access) ){
/*
 *  Name:
 *    con_copen

 *  Purpose:
 *    Provides access to the C "fopen" function from Fortran.

 *  Invocation:
 *    ISTAT = CON_COPEN( NAME, ACCESS )

 *  Arguments:
 *    NAME = CHARACTER * ( * ) (Given)
 *       The name of the file to open
 *    ACCESS = CHARACTER * ( * ) (Given)
 *       The access code with which to open the file.  This should be a
 *       string acceptable to the C fopen function (eg, "w", "w+", etc.)

 *  Function Value:
 *    ISTAT = INTEGER (Returned)
 *       Status: 0 = failure, 1 = success


 */
      GENPTR_CHARACTER(name)
      GENPTR_CHARACTER(access)

      char *c_name,*c_access;

/*  Initialise the file descriptor to NULL  */

      fd = NULL;


/*  Convert the supplied Fortran "NAME" string to a C-style string.  First
 *  get sufficient storage to hold the C string (including a trailing
 *  null). */

      c_name = (char *) malloc( sizeof( char )*( name_length + 1 ) );
      if( c_name ){
         cnf_imprt( name, name_length, c_name );


/*  Convert the supplied Fortran "ACCESS" string to a C-style string. First
 *  get sufficient storage to hold the C string (including a trailing
 *  null). */

         c_access = (char *) malloc( sizeof( char )*( access_length + 1 ) );
         if( c_access ){
            cnf_imprt( access, access_length, c_access );


/*  Call fopen to open the file */

            fd = fopen( c_name, c_access );


/*  Free the storage used to hold the copy of the ACCESS argument. */

            free( c_access );

         }


/*  Free the storage used to hold the copy of the NAME argument. */

         free( c_name );

      }


/*  Return an error code (0) if the file descriptor still has a null
 *  value. */

      if( fd ) {
         return( 1 );

      } else {
         return( 0 );

      }

}

