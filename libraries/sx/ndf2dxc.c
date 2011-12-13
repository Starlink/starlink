#include <stdio.h>
#include "cnf.h"
#include "f77.h"

FILE *fd;   /*  A pointer to the FILE structure identifying the open file */



F77_INTEGER_FUNCTION(fopen)( CHARACTER(name), CHARACTER(access) TRAIL(name)
                       TRAIL(access) ){
/*
 *+
 *  Name:
 *    fopen

 *  Purpose:
 *    Provides access to the C "fopen" function from fortran

 *  Invocation:
 *    ISTAT = FOPEN( NAME, ACCESS )

 *  Arguments:
 *    NAME = CHARACTER*(*) (Given)
 *       The name of the file to open
 *    ACCESS = CHARACTER*(*) (Given)
 *       The access code with which to open the file. This should be a
 *       string acceptable to the C fopen function (eg, "w", "w+", etc)

 *  Function Value:
 *    ISTAT = INTEGER (Returned)
 *       Status: 0 = failure, 1 = success

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

 *  Authors:
 *    DSB: David S. Berry (Starlink)
 *    TIMJ: Tim Jenness (JAC, Hawaii)

 *  History:
 *    16-APR-2006 (TIMJ):
 *        Add prolog.

 *-
 */
      GENPTR_CHARACTER(name)
      GENPTR_CHARACTER(access)

      char *c_name,*c_access;

/*  Initialise the file descriptor to NULL  */

      fd = NULL;


/*  Convert the supplied Fortran "NAME" string to a C-style string. First
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




F77_INTEGER_FUNCTION(fclose)( void  ){
/*
 *+
 *  Name:
 *    fclose

 *  Purpose:
 *    Provides access to the C "fclose" function from fortran

 *  Invocation:
 *    ISTAT = FCLOSE()

 *  Function Value:
 *    ISTAT = INTEGER (Returned)
 *       Status: 0 = failure, 1 = success

 *-
 */

      if( fclose( fd ) ){
         return( 0 );

      } else {
         return( 1 );

      }

}

F77_INTEGER_FUNCTION(fwriter)( REAL(data), INTEGER(n) ){
/*
 *+
 *  Name:
 *    fwriter

 *  Purpose:
 *    Provides access to the C "fwrite" function from fortran
 *    for REAL data

 *  Invocation:
 *    ISTAT = FWRITER( DATA, N )

 *  Arguments:
 *    DATA( N ) = REAL (Given)
 *       The data to write out
 *    N = INTEGER (Given)
 *       The number of values to write out

 *  Function Value:
 *    ISTAT = INTEGER (Returned)
 *        Status: 0 = failure, 1 = success

 *-
 */
      GENPTR_REAL(data)
      GENPTR_INTEGER(n)

      if( fwrite( data, sizeof( float ), *n, fd ) == *n ){
         return( 1 );

      } else {
         return( 0 );

      }

}



F77_INTEGER_FUNCTION(fwrited)( DOUBLE(data), INTEGER(n) ){
/*
 *+
 *  Name:
 *    fwrited

 *  Purpose:
 *    Provides access to the C "fwrite" function from fortran
 *    for DOUBLE data

 *  Invocation:
 *    ISTAT = FWRITED( DATA, N )

 *  Arguments:
 *    DATA( N ) = DOUBLE (Given)
 *       The data to write out
 *    N = INTEGER (Given)
 *       The number of values to write out

 *  Function Value:
 *    ISTAT = INTEGER (Returned)
 *        Status: 0 = failure, 1 = success

 *-
 */
      GENPTR_DOUBLE(data)
      GENPTR_INTEGER(n)

      if( fwrite( data, sizeof( double ), *n, fd ) == *n ){
         return( 1 );

      } else {
         return( 0 );

      }

}



F77_INTEGER_FUNCTION(fwritei)( INTEGER(data), INTEGER(n) ){
/*
 *+
 *  Name:
 *    fwritei

 *  Purpose:
 *    Provides access to the C "fwrite" function from fortran
 *    for INTEGER data

 *  Invocation:
 *    ISTAT = FWRITEI( DATA, N )

 *  Arguments:
 *    DATA( N ) = INTEGER (Given)
 *       The data to write out
 *    N = INTEGER (Given)
 *       The number of values to write out

 *  Function Value:
 *    ISTAT = INTEGER (Returned)
 *        Status: 0 = failure, 1 = success

 *-
 */
      GENPTR_INTEGER(data)
      GENPTR_INTEGER(n)

      if( fwrite( data, sizeof( int ), *n, fd ) == *n ){
         return( 1 );

      } else {
         return( 0 );

      }

}

F77_INTEGER_FUNCTION(fwritec)( CHARACTER(data) TRAIL(data) ){
/*
 *+
 *  Name:
 *    fwritec

 *  Purpose:
 *    Provides access to the C "fwrite" function from fortran
 *    for CHARACTER data

 *  Notes:
 *    This routine forces the length of any string printed to be a multiple of 4.

 *  Invocation:
 *    ISTAT = FWRITEC( DATA )

 *  Arguments:
 *    DATA = CHARACTER *(*) (Given)
 *       The data to write out

 *  Function Value:
 *    ISTAT = INTEGER (Returned)
 *        Status: 0 = failure, 1 = success

 *-
 */
      GENPTR_CHARACTER(data)

      unsigned int n=0;
      int ret=0;
      char *c_data;


/*  Convert the supplied Fortran "DATA" string to a C-style string. First
 *  get sufficient storage to hold the C string (including a trailing
 *  null). */

      c_data = (char *) malloc( sizeof( char )*( data_length ) + 1);
      if( c_data )
         {
/*     Get the string */
         cnf_imprt( data, data_length, c_data );

/*     Write out the C string */
         if( fwrite( c_data, sizeof( char ), data_length, fd ) ==
                    data_length ) ret = 1;

/*     Work out how much space needs to be added to make the string
       length a multiple of 4 */
         n=4*(unsigned int)(1+data_length/4)-data_length;

/*     Write out the appropriate C string terminator */
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

