#include <ctype.h>
#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include <string.h>
#include "mers.h"
#include "ndf_ast.h"

void ndf1Cvtok( const char *forfil, NdfFCB *fcb, HDSLoc *ndfloc,
                const char *ndfnam, int *status ){
/*
*+
*  Name:
*     ndf1Cvtok

*  Purpose:
*     Define standard message tokens for use in conversion commands.

*  Synopsis:
*     void ndf1Cvtok( const char *forfil, NdfFCB *fcb, HDSLoc *ndfloc,
*                     const char *ndfnam, int *status )

*  Description:
*     This function defines a standard set of message tokens to hold
*     information about the datasets involved in foreign file conversion
*     operations.

*  Parameters:
*     forfil
*        Pointer to a null terminated string holding the fully-expanded
*        name of the foreign format file, optioanlly ending with a foreign
*        extension specifier.
*     fcb
*        Pointer to an object describing the file's format. May be NULL to
*        indicate the absence of a foreign format file (i.e. when a native
*        format NDF is accessed normally).
*     ndfloc
*        Locator which, in conjunction with the "ndfnam" parameter,
*        identifies the native format NDF object. If a value of NULL
*        is given, then "ndfnam" should contain the absolute name of this
*        object.
*     ndfnam
*        Pointer to a null terminated string holding the relative HDS name
*        of the native format NDF object (or the absolute name if "ndfloc"
*        is set to NULL).
*     *status
*        The global status.

*  Message Tokens:
*     The function defines the following message tokens:
*        DIR
*           Name of the directory containing the foreign file.
*        NAME
*           The name field of the foreign file.
*        TYPE
*           The type field of the foreign file (including leading ".").
*        VERS
*           The version number field of the foreign file.
*        FXS
*           The foreign extension specifier (including enclosing square
*           brackets).
*        FXSCL
*           A cleaned version of the foreign extension specifier in which
*           all non-alphanumeric characters have been replaced by
*           underscore. This is useful when determining the name of
*           temporary native NDFs in which to store the converted foreign
*           NDFs.
*        FMT
*           Name of the foreign file format (upper case).
*        NDF
*           Full name of the native NDF data structure.
*        NAMECL
*           The name of the foreign file but containing only characters
*           accptable within an HDS name. Any other characters are replaced
*           by an underscore.

*  Notes:
*      -  If "fcb" is NULL, then the DIR, NAME, TYPE and VERS tokens will
*      not be defined by this function.
*      -  If "fcb" is NULL, then the FMT token will be given the value "NDF".
*      -  If "ndfloc" is NULL and "ndfnam" is blank, then the NDF token will
*      not be defined by this function.
*      -  If any token requires a value but none is available (e.g. the
*      corresponding field of the foreign file name is missing or not
*      supported by the operating system), then a blank value will be
*      assigned.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char ch[2];           /* Buffer for single character string */
   char *mem = NULL;     /* Pointer to bynamically allocated memory */
   size_t d1;            /* First character of directory field */
   size_t d2;            /* Last character of directory field */
   size_t i;             /* Character index */
   size_t n1;            /* First character of name field */
   size_t n2;            /* Last character of name field */
   size_t t1;            /* First character of type field */
   size_t t2;            /* Last character of type field */
   size_t v1;            /* First character of version field */
   size_t v2;            /* Last character of version field */
   size_t x1;            /* First character of foreign extension field */
   size_t x2;            /* Last character of foreign extension field */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If a foreign format is being accessed, we must define tokens for each
   of the fields in the foreign file name. */
   if( fcb ) {

/* Initialise. */
      d1 = 0;
      d2 = -1;
      n1 = 0;
      n2 = -1;
      t1 = 0;
      t2 = -1;
      v1 = 0;
      v2 = -1;
      x1 = 0;
      x2 = -1;

/* Split the foreign file name into its directory, name, type,
   version and foreign extension fields */
      if( astChrLen( forfil ) > 0 ) {
         ndf1Spfor( forfil, fcb, &d1, &d2, &n1, &n2, &t1, &t2, &v1, &v2,
                    &x1, &x2, status );
      }
      if( *status == SAI__OK ) {

/* Define the foreign file directory token. */
         if( d1 <= d2 ) {
            mem = ndf1Strip( mem, forfil, d1, d2, NULL, NULL, status );
            msgSetc( "DIR", mem );
         } else {
            msgSetc( "DIR", " " );
         }

/* Define the foreign file name token. */
         if( n1 <= n2 ) {
            mem = ndf1Strip( mem, forfil, n1, n2, NULL, NULL, status );
            msgSetc( "NAME", mem );
         } else {
            msgSetc( "NAME", " " );
         }

/* Define the cleaned foreign file name token. */
         if( n1 <= n2 ) {
            for( i = n1; i <= n2; i++ ){
               if( isalnum( forfil[ i ] ) || forfil[ i ] == '_' ) {
                  ch[ 0 ] = forfil[ i ];
                  ch[ 1 ] = 0;
                  msgSetc( "NAMECL", ch );
               } else {
                  msgSetc( "NAMECL", "_" );
               }
            }
         } else {
            msgSetc( "NAMECL", " " );
         }

/* Define the foreign file type token. */
         if( t1 <= t2 ) {
            mem = ndf1Strip( mem, forfil, t1, t2, NULL, NULL, status );
            msgSetc( "TYPE", mem );
         } else {
            msgSetc( "TYPE", " " );
         }

/* Define the foreign file version token. */
         if( v1 <= v2 ) {
            mem = ndf1Strip( mem, forfil, v1, v2, NULL, NULL, status );
            msgSetc( "VERS", mem );
         } else {
            msgSetc( "VERS", " " );
         }

/* Define the foreign extension token, and a cleaned version containing
   only alphanumeric (and underscore) characters. */
         if( x1 <= x2 ) {
            mem = ndf1Strip( mem, forfil, x1, x2, NULL, NULL, status );
            msgSetc( "FXS", mem );
            for( i = x1; i <= x2; i++ ){
               if( isalnum( forfil[ i ] ) || forfil[ i ] == '_' ) {
                  ch[ 0 ] = forfil[ i ];
                  ch[ 1 ] = 0;
                  msgSetc( "FXSCL", ch );
               } else {
                  msgSetc( "FXSCL", "_" );
               }
            }

         } else {
            msgSetc( "FXS", " " );
            msgSetc( "FXSCL", " " );
         }

      }
   }

/* Define the foreign format name. */
   if( fcb ) {
      msgSetc( "FMT", fcb->name );
   } else {
      msgSetc( "FMT", "NDF" );
   }

/* If necessary, define the native format NDF name. */
   if( !ndfloc ) {
      if( astChrLen( ndfnam ) > 0 ) msgSetc( "NDF", ndfnam );
   } else {
      datMsg( "NDF", ndfloc );
      if( astChrLen( ndfnam ) > 0 ) {
         msgSetc( "NDF", "." );
         msgSetc( "NDF", ndfnam );
      }
   }

/* Free memory */
   mem = astFree( mem );

}

