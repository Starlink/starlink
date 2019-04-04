#include <ctype.h>
#include <stdlib.h>
#include <pthread.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Dnfor( const char *forfil, NdfFCB *fcb, int keep, HDSLoc **ndfloc,
                char *ndfnam, size_t ndfnam_length, size_t *lnam, int *status ){
/*
*+
*  Name:
*     ndf1Dnfor

*  Purpose:
*     Identify a default NDF to be associated with a foreign file.

*  Synopsis:
*     void ndf1Dnfor( const char *forfil, NdfFCB *fcb, int keep,
*                     HDSLoc **ndfloc, char *ndfnam, size_t ndfnam_length,
*                     size_t *lnam, int *status )

*  Description:
*     This function accepts the name of a foreign format file from (or to)
*     which data conversion is to be performed and identifies a default NDF
*     object which can be associated with it and used to hold the
*     (converted) native NDF format version of the data. The NDF is chosen
*     either to be a new NDF file in the current default directory with a
*     name field matching that of the foreign file itself, or a temporary
*     object in the HDS scratch file. The identified NDF is not actually
*     created by this function.

*  Parameters:
*     forfil
*        Pointer to a null terminated string holding the name of the
*        foreign format file, optionally including a foreign extension
*        specifier.
*     fcb
*        Pointer to an object describing the foreign file format (must be
*        non-NULL if "keep" is non-zero).
*     keep
*        If a non-zero value is supplied, it indicates that the NDF will be
*        kept, so it should reside in a file in the default directory.
*        Otherwise, it will reside in the HDS scratch file.
*     *ndfloc
*        Returned holding the locator which, in conjunction with the
*        "ndfnam" value, identifies the NDF. On successful exit, this
*        locator will either have the root locator value NULL (in
*        which case "ndfnam" contains the full NDF name) or will be an
*        active locator (in which case "ndfnam" contains the relative name
*        of the NDF). If active, this locator should be annulled by the
*        caller when no longer required.
*     ndfnam
*        Pointer to an array in which to return a null terminated string
*        holding absolute or relative HDS name which, in conjunction with
*        the "ndfloc" value, identifies the NDF object.
*     ndfnam_length
*        The length of the supplied 'ndfnam' array. This should include
*        room for the terminating null.
*     *lnam
*        Returned holding the number of characters in the "ndfnam" value.
*     *status
*        The global status.

*  Notes:
*     -  If this function is called with "status" set, then an invalid
*     locator will be returned via the "ndfloc" parameter. The same value
*     will also be returned if this function should fail for any reason.

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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char *name;           /* Dynamic array holding constructed name */
   hdsdim dim;           /* Dummy dimension array */
   int namlen;           /* Length of returned name */
   int nc;               /* No. of characters to copy */
   size_t d1;            /* First character of directory field */
   size_t d2;            /* Last character of directory field */
   size_t i;             /* Character index */
   size_t n1;            /* First character of name field */
   size_t n2;            /* Last character of name field */
   size_t t1;            /* First character of type field */
   size_t t2;            /* Last character of type field */
   size_t v1;            /* First character of version field */
   size_t v2;            /* Last character of version field */
   size_t x1;            /* First character of for. extension field */
   size_t x2;            /* Last character of for. extension field */

   static HDSLoc *tmploc = NULL;   /* Locator to temporary structure */
   static int count = 0; /* Number of temporary components */
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;


/* Set an initial null value for the "ndfloc" parameter. */
   *ndfloc = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Permanent NDF.
   =============
   If the NDF is to be saved, then it should reside in the default
   directory, so return a root locator value. */
   if( keep ) {
      *ndfloc = NULL;

/* Split the foreign file name into directory, name, type, version and
   foreign extension fields. */
      ndf1Spfor( forfil, fcb, &d1, &d2, &n1, &n2, &t1, &t2, &v1, &v2, &x1,
                 &x2, status );
      if( *status == SAI__OK ) {

/* Omit any directory specification (the default will be used) and
   form the file name from the name and foreign extension fields of the
   foreign file (if present). Replace any non-alphanumeric characters in
   the foreign extension specifier with underscores, to ensure the
   resulting NDF name is legal. */
         if( n1 <= n2 ) {
            name = ndf1Substr( forfil, n1, n2, status );
            namlen = n2 - n1 + 1;
         } else {
            namlen = 0;
            name = NULL;
         }

         if( x1 <= x2 ) {
            nc = x2 - x1 + 1;
            name = astAppendStringf( name, &namlen, "%.*s", nc, forfil + x1 );
            for( i = namlen - nc - 1; i < namlen; i++ ){
               if( !isalnum( name[ i ] ) ) name[ i ] = '_';
            }
         }

/* Return the NDF file name and its length, if this is not zero. */
         if( namlen > 0 ) {
            ndf1Ccpy( name, ndfnam, ndfnam_length, status );

/* If the name consists of just a type field (unusual, but possible)
   then this must be returned enclosed in quotes to prevent a
   completely blank name resulting. */
         } else {
            namlen = 6;
            ndf1Ccpy( "\".sdf\"", ndfnam, ndfnam_length, status );
         }

/* Return the string length as size_t. */
         *lnam = namlen;

      }

/* Temporary NDF.
   =============
   If the NDF is not to be kept, then it can be a temporary object. */
   } else {

/* Since this section uses static variables, we use a mutex to ensure that
   multiple threads do not access these variables at the same time. */
      pthread_mutex_lock( &mutex );

/* If no temporary HDS structure has yet been created, then create one
   to contain the temporary NDF object. */
      if( !tmploc ) datTemp( "NDF_CVT_AREA", 0, &dim, &tmploc, status );

/* Clone and return a locator for the temporary structure. */
      if( *status == SAI__OK ) {
         datClone( tmploc, ndfloc, status );

/* Increment the count of components within the temporary structure. */
         if( *status == SAI__OK ) {
            count++;

/* Create and return the name of a new component. */
            namlen = 0;
            name = astAppendStringf( NULL, &namlen, "NDF_%d", count );
            *lnam = namlen;
            ndf1Ccpy( name, ndfnam, ndfnam_length, status );
         }
      }

/* Unlock the mutex, allowing other threads to acess the static variables. */
      pthread_mutex_unlock( &mutex );

   }

/* If an error occurred, then return an invalid locator value. */
   if( *status != SAI__OK ) *ndfloc = NULL;

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dnfor", status );

}

