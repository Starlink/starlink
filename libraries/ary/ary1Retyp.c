#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"
#include "star/hds.h"
#include <string.h>

void ary1Retyp( HDSLoc *paren, const char *name, const char *type,
                char state, char bad, int ndim, const hdsdim *dim,
                const char *ntype, char defer, HDSLoc **loc, int *dce,
                int *status ) {
/*
*+
*  Name:
*     ary1Retyp

*  Purpose:
*     Change the data type of a primitive numeric HDS object.

*  Synopsis:
*     void ary1Retyp( HDSLoc *paren, const char *name, const char *type,
*                     char state, char bad, int ndim, const hdsdim *dim,
*                     const char *ntype, char defer, HDSLoc **loc, int *dce,
*                     int *status )

*  Description:
*     This function changes the data type of a primitive numeric HDS
*     object while (optionally) preserving its contents. If the
*     contents are to be preserved, then data type conversion will be
*     performed on them.

*  Parameters:
*     paren
*        HDS locator to the object's parent structure.
*     name
*        HDS name of the object whose type is to be changed.
*     type
*        Initial data type of the HDS object. This must be a primitive
*        numeric HDS data type string (case insensitive).
*     state
*        The HDS state of the data object (non-zero for defined, zero
*        for undefined). This argument determines whether the contents
*        of the object are to be preserved. They are only preserved if
*        its value is non-zero.
*     bad
*        Whether checks for "bad" data values must be made if data type
*        conversion is performed. This argument is not used if "state" is
*        set to zero.
*     ndim
*        Number of object dimensions.
*     DIM( NDIM ) = INTEGER (Given)
*        Object dimension sizes.
*     ntype
*        The new data type required for the object. This must be a
*        primitive numeric HDS data type string (case insensitive).
*     defer
*        Should creation of the new HDS arrays be deferred?
*     loc
*        HDS locator to the object whose type is to be changed. Note
*        that type conversion involves erasing the object and creating
*        a new version, so this locator will be altered if the new type
*        differs from the initial one.
*     dce
*        Returned holding a flag indicating whether data conversion errors
*        occurred. This can only happen if "state" is non-zero.
*     status
*        The global status.

*  Notes:
*     -  The routine will execute most rapidly if "state" is set to
*     zero, since no data values then need to be converted.

*  Implementation Deficiencies:
*     -  This routine requires a parent locator in order to function, so
*     it cannot be used on a top-level HDS object.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   HDSLoc *oldloc=NULL;       /* Locator to old data */
   HDSLoc *tloc=NULL;         /* Locator to temporary structure */
   hdsdim dummy;              /* Dummy dimension array */
   int i;                     /* Loop counter for dimensions */
   int round;                 /* Round floating point values? */
   int there;                 /* Does the named component exist? */
   size_t el;                 /* Number of data elements to convert */
   void *oldptr;              /* Pointer to mapped old data */
   void *pntr;                /* Pointer to mapped new data */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise the DCE value and check whether the new type matches the old
   type. If so, then the data object type does not need changing. Ensure
   that the object is in the expected state by resetting it if appropriate
   (but only if creation of the HDS arrays has not been deferred) . */
   *dce = 0;
   if( !strcasecmp( type, ntype ) ){
      if( !defer && !state && loc ) datReset( *loc, status );

/* If the data object type needs changing, but "state" is 0, then there
   are no data values to convert. Annul the locator to the old object and
   erase it. */
   } else {
      if( !state ){
         if( *loc ) datAnnul( loc, status );
         datThere( paren, name, &there, status );
         if( there ) datErase( paren, name, status );

/* Create a new object of the required type and obtain a locator to it. */
         if( !defer ){
            datNew( paren, name, ntype, ndim, dim, status );
            datFind( paren, name, loc, status );
         }

/* If "state" is non-zero, then the object's data values must be converted.
   Create a temporary structure and move the old data object into it. */
      } else {
         dummy = 0;
         ary1Temp( " ", 0, &dummy, &tloc, status );
         datMove( loc, tloc, "TEMP", status );

/* Get a locator to the old object in its new structure. */
         datFind( tloc, "TEMP", &oldloc, status );

/* Create a new object with the required type and obtain a locator to it. */
         datNew( paren, name, ntype, ndim, dim, status );
         datFind( paren, name, loc, status );

/* Map both objects, the old one for reading and the new one for writing. */
         datMap( oldloc, type, "READ", ndim, dim, &oldptr, status );
         datMap( *loc, ntype, "WRITE", ndim, dim, &pntr, status );

/* Calculate the number of data elements that must be converted. */
         el = 1;
         for( i = 0; i < ndim; i++ ){
            el *= dim[ i ];
         }

/* See if floating point values should be rounded or truncated when
   converting them to integers. */
         round = aryRound( -1 );

/* Compare the new data type with each permitted value in turn and call the
   appropriate routine to convert the old data into the new type. */
         if(!strcasecmp( ntype, "_BYTE" ) ){
            ary1CvtB( bad, el, type, round, oldptr, pntr, dce, status );

         } else if(!strcasecmp( ntype, "_UBYTE" ) ){
            ary1CvtUB( bad, el, type, round, oldptr, pntr, dce, status );

         } else if(!strcasecmp( ntype, "_DOUBLE" ) ){
            ary1CvtD( bad, el, type, round, oldptr, pntr, dce, status );

         } else if(!strcasecmp( ntype, "_INTEGER" ) ){
            ary1CvtI( bad, el, type, round, oldptr, pntr, dce, status );

         } else if(!strcasecmp( ntype, "_REAL" ) ){
            ary1CvtF( bad, el, type, round, oldptr, pntr, dce, status );

         } else if(!strcasecmp( ntype, "_WORD" ) ){
            ary1CvtW( bad, el, type, round, oldptr, pntr, dce, status );

         } else if(!strcasecmp( ntype, "_UWORD" ) ){
            ary1CvtUW( bad, el, type, round, oldptr, pntr, dce, status );

         } else if(!strcasecmp( ntype, "_INT64" ) ){
            ary1CvtK( bad, el, type, round, oldptr, pntr, dce, status );

         } else if( *status == SAI__OK ) {
            *status = ARY__FATIN;
            errRepf( " ", "ary1Retyp: Unsupported data type '%s' encountered "
                     "(programming error).", status, ntype );
         }

/* Unmap the new object. */
         ary1Hunmp( *loc, status );

/* Annul the locator to the old data and erase the temporary structure
   containing it. */
         datAnnul( &oldloc, status );
         ary1Antmp( &tloc, status );
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Retyp", status );

}
