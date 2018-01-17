#include "sae_par.h"
#include "ary1.h"
#include <string.h>
#include "ary_ast.h"

void ary1Dcrep( int defer, const char *type, int ndim, const hdsdim *ubnd,
                int temp, HDSLoc **loc, AryDCB **dcb, int *status ) {
/*
*+
*  Name:
*     ary1Dcrep

*  Purpose:
*     Create a primitive array with an entry in the DCB.

*  Synopsis:
*     void ary1Dcrep( int defer, const char *type, int ndim, const hdsdim *ubnd,
*                     int temp, HDSLoc **loc, AryDCB **dcb, int *status )

*  Description:
*     This function converts an array placeholder object into a primitive
*     array and creates a new entry in the DCB to refer to it. The
*     placeholder object is passed by an HDS locator, which may be
*     annulled afterwards.

*  Parameters:
*     defer
*        Should the creation of the HDS primitive array be defered until
*        it is mapped? This is useful if the properties of the array
*        (i.e. bounds and type) may be changed before it is mapped in
*        such a way as to reduce the size of the array. If the creation
*        of the HDS array is not deferred, then the final size of the
*        container file may be larger than necessary size HDS does not
*        clean up any unneeded disk space leftby reducing the size of a
*        previously created object. A deferred array is indicated by the
*        fact that it has a null locator for its non-imaginary component.
*     type
*        Data type of the array to be created; an HDS primitive numeric
*        data type string (case insensitive).
*     ndim
*        Number of array dimensions.
*     ubnd
*        Upper bounds of the array (the lower bounds are taken to be 1).
*     temp
*        Whether the array is temporary (this is used to set its
*        disposal mode entry in the DCB).
*     loc
*        Locator to an array placeholder object (an empty scalar data
*        structure of type ARRAY); it should not be a cell or a slice.
*        A new locator is returned, since the original structure will
*        be erased and a new primitive array created in its place.
*     dcb
*        Returned holding a DCB pointer for the new data object.
*     status
*        The global status.

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
   HDSLoc *locp = NULL;       /* Parent structure locator */
   char name[DAT__SZNAM+1];   /* Object name */
   int i;                     /* Loop counter for dimensions */
   int nlev;                  /* Levels in HDS path name */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain a free slot in the DCB. Return a NULL DCB pointer if no slot
   could be found. */
   *dcb = (AryDCB *) ary1Ffs( ARY__DCBTYPE, status );
   if( *status != SAI__OK ){
      *dcb = NULL;

/* Obtain a locator to the placeholder object's parent structure. */
   } else {
      datParen( *loc, &locp, status );

/* Obtain the placeholder object's name. Then annul its locator and erase
   the object. */
      datName( *loc, name, status );
      datAnnul( loc, status );
      datErase( locp, name, status );

/* If we are not defering the creation of the HDS array, create a new
   primitive array of the required type and shape in its place and obtain
   a new locator to it. */
      if( !defer ){
         datNew( locp, name, type, ndim, ubnd, status );
         datFind( locp, name, loc, status );

/* If we are defering creation of the array, create an ARRAY structure
   containing a single component called Variant that is set to the value
   "PRIMITIVE". This is picked up by the ary1Dfrm function, and used as an
   indication that the array is primitive (actually, will be primitive
   when it is created). */
      } else {
         ary1Dfppl( locp, name, loc, status );
      }

/* Annul the parent structure locator. */
      datAnnul( &locp, status );

/* Clone a locator to the array for storage in the DCB. Link this locator
   into a private group to prevent external events annulling it. */
      (*dcb)->loc = NULL;
      datClone( *loc, &(*dcb)->loc, status );
      hdsLink( (*dcb)->loc, "ARY_DCB", status );

/* Obtain the new data object file and path names and enter them into the
   DCB. */
      hdsTrace( (*dcb)->loc, &nlev, (*dcb)->path, (*dcb)->file, status,
                sizeof((*dcb)->path), sizeof((*dcb)->file) );

/* If we are not deferring creation of the array, obtain a non-imaginary
   component locator by cloning the data object locator. If we are
   deferring creation, retain a null locator. */
      (*dcb)->dloc = NULL;
      if( !defer ) datClone( (*dcb)->loc, &(*dcb)->dloc, status );

/* Set a null imaginary component locator. */
      (*dcb)->iloc = NULL;

/* If there was an error, then clean up by annulling all the locators which
   may have been acquired. */
      if( *status != SAI__OK ){
         datAnnul( &(*dcb)->loc, status );
         datAnnul( &(*dcb)->dloc, status );

/* Release the allocated DCB slot. */
         *dcb = ary1Rls( (AryObject *) *dcb, status );

/* If there was no error, then initialise the DCB entry for the array. */
      } else {
         (*dcb)->refcount = 0;
         (*dcb)->nread = 0;
         (*dcb)->nwrite = 0;

/* The form is known to be PRIMITIVE, the access mode to be UPDATE and the
   state to be "undefined", since the array has just been created. */
         strcpy( (*dcb)->form, "PRIMITIVE" );
         (*dcb)->kform = 1;
         strcpy( (*dcb)->mode, "UPDATE" );
         (*dcb)->kmode = 1;
         (*dcb)->state = 0;
         (*dcb)->init = 0;
         (*dcb)->kstate = 1;
         (*dcb)->kscl = 0;

/* Set the disposal mode according to whether it is a temporary object or
   not. */
         if( temp ){
            strcpy( (*dcb)->dispose, "TEMP" );
         } else {
            strcpy( (*dcb)->dispose, "KEEP" );
         }

/* Set the bad pixel flag to 1. */
         (*dcb)->bad = 1;
         (*dcb)->kbad = 1;

/* Store the data type (and complexity) information in upper case. */
         strncpy( (*dcb)->type, type, DAT__SZTYP + 1 ) ;
         astChrCase( NULL, (*dcb)->type, 1, 0 );
         (*dcb)->complex = 0;
         (*dcb)->ktype = 1;

/* Store the number of dimensions and the array bounds information, padding
   with 1's if necessary. */
         (*dcb)->ndim = ndim;
         for( i = 0; i < ndim; i++ ){
            (*dcb)->lbnd[ i ] = 1;
            (*dcb)->ubnd[ i ] = ubnd[ i ];
         }
         for( ; i < ARY__MXDIM; i++ ){
            (*dcb)->lbnd[ i ] = 1;
            (*dcb)->ubnd[ i ] = 1;
         }
         (*dcb)->kbnd = 1;

/* Initialise the accumulated pixel shifts to zero. */
         for( i = 0; i < ARY__MXDIM; i++ ){
            (*dcb)->shift[ i ] = 0;
         }
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dcrep", status );

}
