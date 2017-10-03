#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "ary_ast.h"
#include <string.h>

void ary1Dcre( char defer, const char *type, char cmplx, int ndim,
               const hdsdim *lbnd, const hdsdim *ubnd, char temp,
               HDSLoc *loc, AryDCB **dcb, int *status ) {
/*
*+
*  Name:
*     ary1Dcre

*  Purpose:
*     Create a simple array with an entry in the DCB.

*  Synopsis:
*     void ary1Dcre( char defer, const char *type, char cmplx, int ndim,
*                    const hdsdim *lbnd, const hdsdim *ubnd, char temp,
*                    HDSLoc *loc, AryDCB **dcb, int *status )

*  Description:
*     The routine converts an array placeholder object into a simple
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
*     cmplx
*        Whether a complex array is required.
*     ndim
*        Number of array dimensions.
*     lbnd
*        Lower bounds of the array.
*     ubnd
*        Upper bounds of the array.
*     temp
*        Whether the array is temporary (this is used to set its
*        disposal mode entry in the DCB).
*     loc
*        Locator to an array placeholder object (an empty scalar data
*        structure of type ARRAY).
*     dcb
*        Returned holding a pointer to the DCB which refers to the new data object.
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
   HDSLoc *tloc = NULL;       /* Locator for ORIGIN component */
   hdsdim dim[ARY__MXDIM];    /* Dimension sizes of array components */
   int i;                     /* Loop counter for dimensions */
   int nlev;                  /* Levels in HDS path name */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain a free slot in the DCB. Return a DCB index of zero if no slot
   could be found. */
   *dcb = (AryDCB *) ary1Ffs( ARY__DCBTYPE, status );
   if( *status != SAI__OK ){
      *dcb = NULL;

/* Calculate the dimension sizes for the array components to be created. */
   } else {
      for( i = 0; i < ndim; i++ ){
         dim[ i ] = ubnd[ i ] - lbnd[ i ] + 1;
      }

/* Clone a locator to the data object for storage in the DCB. Link this
   locator into a private group to prevent external events annulling it. */
      datClone( loc, &(*dcb)->loc, status );
      hdsLink( (*dcb)->loc, "ARY_DCB", status );

/* Obtain the new data object file and path names and enter them into the
   DCB. */
      hdsTrace( (*dcb)->loc, &nlev, (*dcb)->path, (*dcb)->file,
                status, sizeof((*dcb)->path), sizeof((*dcb)->file) );

/* Tune HDS for the expected maximum number of structure components. */
      hdsTune( "NCOMP", 5, status );

/* If we are not defering the creation of the HDS array, create the
   non-imaginary data component and obtain a locator to it. Store the
   locator in the DCB. */
      (*dcb)->dloc = NULL;
      (*dcb)->iloc = NULL;
      if( !defer ){
         datNew( (*dcb)->loc, "DATA", type, ndim, dim, status );
         datFind( (*dcb)->loc, "DATA", &(*dcb)->dloc, status );

/* If a complex array is required, then create and locate the imaginary
   component similarly. */
         if( cmplx ){
            datNew( (*dcb)->loc, "IMAGINARY_DATA", type, ndim, dim, status );
            datFind( (*dcb)->loc, "IMAGINARY_DATA", &(*dcb)->iloc, status );
         }
      }

/* Create the ORIGIN component and enter the lower bounds information. */
      datNew1I( (*dcb)->loc, "ORIGIN", ndim, status );
      datFind( (*dcb)->loc, "ORIGIN", &tloc, status );
      HDSDIM_TYPE(datPut1)( tloc, ndim, lbnd, status );
      datAnnul( &tloc, status );

/* If there was an error, then clean up by annulling all the locators which
   may have been acquired. */
      if( *status != SAI__OK ){
         datAnnul( &(*dcb)->loc, status );
         if( (*dcb)->dloc ) datAnnul( &(*dcb)->dloc, status );
         if( cmplx && (*dcb)->iloc ) datAnnul( &(*dcb)->iloc, status );

/* Release the allocated DCB slot. */
         *dcb = ary1Rls( (AryObject *) *dcb, status );

/* If there was no error, then initialise the DCB entry for the array. */
      } else {
         (*dcb)->refcount = 0;
         (*dcb)->nread = 0;
         (*dcb)->nwrite = 0;

/* The form is known to be SIMPLE, the access mode to be UPDATE and the
   state to be "undefined", since the array has just been created. */
         strcpy( (*dcb)->form, "SIMPLE" );
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

/* The array is created with a bad pixel flag value of 1. */
         (*dcb)->bad = 1;
         (*dcb)->kbad = 1;

/* Store the data type (and complexity) information in upper case. */
         strncpy( (*dcb)->type, type, DAT__SZTYP + 1 ) ;
         astChrCase( NULL, (*dcb)->type, 1, 0 );
         (*dcb)->complex = cmplx;
         (*dcb)->ktype = 1;

/* Store the number of dimensions and the array bounds information, padding
   with 1's if necessary. */
         (*dcb)->ndim = ndim;
         for( i = 0; i < ndim; i++ ){
            (*dcb)->lbnd[ i ] = lbnd[ i ];
            (*dcb)->ubnd[ i ] = ubnd[ i ];
         }
         for( ; i < ARY__MXDIM; i++ ){
            (*dcb)->lbnd[ i ] = 1;
            (*dcb)->ubnd[ i ] = 1;
         }
         (*dcb)->kbnd = 1;

/* Initialise the accumulated pixel shifts to zero. */
         for( i = 0; i < ARY__MXDIM; i++ ){
            (*dcb)->shift[ i ] = 1;
         }
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dcre", status );

}
