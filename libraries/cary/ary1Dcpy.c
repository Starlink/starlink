#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include <string.h>

void ary1Dcpy( AryDCB *dcb1, int temp, HDSLoc **loc, AryDCB **dcb2, int *status ) {
/*
*+
*  Name:
*     ary1Dcpy

*  Purpose:
*     Copy a data object to a new (or temporary) HDS location and
*     create a DCB entry for it.

*  Synopsis:
*     void ary1Dcpy( AryDCB *dcb1, int temp, HDSLoc **loc, AryDCB **dcb2,
*                    int *status )

*  Description:
*     This function copies a data object, identified by its entry in the
*     DCB, to create a new array in place of an array placeholder
*     object and creates a new DCB entry to describe it. Only valid
*     data components are copied (rogue components are ignored).
*     Otherwise, the new object is identical to the original.

*  Parameters:
*     dcb1
*        The DCB describing the data object to be copied.
*     temp
*        Whether the new array is temporary (this is used to set its
*        disposal mode entry in the DCB).
*     loc
*        Locator to an array placeholder object (an empty scalar data
*        structure of type ARRAY). In the case of a primitive array,
*        the associated structure will be erased and replaced with a
*        new object; a new locator will therefore be returned and the
*        original placeholder object should not be a cell or a slice.
*     dcb2
*        Returned holding a pointer to the DCB which refers to the new
*        data object.
*     status
*        The global status.

*  Notes:
*     -  A value of NULL will be returned for the "dcb2" argument if the
*     routine is called with "status" set, although no further processing
*     will occur.
*     -  A value of NULL will also be returned for the "dcb2" argument if
*     the routine fails for any reason.

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

/* Set an initial value of NULL for the dcb2 argument. */
   *dcb2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain a new slot in the DCB, resetting the dcb2 argument to NULL if
   none could be obtained. */
   *dcb2 = (AryDCB *) ary1Ffs( ARY__DCBTYPE, status );
   if( *status != SAI__OK ){
      *dcb2 = NULL;
   } else {

/* Ensure the data object arrays are available. */
      ary1Dobj( dcb1, status );

/* Ensure that form information is available in the DCB. */
      ary1Dfrm( dcb1, status );

/* Handle each form of array in turn. */
      if( *status == SAI__OK ){

/* Primitive arrays.
   ================ */
         if( !strcmp( dcb1->form, "PRIMITIVE" ) ){

/* Ensure that data type and bounds information are available in the DCB. */
            ary1Dtyp( dcb1, status );
            ary1Dbnd( dcb1, status );

/* Obtain a locator to the placeholder object's parent structure. */
            datParen( *loc, &locp, status );

/* Obtain the placeholder object's name. Then annul the object's locator
   and erase the object. */
            datName( *loc, name, status );
            datAnnul( loc, status );
            datErase( locp, name, status );

/* Copy the input array into its place and obtain a locator to the new
   object. */
            datCopy( dcb1->dloc, locp, name, status );
            datFind( locp, name, loc, status );

/* Annul the parent structure locator. */
            datAnnul( &locp, status );

/* Clone a locator to the new array for storage in the DCB. Link this
   locator into a private group to prevent external events annulling it. */
            (*dcb2)->loc = NULL;
            datClone( *loc, &(*dcb2)->loc, status );
            hdsLink( (*dcb2)->loc, "ARY_DCB", status );

/* Obtain the data object file and path names and store these in the DCB. */
            hdsTrace( (*dcb2)->loc, &nlev, (*dcb2)->path, (*dcb2)->file,
                      status, sizeof((*dcb2)->path), sizeof((*dcb2)->file) );

/* Obtain a non-imaginary data component locator by cloning the data object
   locator and set a null imaginary component locator. */
            (*dcb2)->dloc = NULL;
            datClone( (*dcb2)->loc, &(*dcb2)->dloc, status );
            (*dcb2)->iloc = NULL;

/* There is no scaling information */
            if( (*dcb2)->scloc ) datAnnul( &(*dcb2)->scloc, status );
            (*dcb2)->kscl = 0;

/* Simple, scaled and delta arrays.
   =============================== */
         } else if( !strcmp( dcb1->form, "SIMPLE" ) ||
                    !strcmp( dcb1->form, "SCALED" ) ||
                    !strcmp( dcb1->form, "DELTA" ) ){

/* Ensure that data type, bounds and bad pixel flag information are
   available in the DCB. */
            ary1Dtyp( dcb1, status );
            ary1Dbnd( dcb1, status );
            ary1Dbad( dcb1, status );

/* Clone a locator to the new data object for storage in the DCB. Link this
   locator into a private group to prevent external events annulling it. */
            (*dcb2)->loc = NULL;
            datClone( *loc, &(*dcb2)->loc, status );
            hdsLink( (*dcb2)->loc, "ARY_DCB", status );

/* Obtain the data object file and path names and store these in the DCB. */
            hdsTrace( (*dcb2)->loc, &nlev, (*dcb2)->path, (*dcb2)->file,
                      status, sizeof((*dcb2)->path), sizeof((*dcb2)->file) );

/* Copy the non-imaginary data component to the new array and obtain a
   locator to it. */
            (*dcb2)->dloc = NULL;
            datCopy( dcb1->dloc, (*dcb2)->loc, "DATA", status );
            datFind( (*dcb2)->loc, "DATA", &(*dcb2)->dloc, status );

/* If the input array is complex, then copy the imaginary component also. */
            if( dcb1->complex ){
               (*dcb2)->iloc = NULL;
               datCopy( dcb1->iloc, (*dcb2)->loc, "IMAGINARY_DATA", status );
               datFind( (*dcb2)->loc, "IMAGINARY_DATA", &(*dcb2)->iloc, status );
            }

/* Copy the BAD_PIXEL and ORIGIN components if they exist. */
            ary1Cpync( dcb1->loc, "BAD_PIXEL", (*dcb2)->loc, status );
            ary1Cpync( dcb1->loc, "ORIGIN", (*dcb2)->loc, status );

/* Transfer the other components of a SCALED array. */
            if( !strcmp( dcb1->form, "SCALED" ) ){
               ary1Cpscl( dcb1, *dcb2, status );

/* Transfer the other components of a DELTA array. */
            } else if( !strcmp( dcb1->form, "DELTA" ) ){
               ary1Cpdlt( dcb1, *dcb2, status );
            }

/* If the form information in the DCB was not recognised, then report an
   error. */
         } else {
            *status = ARY__FATIN;
            msgSetc( "F", dcb1->form );
            errRep( " ", "Unsupported array form '^F' found in Data Control"
                    "Block (internal programming error).", status );
         }
      }

/* If there was an error, then annul any locators which may have been
   acquired, release the new DCB slot and reset the dcb2 argument to
   NULL. */
      if( *status != SAI__OK ){
         if( dcb2 ){
            datAnnul( &(*dcb2)->dloc, status );
            datAnnul( &(*dcb2)->iloc, status );
            datAnnul( &(*dcb2)->loc, status );
            *dcb2 = ary1Rls( (AryObject *) *dcb2, status );
         }

/* Set the reference and mapping counts for the new data object to zero. */
      } else {
         (*dcb2)->refcount = 0;
         (*dcb2)->nread = 0;
         (*dcb2)->nwrite = 0;

/* Set the form, access mode and state information for the new data object. */
         strcpy( (*dcb2)->form, dcb1->form );
         (*dcb2)->kform = dcb1->kform;
         strcpy( (*dcb2)->mode, "UPDATE" );
         (*dcb2)->kmode = 1;
         (*dcb2)->state = dcb1->state;
         (*dcb2)->init = dcb1->init;
         (*dcb2)->kstate = dcb1->kstate;

/* Set the disposal mode. */
         if( temp ){
            strcpy( (*dcb2)->dispose, "TEMP" );
         } else {
            strcpy( (*dcb2)->dispose, "KEEP" );
         }

/* Copy the bad pixel flag and data type information. */
         (*dcb2)->bad = dcb1->bad;
         (*dcb2)->kbad = dcb1->kbad;
         strcpy( (*dcb2)->type, dcb1->type );
         (*dcb2)->complex = dcb1->complex;
         (*dcb2)->ktype = dcb1->ktype;

/* Copy the array bounds information. */
         (*dcb2)->ndim = dcb1->ndim;
         for( i = 0; i < (*dcb2)->ndim; i++ ){
            (*dcb2)->lbnd[ i ] = dcb1->lbnd[ i ];
            (*dcb2)->ubnd[ i ] = dcb1->ubnd[ i ];
         }
         for( ; i < ARY__MXDIM; i++ ){
            (*dcb2)->lbnd[ i ] = 1;
            (*dcb2)->ubnd[ i ] = 1;
         }
         (*dcb2)->kbnd = dcb1->kbnd;

/* Copy the accumulated pixel shift information. */
         for( i = 0; i < ARY__MXDIM; i++ ){
            (*dcb2)->shift[ i ] = dcb1->shift[ i ];
         }
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dcpy", status );

}
