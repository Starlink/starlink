#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "mers.h"
#include "prm_par.h"
#include "ary_err.h"
#include <string.h>

void ary1Cpy( AryACB *acb1, int temp, HDSLoc **loc, int expand,
              AryACB **acb2, int *status ) {
/*
*+
*  Name:
*     ary1Cpy

*  Purpose:
*     Make a copy of an ACB array entry in a new (or temporary) HDS
*     location.

*  Synopsis:
*     void ary1Cpy( AryACB *acb1, int temp, HDSLoc **loc, int expand,
*                   AryACB **acb2, int *status )

*  Description:
*     This function creates a new base array containing a copy of an
*     existing array (identified by its ACB entry) and creates a new
*     ACB entry to describe it. The HDS location for the new array is
*     passed by means of a locator to an array placeholder object. This
*     locator may later be annulled.

*  Parameters:
*     acb1
*        ACB entry of the array to be copied.
*     temp
*        Whether the copy is to be a temporary array (this is used to
*        set its disposal mode in the DCB).
*     loc
*        Locator to array placeholder object (a scalar data structure
*        of type ARRAY). If a simple array is created, the supplied
*        object becomes the new array object, and the supplied locator
*        is left unchanged. If a primitive array is created, the supplied
*        object is annulled and a locator for a new object is returned.
*     expand
*        Determines whether base arrays stored in DELTA or SCALED form
*        should be expanded. Note, this only controls what happens with
*        base arrays - if the input ACB entry represents a section of an
*        array the output array will always contain expanded values.
*     acb2
*        Returned holding the ACB of the copy.
*     status
*        The global status.

*  Notes:
*     -  A value of NULL will be returned for the "acb2" argument if the
*     routine is called with "status" set, although no further processing
*     will occur.
*     -  A value of NULL will also be returned for the "acb2" argument if
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
*     22-FEB-2018 (DSB):
*        The "loc" argument is now given and returned (an "HDSLoc **"
*        instead of an "HDSLoc *" ) since the supplied locator will be
*        annulled and a new one returned if a primitive array is created
*        (see ary1Dcrep). This looks like a bug that has been in the Fortran
*        code comments (ary1_cpy.f) for a long time.

*-
*/

/* Local variables: */
   char newtype[DAT__SZTYP+1];/* New data type */
   AryACB *acbc;              /* Index to cloned ACB entry */
   AryDCB *dcb1;              /* Index to input object entry in DCB */
   AryDCB *dcb2 = NULL;       /* Index to output object entry in DCB */
   int bad;                   /* Bad pixel flag */
   int icomp;                 /* Loop counter for array components */
   int ncomp;                 /* Number of array components to copy */
   int pbnd;                  /* Whether bounds are primitive */
   size_t el;                 /* Number of data elements per component */
   size_t size;               /* Number of bytes to copy */
   void *pntr1[2];            /* Pointer to mapped input data */
   void *pntr2[2];            /* Pointer to mapped output data */

/* Set an initial value for the "acb2" argument. */
   *acb2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

   ARY__DCB_LOCK_MUTEX;

/* Get the DCB for the data object to be copied. */
   dcb1 = acb1->dcb;

/* Ensure that form information is available for it in the DCB. */
   ary1Dfrm( dcb1, status );

/* If the input array is a base array, but is not a scaled or delta array,
   then check to see that there is no conflicting mapped access for write
   or update in effect. Report an error if there is. We exclude scaled and
   delta arrays since these need to be converted to simple form, in the
   same way that an array section is copied. */
   if( !acb1->cut && ( ( !strcmp( dcb1->form, "SCALED" ) &&
                         !strcmp( dcb1->form, "DELTA" ) ) ||
                       (!expand) ) ){

      if( dcb1->nwrite != 0 ){
         *status = ARY__ISMAP;
         datMsg( "ARRAY", dcb1->loc );
         errRep( " ", "The array structure ^ARRAY is already mapped for UPDATE"
                 " or WRITE access, perhaps through another identifier "
                 "(possible programming error).", status );

/* Make a direct copy of the data object and then create a new base array
   entry in the ACB to describe it. */
      } else {
         ary1Dcpy( dcb1, temp, loc, &dcb2, status );
         ary1Crnba( dcb2, acb2, status );
      }

/* Handle each form of array in turn... */
   } else {
      if( *status == SAI__OK ){

/* Primitive arrays.
   ================ */
         if( !strcmp( dcb1->form, "PRIMITIVE" ) ){

/* See if the array bounds are consistent with the creation of a new
   primitive array. */
            ary1Pbnd( acb1, &pbnd, status );

/* Ensure that data type information is available in the DCB. */
            ary1Dtyp( dcb1, status );
            if( *status == SAI__OK ){

/* Create a new data object (with an entry in the DCB) with the correct
   type and bounds to accommodate the copied data. Create a primitive
   array if possible. Otherwise, create a simple array. */
               if( pbnd ){
                  ary1Dcrep( 0, dcb1->type, acb1->ndim, acb1->ubnd, temp,
                             loc, &dcb2, status );
               } else {
                  ary1Dcre( 0, dcb1->type, 0, acb1->ndim, acb1->lbnd,
                            acb1->ubnd, temp, *loc, &dcb2, status );
               }
            }

/* Create a new base array entry in the ACB to describe it. */
            ary1Crnba( dcb2, acb2, status );

/* Ensure that state information for the input array is available in the
   DCB. */
            ary1Dsta( dcb1, status );
            if( *status == SAI__OK ){

/* If the input array is in the "defined" state, then its data values must
   be copied to the output array. Produce a cloned copy of its ACB entry,
   to allow it to be mapped even if the existing ACB entry is already
   mapped for access. */
               if( dcb1->state ){
                  ary1Cln( acb1, &acbc, status );
                  if( *status == SAI__OK ){

/* Map the input array for reading through the cloned ACB entry and the
   output array for writing. */
                     ary1Maps( acbc, dcb1->type, 0, "READ", NULL,
                               pntr1, pntr1+ 1, status );
                     ary1Maps( *acb2, dcb1->type, 0, "WRITE", NULL,
                               pntr2, pntr2 + 1, status );

/* Find the number of elements to be copied. */
                     ary1Nel( acb1->ndim, acb1->lbnd,
                              acb1->ubnd, &el, status );

/* Get the number of bytes to copy and then copy data from the input array to
   the output. */

/* ...byte data. */
                     if( !strcmp( dcb1->type, "_BYTE" ) ){
                        size = el*VAL__NBB;

/* ...unsigned byte data. */
                     } else if( !strcmp( dcb1->type, "_UBYTE" ) ){
                        size = el*VAL__NBUB;

/* ...double precision data. */
                     } else if( !strcmp( dcb1->type, "_DOUBLE" ) ){
                        size = el*VAL__NBD;

/* ...integer data. */
                     } else if( !strcmp( dcb1->type, "_INTEGER" ) ){
                        size = el*VAL__NBI;

/* ...real data. */
                     } else if( !strcmp( dcb1->type, "_REAL" ) ){
                        size = el*VAL__NBR;

/* ...word data. */
                     } else if( !strcmp( dcb1->type, "_WORD" ) ){
                        size = el*VAL__NBW;

/* ...unsigned word data. */
                     } else if( !strcmp( dcb1->type, "_UWORD" ) ){
                        size = el*VAL__NBUW;

/* ...64-bit integer data. */
                     } else if( !strcmp( dcb1->type, "_INT64" ) ){
                        size = el*VAL__NBK;

/* Report an error if the type is notr recognised. */
                     } else if( *status == SAI__OK ) {
                        *status = ARY__FATIN;
                        msgSetc( "T", dcb1->type );
                        errRep( " ", "Invalid array type '^T' found in "
                                "Data Control Block (internal programming "
                                "error).", status );
                     }

/* Copy the data. */
                     if( *status == SAI__OK ) {
                        memcpy( pntr2[ 0 ], pntr1[ 0 ], size );
                     }

/* Unmap the (cloned) input and output arrays. */
                     ARY__ACB_LOCK_MUTEX;
                     ary1Ump( acbc, status );
                     ary1Ump( *acb2, status );
                     ARY__ACB_UNLOCK_MUTEX;
                  }

/* Annul the cloned ACB entry. */
                  ARY__ACB_LOCK_MUTEX;
                  ary1Anl( acbc, status );
                  ARY__ACB_UNLOCK_MUTEX;
               }
            }

/* Simple, scaled and delta arrays.
   ================================ */
         } else if( !strcmp( dcb1->form, "SIMPLE" ) ||
                    !strcmp( dcb1->form, "SCALED" ) ||
                    !strcmp( dcb1->form, "DELTA" ) ){

/* Ensure that data type information is available in the DCB. */
            ary1Dtyp( dcb1, status );

/* Get the required data type for the new array. */
            ary1Extyp( dcb1, newtype, status );

/* Create a new simple data object (with an entry in the DCB) with the
   correct type and bounds to accommodate the copied data. */
            ary1Dcre( 0, newtype, dcb1->complex, acb1->ndim,
                      acb1->lbnd, acb1->ubnd, temp,
                      *loc, &dcb2, status );

/* Create a new base array entry in the ACB to describe it. */
            ary1Crnba( dcb2, acb2, status );

/* Ensure that state information for the input array is available in the
   DCB. */
            ary1Dsta( dcb1, status );
            if( *status == SAI__OK ){

/* If the input array is in the "defined" state, then its data values must
   be copied to the output array. Produce a cloned copy of its ACB entry,
   to allow it to be mapped even if the existing ACB entry is already
   mapped for access. */
               if( dcb1->state ){
                  ary1Cln( acb1, &acbc, status );
                  if( *status == SAI__OK ){

/* Map the input array for reading through the cloned ACB entry and the
   output array for writing. This uncompresses the input array if the
   input array is stored in DELTA format. */
                     ary1Maps( acbc, newtype, dcb1->complex, "READ", NULL,
                               pntr1, pntr1 + 1, status );
                     ary1Maps( *acb2, newtype, dcb1->complex, "WRITE", NULL,
                               pntr2, pntr2 + 1, status );

/* Find the number of data components to be copied and the number of
   elements in each component. */
                     ncomp = 1;
                     if( dcb1->complex ) ncomp = 2;
                     ary1Nel( acb1->ndim, acb1->lbnd, acb1->ubnd, &el, status );

/* Loop to copy each component. */
                     for( icomp = 0; icomp < ncomp; icomp++ ){

/* Get the number of bytes to copy and then copy data from the input array to
   the output. */

/* ...byte data. */
                        if( !strcmp( newtype, "_BYTE" ) ){
                           size = el*VAL__NBB;

/* ...unsigned byte data. */
                        } else if( !strcmp( newtype, "_UBYTE" ) ){
                           size = el*VAL__NBUB;

/* ...double precision data. */
                        } else if( !strcmp( newtype, "_DOUBLE" ) ){
                           size = el*VAL__NBD;

/* ...integer data. */
                        } else if( !strcmp( newtype, "_INTEGER" ) ){
                           size = el*VAL__NBI;

/* ...real data. */
                        } else if( !strcmp( newtype, "_REAL" ) ){
                           size = el*VAL__NBR;

/* ...word data. */
                        } else if( !strcmp( newtype, "_WORD" ) ){
                           size = el*VAL__NBW;

/* ...unsigned word data. */
                        } else if( !strcmp( newtype, "_UWORD" ) ){
                           size = el*VAL__NBUW;

/* ...64-bit integer data. */
                        } else if( !strcmp( newtype, "_INT64" ) ){
                           size = el*VAL__NBK;

/* Report an error if the type is notr recognised. */
                        } else if( *status == SAI__OK ) {
                           *status = ARY__FATIN;
                           msgSetc( "T", newtype );
                           errRep( " ", "Invalid array type '^T' returned "
                                   "by ary1Extyp (internal programming "
                                   "error).", status );
                        }

/* Copy the data. */
                        if( *status == SAI__OK ) {
                           memcpy( pntr2[ icomp ], pntr1[ icomp ], size );
                        }
                     }

/* Unmap the (cloned) input and output arrays. */
                     ARY__ACB_LOCK_MUTEX;
                     ary1Ump( acbc, status );
                     ary1Ump( *acb2, status );
                     ARY__ACB_UNLOCK_MUTEX;

/* Transfer the bad pixel flag to the new array. */
                     ary1Bad( acbc, 0, &bad, status );
                     ARY__ACB_LOCK_MUTEX;
                     ary1Sbd( bad, *acb2, status );
                     ARY__ACB_UNLOCK_MUTEX;
                  }

/* Annul the cloned ACB entry. */
                  ARY__ACB_LOCK_MUTEX;
                  ary1Anl( acbc, status );
                  ARY__ACB_UNLOCK_MUTEX;
               }
            }

/* If the DCB form entry was not recognised, then report an error. */
         } else {
            *status = ARY__FATIN;
            msgSetc( "BADFORM", dcb1->form );
            errRep( " ", "Invalid array form '^BADFORM' found in Data Control "
                    "Block (internal programming error).", status );
         }
      }
   }

/* If an error occurred, then annul the new ACB entry and reset the acb2
   argument to NULL. */
   if( ( *status != SAI__OK ) && ( *acb2 ) ){
      ARY__ACB_LOCK_MUTEX;
      ary1Anl( *acb2, status );
      ARY__ACB_UNLOCK_MUTEX;

      *acb2 = NULL;
   }

   ARY__DCB_UNLOCK_MUTEX;

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Cpy", status );

}
