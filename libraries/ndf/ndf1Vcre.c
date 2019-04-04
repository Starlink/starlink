#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"

void ndf1Vcre( NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Vcre

*  Purpose:
*     Create a variance component for an NDF, if necessary.

*  Synopsis:
*     void ndf1Vcre( NdfACB *acb, int *status )

*  Description:
*     This function ensures that a variance array exists for an NDF,
*     creating one if necessary. The array is created using the default
*     variance array attributes stored in the DCB. ARY_ system identifiers
*     for the new array (and appropriate sections thereof) are entered into
*     the DCB and also into those ACB entries which refer to the NDF data
*     object in question. The NDF is identified to this function by its ACB
*     entry.

*  Parameters:
*     acb
*        Pointer to the ACB entry for the NDF.
*     *status
*        The global status.

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
   AryPlace *place;      /* Placeholder for variance array */
   NdfACB *acbt;         /* ACB to test */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   NdfDCB *dcbt;         /* DCB to test */
   char *type;           /* Full type string */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF lower pixel index bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF upper pixel index bounds */
   int islot;            /* Slot index */
   int nc;               /* Length of string */
   int ndim;             /* Number of NDF dimensions */
   int next;             /* Next ACB to test */
   int there;            /* Whether the variance array exists */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that variance information is available in the DCB and ACB. */
   ndf1Vimp( acb, status );

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* See if the ARY_ system identifier for the variance array is valid. If
   not, then the array does not exist and must be created. */
   there = aryValid( dcb->vid, status );
   if( *status == SAI__OK ) {
      if( !there ) {

/* Obtain the NDF bounds from its data array. */
         aryBound( dcb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Obtain a placeholder for the variance array, then handle the
   creation of each form of array in turn using the default attributes
   stored in the DCB. */
         aryPlace( dcb->loc, "VARIANCE", &place, status );

/* Primitive array.
   =============== */
         if( !strcmp( dcb->vfrm, "PRIMITIVE" ) ) {
            aryNewp( dcb->vtyp, ndim, ubnd, &place, &dcb->vid, status );

/* Simple array.
   ============ */
         } else if( !strcmp( dcb->vfrm, "SIMPLE" ) ) {
            if( dcb->vcpx ) {
               type = astAppendStringf( NULL, &nc, "COMPLEX_%s", dcb->vtyp );
               aryNew( type, ndim, lbnd, ubnd, &place, &dcb->vid, status );
               type = astFree( type );
            } else {
               aryNew( dcb->vtyp, ndim, lbnd, ubnd, &place, &dcb->vid, status );
            }

/* If the default variance storage form entry in the DCB was not
   recognised, then report an error. */
         } else {
            *status = NDF__FATIN;
            msgSetc( "BADFORM", dcb->vfrm );
            errRep( " ", "Invalid array storage form '^BADFORM' "
                    "encountered in the NDF_ system Data Control Block "
                    "(internal programming error).", status );
         }

/* Loop to identify all the ACB entries which refer to this DCB entry. */
         next = 0;
         islot = -1;
         NDF__ACB_LOCK_MUTEX;
         acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
         while( ( *status == SAI__OK ) && ( next != -1 ) ){
            islot = next;

/* Select those entries with the correct DCB index. */
            dcbt = acbt->dcb;
            if( dcbt == dcb ) {

/* Create a section from the variance array which matches the ACB's data
   array section and store the resulting ARY_ system identifier in the
   ACB. */
               ndf1Ssdup( dcb->vid, acbt->did, &acbt->vid, status );
            }
            acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
         }
         NDF__ACB_UNLOCK_MUTEX;
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vcre", status );

}

