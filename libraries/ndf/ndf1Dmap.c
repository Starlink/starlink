#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include "mers.h"
#include <string.h>
#include "star/util.h"

void ndf1Dmap( NdfACB *acb, const char *type, int cmplx, const char *mmod,
               int mask, void **dpntr, void **ipntr, int *status ){
/*
*+
*  Name:
*     ndf1Dmap

*  Purpose:
*     Map the data array component of an NDF.

*  Synopsis:
*     void ndf1Dmap( NdfACB *acb, const char *type, int cmplx,
*                    const char *mmod, int mask, void **dpntr,
*                    void **ipntr, int *status )

*  Description:
*     This function obtains mapped access to the data array component of an
*     NDF identified by its ACB entry.

*  Parameters:
*     acb
*        Pointer to the NDF entry in the ACB.
*     type
*        Pointer to a null terminated string holding the numeric data type
*        to be used to access the data; an HDS primitive numeric data type
*        string (case insensitive).
*     cmplx
*        Whether access to complex data is required.
*     mmod
*        Pointer to a null terminated string holding the mapping mode to be
*        used to access the data (case insensitive).
*     mask
*        This parameter specifies whether the mapped data values may later
*        be masked using quality information. If so, then this function
*        will ensure tht a writeable buffer is used to return the mapped
*        values; this may require that a copy of the mapped values be made.
*        If "mask" is zero, then this function may return a read-only copy
*        of the mapped values (i.e. as obtained from HDS).
*     *dpntr
*        Returned holding the pointer to the mapped non-imaginary data
*        component.
*     *ipntr
*        Returned holding the pointer to the mapped imaginary data
*        component (not used if "cmplx" is zero).
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
   AryPlace *place;      /* ARY_ system temporary placeholder */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char ctype[ NDF__SZFTP + 1 ];   /* Complex data type string */
   char inopt[ NDF__SZIOP + 1 ];   /* Initialisation option */
   char mode[ NDF__SZMOD + 1 ];    /* Mapping access mode */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF lower pixel index bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF upper pixel index bounds */
   int ndim;             /* Number of NDF dimensions */
   size_t el;            /* Number of data elements mapped */
   void *dpt;            /* Temporary non-imaginary value pointer */
   void *ipt;            /* Temporary imaginary value pointer */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Validate the mapping mode, decomposing it into an access mode and an
   initialisation option. */
   ndf1Vmmd( mmod, mode, sizeof( mode ), inopt, sizeof( inopt ), status );

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* Check that the data array is not already mapped through the
   specified ACB entry and report an error if it is. */
   if( acb->dmap ) {
      *status = NDF__ISMAP;
      ndf1Amsg( "NDF", acb );
      errRep( " ", "The data component in the NDF structure ^NDF is "
              "already mapped for access through the specified identifier "
              "(possible programming error).", status );

/* Set an initial null ARY_ system identifier for the temporary mapped
   array. */
   } else {
      acb->dmtid = NULL;

/* Map the data array component using the identifier in the ACB. */
      if( cmplx ) {
         aryMapz( acb->did, type, mmod, dpntr, ipntr, &el, status );
      } else {
         aryMap( acb->did, type, mmod, dpntr, &el, status );
      }

/* Obtain the bad pixel flag for the mapped array values and store it in
   the ACB. */
      aryBad( acb->did, 0, &acb->dmbad, status );

/* If a modifiable copy of the mapped values is required, but they may
   be held in a read-only buffer, then a copy must be made in a
   temporary array. */
      if( mask && ( !strcmp( mode, "READ" ) ) ) {

/* Obtain the size of the array from the NDF's data array identifier in
   the ACB. */
         aryBound( acb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Obtain a placeholder for the temporary array. */
         aryTemp( &place, status );

/* Create the array, storing the temporary ARY_ system identifier in
   the ACB. Then map the new array as required and move the original
   mapped values into it. Unmap the original values and save the
   pointer(s) to the new copy. */
         dpt = NULL;
         ipt = NULL;

/* ...Complex data. */
         if( cmplx ) {
            sprintf( ctype, "COMPLEX%.*s", (int) astChrLen(type), type );
            aryNew( ctype, ndim, lbnd, ubnd, &place, &acb->dmtid, status );
            aryMapz( acb->dmtid, type, "WRITE", &dpt, &ipt, &el, status );
            ndf1Move( type, el, *dpntr, dpt, status );
            ndf1Move( type, el, *ipntr, ipt, status );
            aryUnmap( acb->did, status );
            *dpntr = dpt;
            *ipntr = ipt;

/* ...Non-complex data. */
         } else {
            aryNew( type, ndim, lbnd, ubnd, &place, &acb->dmtid, status );
            aryMap( acb->dmtid, type, "WRITE", &dpt, &el, status );
            ndf1Move( type, el, *dpntr, dpt, status );
            aryUnmap( acb->did, status );
            *dpntr = dpt;
         }
      }

/* If there was no error, then note that the data array is mapped and
   update the DCB mapping counts. */
      if( *status == SAI__OK ) {
         acb->dmap = 1;
         dcb->ndmap++;
         dcb->nmap++;

/* Store the mapping type (and complex value flag) and note that the
   bad pixel flag for the mapped values has not been modified. */
         star_strlcpy( acb->dmtyp, type, sizeof( acb->dmtyp ) );
         acb->dmcpx = cmplx;
         acb->dmbmd = 0;

/* Store pointers to the mapped values. */
         acb->dmdpt = *dpntr;
         if( cmplx ) acb->dmipt = *ipntr;
      }
   }

/* If no error has occurred, we use ndf1Event to flag a "data array
   mapped" event. If the caller has registered a handler for this type of
   event (using ndfHndlr), it will be called. */
   if( *status == SAI__OK ) {

/* Assign the name of the data file to the MSG token "NDF_EVENT" */
      ndf1Evmsg( "NDF_EVENT", acb->dcb );

/* Raise an appropriate NDF event. */
      if( !strcmp( mode, "READ" ) ) {
         ndf1Event( "READ_DATA", status );

      } else if( !strcmp( mode, "WRITE" ) ) {
         ndf1Event( "WRITE_DATA", status );

      } else if( !strcmp( mode, "UPDATE" ) ) {
         ndf1Event( "UPDATE_DATA", status );
      }

   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dmap", status );

}

