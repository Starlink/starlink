#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include "mers.h"
#include <string.h>
#include "star/util.h"

void ndf1Qmap( NdfACB *acb, const char *type, const char *mmod,
               void **pntr, int *status ){
/*
*+
*  Name:
*     ndf1Qmap

*  Purpose:
*     Map the quality component of an NDF for access.

*  Synopsis:
*     void ndf1Qmap( NdfACB *acb, const char *type, const char *mmod,
*                    void **pntr, int *status )

*  Description:
*     This function maps the quality component of an NDF for access and
*     returns a pointer to the mapped values.  The NDF is identified by its
*     ACB entry. The mapped ACB entry may subsequently be unmapped by the
*     ndf1Qump function and cannot be re-mapped until this has been done.

*  Parameters:
*     acb
*        Pointer to the NDF's ACB entry.
*     type
*        Pointer to a null terminated string holding the numeric data type
*        required for access to the quality values (case insensitive).
*     mmod
*        Pointer to a null terminated string holding the mapping mode for
*        access (case insensitive).
*     *pntr
*        Returned holding the pointer to the mapped quality values.
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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   AryPlace *place;      /* ARY_ system temporary placeholder */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char inopt[ NDF__SZIOP + 1 ];   /* Initialisation option */
   char mode[ NDF__SZMOD + 1 ];    /* Mapping access mode */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF lower pixel index bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF upper pixel index bounds */
   int ndim;             /* Number of NDF dimensions */
   int there;            /* Whether quality array exists */
   size_t el;            /* Number of elements mapped */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* If the quality component is already mapped through this ACB entry,
   then report an error. */
   if( acb->qmap ) {
      *status = NDF__ISMAP;
      ndf1Amsg( "NDF", acb );
      errRep( " ", "The quality component in the NDF structure ^NDF is "
              "already mapped for access through the specified identifier "
              "(possible programming error).", status );

/* Validate the mapping mode, decomposing it into an access mode and an
   initialisation option. */
   } else {
      ndf1Vmmd( mmod, mode, sizeof( mode ), inopt, sizeof( inopt ), status );

/* Ensure that quality information is available in the DCB and ACB. */
      ndf1Qimp( acb, status );

/* See if the ARY_ system identifier for the quality array is valid. If
   not, then the array does not exist. */
      there = aryValid( acb->qid, status );
      if( *status == SAI__OK ) {

/* Set an initial null value for the temporary mapped quality array
   identifier. */
         acb->qmtid = NULL;

/* If the quality array exists, then map it. */
         if( there ) {
            aryMap( acb->qid, type, mmod, (void **) &acb->qmptr, &el, status );

/* If the quality array does not exist, then see if the access mode and
   initialisation option require it to be created. */
         } else if( ( !strcmp( mode, "WRITE" ) ) || ( ( !strcmp( mode, "UPDATE" ) )
                                                  && ( astChrLen( inopt ) > 0 ) ) ) {

/* If so, then create it (thereby importing identifiers for it into the
   ACB). */
            ndf1Qcre( acb, status );

/* Map the array. */
            aryMap( acb->qid, type, mmod, (void **) &acb->qmptr, &el, status );

/* If the quality array does not exist and READ access with an
   initialisation option was requested, then a temporary array must be
   created.  Determine its bounds from the NDF's data array identifier
   in the ACB. */
         } else if( ( !strcmp( mode, "READ" ) ) && ( astChrLen( inopt ) > 0 ) ) {
            aryBound( acb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Obtain a placeholder for a temporary array. */
            aryTemp( &place, status );

/* Create the array, storing the temporary ARY_ system identifier in the
   ACB. Then map it as required. */
            aryNew( type, ndim, lbnd, ubnd, &place, &acb->qmtid, status );
            aryMap( acb->qmtid, type, mmod, (void **) &acb->qmptr, &el, status );

/* If the array does not exist and the access mode is not WRITE and no
   initialisation option was specified, then report an error. */
         } else {
            *status = NDF__QUDEF;
            ndf1Amsg( "NDF", acb );
            errRep( " ", "The quality component in the NDF structure ^NDF "
                    "is in an undefined state (possible programming "
                    "error).", status );
         }
      }
   }

/* If there were no errors, then note that the ACB entry is mapped and
   increment the DCB counts of mappings to this quality array and of
   total mappings to this NDF. */
   if( *status == SAI__OK ) {
      acb->qmap = 1;
      dcb->nqmap++;
      dcb->nmap++;

/* Store the mapping type and mode in the ACB. */
      star_strlcpy( acb->qmtyp, type, sizeof( acb->qmtyp ) );
      star_strlcpy( acb->qmmod, mode, sizeof( acb->qmmod ) );

/* Return a pointer to the mapped values. */
      *pntr = acb->qmptr;
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Qmap", status );

}

