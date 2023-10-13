#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "star/util.h"

void ndf1Dcrep( const char *ftype, int ndim, const hdsdim ubnd[],
                NdfPCB *pcb, NdfACB **acb, int *status ){
/*
*+
*  Name:
*     ndf1Dcrep

*  Purpose:
*     Create a primitive NDF.

*  Synopsis:
*     void ndf1Dcrep( const char *ftype, int ndim, const hdsdim ubnd[],
*                     NdfPCB *pcb, NdfACB **acb, int *status )

*  Description:
*     This function creates a primitive NDF data object containing just a
*     data array component and returns an ACB index which refers to the
*     resulting new base NDF. The location of the new object is identified
*     by means of an index to a placeholder entry in the PCB. This
*     placeholder should later be annulled.

*  Parameters:
*     ftype
*        Pointer to a null terminated string holding the full data type of
*        the NDF's data array.
*     ndim
*        Number of NDF dimensions.
*     ubnd
*        Upper bounds of the NDF. The supplied "ubnd" array should have at
*        least "ndim" elements.
*     pcb
*        Pointer to a placeholder entry in the PCB which specifies the
*        location (and certain properties) of the new NDF.
*     *acb
*        Pointer to the ACB entry which refers to the new base NDF.
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
   AryPlace *place;      /* ARY_ placeholder for data array */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */

/* Set an initial value for the "acb" parameter. */
   *acb = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain a free slot in the DCB. */
   NDF__DCB_LOCK_MUTEX;
   dcb = ndf1Ffs( NDF__DCBTYPE, status );
   NDF__DCB_UNLOCK_MUTEX;
   if( *status == SAI__OK ) {

/* Take the default for any wild-carded foreign format information in
   the PCB entry and then initialise the new DCB entry with information
   derived from the placeholder. */
      ndf1Prfor( NULL, pcb, status );
      ndf1Pldcb( pcb, dcb, status );

/* Use "hdsTune" to set the optimum number of components in the HDS
   structure. */
      hdsTune( "NCOMP", 10, status );

/* Create the data array component and store an ARY_ system identifier
   for it in the DCB. */
      aryPlace( dcb->loc, "DATA_ARRAY", &place, status );
      aryNewp( ftype, ndim, ubnd, &place, &dcb->did, status );

/* Derive the data array component attributes which are needed as
   default values for other components. */
      aryType( dcb->did, dcb->detyp, status );
      dcb->decpx = 0;
      star_strlcpy( dcb->defrm, "PRIMITIVE", sizeof( dcb->defrm ) );

/* Note whether data array information is available. */
      dcb->kd = ( *status == SAI__OK );

/* Create a new base NDF entry in the ACB to describe the new object. */
      ndf1Crnbn( dcb, acb, status );

/* Assign the name of the data file to the MSG token "NDF_EVENT" */
      ndf1Evmsg( "NDF_EVENT", dcb );

/* Raise an NDF event, describing the opening of a new NDF. */
      ndf1Event( "OPEN_NEW_NDF", status );

/* If there was an error, then clean up by annulling the identifiers
   and locators which may have been acquired. */
      if( *status != SAI__OK ) {
         aryAnnul( &dcb->did, status );
         datAnnul( &dcb->loc, status );

/* Release the allocated DCB slot. */
         NDF__DCB_LOCK_MUTEX;
         dcb = ndf1Rls( ( NdfObject * ) dcb, status );
         NDF__DCB_UNLOCK_MUTEX;
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dcrep", status );

}

