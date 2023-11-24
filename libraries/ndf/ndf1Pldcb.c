#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "star/util.h"

void ndf1Pldcb( NdfPCB *pcb, NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Pldcb

*  Purpose:
*     Initialise a DCB entry from a placeholder.

*  Synopsis:
*     void ndf1Pldcb( NdfPCB *pcb, NdfDCB *dcb, int *status )

*  Description:
*     This function copies information from a placeholder entry in the PCB
*     into a newly-created DCB entry in order to initialise those
*     properties which a new data object inherits from its parent
*     placeholder. These include the data object itself, its attributes,
*     and information about associated foreign data files and the object's
*     disposal mode (e.g. when creating temporary objects).

*  Parameters:
*     pcb
*        Pointer to the placeholder entry in the DCB.
*     dcb
*        Pointer to the DCB entry to be initialised.
*     *status
*        The global status.

*  Prior Requirements:
*     -  The DCB mutex must be locked.

*  Notes:
*     The PCB entry is not modified by this function.

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
   int nlev;             /* HDS object nesting level */
   int prmry;            /* Primary/secondary locator flag */

   NDF__DCB_ASSERT_MUTEX;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Clone a locator to the PCB data object for storage in the DCB.
   Promote it to be a primary locator and link it into a private group
   to prevent external events annulling it. */
   datClone( pcb->loc, &dcb->loc, status );
   prmry = 1;
   datPrmry( 1, &dcb->loc, &prmry, status );
   hdsLink( dcb->loc, "NDF_DCB", status );

/* Obtain the data object file and path names and enter them into the
   DCB. */
   hdsTrace( dcb->loc, &nlev, dcb->path, dcb->file, status,
             sizeof( dcb->path ), sizeof( dcb->file ) );

/* Copy the PCB foreign file format code to the DCB. */
   dcb->fcb = pcb->fcb;

/* If this code is non-zero, also copy the foreign file name, its
   identification code and the flag indicating whether the NDF copy of
   the foreign file is to be kept. */
   if( dcb->fcb != 0 ) {
      star_strlcpy( dcb->forfl, pcb->forfl, sizeof( dcb->forfl ) );
      star_strlcpy( dcb->forid, pcb->forid, sizeof( dcb->forid ) );
      dcb->forkp = pcb->forkp;

/* If the data object (when converted from a foreign format file) is not
   to be kept, then mark its container file as a scratch file to ensure
   it will be deleted when no longer required. */
      if( !dcb->forkp ) ndf1Hscrt( dcb->loc, status );
   }

/* Set the DCB disposal mode if the PCB indicates the object is to be
   temporary. */
   if( pcb->tmp ) star_strlcpy( dcb->dsp, "TEMP", sizeof( dcb->dsp ) );

/* Set the data object's DCB access mode. */
   star_strlcpy( dcb->mod, "UPDATE", sizeof( dcb->mod ) );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Pldcb", status );

}

