#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Imppl( int place, NdfPCB **pcb, int *status ){
/*
*+
*  Name:
*     ndf1Imppl

*  Purpose:
*     Import an NDF placeholder.

*  Synopsis:
*     void ndf1Imppl( int place, NdfPCB **pcb, int *status )

*  Description:
*     This function imports a placeholder value into the NDF_ system,
*     validating it and converting it into a PCB index. If the placeholder
*     value is not valid, then an error will be reported.

*  Parameters:
*     place
*        The placeholder to be imported.
*     *pcb
*        Pointer to the placeholder entry in the PCB.
*     *status
*        The global status.

*  Notes:
*     -  If this function is called with "status" set, then a value of NULL
*     will be returned for the "pcb" pointer, although no further
*     processing will occur.
*     -  A value of NULL will also be returned for the "pcb" pointer if
*     the function should fail for any reason.

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

/* Set an initial value for the "pcb" parameter. */
   *pcb = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Convert the identifier to a PCB pointer. */
   *pcb = (NdfPCB *) ndf1Id2ac( place, 0 );

/* If a valid PCB pointer was not returned, then report an error. */
   if( *pcb == NULL ) {
      *status = NDF__PLINV;
      msgSeti( "BADPLACE", place );
      errRep( " ", "NDF placeholder invalid; its value is ^BADPLACE "
              "(possible programming error).", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Imppl", status );

}

