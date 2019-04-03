#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Nplac( HDSLoc *loc, const char *name, NdfPCB **pcb, int *status ){
/*
*+
*  Name:
*     ndf1Nplac

*  Purpose:
*     Create an NDF placeholder entry in the PCB.

*  Synopsis:
*     void ndf1Nplac( HDSLoc *loc, const char *name, NdfPCB **pcb, int *status )

*  Description:
*     This function creates an NDF placeholder entry in the PCB for a new
*     NDF. This is used to identify a position in the underlying data
*     system and may be passed to other functions to indicate where a newly
*     created NDF should be positioned.

*  Parameters:
*     loc
*        HDS locator which, inconjunction with the "name" parameter,
*        identifies the structure which is to become a new NDF. A value of
*        NULL may be supplied to indicate that the "name" parameter
*        contains an absolute object name.
*     name
*        Pointer to a null terminated string holding the name to be used
*        together with the "loc" value to identify the placeholder object.
*        If "loc" is set to NULL, this should be the absolute HDS name
*        of the object, otherwise it should be a relative name.
*     *pcb
*        Pointer to a new PCB entry identifying the nominated position in
*        the data system.
*     *status
*        The global status.

*  Notes:
*     -  If the object identified by "loc" and "name" exists before this
*     function is invoked (and it is not a top-level object), then it
*     should be an empty scalar structure of type "NDF", otherwise an error
*     will result.
*     -  If this function is called with "status" set, then a value of zero
*     will be returned for the "pcb" parameter, although no further
*     processing will occur. The same value will also be returned if the
*     function should fail for any reason.

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

/* Set an initial null value for the "pcb" parameter. */
   *pcb = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain a free slot in the PCB. */
   *pcb = ndf1Ffs( NDF__PCBTYPE, status );
   if( *status == SAI__OK ) {

/* Create the placeholder object, storing its locator in the PCB. */
      ndf1Plcre( loc, name, &(*pcb)->loc, &(*pcb)->new, status );

/* If an error occurred, then release the PCB slot. */
      if( *status != SAI__OK ) *pcb = ndf1Rls( ( NdfObject * ) *pcb, status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Nplac", status );

}

