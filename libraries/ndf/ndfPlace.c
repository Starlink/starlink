#include "sae_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfPlace_( const HDSLoc *loc, const char *name, int *place, int *status ){
/*
*+
*  Name:
*     ndfPlace

*  Purpose:
*     Obtain an NDF placeholder.

*  Synopsis:
*     void ndfPlace( HDSLoc *loc, const char *name, int *place, int *status )

*  Description:
*     This function returns an NDF placeholder. A placeholder is used to
*     identify a position in the underlying data system (HDS) and may be
*     passed to other functions (e.g. ndfNew) to indicate where a newly
*     created NDF should be positioned.

*  Parameters:
*     loc
*        HDS locator to the structure to contain the new NDF.
*     name
*        Pointer to a null terminated string holding the name of the new
*        structure component (i.e. the NDF).
*     *place
*        Returned holding the NDF placeholder identifying the nominated
*        position in the data system.
*     *status
*        The global status.

*  Notes:
*     -  Placeholders are intended only for local use within an application
*     and only a limited number of them are available simultaneously. They
*     are always annulled as soon as they are passed to another function to
*     create a new NDF, where they are effectively exchanged for an NDF
*     identifier.
*     -  The value given for the "name" parameter may be an HDS path name,
*     consisting of several fields separated by ".", so that an NDF can be
*     created in a sub-component (or a sub-sub-component...) of the
*     structure identified by the locator "loc".  Array subscripts may also
*     be used in this component name.  Thus a string such as
*     "MYSTRUC.ZONE(2).IMAGE" could be used as a valid "name" value.
*     -  Normally, this function will be used as the basis for creating a
*     completely new NDF data structure. However, if the "loc" and "name"
*     arguments refer to a pre-existing object, then this structure will be
*     used as the basis for the new NDF.  An object which is to be used in
*     this way must be an empty scalar structure with an HDS type of "NDF".
*     -  A new NDF can be created within an explicitly named container file
*     by supplying the symbolic value NULL for the "loc" parameter,
*     and specifying the container file within the value supplied for the
*     "name" parameter. If the object is the top level object within a
*     container file, then a new container file is created. If it is not a
*     top level object, then the container file and all structures lying
*     above the object should already exist.
*     -  If a blank value is given for the "name" parameter, then the new
*     NDF will be the object identified directly by the locator "loc". This
*     must be an empty scalar structure of type "NDF".
*     -  If this function is called with "status" set, then a value of
*     NDF__NOPL will be returned for the "place" parameter, although no
*     further processing will occur. The same value will also be returned
*     if the function should fail for any reason.
*     -  The NDF__NOPL constant is defined in the header file "ndf.h". The
*     NULL constant is defined in the header file "dat_par.h" (see
*     SUN/92).

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
   NdfPCB *pcb;          /* Pointer to new NDF entry in the PCB */

/* Set an initial value for the "place" parameter. */
   *place = NDF__NOPL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Obtain a PCB entry for the new NDF and export the required
   placeholder. */
   ndf1Plfor( (HDSLoc *) loc, name, &pcb, status );
   if( *status == SAI__OK ) {
      *place = ndf1Expid( ( NdfObject * ) pcb, status );

/* If an error occurred, then annul the PCB entry. */
      if( *status != SAI__OK ) ndf1Annpl( 1, &pcb, status );
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfPlace: Error obtaining an NDF placeholder.", status );
      ndf1Trace( "ndfPlace", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

