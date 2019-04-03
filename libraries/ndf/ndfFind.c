#include "sae_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfFind_( const HDSLoc *loc, const char *name, int *indf, int *status ){
/*
*+
*  Name:
*     ndfFind

*  Purpose:
*     Find an NDF and import it into the NDF_ system.

*  Synopsis:
*     void ndfFind( HDSLoc *loc, const char *name, int *indf, int *status )

*  Description:
*     This function finds an NDF within an HDS structure or container file,
*     imports it into the NDF_ system and issues an identifier for it. The
*     imported NDF may then be manipulated by the NDF_ functions.

*  Parameters:
*     loc
*        Locator to the enclosing HDS structure.
*     name
*        Pointer to a null terminated string holding the name of the HDS
*        structure component to be imported.
*     *indf
*        Returned holding the NDF identifier.
*     *status
*        The global status.

*  Notes:
*     -  The value given for the "name" parameter may be an HDS path name,
*     consisting of several fields separated by ".", so that an NDF can be
*     found in a sub-component (or a sub-sub-component...) of the structure
*     identified by the locator "loc".  Array subscripts may also be used
*     in this component name.  Thus a string such as
*     "MYSTRUC.ZONE(2).IMAGE" could be used as a valid "name" value.
*     -  An NDF can be accessed within an explicitly named container file
*     by supplying the symbolic value NULL for the "loc" parameter and
*     specifying the container file within the value supplied for the
*     "name" parameter. Only READ access is available to an NDF accessed in
*     this way (for other modes of access, see the ndfOpen function).
*     -  If a blank value is given for the "name" parameter, then the NDF
*     to be imported will be the object identified directly by the locator
*     "loc".
*     -  The locator supplied as input to this function may later be
*     annulled without affecting the behaviour of the NDF_ system.
*     -  If this function is called with "status" set, then a value of
*     NDF__NOID will be returned for the "indf" parameter, although no
*     further processing will occur. The same value will also be returned
*     if the function should fail for any reason.
*     -  The NDF__NOID constant is defined in the header file "ndf.h". The
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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */

/* Set an initial value for the "indf" parameter. */
   *indf = NDF__NOID;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Find and import the NDF, assuming READ access if a root locator is
   supplied. */
   ndf1Opfor( (HDSLoc *) loc, name, "READ", &acb, status );
   if( *status == SAI__OK ) {

/* Export an identifier for the new NDF. */
      *indf = ndf1Expid( ( NdfObject * ) acb, status );

/* If an error occurs, then annul the ACB entry. */
      if( *status != SAI__OK ) ndf1Anl( &acb, status );
   }

/* If an error occurred, then reset the "indf" parameter, report context
   information and call the error tracing function. */
   if( *status != SAI__OK ) {
      *indf = NDF__NOID;
      errRep( " ", "ndfFind: Error finding an NDF and importing it into "
              "the NDF_ system.", status );
      ndf1Trace( "ndfFind", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

