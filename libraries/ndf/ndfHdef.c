#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfHdef_( int indf, const char *appn, int *status ){
/*
*+
*  Name:
*     ndfHdef

*  Purpose:
*     Write default history information to an NDF.

*  Synopsis:
*     void ndfHdef( int indf, const char *appn, int *status )

*  Description:
*     This function writes default information about the current
*     application to the history component of an NDF, creating a new
*     history record if necessary.

*  Parameters:
*     indf
*        NDF identifier.
*     appn
*        Pointer to a null terminated string holding the name of the
*        current application. This will only be used if a new history
*        record is created by this function, otherwise it is ignored. If a
*        blank value is given, then a suitable default will be used
*        instead.
*     *status
*        The global status.

*  Notes:
*     -  The values stored in the history component for any program
*     parameters that have not yet been accessed by the program at the time
*     this function is called may not be correct. The value left by the
*     previous invocation of the program will be used, if it is stored in
*     the program's parameter file. If there is no value for the parameter
*     in the parameter file, a value of "<not yet accessed>" will be stored
*     for the parameter in the history component. For this reason, this
*     function should usually be called once all program parameters have
*     been accessed.
*     -  Default history information will normally be provided
*     automatically by the NDF_ system when an NDF is released, so a call
*     to this function is not usually required. It is provided so that
*     premature writing of default history information can be forced in
*     cases where additional text will then be appended to it (using
*     ndfHput).
*     -  It is expected that the "appn" parameter will usually be left
*     blank. A non-blank value should normally only be given if a more
*     complete identification of the current application can be given than
*     is supplied by default.
*     -  This function will return without action if (a) there is no
*     history component present in the NDF, (b) the NDF's history update
*     mode is currently "DISABLED", (c) default history information has
*     already been written to the NDF, or (d) a previous call has been made
*     to ndfHput specifying that default history information is to be
*     replaced.

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
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* If OK, check that WRITE access is available to the NDF. */
   if( *status == SAI__OK ) {
      ndf1Chacc( acb, "WRITE", status );

/* Obtain an index to the data object entry in the DCB and write the
   default history information. */
      dcb = acb->dcb;
      ndf1Hwdef( dcb, appn, status );
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfHdef: Error writing default history information to "
              "an NDF.", status );
      ndf1Trace( "ndfHdef", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

