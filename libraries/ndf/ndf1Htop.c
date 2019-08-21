#include "sae_par.h"
#include "star/hds.h"
#include "ndf1.h"

void ndf1Htop( HDSLoc *loc1, const char *mode, HDSLoc **loc2, int *status ){
/*
*+
*  Name:
*     ndf1Htop

*  Purpose:
*     Return a top-level locator for an HDS container file.

*  Synopsis:
*     void ndf1Htop( HDSLoc *loc1, const char *mode, HDSLoc **loc2,
*                    int *status )

*  Description:
*     This function returns a top-level (secondary) locator for a container
*     file, given a locator for one of the objects within the file.

*  Parameters:
*     loc1
*        Locator to an object in the container file.
*     mode
*        Pointer to a null terminated string holding the required mode of
*        access: "READ", "UPDATE" or "WRITE".
*     *loc2
*        Returned holding the top-level (secondary) locator for the
*        container file.
*     *status
*        The global status.

*  Notes:
*     A value of NULL will be returned for the "loc2" parameter if
*     this function is called with "status" set. The same value will also
*     be returned if it should fail for any reason.

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
   char file[ NDF__SZFIL + 1 ];    /* HDS container file name */
   char path[ NDF__SZPTH + 1 ];    /* HDS object path */
   hdsbool_t isprim;               /* Primary/secondary flag */
   int nlev;                       /* Object nesting level */

/* Set an initial null value for the returned locator. */
   *loc2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain the file and path names of the HDS object. */
   hdsTrace( loc1, &nlev, path, file, status, sizeof( path ),
             sizeof( file ) );

/* Re-open the HDS container file to obtain a top-level locator. */
   hdsOpen( file, mode, loc2, status );

/* Demote it to be a secondary locator. */
   isprim = 0;
   datPrmry( 1, loc2, &isprim, status );

/* If an error occurred, then annul any locator which may have been
   obtained. */
   if( *status != SAI__OK ) datAnnul( loc2, status );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Htop", status );

}

