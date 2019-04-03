#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Hscrt( HDSLoc *loc, int *status ){
/*
*+
*  Name:
*     ndf1Hscrt

*  Purpose:
*     Mark an HDS container file as a scratch file.

*  Synopsis:
*     void ndf1Hscrt( HDSLoc *loc, int *status )

*  Description:
*     This function marks an HDS container file for deletion (by HDS), thus
*     effectively turning it into a scratch file, which will be deleted
*     when the last primary locator associated with it is annulled (or when
*     HDS terminates if this occurs earlier).

*  Parameters:
*     loc
*        Locator to an object in the container file.
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
   HDSLoc *toploc = NULL;/* Top level locator for file */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain a top-level locator for the HDS container file. */
   ndf1Htop( loc, "UPDATE", &toploc, status );

/* Mark the file for deletion (this also annuls the locator). The file
   will not actually be deleted until its reference count falls to zero. */
   hdsErase( &toploc, status );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Hscrt", status );

}

