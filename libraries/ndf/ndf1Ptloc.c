#include "sae_par.h"
#include "star/subpar.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "par_err.h"

void ndf1Ptloc( const char *param, int ipar, const char *vmode,
                NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Ptloc

*  Purpose:
*     Store locators for an NDF parameter in the parameter system.

*  Synopsis:
*     void ndf1Ptloc( const char *param, int ipar, const char *vmode,
*                     NdfACB *acb, int *status )

*  Description:
*     This function stores two locators in the SUBPAR common blocks. The
*     first is a locator for the top-level container file that contains the
*     specified NDF, and the second is a locator for NDF itself.
*
*     It also stores information about the parameter in the ADAM Parameter
*     Block, so that the ndfCancl function can access it if required.

*  Parameters:
*     param
*        Pointer to a null terminated string holding the name of the ADAM
*        parameter.
*     ipar
*        The SUBPAR index for the parameter.
*     vmode
*        Pointer to a null terminated string holding the validated access
*        mode string
*     acb
*        Pointer to NDF entry in the ACB.
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
   HDSLoc *loc = NULL;   /* Locator to NDF structure */
   HDSLoc *loc0 = NULL;  /* Locator to HDS container file */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain an index for the data object in the DCB and obtain a top-level
   locator for its container file by re-opening the file. */
   dcb = acb->dcb;
   hdsOpen( dcb->file, vmode, &loc0, status );

/* Also clone a locator for the data object itself from its DCB entry. */
   datClone( dcb->loc, &loc, status );

/* Save these locators in the parameter system and link the object
   locator with the parameter name. The subpar functions store the
   supplied locators directly, rather than storing clones of the locators.
   So we do not annul the "loc" and "loc0" locators here. */
   subParPutfloc( ipar, loc0, status );
   subParPutloc( ipar, loc, status );
   hdsLink( loc, param, status );

/* If the ADAM Parameter Block does not yet have a KeyMap to hold
   parameter information, create one now. */
   if( Ndf_APB_pars == 0 ) {
      astBeginPM;
      Ndf_APB_pars = astKeyMap( " " );
      astEndPM;
      astExempt( Ndf_APB_pars );
   }

/* Add an entry for the supplied parameter, storing a value of zero to
   indicate that ndfCancl should include it in its automatic parameter
   cancellation. */
   astMapPut0I( Ndf_APB_pars, param, 0, " " );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Ptloc", status );

}

