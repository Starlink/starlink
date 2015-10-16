/*
*+
*  Name:
*     smf_set_moving

*  Purpose:
*     Routine to set WCS attributes for moving sources

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_set_moving( AstFrame *wcs, AstFitsChan *fchan, int *status);

*  Arguments:
*     wcs = AstFrame * (Given and Returned)
*        World coordinate system information to be checked and modified.
*        This can be a Frame or a FrameSet.
*     fchan = AstFitsChan * (Given and Returned)
*        If the source is moving but no BASECx header is present, they
*        will be constructed from the Frame reference position. Can
*        be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine checks the coordinate system attribute in the given
*     Frame and sets the SkyRefIs and AlignOffset attributes to the
*     appropriate values for objects which have a moving BASE position
*     (e.g. planets). The routine performs no action if the SYSTEM is
*     not AZEL or GAPPT. BASECx headers are written to the FitsChan
*     for moving sources if they are not present already.

*  Notes:

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-08-27 (AGG):
*        Original version
*     2011-02-09 (TIMJ):
*        Add Fitschan to fill in BASECx
*     2011-04-01 (TIMJ):
*        BASECx must be stored in TRACKSYS coordinates, not the
*        system of the current Frame.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 2008 University of British Columbia. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "par.h"
#include "ast.h"
#include "star/atl.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smf_set_moving"

void smf_set_moving ( AstFrame *wcs, AstFitsChan *fchan, int *status ) {

  /* Local variables */
  const char *astsys = NULL;    /* Name of AST-supported coordinate system */

  if ( *status != SAI__OK ) return;

  /* Check for valid WCS Frame */
  if ( wcs ) {
    astsys = astGetC( wcs, "SYSTEM" );
    if ( astsys ) {
      if (strcmp(astsys,"AZEL") == 0 || strcmp(astsys, "GAPPT") == 0 ) {
	msgOutif( MSG__DEBUG, "",
		  "SMF_SET_MOVING: setting attributes for moving sources", status );
	astSet( wcs, "SkyRefIs=Origin,AlignOffset=1" );
      }

      /* if there is a FITS header we now worry about having a BASECx header */
      if (fchan) {
        double dtemp = 0.0;
        /* If we are missing one of the BASE headers add in new versions
           with the base position from the Frame */
        if (!astGetFitsF( fchan, "BASEC1", &dtemp ) ||
            !astGetFitsF( fchan, "BASEC2", &dtemp ) ) {
          char * tracksys = NULL;
          /* To fill it in we need to add it back in the TRACKSYS coordinate
             frame. So take a copy of the WCS before we modify things */
          if (astGetFitsS( fchan, "TRACKSYS", &tracksys )) {
            const char * asttracksys = sc2ast_convert_system( tracksys, status );
            AstFrame * wcs2 = astCopy( wcs );

            astSet( wcs2, "System=%s", asttracksys );
            atlPtftd( fchan, "BASEC1", AST__DR2D * astGetD( wcs2, "SkyRef(1)"),
                      "C1 BASE position", status);
            atlPtftd( fchan, "BASEC2", AST__DR2D * astGetD( wcs2, "SkyRef(2)"),
                      "C2 BASE position", status);
          }
        }
      }
    }
  } else {
    msgOutif( MSG__DEBUG, "", "SMF_SET_MOVING: Input WCS is NULL", status );
  }

}
