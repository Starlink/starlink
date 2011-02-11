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
*     smf_set_moving( AstFrameSet * wcs, AstFitsChan *fchan, int *status);

*  Arguments:
*     wcs = AstFrameSet * (Given and Returned)
*        World coordinate system information to be checked and modified.
*     fchan = AstFitsChan * (Given and Returned)
*        If the source is moving but no BASECx header is present, they
*        will be constructed from the frameset reference position. Can
*        be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine checks the coordinate system attribute in the given
*     FrameSet and sets the SkyRefIs and AlignOffset attributes to the
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

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

#define FUNC_NAME "smf_set_moving"

void smf_set_moving ( AstFrameSet* wcs, AstFitsChan * fchan, int *status ) {

  /* Local variables */
  const char *astsys = NULL;    /* Name of AST-supported coordinate system */

  if ( *status != SAI__OK ) return;

  /* Check for valid WCS FrameSet */
  if ( wcs ) {
    astsys = astGetC( wcs, "SYSTEM" );
    if ( astsys ) {
      if (strcmp(astsys,"AZEL") == 0 || strcmp(astsys, "GAPPT") == 0 ) {
	msgOutif( MSG__DEBUG, "",
		  "SMF_SET_MOVING: setting attributes for moving sources", status );
	astSet( wcs, "SkyRefIs=Origin,AlignOffset=1" );

        if (fchan && strcmp(astsys, "GAPPT" ) == 0 ) {
          double dtemp = 0.0;
          /* If we are missing one of the BASE headers add in new versions
             with the base position from the frameset */
          if (!astGetFitsF( fchan, "BASEC1", &dtemp ) ||
              !astGetFitsF( fchan, "BASEC2", &dtemp ) ) {
            atlPtftd( fchan, "BASEC1", AST__DR2D * astGetD( wcs, "SkyRef(1)"),
                      "C1 BASE position", status);
            atlPtftd( fchan, "BASEC2", AST__DR2D * astGetD( wcs, "SkyRef(2)"),
                      "C2 BASE position", status);
          }
        }
      }
    }
  } else {
    msgOutif( MSG__DEBUG, "", "SMF_SET_MOVING: Input WCS is NULL", status );
  }

}
