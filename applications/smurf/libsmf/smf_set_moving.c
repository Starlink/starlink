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
*     smf_set_moving( AstFrameSet * wcs, int *status);

*  Arguments:
*     wcs = AstFrameSet * (Given and Returned)
*        World coordinate system information to be checked and modified.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine checks the coordinate system attribute in the given
*     FrameSet and sets the SkyRefIs and AlignOffset attributes to the
*     appropriate values for objects which have a moving BASE position
*     (e.g. planets). The routine performs no action if the SYSTEM is
*     not AZEL or GAPPT.

*  Notes:

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-08-27 (AGG):
*        Original version
*     {enter_further_changes_here}

*  Copyright:
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

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_set_moving"

void smf_set_moving ( AstFrameSet* wcs, int *status ) {

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
      }
    }
  } else {
    msgOutif( MSG__DEBUG, "", "SMF_SET_MOVING: Input WCS is NULL", status );
  }

}
