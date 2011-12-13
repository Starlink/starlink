/*
*+
*  Name:
*     gsdac_getSampleMode.c

*  Purpose:
*     Gets the sampling mode and obstype.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_getSampleMode ( const gsdVars *gsdVars,
*                           char *samMode, char *obsType,
*                           int *status )

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD headers and arrays
*     samMode = char* (Given and Returned)
*        Sampling Mode
*     obsType = char* (Given and Returned)
*        Observation type
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Determines the sampling mode and observation type in ACSIS format
*     from the GSD headers.

*  Authors:
*     J.Balfour (UBC)
*     V.Tilanus (JAC)
*     {enter_new_authors_here}

*  History :
*     2008-02-11 (JB):
*        Original
*     2008-03-19 (JB):
*        Include mers.h
*     2010-07-01 (VT):
*        Use ACSIS oberving modes

*  Copyright:
*     Copyright (C) 2008,2010 Science and Technology Facilities Council.
*     All Rights Reserved.

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

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "gsdac.h"

#define FUNC_NAME "gsdac_getSampleMode"

void gsdac_getSampleMode ( const gsdVars *gsdVars,
                           char *samMode, char *obsType,
                           int *status )

{

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the observation type (science, pointing, or focus). */
  if ( strncmp ( gsdVars->obsType, "FIVEPOINT", 9 ) == 0 )
    strcpy ( obsType, "pointing" );
  else if ( strncmp ( gsdVars->obsType, "FOCUS", 5 ) == 0 )
    strcpy ( obsType, "focus" );
  else if ( strncmp ( gsdVars->obsType, "SKYDIP", 6 ) == 0 )
    strcpy ( obsType, "skydip" );
  else if ( strncmp ( gsdVars->obsType, "SAMPLE", 6 ) == 0 ||
            strncmp ( gsdVars->obsType, "GRID", 4 ) == 0 ||
            strncmp ( gsdVars->obsType, "ON/OFF", 6 ) == 0 ||
            strncmp ( gsdVars->obsType, "PATTERN", 7 ) == 0 ||
            strncmp ( gsdVars->obsType, "RASTER", 6 ) == 0 ||
                        strncmp ( gsdVars->obsType, "SPIRAL", 6 ) == 0 ) {
    strcpy ( obsType, "science" );
  } else {
    *status = SAI__ERROR;
    errRep ( "gsdac_getSampleMode", "Error getting OBS_TYPE", status );
    return;
  }

  /* Get the switch mode and sample mode in ACSIS format. */
  if ( strncmp ( gsdVars->obsType, "RASTER", 6 ) == 0 )
    strcpy ( samMode, "scan" );
  else
    strcpy ( samMode, "grid" );

}
