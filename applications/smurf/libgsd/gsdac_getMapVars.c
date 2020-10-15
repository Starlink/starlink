/*
*+
*  Name:
*     gsdac_getMapVars.c

*  Purpose:
*     Process the Map and Switching data from the GSD file to fill
*     the required FITS headers.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_getMapVars ( const gsdVars *gsdVars, const char *samMode,
*                        mapVars *mapVars, int *status )

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD file access parameters
*     samMode = const char* (Given)
*        Sampling Mode
*     mapVars = mapVars* (Given and Returned)
*        Map/Chop/Scan variables
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Retrieves values from the GSD headers in order to fill the
*     Sampling/Switch modes and the chop, scan and map parameters.

*  Authors:
*     J.Balfour (UBC)
*     V.Tilanus (JAC)
*     Malcolm J. Currie (JAC)
*     {enter_new_authors_here}

*  History :
*     2008-02-05 (JB):
*        Original.
*     2008-02-14 (JB):
*        Use gsdVars struct to store headers/arrays.
*     2008-02-27 (JB):
*        Check for Equatorial cellCode (unsupported).
*     2008-02-28 (JB):
*        Use mapVars struct.
*     2008-03-24 (JB):
*        Get mapPA and scanPA.
*     2008-04-03 (JB):
*        Correct scanVel calculation.
*     2008-04-14 (JB):
*        Remove obsType argument (not needed).
*     2010-07-01 (VT):
*        Fixed string equalities.
*        Renamed Raster to Scan.
*     2014 March 7 (MJC):
*        Switch mode freq should be freqsw for CAOM2 ingestion.

*  Copyright:
*     Copyright (C) 2008, 2010, 2014 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 3 of
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
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "star/one.h"

/* SMURF includes */
#include "gsdac.h"

#define FUNC_NAME "gsdac_getMapVars"

void gsdac_getMapVars ( const gsdVars *gsdVars, const char *samMode,
                        mapVars *mapVars, int *status )

{

  const char * cell_sys;

  /* Check inherited status */
  if ( *status != SAI__OK ) return;


  /* Translate Switch mode. */
  if ( strncmp ( gsdVars->swMode, "POSITION_SWITCH", 15 ) == 0 ) {

    if ( gsdVars->chopping ) {
      if ( gsdVars->referenceX == 0.0 && gsdVars->referenceY == 0.0 )
        strcpy ( mapVars->swMode, "freqsw" );
      else
        strcpy ( mapVars->swMode, "pssw" );
    } else {
      if ( gsdVars->referenceX == 0.0 && gsdVars->referenceY == 0.0 ) {
        strcpy ( mapVars->swMode, "freqsw" );

        /* Print a message, this was likely intended as a freq. sw. */
        msgOutif(MSG__VERB," ", "SWITCH_MODE was POSITION_SWITCH and CHOPPING was 0, this was likely intended to be a frequency switch", status);
      } else strcpy ( mapVars->swMode, "pssw" );
    }

  } else if ( strncmp ( gsdVars->swMode, "BEAMSWITCH", 10 ) == 0 ) {

    if ( gsdVars->chopping ) {
      strcpy ( mapVars->swMode, "chop" );
    } else {
      strcpy ( mapVars->swMode, "none" );
      msgOutif(MSG__VERB," ", "SWITCH_MODE was BEAMSWITCH and CHOPPING was 0, this may be an error...)", status);
    }

  } else if ( strncmp ( gsdVars->swMode, "CHOPPING", 8 ) == 0 ) {

    strcpy ( mapVars->swMode, "freqsw" );
    if ( gsdVars->chopping ) {
      msgOutif(MSG__VERB," ", "SWITCH_MODE was CHOPPING and CHOPPING was 1, this appears to be a misconfigured frequency switch", status);
    }

  } else if ( strncmp ( gsdVars->swMode, "NO_SWITCH", 7 ) == 0 ) {

    strcpy ( mapVars->swMode, "none" );
    if ( gsdVars->chopping ) {
      msgOutif(MSG__VERB," ", "SWITCH_MODE was NO_SWITCH and CHOPPING was 1, this may be an error...", status);
    }

  } else {
    *status = SAI__ERROR;
    msgSetc ( "SWITCHMODE", gsdVars->swMode );
    errRep ( "gsdac_getMapVars", "Couldn't identify switch mode ^SWITCHMODE", status );
    return;
  }

  /* Get the chopping parameters for grid beamswitches
     and samples. */

  if ( strcmp ( mapVars->swMode, "chop" ) == 0 ) {

    if ( strncmp ( gsdVars->chopCoords, "AZ", 2 ) == 0 )
      strcpy ( mapVars->chopCrd, "AZEL" );
    else if ( strncmp ( gsdVars->chopCoords, "EQ", 2 ) == 0 )
	  strcpy ( mapVars->chopCrd, "HADEC" );
    else if ( strncmp ( gsdVars->chopCoords, "RB", 2 ) == 0 )
      strcpy ( mapVars->chopCrd, "B1950" );
    else if ( strncmp ( gsdVars->chopCoords, "RJ", 2 ) == 0 )
      strcpy ( mapVars->chopCrd, "J2000" );
    else if ( strncmp ( gsdVars->chopCoords, "RD", 2 ) == 0 )
      strcpy ( mapVars->chopCrd, "APP" );
    else if ( strncmp ( gsdVars->chopCoords, "GA", 2 ) == 0 )
      strcpy ( mapVars->chopCrd, "GAL" );
    else {
      strcpy ( mapVars->chopCrd, "" );
      msgOutif(MSG__VERB," ",
	       "Couldn't identify chop coordinates, continuing anyway", status);
    }

  }

  /* Get the local offset coordinate system. */
  cell_sys = gsdac_code2tcssys( gsdVars->cellCode, status );
  if (*status == SAI__OK && strlen(cell_sys) == 0) {
    msgOutif(MSG__VERB," ",
             "Couldn't identify cell coordinates, continuing anyway", status);
  }
  one_strlcpy( mapVars->loclCrd, cell_sys,
               sizeof(mapVars->loclCrd), status );

  /* Convert to ACSIS formatted string. */
  sprintf ( mapVars->skyRefX, "[OFFSET] %.0f [%.55s]",
            gsdVars->referenceX, mapVars->loclCrd );
  sprintf ( mapVars->skyRefY, "[OFFSET] %.0f [%.55s]",
            gsdVars->referenceY, mapVars->loclCrd );

  /* Get the scanning coordinates. */
  strcpy ( mapVars->scanCrd, mapVars->loclCrd );

  /* Get the map and scan parameters for rasters. */
  if ( strcmp ( samMode, "scan" ) == 0
       && strcmp ( mapVars->swMode, "pssw" ) == 0 ) {

    /* Get the map height and width. */
    mapVars->mapHght = gsdVars->nMapPtsX * gsdVars->cellX;
    mapVars->mapWdth = gsdVars->nMapPtsY * gsdVars->cellY;

    /* Get the scan velocity and spacing. */
    if ( strncmp ( gsdVars->obsDirection, "HORIZONTAL", 10 ) == 0 ) {
      mapVars->scanVel = gsdVars->cellX * gsdVars->nMapPtsX /
                          gsdVars->scanTime;
      mapVars->scanDy = gsdVars->cellY;
    } else if ( strncmp ( gsdVars->obsDirection, "VERTICAL", 8 ) == 0 ) {
      mapVars->scanVel = gsdVars->cellY * gsdVars->nMapPtsY /
                          gsdVars->scanTime;
      mapVars->scanDy = gsdVars->cellX;
    } else {
      *status = SAI__ERROR;
      errRep ( "gsdac_getMapVars", "Error getting scan velocity",
               status );
      return;
    }

    if ( gsdVars->scanRev ) strcpy ( mapVars->scanPat, "BOUSTROPHEDON" );
    else strcpy ( mapVars->scanPat, "RASTER" );

  }

  mapVars->scanPA = gsdVars->cellV2X;
  mapVars->mapPA = mapVars->scanPA - 90.0;

}
