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
*     gsdac_getMapVars ( const gsdVars *gsdVars, 
*                        const char *samMode, const char *obsType,
*                        char *skyRefX, char *skyRefY, char *swMode, 
*                        char *chopCrd, float *mapHght, float *mapPA, 
*                        float *mapWdth, char *loclCrd, char *scanCrd, 
*                        float *scanVel, float *scanDy, 
*                        float *scanPA, char *scanPat, int *status )

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD file access parameters
*     samMode = const char* (Given)
*        Sampling Mode
*     obsType = const char* (Given)
*        Observation type
*     skyRefX = char* (Given and Returned)
*        X-coord of reference position
*     skyRefY = char* (Given and Returned)
*        Y-coord of reference position
*     swMode = char* (Given and Returned)
*        Switch mode
*     chopCrd = char* (Given and Returned)
*        Chop coordinate frame
*     mapHght = double* (Given and Returned)
*        Requested height of map
*     mapPA = double* (Given and Returned)
*        Requested position angle of map
*     mapWdth = float* (Given and Returned)
*        Requested width of map
*     loclCrd = char* (Given and Returned)
*        Local offset coordinate system for
*        MAP_X/MAP_Y
*     scanCrd = char* (Given and Returned)
*        Coordinate system of scan
*     scanVel = float* (Given and Returned)
*        Scan velocity
*     scanDy = float* (Given and Returned)
*        Scan spacing perpendicular to scan
*     scanPA = float* (Given and Returned)
*        Scan PA rel. to lat. line
*     scanPat = char* (Given and Returned)
*        Name of scanning scheme
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Retrieves values from the GSD headers in order to fill the 
*     Sampling/Switch modes and the chop, scan and map parameters.

*  Authors:
*     J.Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2008-02-05 (JB):
*        Original
*     2008-02-14 (JB):
*        Use gsdVars struct to store headers/arrays

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

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

#define FUNC_NAME "gsdac_getMapVars"

void gsdac_getMapVars ( const gsdVars *gsdVars, 
                        const char *samMode, const char *obsType,
                        char *skyRefX, char *skyRefY, char *swMode, 
                        char *chopCrd, float *mapHght, float *mapPA, 
                        float *mapWdth, char *loclCrd, char *scanCrd, 
                        float *scanVel, float *scanDy, 
                        float *scanPA, char *scanPat, int *status )

{

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  if ( strncmp ( gsdVars->swMode, "POSITION_SWITCH", 15 ) == 0 ) {

    if ( gsdVars->chopping ) {
      if ( gsdVars->referenceX == 0.0 && gsdVars->referenceY == 0.0 ) 
        strcpy ( swMode, "freq" );
      else 
        strcpy ( swMode, "pssw" );
    } else {
      if ( gsdVars->referenceX == 0.0 && gsdVars->referenceY == 0.0 ) { 
        strcpy ( swMode, "freq" );
        /* Print a message, this was likely intended as a freq. sw. */
        msgOutif(MSG__VERB," ", "SWITCH_MODE was POSITION_SWITCH and CHOPPING was 0, this was likely intended to be a frequency switch", status);
      } else strcpy ( swMode, "pssw" );
    } 

  } else if ( strncmp ( gsdVars->swMode, "BEAMSWITCH", 10 ) == 0 ) {

    if ( gsdVars->chopping ) {
      strcpy ( swMode, "chop" );
    } else {    
      strcpy ( swMode, "none" );
      msgOutif(MSG__VERB," ", "SWITCH_MODE was BEAMSWITCH and CHOPPING was 0, this may be an error...)", status);
    } 

  } else if ( strncmp ( gsdVars->swMode, "CHOPPING", 8 ) == 0 ) {

    strcpy ( swMode, "freq" );
    if ( gsdVars->chopping ) {
      msgOutif(MSG__VERB," ", "SWITCH_MODE was CHOPPING and CHOPPING was 1, this appears to be a misconfigured frequency switch", status);
    } 

  } else if ( strncmp ( gsdVars->swMode, "NO_SWITCH", 7 ) == 0 ) {

    strcpy ( swMode, "none" );
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
  if ( ( strcmp ( samMode, "grid" ) == 0 && 
         strcmp ( swMode, "chop" ) == 0 ) ||
       strcmp ( samMode, "sample" ) == 0 ) {

    if ( *status != SAI__OK ) {
      *status = SAI__ERROR;
      errRep ( "gsdac_getMapVars", "Error getting chop parameters", status );
      return; 
    }

    if ( strncmp ( gsdVars->chopCoords, "AZ", 2 ) ) 
      strcpy ( chopCrd, "AZEL" );
    else if ( strncmp ( gsdVars->chopCoords, "RB", 2 ) ) 
      strcpy ( chopCrd, "B1950" );
    else if ( strncmp ( gsdVars->chopCoords, "RJ", 2 ) ) 
      strcpy ( chopCrd, "J2000" );
    else if ( strncmp ( gsdVars->chopCoords, "RD", 2 ) ) 
      strcpy ( chopCrd, "RA/Dec" );//k
    else if ( strncmp ( gsdVars->chopCoords, "GA", 2 ) ) 
      strcpy ( chopCrd, "GA" );//k
    else {
      strcpy ( chopCrd, "" );
      msgOutif(MSG__VERB," ", 
	       "Couldn't identify chop coordinates, continuing anyway", status); 
    } 

  }

  /* Get the local offset coordinate system. */
  switch ( gsdVars->cellCode ) {
    
    case 1:
      strcpy ( loclCrd, "AZEL" );
      break;
    case 3:
      strcpy ( loclCrd, "EQ" );//k
      break;
    case 4:
      strcpy ( loclCrd, "RA/Dec" );//k
      break;
    case 6:
      strcpy ( loclCrd, "B1950" );
      break;
    case 7:
      strcpy ( loclCrd, "J2000" );
      break;
    case 8:
      strcpy ( loclCrd, "GA" );//k
      break;
    default:
      strcpy ( loclCrd, "" );
      msgOutif(MSG__VERB," ", 
               "Couldn't identify local map coordinates, continuing anyway", status);   
      break;

  }

  /* Convert to ACSIS formatted string. */
  sprintf ( skyRefX, "[OFFSET] %.0f [%s]", 
            gsdVars->referenceX, loclCrd );
  sprintf ( skyRefY, "[OFFSET] %.0f [%s]", 
            gsdVars->referenceY, loclCrd );

  /* Get the scanning coordinates. */
  strcpy ( scanCrd, loclCrd );

  /* Get the map and scan parameters for rasters. */
  if ( strcmp ( samMode, "raster" ) == 0
       && strcmp ( swMode, "pssw" ) == 0 ) {

    if ( *status != SAI__OK ) {
      *status = SAI__ERROR;
      errRep ( "gsdac_getMapVars", "Error getting map & scan parameters", status );
      return; 
    }

    /* Get the map height and width. */
    *mapHght = gsdVars->nMapPtsX * gsdVars->cellX;
    *mapWdth = gsdVars->nMapPtsY * gsdVars->cellY;

    /* Get the scan velocity and spacing. */
    if ( strncmp ( gsdVars->obsDirection, "HORIZONTAL", 10 ) == 0 ) {          
      *scanVel = gsdVars->cellX / gsdVars->scanTime;
      *scanDy = gsdVars->cellY;
    } else if ( strncmp ( gsdVars->obsDirection, "VERTICAL", 8 ) == 0 ) {
      *scanVel = gsdVars->cellY / gsdVars->scanTime;
      *scanDy = gsdVars->cellX;
    } else {
      *status = SAI__ERROR;
      errRep ( "gsdac_getMapVars", "Error getting scan velocity", 
               status );
      return;
    }

    if ( gsdVars->scanRev ) strcpy ( scanPat, "BOUSTROPHEDON" );
    else strcpy ( scanPat, "RASTER" );

  } 

  /* Kludged...*/
  *mapPA = 0.0;//k
  *scanPA = 0.0;//k

}
