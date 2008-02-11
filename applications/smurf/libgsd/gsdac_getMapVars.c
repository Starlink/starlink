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
*     gsdac_getMapVars ( const struct gsdac_gsd_struct *gsd, char *samMode, 
*                        char *swMode, char *skyRefX, char *skyRefY, 
*                        char *obsType, char *chopCrd, float *chopFrq, 
*                        float *chopPA, float *chopThr, 
*                        float *mapHght, float *mapPA, float *mapWdth, 
*                        int *numPtsX, int numPtsY, char *obsDirection,
*                        char *loclCrd, double *mapX, double *mapY, 
*                        char *scanCrd, float *scanVel, float *scanDy, 
*                        float *scanPA, char *scanPat, int *status )

*  Arguments:
*     gsd = const struct gsdac_gsd_struct* (Given)
*        GSD file access parameters
*     samMode = char* (Given and Returned)
*        Sampling Mode
*     swMode = char* (Given and Returned)
*        Switch Mode
*     skyRefX = char* (Given and Returned)
*        X-coord of reference position
*     skyRefY = char* (Given and Returned)
*        Y-coord of reference position
*     obsType = char* (Given and Returned)
*        Observation type
*     chopCrd = char* (Given and Returned)
*        Chop coordinate frame
*     chopFrq = float* (Given and Returned)
*        Chop frequency
*     chopPA = float* (Given and Returned)
*        Chop position angle
*     chopThr = float* (Given and Returned)
*        Chop throw
*     mapHght = double* (Given and Returned)
*        Requested height of map
*     mapPA = double* (Given and Returned)
*        Requested position angle of map
*     mapWdth = float* (Given and Returned)
*        Requested width of map
*     numPtsX = int* (Given and Returned)
*        Number of map points in x direction
*     numPtsY = int* (Given and Returned)
*        Number of map points in y direction
*     obsDirection = char* (Given and Returned)
*        Direction of map rows
*     loclCrd = char* (Given and Returned)
*        Local offset coordinate system for
*        MAP_X/MAP_Y
*     mapX = float* (Given and Returned)
*        Requested map x offset from centre
*     mapY = float* (Given and Returned)
*        Requested map y offset from centre
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
#include "ast.h"
#include "gsd.h"
#include "sae_par.h"
#include "mers.h"

/* GSDAC includes */
#include "gsdac.h"

#define MAXFITS 80

#define FUNC_NAME "gsdac_getMapVars"

void gsdac_getMapVars ( const struct gsdac_gsd_struct *gsd, 
                        char *samMode, 
                        char *swMode, char *skyRefX, char *skyRefY, 
                        char *obsType, char *chopCrd, float *chopFrq, 
                        float *chopPA, float *chopThr, 
                        float *mapHght, float *mapPA, float *mapWdth, 
                        int *numPtsX, int *numPtsY, char *obsDirection,
                        char *loclCrd,  double *mapX, double *mapY, 
                        char *scanCrd, float *scanVel, float *scanDy, 
                        float *scanPA, char *scanPat, int *status )

{

  /* Local variables */
  int cellCode;               /* code for local map coords */
  char chopping;              /* flag for chopping */
  char curChar;               /* character pointer */
  int deg;                    /* degrees */
  double dx;                  /* width of cells in x dimension */
  double dy;                  /* width of cells in y dimension */
  int i;                      /* loop counter */
  int min;                    /* minutes */
  double refX;                /* reference X position */
  double refY;                /* reference Y position */
  char scanRev;               /* scan reversal flag */
  double sec;                 /* seconds */
  int scanTime;               /* scan time in seconds */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the Sampling Mode. */
  gsdac_get0c ( gsd, "c6st", samMode, status ); 

  /* Get the Switch Mode. */
  gsdac_get0c ( gsd, "C6MODE", swMode, status ); 

  /* Check for chopping. */
  gsdac_get0l ( gsd, "c4sm", &chopping, status );

  /* Get the local offset coordinate system and
     reference position offset. */ 
  gsdac_get0i ( gsd, "C6FC", &cellCode, status );
  gsdac_get0d ( gsd, "C4RX", &refX, status );
  gsdac_get0d ( gsd, "C4RY", &refY, status );

  if ( *status != SAI__OK ) {
    *status = SAI__ERROR;
    errRep ( "gsdac_getMapVars", "Error getting GSD parameters", status );
    return; 
  }

  /* Get the observation type (science, pointing, or focus). */
  if ( strncmp ( samMode, "FIVEPOINT", 9 ) == 0 ) 
    strcpy ( obsType, "pointing" );
  else if ( strncmp ( samMode, "FOCUS", 5 ) == 0 )
    strcpy ( obsType, "focus" );
  else if ( strncmp ( samMode, "SAMPLE", 6 ) == 0 ||
            strncmp ( samMode, "GRID", 4 ) == 0 ||
            strncmp ( samMode, "ON/OFF", 6 ) == 0 ||
            strncmp ( samMode, "PATTERN", 7 ) == 0 ||
            strncmp ( samMode, "RASTER", 6 ) == 0 ||
            strncmp ( samMode, "SKYDIP", 6 ) == 0 ||
            strncmp ( samMode, "SPIRAL", 6 ) == 0 ) {
    strcpy ( obsType, "science" );
  } else {
    *status = SAI__ERROR;
    errRep ( "gsdac_getMapVars", "Error getting OBS_TYPE", status );
    return;
  }

  /* Get the switch mode and sample mode in ACSIS format. */
  if ( strncmp ( samMode, "RASTER", 6 ) == 0 )
    strcpy ( samMode, "raster" );
  else
    strcpy ( samMode, "grid" );

  if ( strncmp ( swMode, "POSITION_SWITCH", 15 ) == 0 ) {

    if ( chopping ) {
      if ( refX == 0.0 && refY == 0.0 ) strcpy ( swMode, "freq" );
      else strcpy ( swMode, "pssw" );
    } else {
      if ( refX == 0.0 && refY == 0.0 ) { 
        strcpy ( swMode, "freq" );
        /* Print a message, this was likely intended as a freq. sw. */
        msgOutif(MSG__VERB," ", "SWITCH_MODE was POSITION_SWITCH and CHOPPING was 0, this was likely intended to be a frequency switch", status);
      } else strcpy ( swMode, "pssw" );
    } 

  } else if ( strncmp ( swMode, "BEAMSWITCH", 10 ) == 0 ) {

    if ( chopping ) {
      strcpy ( swMode, "chop" );
    } else {    
      strcpy ( swMode, "none" );
      msgOutif(MSG__VERB," ", "SWITCH_MODE was BEAMSWITCH and CHOPPING was 0, this may be an error...)", status);
    } 

  } else if ( strncmp ( swMode, "CHOPPING", 8 ) == 0 ) {

    strcpy ( swMode, "freq" );
    if ( chopping ) {
      msgOutif(MSG__VERB," ", "SWITCH_MODE was CHOPPING and CHOPPING was 1, this appears to be a misconfigured frequency switch", status);
    } 

  } else if ( strncmp ( swMode, "NO_SWITCH", 7 ) == 0 ) {

    strcpy ( swMode, "none" );
    if ( chopping ) {
      msgOutif(MSG__VERB," ", "SWITCH_MODE was NO_SWITCH and CHOPPING was 1, this may be an error...", status);
    } 
 
  } else {
    *status = SAI__ERROR;
    msgSetc ( "SWITCHMODE", swMode );
    errRep ( "gsdac_getMapVars", "Couldn't identify switch mode ^SWITCHMODE", status );
    return;      
  }

  /* Get the chopping parameters for grid beamswitches
     and samples. */
  if ( ( strcmp ( samMode, "grid" ) == 0 && 
         strcmp ( swMode, "chop" ) == 0 ) ||
       strcmp ( samMode, "sample" ) == 0 ) {

    gsdac_get0c ( gsd, "c4smco", chopCrd, status );
    gsdac_get0r ( gsd, "c4frq", chopFrq, status );
    gsdac_get0r ( gsd, "c4posang", chopPA, status );
    gsdac_get0r ( gsd, "c4throw", chopThr, status );

    if ( *status != SAI__OK ) {
      *status = SAI__ERROR;
      errRep ( "gsdac_getMapVars", "Error getting chop parameters", status );
      return; 
    }

    if ( strncmp ( chopCrd, "AZ", 2 ) ) 
      strcpy ( chopCrd, "AZEL" );
    else if ( strncmp ( chopCrd, "RB", 2 ) ) 
      strcpy ( chopCrd, "B1950" );
    else if ( strncmp ( chopCrd, "RJ", 2 ) ) 
      strcpy ( chopCrd, "J2000" );
    else if ( strncmp ( chopCrd, "RD", 2 ) ) 
      strcpy ( chopCrd, "RA/Dec" );//k
    else if ( strncmp ( chopCrd, "GA", 2 ) ) 
      strcpy ( chopCrd, "GA" );//k
    else {
      strcpy ( chopCrd, "" );
      msgOutif(MSG__VERB," ", 
	       "Couldn't identify chop coordinates, continuing anyway", status); 
    } 

  }

  /* Get the local offset coordinate system. */
  switch ( cellCode ) {
    
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
  sprintf ( skyRefX, "[OFFSET] %.0f [%s]", refX, loclCrd );
  sprintf ( skyRefY, "[OFFSET] %.0f [%s]", refY, loclCrd );

  /* Get the scanning coordinates. */
  strcpy ( scanCrd, loclCrd );

  /* Get the map and scan parameters for rasters. */
  if ( strcmp ( samMode, "raster" ) == 0
       && strcmp ( swMode, "pssw" ) == 0 ) {

    /* Get the map parameters. */
    gsdac_get0i ( gsd, "C6XNP", numPtsX, status );
    gsdac_get0i ( gsd, "C6YNP", numPtsY, status );
    gsdac_get0d ( gsd, "C6DX", &dx, status );
    gsdac_get0d ( gsd, "C6DY", &dy, status );  

    gsdac_get0d ( gsd, "C4SX", mapX, status );
    gsdac_get0d ( gsd, "C4SY", mapY, status );

    /* Get the scan parameters. */
    gsdac_get0i ( gsd, "C3SRT", &scanTime, status );
    gsdac_get0c ( gsd, "c6sd", obsDirection, status );
    gsdac_get0l ( gsd, "c6rev", &scanRev, status );

    if ( *status != SAI__OK ) {
      *status = SAI__ERROR;
      errRep ( "gsdac_getMapVars", "Error getting map & scan parameters", status );
      return; 
    }

    /* Get the map height and width. */
    *mapHght = *numPtsY * dy;
    *mapWdth = *numPtsX * dx;

    /* Get the scan velocity and spacing. */
    if ( strncmp ( obsDirection, "HORIZONTAL", 10 ) == 0 ) {          
      *scanVel = dx / scanTime;
      *scanDy = dy;
    } else if ( strncmp ( obsDirection, "VERTICAL", 8 ) == 0 ) {
      *scanVel = dy / scanTime;
      *scanDy = dx;
    } else {
      *status = SAI__ERROR;
      errRep ( "gsdac_getMapVars", "Error getting scan velocity", 
               status );
      return;
    }

    if ( scanRev == '1' ) strcpy ( scanPat, "BOUSTROPHEDON" );
    else strcpy ( scanPat, "RASTER" );

  } 

}
