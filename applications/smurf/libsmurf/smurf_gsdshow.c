/*
*+
*  Name:
*     GSDSHOW

*  Purpose:
*     Display the contents of headers and arrays for GSD files.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_gsdshow( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Opens a GSD file for reading, and checks the version (currently
*     only supports GSD version 5.3). Then displays the contents of
*     the headers and data arrays.

*  ADAM Parameters:
*     DESCRIPTIONS = _LOGICAL (Read)
*          Flag for showing header descriptions. [FALSE]
*     IN = CHAR (Read)
*          Name of the input GSD file to be listed.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     SHOWDATA = _LOGICAL (Read)
*          Flag for showing data array. [FALSE]

*  Related Applications:
*     SMURF: GSD2ACSIS;
*     GSDPRINT;
*     SPECX;
*     JCMTDR

*  Authors:
*     Jen Balfour (JAC, UBC)
*     {enter_new_authors_here}

*  History:
*     18-APR-2008 (JB):
*        Original version.
*     22-APR-2008 (JB):
*        Use T/F for logical values.

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
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/


#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Standard includes */
#include <string.h>
#include <stdio.h>
#include <math.h>

/* STARLINK includes */
#include "mers.h"
#include "gsd.h"
#include "sae_par.h"
#include "par.h"

/* SMURF includes */
#include "libgsd/gsdac_struct.h"
#include "libgsd/gsdac.h"
#include "smurflib.h"

#define FUNC_NAME "smurf_gsdshow"
#define TASK_NAME "GSDSHOW"

#define MAXNAME 80
#define NRECEP 2

void smurf_gsdshow ( int *status ) {

  /* Local variables */
  dasFlag dasFlag;            /* file structure type */
  char filename[MAXNAME];     /* name of the GSD file */
  FILE *fptr;                 /* pointer to GSD file */
  struct gsdac_gsd_struct gsd; /* GSD file access parameters */
  gsdVars gsdVars;            /* GSD headers and arrays */
  char logConv[2];            /* logical converter */
  char label[41];             /* GSD label */
  int nitem;                  /* number of items in GSD file */
  int nObs;                   /* integer value of observation number */
  int showData;               /* print out data array? */
  int showDesc;               /* print out descriptions? */
  float version;              /* GSD file version */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the user defined input and output file names. */
  parGet0c ( "IN", filename, MAXNAME, status );

  if ( *status != SAI__OK ) return;

  /* Find out if the user wants data. */
  parGet0l ( "DESCRIPTIONS", &showDesc, status );

  if ( *status == PAR__NULL ) {
    errAnnul ( status );
    showDesc = 0;
  }

  /* Find out if the user wants data. */
  parGet0l ( "SHOWDATA", &showData, status );

  if ( *status == PAR__NULL ) {
    errAnnul ( status );
    showData = 0;
  }

  if ( *status != SAI__OK ) return;

  msgOutif(MSG__VERB," ",
	     "Opening GSD file for reading", status);

  /* Open the GSD file. */
  CALLGSD( gsdOpenRead ( filename, &version, label, &nitem,
                         &fptr, &(gsd.fileDsc), &(gsd.itemDsc),
			 &(gsd.dataPtr) ),
           status,
           errRep ( FUNC_NAME, "gsdOpenRead : Could not find input GSD file.", status ); );

  if ( *status != SAI__OK ) return;

  msgOutif(MSG__VERB," ",
	     "Checking backend name", status);

  /* Check to see if this is DAS or AOSC data. */
  gsdac_get0c ( &gsd, "C1BKE", gsdVars.backend, status );

  if ( *status != SAI__OK ) return;

  if ( strncmp ( gsdVars.backend, "DAS", 3 ) != 0
       && strncmp ( gsdVars.backend, "AOSC", 4 ) != 0 ) {
    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "File does not contain DAS or AOSC data", status );
    return;
  }

  msgOutif(MSG__VERB," ",
	     "Checking version of GSD file", status);

  /* Check the version of the opened file. */
  if ( fabs(version - 5.300 ) > 0.0001 ) {
    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "GSD version is not 5.300.", status );
    return;
  }

  /* Get the file structure flag. */
  gsdac_getDASFlag ( &gsd, &dasFlag, status );

  if ( *status != SAI__OK ) return;

  if ( dasFlag == DAS_NONE )
    msgOutif(MSG__VERB," ",
	     "DAS file type is DAS_NONE", status);
  else if ( dasFlag == DAS_TP )
    msgOutif(MSG__VERB," ",
	     "DAS file type is DAS_TP", status);
  else if ( dasFlag == DAS_CONT_CAL )
    msgOutif(MSG__VERB," ",
	     "DAS file type is DAS_CONT_CAL", status);
  else if ( dasFlag == DAS_CROSS_CORR )
    msgOutif(MSG__VERB," ",
	     "DAS file type is DAS_CROSS_CORR", status);
  else {
    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "Could not identify DAS file type.", status );
    return;
  }

  /* Get the GSD file headers and data arrays. */
  gsdac_getGSDVars ( &gsd, dasFlag, &gsdVars, status );

  if ( *status != SAI__OK ) {
    errRep ( FUNC_NAME, "Couldn't get GSD headers and arrays.", status );
    return;
  }

  /* Close the GSD file. */
  msgOutif(MSG__VERB," ",
	     "Closing GSD file", status);

  /* Close the GSD file. */
  CALLGSD( gsdClose ( fptr, gsd.fileDsc, gsd.itemDsc, gsd.dataPtr ),
           status,
           errRep ( FUNC_NAME, "gsdClose : Error closing GSD file.", status ); );

  if ( *status != SAI__OK ) return;

  /* Write out the contents of the file. */

  logConv[1] = '\0';

  gsdac_printHdr ( "C1TEL", "TEL_NAME", 5,
                   "Telescope name",
                   &(gsdVars.telName[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C1PID", "PROJECT", 5,
                   "Identifies the observing program",
                   &(gsdVars.project[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C1OBS", "PROJECT_OBS_1", 5,
                   "Name of the primary observer",
                   &(gsdVars.projectObs1[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C1ONA1", "PROJECT_OBS_2", 5,
                   "Name of the support scientist",
                   &(gsdVars.projectObs2[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C1ONA2", "PROJECT_OBS_3", 5,
                   "Name of the telescope operator",
                   &(gsdVars.projectObs3[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C1SNA1", "CENTRE_NAME_1", 5,
                   "Source name part 1",
                   &(gsdVars.object1[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C1SNA2", "CENTRE_NAME_2", 5,
                   "Source name part 2 or altern. name",
                   &(gsdVars.object2[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4CSC", "CENTRE_COORDS", 5,
                   "Character code of commanded centre or source coordinate system",
                   &(gsdVars.centreCoords[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4CECO", "CENTRE_CODE", 1,
                   "Centre coords. AZ= 1;EQ=3;RD=4;RB=6;RJ=7;GA=8",
                   &(gsdVars.centreCode), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4EPT", "EPOCH_TYPE", 5,
                   "Type of epoch, JULIAN, BESSELIAN or APPARENT",
                   &(gsdVars.epochType[0]), 0, 0, showDesc, status );

  if ( (int)gsdVars.centreMoving )
    logConv[0] = 'T';
  else
    logConv[0] = 'F';

  gsdac_printHdr ( "C4MCF", "CENTRE_MOVING", 5,
                   "Centre moving flag (solar system object)",
                   logConv, 0, 0, showDesc, status );

  gsdac_printHdr ( "C4EPH", "CENTRE_EPOCH", 2,
                   "Date of the RA",
                   &(gsdVars.centreEpoch), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4ERA", "CENTRE_RA1950", 2,
                   "Right ascension of source for EPOCH",
                   &(gsdVars.centreRA1950), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4EDEC", "CENTRE_DEC1950", 2,
                   "Declination of source for EPOCH",
                   &(gsdVars.centreDec1950), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4RADATE", "CENTRE_RA", 2,
                   "Right Ascension of date",
                   &(gsdVars.centreRA), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4DECDATE", "CENTRE_DEC", 2,
                   "Declination of date",
                   &(gsdVars.centreDec), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4RA2000", "CENTRE_RA2000", 2,
                   "Right ascension J2000",
                   &(gsdVars.centreRA2000), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4EDEC2000", "CENTRE_DEC2000", 2,
                   "Declination J2000",
                   &(gsdVars.centreDec2000), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4GL", "CENTRE_GL", 2,
                   "Galactic longitude",
                   &(gsdVars.centreGL), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4GB", "CENTRE_GB", 2,
                   "Galactic latitude",
                   &(gsdVars.centreGB), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4AZ", "CENTRE_AZ", 2,
                   "Azimuth at observation date",
                   &(gsdVars.centreAz), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4EL", "CENTRE_EL", 2,
                   "Elevation at observation date",
                   &(gsdVars.centreEl), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4LSC", "CELL_COORDS", 5,
                   "har. code for local x-y coord.system",
                   &(gsdVars.cellCoords[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C6FC", "CELL_CODE", 1,
                   "Local x-y AZ= 1;EQ=3;RD=4;RB=6;RJ=7;GA=8",
                   &(gsdVars.cellCode), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4ODCO", "CELL_UNIT", 5,
                   "Units of cell and mapping coordinates;offset definition code",
                   &(gsdVars.cellUnit[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C6DX", "CELL_X", 2,
                   "Cell x dim,; descriptive origin item 1",
                   &(gsdVars.cellX), 0, 0, showDesc, status );

  gsdac_printHdr ( "C6DY", "CELL_Y", 2,
                   "Cell y dimension; descriptive origin item 2",
                   &(gsdVars.cellY), 0, 0, showDesc, status );

  gsdac_printHdr ( "C6MSA", "CELL_V2X", 2,
                   "Scanning angle - angle from local vertical to x axis measured CW",
                   &(gsdVars.cellV2X), 0, 0, showDesc, status );

  gsdac_printHdr ( "CELL_V2Y", "CELL_V2Y", 2,
                   "Position angle of cell y axis (CCW)",
                   &(gsdVars.cellV2Y), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4AXY", "CELL_X2Y", 2,
                   "Angle between cell y axis and x-axis (CCW)",
                   &(gsdVars.cellX2Y), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4SX", "CENTRE_OFFSET_X", 2,
                   "Commanded x centre position (JCMT cells wrt to centre; NRAO abs. degrees)",
                   &(gsdVars.centreOffsetX), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4SY", "CENTRE_OFFSET_Y", 2,
                   "Commanded y centre position (JCMT cells wrt to centre; NRAO abs. degrees)",
                   &(gsdVars.centreOffsetY), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4RX", "REFERENCE_X", 2,
                   "Reference x position (JCMT cells wrt to centre; NRAO abs. degrees)",
                   &(gsdVars.referenceX), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4RY", "REFERENCE_Y", 2,
                   "Reference y position (JCMT cells wrt to centre; NRAO abs. degrees)",
                   &(gsdVars.referenceY), 0, 0, showDesc, status );

  gsdac_printHdr ( "C1HGT", "TEL_HEIGHT", 2,
                   "Height of telescope above sea level",
                   &(gsdVars.telHeight), 0, 0, showDesc, status );

  gsdac_printHdr ( "C1LONG", "TEL_LONGITUDE", 2,
                   "Geographical longitude of telescope (West +ve)",
                   &(gsdVars.telLongitude), 0, 0, showDesc, status );

  gsdac_printHdr ( "C1LAT", "TEL_LATITUDE", 2,
                   "Geodetic latitude of telescope (North +ve)",
                   &(gsdVars.telLatitude), 0, 0, showDesc, status );

  /* Cast observation number to an int (why is it a double?) */
  nObs = (int)gsdVars.nObs;
  gsdac_printHdr ( "C1SNO", "NOBS", 1,
                   "Observation number",
                   &(nObs), 0, 0, showDesc, status );

  gsdac_printHdr ( "C6ST", "OBS_TYPE", 5,
                   "Type of observation",
                   &(gsdVars.obsType[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C1RCV", "FRONTEND", 5,
                   "Name of the frontend",
                   &(gsdVars.frontend[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C1FTYP", "FE_TYPE", 5,
                   "Type of frontend",
                   &(gsdVars.FEType[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C1BKE", "BACKEND", 5,
                   "Name of the backend",
                   &(gsdVars.backend[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C1BTYP", "BE_TYPE", 5,
                   "Type of backend",
                   &(gsdVars.BEType[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3DAT", "OBS_UT1D", 2,
                   "UT1 date of observation",
                   &(gsdVars.obsUT1d), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3UT", "OBS_UT1H", 2,
                   "UT1 hour of observation",
                   &(gsdVars.obsUT1h), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3UT1C", "OBS_UT1C", 2,
                   "UT1-UTC correction interpolated from time service telex (in days)",
                   &(gsdVars.obsUT1C), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3LST", "OBS_LST", 2,
                   "Local sidereal time at the start of the observation",
                   &(gsdVars.obsLST), 0, 0, showDesc, status );

  if ( (int)gsdVars.obsCalibration )
    logConv[0] = 'T';
  else
    logConv[0] = 'F';

  gsdac_printHdr ( "C3CAL", "OBS_CALIBRATION", 5,
                   "Calibration observation?",
                   logConv, 0, 0, showDesc, status );

  if ( (int)gsdVars.obsCentre )
    logConv[0] = 'T';
  else
    logConv[0] = 'F';

  gsdac_printHdr ( "C3CEN", "OBS_CENTRE", 5,
                   "Centre moves between scans?",
                   logConv, 0, 0, showDesc, status );

  if ( (int)gsdVars.obsContinuous )
    logConv[0] = 'T';
  else
    logConv[0] = 'F';

  gsdac_printHdr ( "C3FLY", "OBS_CONTINUOUS", 5,
                   "Data taken on the fly or in discrete mode?",
                   logConv, 0, 0, showDesc, status );

  if ( (int)gsdVars.obsFocus )
    logConv[0] = 'T';
  else
    logConv[0] = 'F';

  gsdac_printHdr ( "C3FOCUS", "OBS_FOCUS", 5,
                   "Focus observation?",
                   logConv, 0, 0, showDesc, status );

  if ( (int)gsdVars.obsMap )
    logConv[0] = 'T';
  else
    logConv[0] = 'F';

  gsdac_printHdr ( "C3MAP", "OBS_MAP", 5,
                   "Map observation?",
                   logConv, 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NPP", "NO_MAP_DIMS", 1,
                   "Number of dimension in the map table",
                   &(gsdVars.nMapDims), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NMAP", "NO_MAP_PNTS", 1,
                   "Number of map points",
                   &(gsdVars.nMapPts), 0, 0, showDesc, status );

  gsdac_printHdr ( "C6XNP", "NO_X_MAP_PNTS", 1,
                   "X map dimension; number of points in the x-direction",
                   &(gsdVars.nMapPtsX), 0, 0, showDesc, status );

  gsdac_printHdr ( "C6YNP", "NO_Y_MAP_PNTS", 1,
                   "Y map dimension; number of points in the y-direction",
                   &(gsdVars.nMapPtsY), 0, 0, showDesc, status );

  gsdac_printHdr ( "C6XGC", "X_MAP_START", 3,
                   "X coordinate of the first map point",
                   &(gsdVars.mapStartX), 0, 0, showDesc, status );

  gsdac_printHdr ( "C6YGC", "Y_MAP_START", 3,
                   "Y coordinate of the first map point",
                   &(gsdVars.mapStartY), 0, 0, showDesc, status );

  if ( (int)gsdVars.scanRev )
    logConv[0] = 'T';
  else
    logConv[0] = 'F';

  gsdac_printHdr ( "C6REV", "SCAN_REVERSAL", 5,
                   "Map rows scanned in alternate directions?",
                   logConv, 0, 0, showDesc, status );

  gsdac_printHdr ( "C6SD", "OBS_DIRECTION", 5,
                   "Map rows are in X (horizontal) or Y(vertical) direction",
                   &(gsdVars.obsDirection[0]), 0, 0, showDesc, status );

  if ( (int)gsdVars.mapPosX )
    logConv[0] = 'T';
  else
    logConv[0] = 'F';

  gsdac_printHdr ( "C6XPOS", "X_MAP_POSITIVE", 5,
                   "In first row x increases (TRUE) or decreases (FALSE)",
                   logConv, 0, 0, showDesc, status );

  if ( (int)gsdVars.mapPosY )
    logConv[0] = 'T';
  else
    logConv[0] = 'F';

  gsdac_printHdr ( "C6YPOS", "Y_MAP_POSITIVE", 5,
                   "In first row y increases (TRUE) or decreases (FALSE)",
                   &(logConv), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NIS", "NO_SCANS", 1,
                   "Number of scans",
                   &(gsdVars.noScans), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NSAMPLE", "NSCAN", 1,
                   "Number of scans done",
                   &(gsdVars.nScan), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NO_SCAN_VARS1", "NO_SCAN_VARS1", 1,
                   "Number of scan table 1 variables",
                   &(gsdVars.nScanVars1), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NO_SCAN_VARS2", "NO_SCAN_VARS2", 1,
                   "Number of scan table 2 variables",
                   &(gsdVars.nScanVars2), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3SRT", "SCAN_TIME", 1,
                   "Total time of scan (=total integration time if OBS_CONTINUOUS = .FALSE.)",
                   &(gsdVars.scanTime), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3MXP", "NO_SCAN_PNTS", 1,
                   "Maximum number of map points done in a phase",
                   &(gsdVars.nScanPts), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NCI", "NO_CYCLES", 1,
                   "Maximum number of cycles in the scan",
                   &(gsdVars.noCycles), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NCYCLE", "NCYCLE", 1,
                   "Number of cycles done in the scan",
                   &(gsdVars.nCycle), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3CL", "CYCLE_TIME", 1,
                   "Duration of each cycle",
                   &(gsdVars.cycleTime), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NCP", "NO_CYCLE_PNTS", 1,
                   "Total number of xy positions observed during a cycle",
                   &(gsdVars.noCyclePts), 0, 0, showDesc, status );

  gsdac_printHdr ( "C6NP", "NCYCLE_PNTS", 1,
                   "Number of sky points completed in the observation",
                   &(gsdVars.nCyclePts), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NSV", "NO_PHASE_VARS", 1,
                   "Number of phase table variables",
                   &(gsdVars.nPhaseVars), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3PPC", "NO_PHASES", 1,
                   "Number of phases per cycle",
                   &(gsdVars.nPhases), 0, 0, showDesc, status );

  gsdac_printHdr ( "C5AT", "TAMB", 2,
                   "Ambient temperature",
                   &(gsdVars.tamb), 0, 0, showDesc, status );

  gsdac_printHdr ( "C5PRS", "PAMB", 2,
                   "Mean atmospheric pressure",
                   &(gsdVars.pamb), 0, 0, showDesc, status );

  gsdac_printHdr ( "C5RH", "HAMB", 2,
                   "Mean atmospheric relative humidity",
                   &(gsdVars.hamb), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4AZERR", "SDIS(7)", 2,
                   "DAZ:Net Az offset at start (inc.tracker ball setting and user correction)",
                   &(gsdVars.errAz), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4ELERR", "SDIS(8)", 2,
                   "DEL:Net El offset at start (inc.tracker ball setting and user correction)",
                   &(gsdVars.errEl), 0, 0, showDesc, status );

  gsdac_printHdr ( "UAZ", "SDIS(36)", 2,
                   "User az correction",
                   &(gsdVars.userAz), 0, 0, showDesc, status );

  gsdac_printHdr ( "UEL", "SDIS(37)", 2,
                   "User el correction",
                   &(gsdVars.userEl), 0, 0, showDesc, status );

  gsdac_printHdr ( "C7SZVRAD", "SZVRAD", 1,
                   "Number of elements of vradial array",
                   &(gsdVars.nVRad), 0, 0, showDesc, status );

  gsdac_printHdr ( "C8AAE", "APERTURE_EFF", 2,
                   "Ratio total power observed",
                   &(gsdVars.apertureEff), 0, 0, showDesc, status );

  gsdac_printHdr ( "C8ABE", "BEAM_EFF", 2,
                   "Fraction of beam in diffraction limited main beam",
                   &(gsdVars.beamEff), 0, 0, showDesc, status );

  gsdac_printHdr ( "C8GN", "ANTENNA_GAIN", 2,
                   "Antenna gain",
                   &(gsdVars.antennaGain), 0, 0, showDesc, status );

  gsdac_printHdr ( "C8EL", "ETAL", 2,
                   "Rear spillover and scattering efficiency",
                   &(gsdVars.etal), 0, 0, showDesc, status );

  gsdac_printHdr ( "C8EF", "ETAFSS", 2,
                   "Forward spillover and scattering efficiency",
                   &(gsdVars.etafss), 0, 0, showDesc, status );

  if ( (int)gsdVars.chopping )
    logConv[0] = 'T';
  else
    logConv[0] = 'F';

  gsdac_printHdr ( "C4SM", "CHOPPING", 5,
                   "Secondary mirror is chopping",
                   logConv, 0, 0, showDesc, status );

  gsdac_printHdr ( "C4FUN", "WAVEFORM", 5,
                   "Secondary mirror chopping waveform",
                   &(gsdVars.chopWaveform[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4FRQ", "FREQUENCY", 3,
                   "Secondary mirror chopping period",
                   &(gsdVars.chopFrequency), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4SMCO", "COORDS", 5,
                   "Secondary mirror chopping coordinate system",
                   &(gsdVars.chopCoords[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4THROW", "THROW", 3,
                   "Secondary mirror chop throw",
                   &(gsdVars.chopThrow), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4POSANG", "POSANG", 3,
                   "Secondary mirror chop position angle",
                   &(gsdVars.chopPA), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4OFFS_EW", "OFFS_EW", 3,
                   "Secondary mirror offset parallel to lower axis (East-West Tilt)",
                   &(gsdVars.smuOffsEW), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4OFFS_NS", "OFFS_NS", 3,
                   "Secondary mirror offset parallel to upper axis (North-South Tilt)",
                   &(gsdVars.smuOffsNS), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4X", "X", 3,
                   "Secondary mirror absolute X position at observation start",
                   &(gsdVars.smuX), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4Y", "Y", 3,
                   "Secondary mirror absolute Y position at observation start",
                   &(gsdVars.smuY), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4Z", "Z", 3,
                   "Secondary mirror absolute Z position at observation start",
                   &(gsdVars.smuZ), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4EW_SCALE", "EW_AMPL_SCALE", 3,
                   "Secondary mirror ew chop scale",
                   &(gsdVars.smuEWScale), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4NS_SCALE", "NS_AMPL_SCALE", 3,
                   "Secondary mirror ns chop scale",
                   &(gsdVars.smuNSScale), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4EW_ENCODER", "AMPL_E_SET", 1,
                   "Secondary mirror ew encoder value",
                   &(gsdVars.smuEWEnc), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4NS_ENCODER", "AMPL_N_SET", 1,
                   "Secondary mirror ns encoder value",
                   &(gsdVars.smuNSEnc), 0, 0, showDesc, status );

  gsdac_printHdr ( "C2FV", "DX", 3,
                   "Secondary mirror x displacement from nominal at observation start",
                   &(gsdVars.smuDX), 0, 0, showDesc, status );

  gsdac_printHdr ( "C2FL", "DY", 3,
                   "Secondary mirror y displacement from nominal at observation start",
                   &(gsdVars.smuDY), 0, 0, showDesc, status );

  gsdac_printHdr ( "C2FR", "DZ", 3,
                   "Secondary mirror z displacement from nominal at observation start",
                   &(gsdVars.smuDZ), 0, 0, showDesc, status );

  gsdac_printHdr ( "C4MOCO", "TEL_COORDS", 5,
                   "Mounting of telescope; defined as LOWER",
                   &(gsdVars.telCoords[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NFOC", "NO_FE_O_CH", 1,
                   "NO_FE_O_CH:No. of frontend output channels",
                   &(gsdVars.nFEChans), 0, 0, showDesc, status );

  gsdac_printHdr ( "C7VR", "VELOCITY", 2,
                   "Radial velocity of the source",
                   &(gsdVars.velocity), 0, 0, showDesc, status );

  gsdac_printHdr ( "C12TCOLD", "T_COLD", 3,
                   "Cold load temperature",
                   &(gsdVars.tCold), 0, 0, showDesc, status );

  gsdac_printHdr ( "C12TAMB", "T_HOT", 3,
                   "Ambient load temperature",
                   &(gsdVars.tHot), 0, 0, showDesc, status );

  gsdac_printHdr ( "C12VDEF", "VEL_DEFN", 5,
                   "Velocity definition code - radio, optical, or relativistic",
                   &(gsdVars.velDefn[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C12VREF", "VEL_REF", 5,
                   "Velocity frame of reference - LSR, Bary-, Helio-, or Geo- centric",
                   &(gsdVars.velRef[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NRC", "NO_BE_I_CH", 1,
                   "Number of backend input channels",
                   &(gsdVars.nBEChansIn), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NCH", "NO_BE_O_CH", 1,
                   "Number of backend output channels",
                   &(gsdVars.nBEChansOut), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NRS", "NO_BES", 1,
                   "Number of backend sections",
                   &(gsdVars.nBESections), 0, 0, showDesc, status );

  gsdac_printHdr ( "C7BCV", "BAD_CHANNEL", 3,
                   "Bad channel value",
                   &(gsdVars.badVal), 0, 0, showDesc, status );

  gsdac_printHdr ( "C12CAL", "DATA_UNITS", 5,
                   "Units of spectrum data",
                   &(gsdVars.dataUnits[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C6MODE", "SWITCH_MODE", 5,
                   "Observation mode",
                   &(gsdVars.swMode[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C12CALTASK", "BE_CAL_TASK", 5,
                   "Calibration instrument used (FE, BE, or USER)",
                   &(gsdVars.calInstrument[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C12CALTYPE", "BE_CAL_TYPE", 5,
                   "Type of calibration (THREELOADS or TWOLOADS)",
                   &(gsdVars.calType[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C12REDMODE", "BE_RED_MODE", 5,
                   "Way of calibrating the data (RATIO or DIFFERENCE)",
                   &(gsdVars.calMode[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3NOIFPBES", "NO_IF_PER_BES", 1,
                   "Number of IF inputs to each section (2 for correlator, 1 for AOS)",
                   &(gsdVars.IFPerSection), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3CONFIGNR", "DAS_CONF_NR", 1,
                   "Backend configuration",
                   &(gsdVars.BEConfig), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3DASOUTPUT", "DAS_OUTPUT", 5,
                   "Description of output in DAS DATA (SPECTRUM, T_REC, T_SYS, etc.)",
                   &(gsdVars.dataOutput[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3DASCALSRC", "DAS_CAL_SOURCE", 5,
                   "DAS calibration source for backend calibration (POWER or DATA)",
                   &(gsdVars.calSource[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3DASSHFTFRAC", "DAS_SHIFT_FRAC", 3,
                   "DAS calibration source for backend calibration (POWER or DATA)",
                   &(gsdVars.shiftFrac), 0, 0, showDesc, status );

  gsdac_printHdr ( "C7TAU225", "CSO_TAU", 3,
                   "CSO tau at 225GHz",
                   &(gsdVars.tau225), 0, 0, showDesc, status );

  gsdac_printHdr ( "C7TAURMS", "CSO_TAU_RMS", 3,
                   "CSO tau rms",
                   &(gsdVars.tauRMS), 0, 0, showDesc, status );

  gsdac_printHdr ( "C7TAUTIME", "CSO_YYMMDDHHMM", 5,
                   "CSO tau time (YYMMDDHHMM)",
                   &(gsdVars.tauTime[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C7SEEING", "SAO_SEEING", 3,
                   "Seeing at JCMT",
                   &(gsdVars.seeing), 0, 0, showDesc, status );

  gsdac_printHdr ( "C7SEETIME", "SAO_YYMMDDHHMM", 5,
                   "SAO seeing time (YYMMDDHHMM)",
                   &(gsdVars.seeTime[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3POLARITY", "C3POLARITY", 5,
                   "Frontend Polarity",
                   &(gsdVars.polarity[0]), 0, 0, showDesc, status );

  gsdac_printHdr ( "C3SBMODE", "C3SBMODE", 5,
                   "Sideband mode",
                   &(gsdVars.sbMode[0]), 0, 0, showDesc, status );

  if ( dasFlag == DAS_TP || dasFlag == DAS_CROSS_CORR ) {

    gsdac_printHdr ( "C55NPH", "DAS_NO_PHASE", 1,
                     "DAS number of phases for interferometry observing",
                     &(gsdVars.IFONPhase), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55NCYC", "DAS_NO_CYCLES", 1,
                     "DAS number of phases for interferometry observing",
                     &(gsdVars.IFONCycle), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55NINT", "DAS_NINTCYCLE", 1,
                     "DAS number of phases for interferometry observing",
                     &(gsdVars.IFONIntCycle), 0, 0, showDesc, status );

  }

  if ( dasFlag == DAS_CROSS_CORR ) {

    gsdac_printHdr ( "C55NCORR", "DAS_NCORRCYCLE", 1,
                     "DAS number of correlation cycles",
                     &(gsdVars.nCorrCycle), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55DASPRBIT", "DAS_PROC_BITS", 1,
                     "DAS data processing done",
                     &(gsdVars.procBits), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55DASPRLOC", "DAS_PROC_LOC", 5,
                     "Description of where processing is done",
                     &(gsdVars.procLoc[0]), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55LX", "RXJ.LX", 2,
                     "RXJ X length of projected baseline in metres",
                     &(gsdVars.RXJLengthX), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55LY", "RXJ.LY", 2,
                     "RXJ Y length of projected baseline in metres",
                     &(gsdVars.RXJLengthY), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55LZ", "RXJ.LZ", 2,
                     "RXJ Z length of projected baseline in metres",
                     &(gsdVars.RXJLengthZ), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55A", "RXJ.A", 2,
                     "RXJ Coefficient of sin term in expression for fringe rate (metres)",
                     &(gsdVars.RXJSin), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55B", "RXJ.B", 2,
                     "RXJ Coefficient of cos term in expression for fringe rate (metres)",
                     &(gsdVars.RXJCos), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55C", "RXJ.C", 2,
                     "RXJ Coefficient of constant term in expression for fringe rate (metres)",
                     &(gsdVars.RXJConstant), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55CSOSW", "RXJ.CSO_SWITCH", 1,
                     "RXJ Delay setting of RXJ micro for CSO side",
                     &(gsdVars.RXJCSOSwitch), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55JCMTSW", "RXJ.JCMT_SWITCH", 1,
                     "RXJ Delay setting of RXJ micro for JCMT side",
                     &(gsdVars.RXJJCMTSwitch), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55SECOND", "RXJ.NSECS", 1,
                     "RXJ Number of the tick on which integration started",
                     &(gsdVars.RXJNSecs), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55ABSORB", "RXJ.ABSORB", 5,
                     "CSO Position of absorber IN or OUT",
                     &(gsdVars.CSOAbsorb[0]), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55TAU", "RXJ.CSO_TAU", 3,
                     "CSO TAU value",
                     &(gsdVars.CSOTau), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55DAZ", "RXJ.DAZ", 3,
                     "CSO Position offset in az (arcsec)",
                     &(gsdVars.CSODAz), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55DEL", "RXJ.DEL", 3,
                     "CSO Position offset in elevation (arcsec)",
                     &(gsdVars.CSODEl), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55RA", "RXJ.RA", 2,
                     "CSO RA",
                     &(gsdVars.CSORA), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55DEC", "RXJ.DEC", 2,
                     "CSO Dec",
                     &(gsdVars.CSODec), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55EPOCH", "RXJ.EPOCH", 2,
                     "CSO Epoch of CSO RA and Dec",
                     &(gsdVars.CSOEpoch), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55PAZ", "RXJ.PAZ", 3,
                     "CSO Pointing offset in az (arcsec)",
                     &(gsdVars.CSOPAz), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55PEL", "RXJ.PEL", 3,
                     "CSO Pointing offset in el (arcsec)",
                     &(gsdVars.CSOPEl), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55TRACK", "RXJ.TRACK", 5,
                     "CSO Track mode of telescope Y or N",
                     &(gsdVars.CSOTrack[0]), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55FMODE", "RXJ.FMODE", 5,
                     "CSO Focus mode of CSO",
                     &(gsdVars.CSOFocus[0]), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55FX", "RXJ.FX", 3,
                     "CSO X position of focus",
                     &(gsdVars.CSOFocusX), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55FY", "RXJ.FY", 3,
                     "CSO Y position of focus",
                     &(gsdVars.CSOFocusY), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55FZ", "RXJ.FZ", 3,
                     "CSO Z position of focus",
                     &(gsdVars.CSOFocusZ), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55VLSR", "RXJ.VLSR", 3,
                     "CSO LSR velocity of source (km/s)",
                     &(gsdVars.CSOVelocity), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55VOFF", "RXJ.COFF", 3,
                     "CSO velocity offset (km/s)",
                     &(gsdVars.CSOVelOffset), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55VRAD", "RXJ.VRAD", 3,
                     "CSO radial velocity (km/s)",
                     &(gsdVars.CSORadVel), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55PLOCK", "RXJ.PLOCK", 5,
                     "CSO Phase lock status L or U",
                     &(gsdVars.CSOPhaseLock[0]), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55RFREQ", "RXJ.RFREQ", 2,
                     "CSO Rest frequency of line (GHz)",
                     &(gsdVars.CSORestFreq), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55IFFREQ", "RXJ.IFFREQ", 2,
                     "CSO IF frequency (GHz)",
                     &(gsdVars.CSOIFFreq), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55LOFREQ", "RXJ.LOFREQ", 2,
                     "CSO LO frequency (GHz)",
                     &(gsdVars.CSOLOFreq), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55FREQOFF", "RXJ.FREQOFF", 2,
                     "CSO frequency offset (GHz)",
                     &(gsdVars.CSOFreqOffset), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55SIDEBAND", "RXJ.SIDEBAND", 5,
                     "CSO Sideband U or L",
                     &(gsdVars.CSOSideband[0]), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55MHN", "RXJ.MHN", 1,
                     "CSO Multiplier Harmonic number",
                     &(gsdVars.CSOMultHarm), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55CSOSTATUS", "RXJ.CSO_STATUS", 1,
                     "CSO overall status 0 = bad 1 = good",
                     &(gsdVars.CSOStatus), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55TELAZ", "TEL.SDIS(9)", 2,
                     "CENTRE_AZ from tel sdis array",
                     &(gsdVars.telAz), 0, 0, showDesc, status );

    gsdac_printHdr ( "C55TELEL", "TEL.SDIS(10)", 2,
                     "CENTRE_EL from tel sdis array",
                     &(gsdVars.telEl), 0, 0, showDesc, status );

  }

  if ( dasFlag == DAS_CROSS_CORR ) {

    gsdac_printHdr ( "C55FENUOBS", "FE_NUOBS", 2,
                     "Observing frequencies",
                     gsdVars.FEFreqs, 1,
                     gsdVars.nFEChans,
                     showDesc, status );

    gsdac_printHdr ( "C55FESBSIGN", "FE_SB_SIGN", 1,
                     "FE sideband signs",
                     gsdVars.FESBSigns, 1,
                     gsdVars.nFEChans,
                     showDesc, status );

    gsdac_printHdr ( "C55FENULO", "FE_NULO", 2,
                     "FE LO frequencies",
                     gsdVars.FELOFreqs, 1,
                     gsdVars.nFEChans,
                     showDesc, status );

  }

  if ( dasFlag == DAS_NONE || dasFlag == DAS_CONT_CAL ) {

    gsdac_printHdr ( "C7VRADIAL", "C7VRADIAL", 2,
                     "Array for computer radial velocities",
                     gsdVars.vRadial, 1,
                     gsdVars.nVRad,
                     showDesc, status );

  }

  gsdac_printHdr ( "C12SCAN_VARS1", "SCAN_VARS1", 5,
                   "Names of the cols. of scan table1",
                   gsdVars.scanVars1, 1,
                   gsdVars.nScanVars1,
                   showDesc, status );

  gsdac_printHdr ( "C12SCAN_VARS2", "SCAN_VARS2", 5,
                   "Names of the cols. of scan table2",
                   gsdVars.scanVars2, 1,
                   gsdVars.nScanVars2,
                   showDesc, status );

  gsdac_printHdr ( "C12SCAN_TABLE_1", "SCAN_TABLE1", 3,
                   "Begin scan table",
                   gsdVars.scanTable1, 1,
                   gsdVars.nScanVars1 * gsdVars.noScans,
                   showDesc, status );

  gsdac_printHdr ( "C12SCAN_TABLE_2", "SCAN_TABLE2", 3,
                   "End scan table",
                   gsdVars.scanTable2, 1,
                   gsdVars.nScanVars2 * gsdVars.noScans,
                   showDesc, status );

  gsdac_printHdr ( "C14PHIST", "MAP_TABLE", 3,
                   "List of xy offsets for each scan",
                   gsdVars.mapTable, 1,
                   gsdVars.nMapDims * gsdVars.nMapPts,
                   showDesc, status );

  gsdac_printHdr ( "C11VD", "PHASE_VARS", 5,
                   "Names of the cols. of phase table",
                   gsdVars.phaseVars, 1,
                   gsdVars.nPhaseVars,
                   showDesc, status );

  gsdac_printHdr ( "C11PHA", "PHASE_TABLE", 3,
                   "Phase table: switching scheme dependent",
                   gsdVars.phaseTable, 1,
                   gsdVars.nPhaseVars * gsdVars.nPhases,
                   showDesc, status );

  gsdac_printHdr ( "C12CM", "BES_CORR_MODE", 1,
                   "Correlation function mode",
                   gsdVars.corrModes, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C12BM", "BES_BITMODE", 1,
                   "Correlation bit mode",
                   gsdVars.bitModes, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C3OVERLAP", "BES_OVERLAP", 3,
                   "Subband overlap",
                   gsdVars.sbOverlaps, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C3MIXNUM", "DAS_MIXER", 1,
                   "",
                   gsdVars.mixNums, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C3BESCONN", "BES_CONN", 1,
                   "BE input channels connected to this section",
                   gsdVars.BEInputChans, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C3BEINCON", "BE_IN_CONN", 1,
                   "IF output channels connected to BE input channels",
                   gsdVars.BEConnChans, 1,
                   gsdVars.nBEChansIn,
                   showDesc, status );

  gsdac_printHdr ( "C3LSPC", "NO_BES_O_CH", 1,
                   "Number of channels per backend section",
                   gsdVars.BEChans, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C3BESSPEC", "BES_SPECTRUM", 1,
                   "Subsystem nr to which each backend section belongs.",
                   gsdVars.BESubsys, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C12CF", "BES_NUOBS", 2,
                   "Centre frequencies (rest frame of source) [GHz]",
                   gsdVars.centreFreqs, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C12RF", "BES_NUREST", 2,
                   "Rest frequency [GHz]",
                   gsdVars.restFreqs, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C3BEFENULO", "BES_FE_NULO", 2,
                   "Copy of frontend LO frequency per backend section",
                   gsdVars.LOFreqs, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C3BETOTIF", "BES_TOT_IF", 2,
                   "Total IF per backend section",
                   gsdVars.totIFs, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C3BEFESB", "BES_FE_SB_SIGN", 1,
                   "Copy of frontend sideband sign per backend section",
                   gsdVars.sbSigns, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C12INFREQ", "BE_NUIN", 2,
                   "BE input frequencies [GHz]",
                   gsdVars.BEInputFreqs, 1,
                   gsdVars.nBEChansIn,
                   showDesc, status );

  gsdac_printHdr ( "C12FR", "BES_DELTANU", 3,
                   "Frequency resolution [MHz]",
                   gsdVars.freqRes, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C12BW", "BES_BANDWIDTH", 3,
                   "Bandwidth [MHz]",
                   gsdVars.bandwidths, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C12RT", "BES_T_REC", 3,
                   "Receiver temperature",
                   gsdVars.recTemps, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  if ( dasFlag == DAS_CONT_CAL ) {

    gsdac_printHdr ( "C12SST", "BES_T_SYS", 3,
                     "System temperature",
                     gsdVars.sourceSysTemps, 1,
                     gsdVars.nBESections * gsdVars.noScans,
                     showDesc, status );

    gsdac_printHdr ( "C12TSKY", "BES_T_SKY", 3,
                     "Sky temperature at last calibration",
                     gsdVars.skyTemps, 1,
                     gsdVars.nBESections * gsdVars.noScans,
                     showDesc, status );

  } else {

    gsdac_printHdr ( "C12SST", "BES_T_SYS", 3,
                     "System temperature",
                     gsdVars.sourceSysTemps, 1,
                     gsdVars.nBESections,
                     showDesc, status );

    gsdac_printHdr ( "C12TSKY", "BES_T_SKY", 3,
                     "Sky temperature at last calibration",
                     gsdVars.skyTemps, 1,
                     gsdVars.nBESections,
                     showDesc, status );

  }

  gsdac_printHdr ( "C12TTEL", "BES_T_TEL", 3,
                   "Telescope temp. from last skydip",
                   gsdVars.telTemps, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C12GAINS", "BES_GAIN", 3,
                   "Gain value (kelvins per volt or equivalent)",
                   gsdVars.gains, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C12CT", "BES_T_TEL", 3,
                   "Calibration temperature",
                   gsdVars.calTemps, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C12WO", "BES_T_TEL", 3,
                   "Water opacity",
                   gsdVars.opacities, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  if ( dasFlag == DAS_CONT_CAL ) {

    gsdac_printHdr ( "C12ETASKY", "BES_ETA_SKY", 3,
                     "Sky transmission from last calibration",
                     gsdVars.skyTrans, 1,
                     gsdVars.nBESections * gsdVars.noScans,
                     showDesc, status );

  } else {

    gsdac_printHdr ( "C12ETASKY", "BES_ETA_SKY", 3,
                     "Sky transmission from last calibration",
                     gsdVars.skyTrans, 1,
                     gsdVars.nBESections,
                     showDesc, status );

  }

  gsdac_printHdr ( "C12ALPHA", "BES_ALPHA", 3,
                   "Ratio of signal sideband to image sideband sky transmission",
                   gsdVars.alphas, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C12GS", "BES_G_S", 3,
                   "Normalizes signal sideband gain",
                   gsdVars.sbGainNorms, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  gsdac_printHdr ( "C12ETATEL", "BES_ETA_TEL", 3,
                   "Telescope transmission",
                   gsdVars.telTrans, 1,
                   gsdVars.nBESections,
                   showDesc, status );

  if ( dasFlag == DAS_CONT_CAL ) {

    gsdac_printHdr ( "C12TSKYIM", "BES_T_SKY_IM", 3,
                     "Frontend-derived Tsky, image sideband",
                     gsdVars.FETSkyIm, 1,
                     gsdVars.nBESections * gsdVars.noScans,
                     showDesc, status );

    gsdac_printHdr ( "C12ETASKYIM", "BES_ETA_SKY_IM", 3,
                     "Frontend-derived sky transmission",
                     gsdVars.FESkyTrans, 1,
                     gsdVars.nBESections * gsdVars.noScans,
                     showDesc, status );

    gsdac_printHdr ( "C12TSYSIM", "BES_T_SYS_IM", 3,
                     "Frontend-derived Tsys, image sideband",
                     gsdVars.FETSysIm, 1,
                     gsdVars.nBESections * gsdVars.noScans,
                     showDesc, status );

    gsdac_printHdr ( "C12TASKY", "BES_TA_SKY", 3,
                     "Ratio of signal sideband to image sideband sky transmission",
                     gsdVars.sbRatios, 1,
                     gsdVars.nBESections * gsdVars.noScans,
                     showDesc, status );

  } else {

    gsdac_printHdr ( "C12TSKYIM", "BES_T_SKY_IM", 3,
                     "Frontend-derived Tsky, image sideband",
                     gsdVars.FETSkyIm, 1,
                     gsdVars.nBESections,
                     showDesc, status );

    gsdac_printHdr ( "C12ETASKYIM", "BES_ETA_SKY_IM", 3,
                     "Frontend-derived sky transmission",
                     gsdVars.FESkyTrans, 1,
                     gsdVars.nBESections,
                     showDesc, status );

    gsdac_printHdr ( "C12TSYSIM", "BES_T_SYS_IM", 3,
                     "Frontend-derived Tsys, image sideband",
                     gsdVars.FETSysIm, 1,
                     gsdVars.nBESections,
                     showDesc, status );

    gsdac_printHdr ( "C12TASKY", "BES_TA_SKY", 3,
                     "Ratio of signal sideband to image sideband sky transmission",
                     gsdVars.sbRatios, 1,
                     gsdVars.nBESections,
                     showDesc, status );

  }

  gsdac_printHdr ( "C3INTT", "INTGRN_TIME", 1,
                   "Scan integration time",
                   gsdVars.intTimes, 1,
                   gsdVars.noScans,
                   showDesc, status );

  if ( showData ) {

    gsdac_printHdr ( "C13DAT", "DATA", 3,
                     "Spectrum data",
                     gsdVars.data, 1,
                     gsdVars.nBEChansOut * gsdVars.nScanPts * gsdVars.noScans,
                     showDesc, status );

  }

  if ( dasFlag == DAS_CROSS_CORR ) {

    gsdac_printHdr ( "C55HOTPOWER", "DAS_HOT_POWER", 3,
                     "Total power measurement on hot load",
                     gsdVars.hotPower, 1,
                     gsdVars.nBESections * gsdVars.IFPerSection,
                     showDesc, status );

    gsdac_printHdr ( "C55SKYPOWER", "DAS_SKY_POWER", 3,
                     "Total power measurement on hot load",
                     gsdVars.skyPower, 1,
                     gsdVars.nBESections * gsdVars.IFPerSection,
                     showDesc, status );

    gsdac_printHdr ( "C55SAM", "SAMPLES", 3,
                     "Samples to store for cross_correlation mode",
                     gsdVars.samples, 1,
                     gsdVars.nBEChansOut * gsdVars.IFONPhase *
                     gsdVars.noCycles,
                     showDesc, status );

    gsdac_printHdr ( "C55POWER", "DAS_POWER", 3,
                     "Total power measurement per subband per integration",
                     gsdVars.totPower, 1,
                     gsdVars.nBESections * gsdVars.IFPerSection *
                     gsdVars.IFONPhase * gsdVars.noCycles,
                     showDesc, status );

  } else if ( dasFlag == DAS_TP ) {

    gsdac_printHdr ( "C55POWER", "DAS_POWER", 3,
                     "Total power measurement per subband per integration",
                     gsdVars.totPower, 1,
                     gsdVars.nBESections * gsdVars.IFPerSection *
                     gsdVars.IFONPhase * gsdVars.nPhases * gsdVars.noCycles,
                     showDesc, status );

  }

}
