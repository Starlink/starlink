/*
*+
*  Name:
*     gsdac_getWCS.c

*  Purpose:
*     Determines the time and pointing values for each 
*     time step in the observation.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_getWCS ( const gsdVars *gsdVars, const unsigned int stepNum,
*                    const int subBandNum, const dasFlag dasFlag
*                    gsdWCS *wcs, int *status )

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD headers and arrays
*     stepNum = const unsigned int (Given)
*        Number of this time step
*     subBandNum = const int (Given)
*        Number of this subband
*     dasFlag = const dasFlag (Given)
*        DAS file structure type
*     wcs = gsdWCS* (Given and Returned)
*        Time and Pointing values
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*    This routine calculates the pointing, time, and airmass 
*    values to fill the JCMTState. NOTE: adequate memory for the
*    arrays must be allocated prior to calling this function.

*  Authors:
*     J.Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2008-02-14 (JB):
*        Original
*     2008-02-20 (JB):
*        Calculate TAI for each time step
*     2008-02-21 (JB):
*        Fix TAI calculations for rasters
*     2008-02-26 (JB):
*        Only calculate values for this time step & subarray
*     2008-02-27 (JB):
*        Fill KeyMaps and call atlWcspx
*     2008-02-28 (JB):
*        Replace subsysNum with subBandNum
*     2008-03-04 (JB):
*        Use updated version of atlWcspx.

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
*     Many of the values are currently kludged with defaults.
*     These are indicated by //k.
*-
*/

/* Standard includes */
#include <string.h>
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"

/* SMURF includes */
#include "gsdac.h"
#include "smurf_par.h"

#define SOLSID 1.00273790935

#define FUNC_NAME "gsdac_getWCS.c"

void gsdac_getWCS ( const gsdVars *gsdVars, const unsigned int stepNum,
                    const int subBandNum, const dasFlag dasFlag, 
                    gsdWCS *wcs, int *status )

{

  /* Local variables */
  AstKeyMap *cellMap;         /* Ast KeyMap for cell description */
  double cellV2YRad;          /* position angle of cell y axis in radians */
  double cellX2YRad;          /* angle between cell y axis and x axis
                                 in radians */
  double coordIn[3];          /* input coordinates to transformation */
  double coordOut[3];         /* output coordinates from transformation */
  int dataDims[3];            /* dimensions of data */
  AstKeyMap *datePointing;    /* Ast KeyMap for pointing and times */
  char dateString[SZFITSCARD];/* temporary string for date conversions. */
  int day;                    /* days */
  double dLST;                /* difference in LSTs */
  double dPos[2];             /* RA/Dec offsets */
  double dut1;                /* UT1-UTC correction */
  AstFrameSet *frame;         /* frame for pointing */
  int hour;                   /* hours */
  int i;                      /* loop counter */
  char iDate[10];             /* date in specx string format */
  long index;                 /* index into array data */
  char iTime[9];              /* time in specx string format */
  int LSRFlg;                 /* LSRFlg for velocity frame/def'n */
  double LSTStart;            /* start LST time */
  int min;                    /* minutes */
  int month;                  /* months */
  double offsetX;             /* x offset in arcsec */
  double offsetY;             /* y offset in arcsec */
  double sec;                 /* seconds */
  double TAIStart;            /* start TAI time */
  AstTimeFrame *tempFrame = NULL; /* AstTimeFrame for retrieving TAI times */
  const char *tempString;     /* temporary string */
  AstTimeFrame *tFrame = NULL;  /* AstTimeFrame for retrieving TAI times */
  const char *UTCString;      /* UTC time as a string */
  double UTCTime;             /* UTC time */
  int year;                   /* year */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* To get the TAI times, first retrieve the starting LST, then for
     each time step calculate the difference in the LSTs and add it
     to the starting TAI. */

  /* Parse the OBS_UT1D to get the year, month, and day. */
  sprintf ( dateString, "%8.4f", gsdVars->obsUT1d );
  sscanf ( dateString, "%04d.%02d%02d", &year, &month, &day );

  /* Parse time to get hour/min/sec. */
  hour = (double)( (int)gsdVars->obsUT1h );
  min = (double) ( (int)( ( gsdVars->obsUT1h - hour ) * 60.0 ) );
  sec = ( ( ( gsdVars->obsUT1h - hour ) * 60.0 ) - min ) * 60.0;

  /* Set up the timeframe. */
  tFrame = astTimeFrame ( "timescale=UT1" );

  astSet ( tFrame, "TimeOrigin=%04d-%02d-%02dT%02d:%02d:%f", 
           year, month, day, hour, min, sec );

  /* Apply the UT1-UTC correction. */
  dut1 = gsdVars->obsUT1C * 86400.0;
  astSet ( tFrame, "DUT1=%f", dut1 );

  /* Set the telescope location. */
  if ( gsdVars->telLatitude > 0 )
    astSet ( tFrame, "obslat=N%f", gsdVars->telLatitude );
  else
    astSet ( tFrame, "obslat=S%f", gsdVars->telLatitude ); 
  if ( gsdVars->telLongitude > 0 )   
    astSet ( tFrame, "obslon=W%f", gsdVars->telLongitude );
  else
    astSet ( tFrame, "obslon=E%f", gsdVars->telLongitude ); 

  /* Make a copy so that we don't mess with the original
     UTC-based TimeFrame (this is a kludge to avoid a current
     glitch in the UTC->LAST conversion). */
  tempFrame = astCopy ( tFrame );     

  /* Get the LST start. */
  astSet ( tempFrame, "timescale=LAST" );
  LSTStart = astGetD ( tempFrame, "TimeOrigin" );

  /* Get the LST in hours. */
  LSTStart = ( LSTStart - (int)LSTStart ) * 24.0;

  /* Figure out what coordinates we are tracking in.  Then 
     the base can be copied from the corresponding CENTRE_
     value in the GSD header.  Remember that the GSD file
     stored RAs in degrees!  Do this check first in case
     we encounter EQ as the centreCode. */
  switch ( gsdVars->centreCode ) {
    
    case COORD_AZ:
      wcs->baseTr1 = gsdVars->centreAz;
      wcs->baseTr2 = gsdVars->centreEl;
      break;
    case COORD_EQ:
      *status = SAI__ERROR;
      errRep ( FUNC_NAME, "Equatorial coordinates not supported", status );
      return;
      /*wcs->baseTr1 = gsdVars->obsLST - 
	               ( gsdVars->centreDec * 24.0 / 360.0 );
      if ( wcs->baseTr1 < 0 ) wcs->baseTr1 += 24.0;
      wcs->baseTr2 = gsdVars->centreEl;*/
      break;
    case COORD_RD:
      wcs->baseTr1 = gsdVars->centreRA * 24.0 / 360.;
      wcs->baseTr2 = gsdVars->centreDec;
      break;
    case COORD_RB:
      wcs->baseTr1 = gsdVars->centreRA1950 * 24.0 / 360.;
      wcs->baseTr2 = gsdVars->centreDec1950;
      break;
    case COORD_RJ:
      wcs->baseTr1 = gsdVars->centreRA2000 * 24.0 / 360;
      wcs->baseTr2 = gsdVars->centreDec2000;
      break;
    case COORD_GA:
      wcs->baseTr1 = gsdVars->centreGL;
      wcs->baseTr2 = gsdVars->centreGB;
      break;
    default:
      *status = SAI__ERROR;
      errRep ( FUNC_NAME, "Error getting tracking coordinates of base", 
               status );
      return;
      break;

  }

  /* Get the index into the observing area.  For rasters, this is
     the row number (incremented on new rows) and for grids it is
     the grid offset. */
  if ( gsdVars->obsContinuous ) {
    wcs->index = (int)( stepNum / gsdVars->nScanPts );
  } else {
    wcs->index = stepNum;
  }

  /* If this is a raster, work out the LST from the scan_time
     and the number of map points in each scan. */
  if ( gsdVars->obsContinuous ) {

   /* Get the dLST of the start of this row. */
    index = (int) ( stepNum / gsdVars->nScanPts );
    dLST = ( gsdVars->scanTable1[index] - LSTStart ) / 24.0;

    /* Add the integration times up to this scan point. */
    dLST = dLST + ( ( stepNum % gsdVars->nScanPts ) *
                    ( gsdVars->scanTime / gsdVars->nScanPts ) );

  } else {

    /* Get the difference between the start LST and this LST
       and add it to the start TAI. */
    index = stepNum * gsdVars->nScanVars1;
    dLST = ( gsdVars->scanTable1[index] - LSTStart ) / 24.0;

  }

  /* Check for wrapping LST times. */
  if ( dLST < 0 ) dLST = dLST + 1.0;

  /* Get the start TAI. */
  astSet ( tFrame, "timescale=TAI" );  

  TAIStart = astGetD ( tFrame, "TimeOrigin" );

  /* Correct for difference between solar and sidereal time. */
  wcs->tai = TAIStart + ( dLST / SOLSID );

  /* Get the idate and itime.  We need to convert the time for this
     step to UTC and get the correct formatting. */

  astSetD ( tFrame, "TimeOrigin", wcs->tai );

  astSet ( tFrame, "timescale=UTC" );

  UTCTime = astGetD ( tFrame, "TimeOrigin" );

  tempFrame = astCopy ( tFrame );
  astClear ( tempFrame, "timeOrigin" );

  astSet ( tempFrame, "format(1)=iso.2" );
  UTCString = astFormat ( tempFrame, 1, UTCTime );

  gsdac_tranTime ( UTCString, iDate, iTime, status );

  /* Get the velocity definition. */
  gsdac_velEncode ( gsdVars->velRef, gsdVars->velDefn, &LSRFlg, status );

  if ( *status != SAI__OK ) {
    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "Error preparing values for atlWcspx", 
             status );
    return; 
  }

  /* Get the RA & Dec offsets in cells. */
  offsetX = gsdVars->mapTable[stepNum*2];
  offsetY = gsdVars->mapTable[stepNum*2 + 1];

  /* Multiply by cell sizes. */
  offsetX = offsetX * gsdVars->cellX;
  offsetY = offsetY * gsdVars->cellY;

  /* Get axis angles in radians. */
  cellV2YRad = gsdVars->cellV2Y * DD2R;
  cellX2YRad = gsdVars->cellX2Y * DD2R;

  /* Get the RA/Dec offsets. */
  dPos[0] = sin( cellV2YRad - cellX2YRad ) * offsetX +
            sin( cellV2YRad ) * offsetY;
  dPos[1] = cos( cellV2YRad - cellX2YRad ) * offsetX +
            cos( cellV2YRad ) * offsetY;

  /* Create the keymaps. */
  datePointing = astKeyMap( "" );
  cellMap = astKeyMap( "" ); 

  /* Fill the keymaps from the input GSD. */
  astMapPut0I( datePointing, "JFREST(1)", 
               gsdVars->restFreqs[subBandNum]*1000000.0, "" );

  /* Check CENTRE_CODE (for now...). */
  switch ( gsdVars->centreCode ) {
    case COORD_AZ:
      astMapPut0D( datePointing, "RA_DEC(1)", gsdVars->centreAz, "" );
      astMapPut0D( datePointing, "RA_DEC(2)", gsdVars->centreEl, "" ); 
      break;
    case COORD_RD:
      astMapPut0D( datePointing, "RA_DEC(1)", gsdVars->centreRA, "" );
      astMapPut0D( datePointing, "RA_DEC(2)", gsdVars->centreDec, "" ); 
      break;
    case COORD_RB:
      astMapPut0D( datePointing, "RA_DEC(1)", gsdVars->centreRA1950, "" );
      astMapPut0D( datePointing, "RA_DEC(2)", gsdVars->centreDec1950, "" ); 
      break;
    case COORD_RJ:
      astMapPut0D( datePointing, "RA_DEC(1)", gsdVars->centreRA2000, "" );
      astMapPut0D( datePointing, "RA_DEC(2)", gsdVars->centreDec2000, "" ); 
      break;
    case COORD_GA:
      astMapPut0D( datePointing, "RA_DEC(1)", gsdVars->centreGL, "" );
      astMapPut0D( datePointing, "RA_DEC(2)", gsdVars->centreGB, "" ); 
      break;
  }            

  astMapPut0I( datePointing, "DPOS(1)", dPos[0], "" );
  astMapPut0I( datePointing, "DPOS(2)", dPos[1], "" );
  astMapPut0C( datePointing, "IDATE", iDate, "" );
  astMapPut0C( datePointing, "ITIME", iTime, "" ); 
  astMapPut0I( datePointing, "LSRFLG", LSRFlg, "" );

  if ( dasFlag == DAS_CROSS_CORR || dasFlag == DAS_TP )
    astMapPut0D( datePointing, "V_SETL(4)", 0, "" );
  else
    astMapPut0D( datePointing, "V_SETL(4)", gsdVars->velocity, "" );
  astMapPut0I( datePointing, "JFCEN(1)", 
               gsdVars->centreFreqs[subBandNum]*1000000.0, "" ); 
  astMapPut0I( datePointing, "JFINC(1)", 
               gsdVars->freqRes[subBandNum]*1000000.0, "" );
  astMapPut0I( datePointing, "IFFREQ(1)", 
               gsdVars->totIFs[subBandNum], "" );
  astMapPut0I( datePointing, "CENTRECODE", gsdVars->centreCode, "" );

  astMapPut0D( cellMap, "CELLSIZE(1)", gsdVars->cellX, "" );
  astMapPut0D( cellMap, "CELLSIZE(2)", gsdVars->cellY, "" );
  astMapPut0D( cellMap, "POSANGLE", gsdVars->cellV2Y, "" );
  astMapPut0I( cellMap, "CELLCODE", gsdVars->cellCode, "" );

  /* Get the dimensions of the data array. */
  dataDims[0] = gsdVars->nMapPtsX;
  dataDims[1] = gsdVars->nMapPtsY;
  dataDims[2] = gsdVars->nBEChansOut;

  /* Get a frameset describing the mapping from cell to sky. */
  atlWcspx ( datePointing, cellMap, dataDims, gsdVars->telLongitude * -1.0, 
             gsdVars->telLatitude, &frame, status );

  if ( *status != SAI__OK ) {

    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "Couldn't create FrameSet for grid map", 
             status );
    return; 
  }

  /* Get Az and El of current cell. */
  wcs->acAz = 0.0;//k
  wcs->acEl = 0.0;//k

  /* Calculate airmass from El of current cell. */
  wcs->airmass = 0.0;//k

  coordIn[0] = gsdVars->mapTable[stepNum*2];
  coordIn[1] = gsdVars->mapTable[stepNum*2+1];
  coordIn[2] = 0.0;

  astTranN( frame, 1, 3, 1, coordIn, 1, 3, 1, coordOut );//k

  wcs->acTr1 = coordOut[0];
  wcs->acTr2 = coordOut[1];

  wcs->azAng = 0.0;
  wcs->baseAz = 0.0;//k
  wcs->baseEl = 0.0;//k 
  wcs->trAng = 0.0;//k

  astAnnul( frame );

}
