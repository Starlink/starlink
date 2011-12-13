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
*                    const int subBandNum, const dasFlag dasFlag,
*                    const double *lineFreqs, const double *IFFreqs,
*                    gsdWCS *wcs, AstFrameSet **WCSFrame,
*                    int *status )

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD headers and arrays.
*     stepNum = const unsigned int (Given)
*        Number of this time step.
*     subBandNum = const int (Given)
*        Number of this subband.
*     dasFlag = const dasFlag (Given)
*        DAS file structure type.
*     lineFreqs = const double* (Given)
*        Molecular transition lines for each subband.
*     IFFreqs = const double* (Given)
*        IF for each subband.
*     wcs = gsdWCS** (Given and Returned)
*        Time and Pointing values.
*     WCSFrame = AstFrameSet* (Given and Returned)
*        WCS frameset of RA/Dec and frequency.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*    This routine calculates the pointing, time, and airmass
*    values to fill the JCMTState. NOTE: adequate memory for the
*    arrays must be allocated prior to calling this function.

*  Authors:
*     J.Balfour (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History :
*     2008-02-14 (JB):
*        Original.
*     2008-02-20 (JB):
*        Calculate TAI for each time step.
*     2008-02-21 (JB):
*        Fix TAI calculations for rasters.
*     2008-02-26 (JB):
*        Only calculate values for this time step & subarray.
*     2008-02-27 (JB):
*        Fill KeyMaps and call atlWcspx.
*     2008-02-28 (JB):
*        Replace subsysNum with subBandNum.
*     2008-03-04 (JB):
*        Use updated version of atlWcspx.
*     2008-03-07 (JB):
*        Convert to radians before calling atlWcspx.
*     2008-03-18 (JB):
*        Add debugging statements.
*     2008-03-19 (JB):
*        Calculate offsets.
*     2008-03-21 (JB):
*        Use debug flag.
*     2008-03-25 (JB):
*        Return WCSFrame.
*     2008-04-03 (JB):
*        Correct LST offsets for rasters.
*     2008-04-04 (TIMJ):
*        Store DUT1 in WCS frameset for later use by specwrite.
*     2008-04-08 (JB):
*        Use 1,1 as centre of grid, adjust CELL offsets accordingly.
*     2008-04-16 (JB):
*        Use crpix instead of dims as argument to atlWcspx.
*     2008-04-21 (JB):
*        Check special configuration flag.
*     2008-04-22 (JB):
*        Set IFFREQ to default of -4 GHz.
*     2008-04-23 (JB):
*        Use frequencies from matchFreqs for refchan/IF.
*     2008-04-24 (JB):
*        Fix calculation of TCS_TR_ANG.
*     2008-04-25 (JB):
*        Update TCS_TAI to match frameset.

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*-
*/

/* Standard includes */
#include <string.h>
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "star/slalib.h"
#include "mers.h"
#include "star/atl.h"

/* SMURF includes */
#include "gsdac.h"
#include "smurf_par.h"

#define SOLSID 1.00273790935

#define DEBUGON 0

#define FUNC_NAME "gsdac_getWCS.c"

void gsdac_getWCS ( const gsdVars *gsdVars, const unsigned int stepNum,
                    const int subBandNum, const dasFlag dasFlag,
                    const double *lineFreqs, const double *IFFreqs,
                    gsdWCS *wcs, AstFrameSet **WCSFrame,
                    int *status )

{

  /* Local variables */
  AstKeyMap *cellMap;         /* Ast KeyMap for cell description */
  double coordIn[3];          /* input coordinates to transformation */
  double coordOut[3];         /* output coordinates from transformation */
  double crpix[3];            /* pixel centres of data */
  AstKeyMap *datePointing;    /* Ast KeyMap for pointing and times */
  char dateString[SZFITSCARD];/* temporary string for date conversions. */
  int day;                    /* days */
  double dLST;                /* difference in LSTs */
  double dut1;                /* UT1-UTC correction */
  AstFrameSet *frame;         /* frame for pointing */
  int hour;                   /* hours */
  char iDate[10];             /* date in specx string format */
  long index;                 /* index into array data */
  char iTime[9];              /* time in specx string format */
  int LSRFlg;                 /* LSRFlg for velocity frame/def'n */
  AstTimeFrame *LSTFrame = NULL; /* AstTimeFrame for retrieving LST times */
  double LSTStart;            /* start LST time */
  int min;                    /* minutes */
  int month;                  /* months */
  double sec;                 /* seconds */
  AstTimeFrame *TAIFrame;     /* AstTimeFrame for TCS_TAI */
  double TAIStart;            /* start TAI time */
  AstTimeFrame *tFrame = NULL;  /* AstTimeFrame for retrieving TAI times */
  AstTimeFrame *UTCFrame = NULL; /* AstTimeFrame for retrieving UTC times */
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
  tFrame = astTimeFrame ( "TimeScale=UT1" );

  astSet ( tFrame, "TimeOrigin=%04d-%02d-%02dT%02d:%02d:%f",
           year, month, day, hour, min, sec );

  /* Apply the UT1-UTC correction. */
  dut1 = gsdVars->obsUT1C * SPD;
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
  LSTFrame = astCopy ( tFrame );

  /* Get the LST start. */
  astSet ( LSTFrame, "TimeScale=LAST" );

  LSTStart = astGetD ( LSTFrame, "TimeOrigin" );

  /* Get the LST in hours. */
  LSTStart = ( LSTStart - (int)LSTStart ) * 24.0;

  /* Figure out what coordinates we are tracking in.  Then
     the base can be copied from the corresponding CENTRE_
     value in the GSD header.  Remember that the GSD file
     stored RA/Decs in degrees!  Do this check first in case
     we encounter EQ as the centreCode. */
  switch ( gsdVars->centreCode ) {

    case COORD_AZ:
      wcs->baseTr1 = gsdVars->centreAz * AST__DD2R;
      wcs->baseTr2 = gsdVars->centreEl * AST__DD2R;
      break;
    case COORD_EQ:
      *status = SAI__ERROR;
      errRep ( FUNC_NAME, "Equatorial coordinates not supported", status );
      return;
      break;
    case COORD_RD:
      wcs->baseTr1 = gsdVars->centreRA * AST__DD2R;
      wcs->baseTr2 = gsdVars->centreDec * AST__DD2R;
      break;
    case COORD_RB:
      wcs->baseTr1 = gsdVars->centreRA1950 * AST__DD2R;
      wcs->baseTr2 = gsdVars->centreDec1950 * AST__DD2R;
      break;
    case COORD_RJ:
      wcs->baseTr1 = gsdVars->centreRA2000 * AST__DD2R;
      wcs->baseTr2 = gsdVars->centreDec2000 * AST__DD2R;
      break;
    case COORD_GA:
      wcs->baseTr1 = gsdVars->centreGL * AST__DD2R;
      wcs->baseTr2 = gsdVars->centreGB * AST__DD2R;
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
    wcs->index = stepNum / gsdVars->nScanPts + 1;
  } else {
    wcs->index = stepNum + 1;
  }

  /* If this is a raster, work out the LST from the scan_time
     and the number of map points in each scan. */
  if ( gsdVars->obsContinuous ) {

    /* Get the dLST of the start of this row. */
    index = ( stepNum / gsdVars->nScanPts ) * gsdVars->nScanVars1;
    dLST = ( gsdVars->scanTable1[index] - LSTStart ) / 24.0;

    /* Add the integration times up to this scan point. */
    dLST = dLST + ( ( stepNum % gsdVars->nScanPts ) *
                    ( gsdVars->scanTime / gsdVars->nScanPts ) /
                    SPD );

  } else {

    /* Get the difference between the start LST and this LST
       and add it to the start TAI. */
    index = stepNum * gsdVars->nScanVars1;
    dLST = ( gsdVars->scanTable1[index] - LSTStart ) / 24.0;

  }

  /* Check for wrapping LST times. */
  if ( dLST < 0 ) dLST = dLST + 1.0;

  /* Get the start TAI. */
  astSet ( tFrame, "TimeScale=TAI" );

  TAIStart = astGetD ( tFrame, "TimeOrigin" );

  /* Get the idate and itime.  We need to convert the time for this
     step to UTC and get the correct formatting.  The time origin is
     the starting TAI time plus the LST offset (corrected for the
     difference between solar and sidereal time). */
  astSetD ( tFrame, "TimeOrigin", TAIStart + ( dLST / SOLSID ) );

  astSet ( tFrame, "Timescale=UTC" );

  UTCTime = astGetD ( tFrame, "TimeOrigin" );

  UTCFrame = astCopy ( tFrame );
  astClear ( UTCFrame, "TimeOrigin" );

  astSet ( UTCFrame, "format(1)=iso.2" );
  UTCString = astFormat ( UTCFrame, 1, UTCTime );

  gsdac_tranTime ( UTCString, iDate, iTime, status );

  /* Get the velocity definition. */
  gsdac_velEncode ( gsdVars->velRef, gsdVars->velDefn, &LSRFlg, status );

  if ( *status != SAI__OK ) {
    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "Error preparing values for atlWcspx",
             status );
    return;
  }

  /* Create the keymaps. */
  datePointing = astKeyMap( " " );
  cellMap = astKeyMap( " " );

  if ( subBandNum == 0 && DEBUGON ) printf ( "CENTRE (base) RA_DEC (radians) : %f %f\n", wcs->baseTr1, wcs->baseTr2 );

  /* Fill the keymaps from the input GSD. */
  astMapPut0I ( datePointing, "JFREST(1)",
                lineFreqs[subBandNum]*1000000.0, "" );
  astMapPut0D( datePointing, "RA_DEC(1)", wcs->baseTr1 / AST__DD2R, "" );
  astMapPut0D( datePointing, "RA_DEC(2)", wcs->baseTr2 / AST__DD2R, "" );
  astMapPut0I( datePointing, "DPOS(1)", 0.0, "" );
  astMapPut0I( datePointing, "DPOS(2)", 0.0, "" );
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
  astMapPut0D( datePointing, "IFFREQ(1)", IFFreqs[subBandNum], "" );
  astMapPut0I( datePointing, "CENTRECODE", gsdVars->centreCode, "" );

  /* Convert cell sizes to radians. */
  astMapPut0D( cellMap, "CELLSIZE(1)", gsdVars->cellX, "" );
  astMapPut0D( cellMap, "CELLSIZE(2)", gsdVars->cellY, "" );
  astMapPut0D( cellMap, "POSANGLE", gsdVars->cellV2Y, "" );
  astMapPut0I( cellMap, "CELLCODE", gsdVars->cellCode, "" );

  /* Set the centre of the grid to 0,0, and set the centre
     of the third axis to the reference channel (offset
     from channel which contains IFFreq).*/
  crpix[0] = 0.0;
  crpix[1] = 0.0;
  crpix[2] = ( (double)( gsdVars->BEChans[subBandNum] ) / 2.0 ) +
             ( ( IFFreqs[subBandNum] - gsdVars->totIFs[subBandNum] ) /
               ( fabs( gsdVars->freqRes[subBandNum] ) / 1000.0 ) );

  /* Get a frameset describing the mapping from cell to sky. */
  atlWcspx ( datePointing, cellMap, crpix,
             gsdVars->telLongitude * -1.0 * AST__DD2R,
             gsdVars->telLatitude * AST__DD2R, WCSFrame, status );

  /* Set DUT1 attribute (which will not have been set in the ATL call
     since it is not needed there but specwrite uses it) */
  astSet ( *WCSFrame, "DUT1(1)=%f,DUT1(3)=%f", dut1, dut1 );

  if ( subBandNum == 0 && DEBUGON )
    printf("Epoch = %s MJD=%f\n",astGetC(*WCSFrame,"Epoch"),
            slaEpj2d(astGetD(*WCSFrame,"Epoch")));

  /* Update TCS_TAI to match frameset. */
  TAIFrame = astTimeFrame ( "TimeScale=TDB,System=JEPOCH" );
  astSetD ( TAIFrame, "TimeOrigin", astGetD ( *WCSFrame, "Epoch" ) );
  astSet ( TAIFrame, "System=MJD,TimeScale=TAI" );
  wcs->tai = astGetD ( TAIFrame, "TimeOrigin" );

  /* Make a copy of the frameset for local calculations. */
  frame = astCopy ( *WCSFrame );

  if ( *status != SAI__OK ) {

    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "Couldn't create FrameSet for grid map",
             status );
    return;
  }

  /* Calculate the centre in tracking. */
  coordIn[0] = 0.0;
  coordIn[1] = 0.0;
  coordIn[2] = 0.0;

  astTranN( frame, 1, 3, 1, coordIn, 1, 3, 1, coordOut );

  astNorm ( frame, coordOut );

  if ( subBandNum == 0 && DEBUGON )
    printf ( "GRID (base) coordinates  : %f %f\n",
             coordIn[0], coordIn[1] );

  if ( subBandNum == 0 && DEBUGON )
    printf ( "RA/Dec (base) coordinates (radians)  : %f %f\n",
             coordOut[0], coordOut[1] );

  wcs->baseTr1 = coordOut[0];
  wcs->baseTr2 = coordOut[1];

  /* Calculate the cell offsets in tracking. */
  coordIn[0] = -gsdVars->mapTable[stepNum*2];
  coordIn[1] = gsdVars->mapTable[stepNum*2+1];

  astTranN( frame, 1, 3, 1, coordIn, 1, 3, 1, coordOut );

  astNorm ( frame, coordOut );

  if ( subBandNum == 0 && DEBUGON )
    printf ( "GRID (offset) coordinates  : %f %f\n",
             coordIn[0], coordIn[1] );

  if ( subBandNum == 0 && DEBUGON )
    printf ( "RA/Dec (offset) coordinates (radians)  : %f %f\n",
             coordOut[0], coordOut[1] );

  wcs->acTr1 = coordOut[0];
  wcs->acTr2 = coordOut[1];

  astSetC( frame, "System(1)", "AZEL" );

  /* Get centre in AZEL. */
  coordIn[0] = 0.0;
  coordIn[1] = 0.0;

  astTranN( frame, 1, 3, 1, coordIn, 1, 3, 1, coordOut );

  astNorm ( frame, coordOut );

  if ( subBandNum == 0 && DEBUGON )
    printf ( "GRID (base) coordinates  : %f %f\n",
             coordIn[0], coordIn[1] );

  if ( subBandNum == 0 && DEBUGON )
    printf ( "AZEL (base) coordinates (radians)  : %f %f\n",
             coordOut[0], coordOut[1] );

  wcs->baseAz = coordOut[0];
  wcs->baseEl = coordOut[1];

  /* Calculate the cell offsets in AZEL. */
  coordIn[0] = -gsdVars->mapTable[stepNum*2];
  coordIn[1] = gsdVars->mapTable[stepNum*2+1];

  astTranN( frame, 1, 3, 1, coordIn, 1, 3, 1, coordOut );

  astNorm ( frame, coordOut );

  if ( subBandNum == 0 && DEBUGON )
    printf ( "GRID (offset) coordinates  : %f %f\n",
             coordIn[0], coordIn[1] );

  if ( subBandNum == 0 && DEBUGON )
    printf ( "AZEL (offset) coordinates (radians)  : %f %f\n",
             coordOut[0], coordOut[1] );

  wcs->acAz = coordOut[0];
  wcs->acEl = coordOut[1];

  /* Calculate airmass from El of current cell. */
  wcs->airmass = slaAirmas( AST__DPIBY2 - wcs->acEl );

  /* Focal plane is AZEL so AZ angle is 0. */
  wcs->azAng = 0.0;

  /* The angle between the focal plane and PA=0 and PA=0 in the
     tracking coordinate frame is determined from the hour angle,
     the declination, and the latitude. */
  astSet( frame, "System(1)=GAPPT" );
  astTranN( frame, 1, 3, 1, coordIn, 1, 3, 1, coordOut );

  coordOut[0] = ( ( LSTStart + ( dLST * 24.0 ) ) * 2.0 * AST__DPI / 24.0 ) - coordOut[0];

  astNorm ( frame, coordOut );

  wcs->trAng = slaPa( coordOut[0], coordOut[1], gsdVars->telLatitude );

}
