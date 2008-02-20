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
*     gsdac_getWCS ( const gsdVars *gsdVars, const int nSteps,
*                    gsdWCS *wcs, int *status )

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD headers and arrays
*     nSteps = const int (Given)
*        Number of steps in the observation
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

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"

/* SMURF includes */
#include "gsdac.h"
#include "smurf_par.h"

#define FUNC_NAME "gsdac_getWCS.c"

void gsdac_getWCS ( const gsdVars *gsdVars, const int nSteps,
                    gsdWCS *wcs, int *status )

{

  /* Local variables */
  char dateString[SZFITSCARD];/* temporary string for date conversions. */
  int day;                    /* days */
  double dut1;                /* UT1-UTC correction */
  int hour;                   /* hours */
  int i;                      /* loop counter */
  long index;                 /* index into array data */
  double LSTStart;            /* start LST time */
  int min;                    /* minutes */
  int month;                  /* months */
  float sec;                  /* seconds */
  double TAIStart;            /* start TAI time */
  AstTimeFrame *tempFrame = NULL; /* AstTimeFrame for retrieving TAI times */
  const char *tempString;     /* temporary string */
  AstTimeFrame *tFrame = NULL;  /* AstTimeFrame for retrieving TAI times */
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
  LSTStart = astGetD ( tempFrame, "timeOrigin" );

  /* Get the LST in hours. */
  LSTStart = ( LSTStart - (int)LSTStart ) * 24.0;

  /* Get the start TAI. */
  astSet ( tFrame, "timescale=TAI" );  
  TAIStart = astGetD ( tFrame, "timeOrigin" );

  for ( i = 0; i < nSteps; i++ ) {

    /* Kludges for now... */
    wcs[i].airmass = 0.0;
    wcs[i].acAz = 0.0;
    wcs[i].acEl = 0.0;
    wcs[i].acTr1 = 0.0;
    wcs[i].acTr2 = 0.0;    
    wcs[i].azAng = 0.0;  
    wcs[i].baseAz = 0.0;
    wcs[i].baseEl = 0.0;  
    wcs[i].baseTr1 = 0.0;
    wcs[i].baseTr2 = 0.0;
    wcs[i].index = 0.0;

    /* Get the difference between the start LST and this LST
       and add it to the start TAI. */
    index = i * gsdVars->nScanVars1;
    wcs[i].tai = TAIStart + 
                 ( ( (gsdVars->scanTable1)[index] - LSTStart ) / 24.0 );

    wcs[i].trAng = 0.0;//k

  }  

}
