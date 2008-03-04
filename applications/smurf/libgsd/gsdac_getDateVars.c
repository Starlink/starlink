/*
*+
*  Name:
*     gsdac_getDateVars.c

*  Purpose:
*     Process the date and time data from the GSD file to fill
*     the required FITS headers.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_getDateVars ( const gsdVars *gsdVars, const char *backend, 
*                         const unsigned int obsNum, dateVars *dateVars, 
*                         int *status )

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD file access parameters
*     backend = const char* (Given)
*        Name of the backend
*     obsNum = const unsigned int (Given)
*        Observation number
*     dateVars = dateVars* (Given and Returned)
*        Date and time variables
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     This routine performs the required conversions to get 
*     the date and time variables required for ACSIS FITS headers.

*  Authors:
*     J.Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2008-02-05 (JB):
*        Original
*     2008-02-14 (JB):
*        Use gsdVars struct to store headers/arrays
*     2008-02-28 (JB):
*        Replace subsysNum with subBandNum
*     2008-02-28 (JB):
*        Use dateVars struct
*     2008-03-04 (JB):
*        Use number of scans actually completed.

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
#include "sae_par.h"

/* SMURF includes */
#include "smurf_par.h"
#include "gsdac.h"

#define FUNC_NAME "gsdac_getDateVars"

void gsdac_getDateVars ( const gsdVars *gsdVars, const char *backend, 
                         const unsigned int obsNum, dateVars *dateVars, 
                         int *status )

{

  /* Local variables */
  char curChar;               /* character pointer */
  char dateString[SZFITSCARD];/* temporary string for date conversions. */
  int day = 0;                /* days */
  double dut1 = 0.0;          /* UT1-UTC correction */
  int hour = 0;               /* hours */
  int i = 0;                  /* loop counter */
  int min = 0;                /* minutes */
  int month = 0;              /* months */
  float sec = 0.0;            /* seconds */
  int tableDims = 0;          /* dimensionality of data table */
  unsigned int tableSize;     /* number of elements of data table */
  AstTimeFrame *tempFrame = NULL; /* AstTimeFrame for UT1-UTC conversion */
  const char *tempString;     /* temporary string */
  AstTimeFrame *tFrame = NULL;  /* AstTimeFrame for UT1-UTC conversion */
  double utcEnd = 0.0;        /* end UTC time */
  double utcHSTend = 0.0;     /* end HST time */
  double utcHSTstart = 0.0;   /* start HST time */
  double utcStart = 0.0;      /* start UTC time */
  int year = 0;               /* years */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the DATE-OBS. */

  /* Parse date to get year/month/day. */
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
  astSet ( tFrame, "timescale=UTC" );

  utcStart = astGetD ( tFrame, "timeOrigin" );

  astSet ( tFrame, "format(1)=iso.2" );

  tempFrame = astCopy ( tFrame );
  astClear ( tempFrame, "timeOrigin" );
  astSet ( tempFrame, "format(1)=iso.2" );

  tempString = astFormat ( tempFrame, 1, utcStart );

  /* Copy the UTC date string. */  
  strncpy ( dateVars->dateObs, tempString, 10 );
  dateVars->dateObs[10] =  'T';
  strcpy ( &(dateVars->dateObs[11]), &(tempString[11]) );

  /* Get the OBSID. */

  /* Check to see that the backend is DAS. */
  if ( strncmp ( backend, "DAS", 3 ) != 0 ) {
    *status = SAI__ERROR;
    msgSetc ( "BACKEND", backend );
    errRep ( "gsdac_getDateVars", "Backend ^BACKEND not supported", status );
    return;
  }

  sprintf ( dateVars->obsID, "%s_%05d_%04d%02d%02dT%02d%02d%02d", 
            backend, obsNum, year, month, day, hour, min, (int)sec );

  /* Get the DATE-END. This will be DATE-OBS + ( last LST - first LST ). */ 
  tableSize = gsdVars->nScanVars1 * gsdVars->nScan;
  tableDims = gsdVars->nScanVars1;

  utcEnd = utcStart + ( gsdVars->scanTable1[tableSize-tableDims] - 
                        gsdVars->scanTable1[0] ) 
                         / 24.0;

  tempString = astFormat ( tempFrame, 1, utcEnd );

  /* Copy the UTC date string. */    
  strncpy ( dateVars->dateEnd, tempString, 10 );
  dateVars->dateEnd[10] =  'T';
  strcpy ( &(dateVars->dateEnd[11]), &(tempString[11]) );

  /* Get the LSTstart. */ 
  hour = (int)( gsdVars->scanTable1[0] );
  min = (int)(( gsdVars->scanTable1[0] - hour ) * 60.0 );
  sec = ( gsdVars->scanTable1[0] - hour - ( min / 60.0 ) ) * 3600.0;

  sprintf ( dateVars->LSTstart, "%02d:%02d:%6.4f", hour, min, sec );

  /* Get the LSTend. */
  hour = (int)( gsdVars->scanTable1[tableSize-tableDims] ); 
  min = (int)(( gsdVars->scanTable1[tableSize-tableDims] - hour ) * 60.0 );
  sec = ( gsdVars->scanTable1[tableSize-tableDims] - hour - 
        ( min / 60.0 ) ) * 3600.0;

  sprintf ( dateVars->LSTend, "%02d:%02d:%6.4f", hour, min, sec );

  /* Get the HSTstart and HSTend. */
  utcHSTstart = utcStart - 10.0 / 24.0;
  utcHSTend = utcEnd - 10.0 / 24.0;

  tempString = astFormat ( tempFrame, 1, utcHSTstart ); 

  /* Copy the HST date string. */    
  strncpy ( dateVars->HSTstart, tempString, 10 );
  dateVars->HSTstart[10] =  'T';
  strcpy ( &(dateVars->HSTstart[11]), &(tempString[11]) ); 

  tempString = astFormat ( tempFrame, 1, utcHSTend ); 

  /* Copy the HST date string. */    
  strncpy ( dateVars->HSTend, tempString, 10 );
  dateVars->HSTend[10] =  'T';
  strcpy ( &(dateVars->HSTend[11]), &(tempString[11]) ); 

}
