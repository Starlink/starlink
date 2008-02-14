/*
*+
*  Name:
*     gsdac_putFits.c

*  Purpose:
*     Retrieve values from the GSD headers to fill the FITS headers
*     in the ACSIS file.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_putFits ( const gsdVars *gsdVars, 
*                     const int subsysNum, const int obsNum, 
*                     const int utDate, const int nSteps, 
*                     const char *backend, char *recepNames[],
*                     const char *samMode, const char *obsType,
*                     const struct JCMTState *record, 
*                     const AstFitsChan *fitschan, int *status )

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD file access parameters
*     subsysNum = const int (Given)
*        Subsystem number
*     obsNum = const int (Given)
*        Observation number
*     utDate = const int (Given)
*        UT observation date
*     nSteps = const int (Given)
*        Number of time steps in observation
*     backend = const char* (Given)
*        Name of the backend
*     recepnames = char*[] (Given)
*        Names of receptors
*     samMode = const char* (Given)
*        Sample mode
*     obsType = const char* (Given)
*        Observation type
*     record = const JCMTState* (Given)
*        JCMTState headers
*     fitschan = const AstFitsChan* (Given and Returned)
*        FITS headers
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     This routine fills the AstFitsChan with the values retrieved
*     from the GSD headers.  

*  Authors:
*     J.Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2008-02-04 (JB):
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
*     Many of the values are currently kludged with defaults.
*     These are indicated by //k.
*-
*/

/* Standard includes */
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "cnf.h"

/* SMURF includes */
#include "smurf_par.h"
#include "gsdac.h"

#define FUNC_NAME "gsdac_putFits"

void gsdac_putFits ( const gsdVars *gsdVars, 
                     const int subsysNum, const int obsNum, 
                     const int utDate, const int nSteps, 
                     const char *backend, char *recepNames[],
                     const char *samMode, const char *obsType,
                     const struct JCMTState *record, 
                     const AstFitsChan *fitschan, int *status )

{

  /* Local variables */
  float amEnd;                /* airmass at end of observation */
  float amStart;              /* airmass at start of observation */
  double azEnd;               /* Azimuth at observation end (deg) */
  double azStart;             /* Azimuth at observation start (deg) */
  double bp;                  /* pressure (mbar) */
  char bwMode[SZFITSCARD];    /* ACSIS total bandwidth setup */
  char chopCrd[SZFITSCARD];   /* chopper coordinate system */
  char curChar;               /* character pointer */
  char dateEnd[SZFITSCARD];   /* UTC datetime of end of observation 
                                 in format YYYY-MM-DDTHH:MM:SS */
  char dateObs[SZFITSCARD];   /* UTC datetime of start of observation 
                                 in format YYYY-MM-DDTHH:MM:SS */
  int day;                    /* days for time conversion. */
  char doppler[SZFITSCARD];   /* doppler velocity definition */
  double elEnd;               /* elevation at observation end (deg) */
  double elStart;             /* elevation at observation start (deg) */
  double etal;                /* telescope efficiency */
  int hour;                   /* hours for time conversion. */
  char HSTend[SZFITSCARD];    /* HST at observation end in format 
                                 YYYY-MM-DDTHH:MM:SS */
  char HSTstart[SZFITSCARD];  /* HST at observation start in format 
                                 YYYY-MM-DDTHH:MM:SS */
  int i;                      /* loop counter */
  float IFchanSp;             /* TOPO IF channel spacing (Hz) */
  double IFfreq;              /* IF frequency (GHz) */
  char instrume[SZFITSCARD];  /* front-end receiver */
  double intTime;             /* total time spent integrating (s) */
  int josMin;                 /* ?? */
  char loclCrd[SZFITSCARD];   /* local offset coordinate system for 
                                 map_x / map_y */
  char LSTstart[SZFITSCARD];  /* LST at observation start in format
                                 YYYY-MM-DDTHH:MM:SS */
  char LSTend[SZFITSCARD];    /* LST at observation end in format
                                 YYYY-MM-DDTHH:MM:SS */
  float mapHght;              /* requested height of rectangle to be mapped 
                                 (arcsec) */
  float mapPA;                /* requested PA of map vertical, +ve towards
                                 +ve long */
  float mapWdth;              /* requested width of rectangle to be mapped
                                 (arcsec) */
  int min;                    /* minutes for time conversion. */
  char molecule[SZFITSCARD];  /* target molecular species */
  int month;                  /* months for time conversion. */
  int nMix;                   /* number of mixers */
  double nRefStep;            /* number of nod sets repeated */
  char object[SZFITSCARD];    /* object of interest */
  char object2[SZFITSCARD];   /* object of interest (second half of name) */
  char obsID[SZFITSCARD];     /* unique observation number in format
                                 INSTR_NNNNN_YYYYMMDDTHHMMSS */
  char obsIDs[SZFITSCARD];    /* unique observation number + subsystem
                                 number in format
                                 INSTR_NNNNN_YYYYMMDDTHHMMSS_N */
  char obsSB[SZFITSCARD];     /* observed sideband */
  int parse;                  /* flag for incorrect date string. */
  char recptors[SZFITSCARD];  /* active FE receptor IDs for this obs */
  double refChan;             /* reference IF channel no. */
  char scanCrd[SZFITSCARD];   /* coordinate system of scan */
  float scanDy;               /* scan spacing perpendicular to scan
                                 (arcsec) */
  float scanPA;               /* Scan PA rel. to lat. line; 0=lat, 
                                 90=long in scanCrd system */
  char scanPat[SZFITSCARD];   /* name of scanning scheme */
  float scanVel;              /* scan velocity (arcsec/sec) */
  char seeDatSt[SZFITSCARD];  /* time of seeingSt in format
                                 YYYY-MM-DDTHH:MM:SS */
  char skyRefX[SZFITSCARD];   /* X co-ord of reference position (arcsec) */  
  char skyRefY[SZFITSCARD];   /* Y co-ord of reference position (arcsec) */  
  char sSysObs[SZFITSCARD];   /* spectral ref. frame during observation */
  int standard;               /* true for spectral line standards */
  int startIdx;               /* index in pattern at start of observation */
  int stBetRef;               /* max number of steps between refs */
  char subBands[SZFITSCARD];  /* ACSIS sub-band set-up */
  char swMode[SZFITSCARD];    /* switch mode */
  int tableSize = 0;          /* number of elements of data table */
  char tauDatSt[SZFITSCARD];  /* time of tau225St observation in 
                                 format YYYY-MM-DDTHH:MM:SS */
  char telName[SZFITSCARD];   /* telescope name */
  char transiti[SZFITSCARD];  /* target transition for molecule */
  int year;                   /* year for time conversion. */  

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

/* NOTE!!!!!! Kludged calcs indicated by //k */

  /* Get the telescope efficiency and convert from percentage to decimal */
  etal = gsdVars->etal / 100.0; 


  /* Obs Id, Date, Pointing Info */


  /* Truncate the object names and concatenate. */
  cnfImprt ( gsdVars->object1, 16, object );
  cnfImprt ( gsdVars->object2, 16, object2 );

  if ( strncmp ( object2, " ", 1 ) != 0 ) {
    strcat ( object, ", " ); 
    strcat ( object, object2 );
  }

  /* Determine if this is a spectral line standard. */
  standard = 0;//k

  /* Get the UTC start and end times and observation IDs. */
  gsdac_getDateVars ( gsdVars, subsysNum, obsNum, backend, dateObs, 
                      dateEnd, obsID, obsIDs, HSTstart, HSTend, 
                      LSTstart, LSTend, status );
  
  if ( *status == SAI__OK ) {
 
    /* Get the airmass at start/end. */
    amStart = record[0].tcs_airmass;
    amEnd = record[nSteps - 1].tcs_airmass;

    /* Get the azimuth at start/end. */
    azStart = record[0].tcs_az_ac1;
    azEnd = record[nSteps - 1].tcs_az_ac1;

    /* Get the elevation at start/end. */
    elStart = record[0].tcs_az_ac2;
    elEnd = record[nSteps - 1].tcs_az_ac2;

  }


  /* Integration time related. */

  /* Get the dimensionality of the scan table 2. */
  tableSize = gsdVars->nScanVars2 * gsdVars->noScans;

  /* Get the sum of the integration times. */
  if ( *status == SAI__OK ) {

    i = 0;
    intTime = 0.0;
    while ( i < tableSize ) {
      intTime += (gsdVars->intTimes)[i];
      i += gsdVars->nScanVars2;
    }

  }


  /* ACSIS Specific. */

  /* Get the molecule. */
  strcpy ( molecule, "" );//k

  /* Get the transition. */
  strcpy ( transiti, "" );//k

  /* Get the bandwidth setup. */
/***** NOTE: may be different for rxb widebands *****/  
  sprintf ( bwMode, "%iMHzx%i", (int)(gsdVars->bandwidths)[subsysNum-1], 
            (gsdVars->BEChans)[subsysNum-1] );

/***** NOTE: Possibly undef? *****/
  strcpy ( subBands, bwMode );

  /* Get the reference channel. */
  refChan = (double)( (gsdVars->BEChans)[subsysNum-1] ) / 2.0;

  IFchanSp = (gsdVars->freqRes)[subsysNum-1] * 1000000.0;


  /* FE Specific. */

  /* Truncate the name of the frontend. */
  cnfImprt ( gsdVars->frontend, 16, instrume );

  /* Get the IF frequency and make sure it's always positive. */
  IFfreq = abs( (gsdVars->totIFs)[subsysNum-1] );  

  /* Get the number of mixers. */
  nMix = 1;//k

  /* Get the observed sideband (-ve value = LSB, +ve value = USB ). */
  if ( (gsdVars->sbSigns)[subsysNum-1] > 0 ) strcpy ( obsSB, "USB" );
  else strcpy ( obsSB, "LSB" );

  /* Get the names of the receptors. */
  strcpy ( recptors, recepNames[0] );
  for ( i = 1; i < gsdVars->nFEChans; i++ ) {
    strcat ( recptors, " " );
    strcat ( recptors, recepNames[i] );   
  }

  /* Truncate the doppler velocity definition and set
     to lowercase. */
  cnfImprt ( gsdVars->velDefn, 16, doppler );
  i = 0;
  curChar = doppler[i];
  while ( curChar != '\0' ) {
    doppler[i] = tolower(curChar);
    i++;
    curChar = doppler[i];
  }  

/***** NOTE : Possibly comes from VEL_REF (c12vref). */
    strcpy ( sSysObs, "TOPOCENT" );


  /* Environmental data. */

  /* Convert pressure from mmHg to mbar. */
  bp = (gsdVars->pamb) * 1.33322;

  /* Convert dates from YYMMDDHHMMSS to 
     YYYY-MM-DDTHH:MM:SS. */
  parse = sscanf ( gsdVars->tauTime, "%02d%02d%02d%02d%02d", &year, 
                   &month, &day, &hour, &min );

  if ( parse == 0 || parse == EOF ) {

    msgOut ( FUNC_NAME, "Couldn't convert CSO tau time.", status );
    strcpy ( tauDatSt, "" );

  } else {
    
    /* Inelegant method to get YYYY from YY. */
    if ( year > 70 ) year = year + 1900;
    else year = year + 2000;
 
    sprintf ( tauDatSt, "%04d-%02d-%02dT%02d:%02d:00", 
              year, month, day, hour, min );

  }


  /* Convert dates from YYMMDDHHMMSS to 
     YYYY-MM-DDTHH:MM:SS. */
  parse = sscanf ( gsdVars->seeTime, "%02d%02d%02d%02d%02d", &year, 
                   &month, &day, &hour, &min );

  if ( parse == 0 || parse == EOF ) {\
    msgOut ( FUNC_NAME, "Couldn't convert seeing time, continuing anyway.", 
             status );
    strcpy ( seeDatSt, "" );
  } else {
    
    /* Kludge to get YYYY from YY. */
    if ( year > 70 ) year = year + 1900;
    else year = year + 2000;

    sprintf ( seeDatSt, "%04d-%02d-%02dT%02d:%02d:00", 
           year, month, day, hour, min );

  }


  /* Switching and Map setup for the observation. */
  gsdac_getMapVars ( gsdVars, samMode, obsType, skyRefX, skyRefY, 
                     swMode, chopCrd, &mapHght, &mapPA, &mapWdth, 
                     loclCrd, scanCrd, &scanVel, &scanDy, 
                     &scanPA, scanPat, status );


  /* JOS parameters */

  /* Get the JOS_MIN (1 for raster, for sample this is the number
     of STEPTIME integrations coadded into a single spectrum. */
  if ( strcmp ( samMode, "sample" ) == 0 )
    josMin = gsdVars->noScans;
  else
    josMin = 1;//k

  /* Get the length of time in the reference, and the number
     of steps between references. */
  if ( strcmp ( samMode, "raster" ) == 0 ) {

    /* For rasters, determine the number of time scanning
       each point in one row from the total time for
       the row / number of points in the row.  The length
       of time in the reference is then sqrt (number of 
       points in the row) * (time per point). */
    nRefStep = sqrt ( (double)(gsdVars->noCyclePts) ) * 
           ( (double)(gsdVars->scanTime) / (double)(gsdVars->noCyclePts) );

    stBetRef = gsdVars->noCyclePts;
    
  } else { 

    nRefStep = (double)(gsdVars->scanTime);

    stBetRef = 1;

  }  

  if ( strcmp ( swMode, "chop" ) == 0 )
    stBetRef = AST__UNDEFI;

  /* Get the starting index into the pattern. */
  gsdac_getStartIdx ( gsdVars, samMode, &startIdx, status );



  /************************************/
  /*      WRITE OUT FITS HEADERS      */
  /************************************/

  astSetFitsS ( fitschan, "TELESCOP", gsdVars->telName, 
	        "Name of Telescope", *status );

  astSetFitsS ( fitschan, "ORIGIN", "Joint Astronomy Centre, Hilo", 
                "Origin of file", *status );

  astSetFitsF ( fitschan, "ALT-OBS", 4111.0, 
                "[m] Height of observation above sea level", *status );

  astSetFitsF ( fitschan, "LAT-OBS", 19.825833335521,
                "[deg] Latitude of Observatory", *status );

  astSetFitsF ( fitschan, "LONG-OBS", -155.4797222301,
                "[deg] Latitude of Observatory", *status ); 
  
  astSetFitsF ( fitschan, "OBSGEO-X", -5464594.335493,
                "[m]", *status );  

  astSetFitsF ( fitschan, "OBSGEO-Y", -2592695.151639,
                "[m]", *status );  

  astSetFitsF ( fitschan, "OBSGEO-Z", 2150635.34,
                "[m]", *status ); 

  astSetFitsF ( fitschan, "ETAL", etal,
                "Telescope efficiency", *status ); 


  /* OMP and ORAC-DR Specific */
  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- OMP and ORAC-DR Specific ----", *status );

  astSetFitsS ( fitschan, "PROJECT", gsdVars->project, 
	        "PATT number", *status );

/***** NOTE: possiby REDUCE_POINTING for spectral 5 points *****/
  astSetFitsS ( fitschan, "RECIPE", "REDUCE_SCIENCE", 
	        "ORAC-DR recipe", *status );

  astSetFitsS ( fitschan, "DRGROUP", AST__UNDEFS, 
	        "Data Reduction group ID", *status );

  astSetFitsS ( fitschan, "MSBID", AST__UNDEFS, 
	        "ID of minimum schedulable block", *status );

  astSetFitsS ( fitschan, "MSBTID", AST__UNDEFS, 
	        "Transaction ID of MSB", *status );

  astSetFitsS ( fitschan, "SURVEY", AST__UNDEFS, 
	        "Survey Name", *status );

  astSetFitsS ( fitschan, "RMTAGENT", AST__UNDEFS, 
	        "name of Remote Agent", *status );

  astSetFitsS ( fitschan, "AGENTID", AST__UNDEFS, 
	        "Unique identifier for remote agent", *status );


  /* Obs Id, Date, Pointing Info */
  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- Obs Id, Date, pointing Info ----", *status );

  astSetFitsS ( fitschan, "OBJECT", object, 
	        "Object of interest", *status );

  astSetFitsL ( fitschan, "STANDARD", standard, 
                "True if the spectral line is a standard", *status );

  astSetFitsI ( fitschan, "OBSNUM", obsNum, 
                "Observation number", *status );

  astSetFitsI ( fitschan, "NSUBSCAN", 1,
		"Sub-scan number", *status );

  astSetFitsL ( fitschan, "OBSEND", 1, 
                "True if the file is the last in current observation",
		*status );
  
  astSetFitsI ( fitschan, "UTDATE", utDate, 
                "UT Date as integer in yyyymmdd format", *status );

  astSetFitsS ( fitschan, "DATE-OBS", dateObs, 
                "UTC Datetime of start of observation", *status );

  astSetFitsS ( fitschan, "DATE-END", dateEnd, 
                "UTC Datetime of end of observation", *status );

  astSetFitsF ( fitschan, "DUT1", gsdVars->obsUT1C, 
                "[d] UT1-UTC correction", *status );

  astSetFitsS ( fitschan, "OBSID", obsID, 
                "Unique observation identifier", *status );

  astSetFitsS ( fitschan, "OBSIDS", obsIDs, 
                "Unique observation + subsystem ID", *status );

/***** NOTE: possibly same as REFRECEP *****/
  astSetFitsS ( fitschan, "INSTAP", AST__UNDEFS,
                "Receptor at tracking centre (if any)", *status );

  astSetFitsF ( fitschan, "INSTAP_X", AST__UNDEFF,
                "[arcsec] Aperture X off. rel. to instr centre", 
                *status );

  astSetFitsF ( fitschan, "INSTAP_Y", AST__UNDEFF,
                "[arcsec] Aperture Y off. rel. to instr centre", 
                *status );

  astSetFitsF ( fitschan, "AMSTART", amStart,
                "Airmass at start of observation", *status );

  astSetFitsF ( fitschan, "AMEND", amEnd,
                "Airmass at end of observation", *status );

  astSetFitsF ( fitschan, "AZSTART", azStart,
                "[deg] Azimuth at start of observation", *status );

  astSetFitsF ( fitschan, "AZEND", azEnd,
                "[deg] Azimuth at end of observation", *status );

  astSetFitsF ( fitschan, "ELSTART", elStart,
                "[deg] Elevation at start of observation", *status );

  astSetFitsF ( fitschan, "ELEND", elEnd,
                "[deg] Elevation at end of observation", *status );

  astSetFitsS ( fitschan, "HSTSTART", HSTstart,
                "HST at start of observation", *status );

  astSetFitsS ( fitschan, "HSTEND", HSTend,
                "HST at end of observation", *status );

  astSetFitsS ( fitschan, "LSTSTART", LSTstart,
                "LST at start of observation", *status );

  astSetFitsS ( fitschan, "LSTEND", LSTend,
                "LST at end of observation", *status );

  /* Integration time related. */
  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- Integration time related ----", *status );

  astSetFitsF ( fitschan, "INT_TIME", intTime,
                "Time spent integrating, entire", *status );


  /* ACSIS Specific. */
  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- ACSIS Specific ----", *status );

  astSetFitsS ( fitschan, "BACKEND", backend,
                "Name of the backend", *status );

  astSetFitsS ( fitschan, "DRRECIPE", AST__UNDEFS,
                "ACSIS-DR recipe name", *status );

  astSetFitsS ( fitschan, "BWMODE", bwMode,
                "Bandwidth setup", *status );

  astSetFitsI ( fitschan, "SUBSYSNR", subsysNum, 
                "Sub-system number", *status );

  astSetFitsS ( fitschan, "SUBBANDS", bwMode,
                "Sub-band setup", *status );

  astSetFitsI ( fitschan, "NSUBBAND", 1, 
                "Number of subbands", *status );

  astSetFitsF ( fitschan, "SUBREFP1", refChan, 
                "Reference channel for subband1", *status );

  astSetFitsF ( fitschan, "SUBREFP2", AST__UNDEFF,
                "Reference channel for subband2", *status );

  astSetFitsI ( fitschan, "REFCHAN", refChan, 
                "Reference IF channel No.", *status );

  astSetFitsF ( fitschan, "IFCHANSP", IFchanSp,
                "[Hz] TOPO IF channel spacing (signed)", *status ); 

  astSetFitsS ( fitschan, "FFT_WIN", AST__UNDEFS, 
	        "Type of window used for FFT", *status ); 

  astSetFitsF ( fitschan, "BEDEGFAC", AST__UNDEFF, 
	        "Backend degradation factor", *status ); 

  astSetFitsS ( fitschan, "MSROOT", AST__UNDEFS, 
	        "Root name of raw measurement sets", *status ); 


  /* FE Specific. */
  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- FE Specific ----", *status );

  astSetFitsS ( fitschan, "INSTRUME", instrume, 
	        "Front-end receiver", *status ); 

  astSetFitsS ( fitschan, "SB_MODE", gsdVars->sbMode, 
	        "Sideband mode", *status ); 

  astSetFitsF ( fitschan, "IFFREQ", IFfreq,
                "[GHz] IF Frequency", *status );

  astSetFitsI ( fitschan, "N_MIX", nMix, 
                "No. of mixers", *status );

  astSetFitsS ( fitschan, "OBS_SB", obsSB, 
		"The observed sideband", *status );

  astSetFitsF ( fitschan, "LOFREQS", (gsdVars->LOFreqs)[subsysNum-1],
		"[GHz] LO Frequency at start of obs", *status );

  astSetFitsF ( fitschan, "LOFREQE", (gsdVars->LOFreqs)[subsysNum-1],
                "[GHz] LO Frequency at end of obs", *status );

  astSetFitsS ( fitschan, "RECPTORS", recptors,
                "Active FE receptor IDs for this obs", *status );

  astSetFitsS ( fitschan, "REFRECEP", recepNames[0], 
                "Receptor with unit sensitivity", *status );

  if ( strcmp ( samMode, "sample" ) == 0 ) {
    astSetFitsF ( fitschan, "MEDTSYS", AST__UNDEFF,
		  "[K] Median of the T-sys across all receptors", 
                  *status );
  } else {
    astSetFitsF ( fitschan, "MEDTSYS", 
                  (gsdVars->sourceSysTemps)[subsysNum-1],
		  "[K] Median of the T-sys across all receptors", 
                  *status );
  }

  astSetFitsS ( fitschan, "TEMPSCAL", "TA*", 
                "Temperature scale in use", *status );

  astSetFitsS ( fitschan, "DOPPLER", doppler,
                "Doppler velocity definition", *status );

  astSetFitsS ( fitschan, "SSYSOBS", sSysObs,
                "Spectral ref. frame during observation", *status );


  /* Environmental data. */
  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- Environmental Data ----", *status );

  astSetFitsF ( fitschan, "ATSTART", gsdVars->tamb, 
                "[degC] Air temp at start of observation", 
                *status );

  astSetFitsF ( fitschan, "ATEND", gsdVars->tamb, 
                "[degC] Air temp at end of observation", 
                *status );

  astSetFitsF ( fitschan, "HUMSTART",gsdVars->hamb , 
                "Rel Humidity at observation start", *status );

  astSetFitsF ( fitschan, "HUMEND", gsdVars->hamb, 
                "Rel Humidity observation end", *status );

  astSetFitsF ( fitschan, "BPSTART", bp, 
                "[mbar] Pressure at observation start", 
                *status );

  astSetFitsF ( fitschan, "BPEND", bp, 
                "[mbar] Pressure at observation end", *status );

  astSetFitsF ( fitschan, "WNDSPDST", AST__UNDEFF, 
                "[km/h] Wind Speed at obs start", *status );
  
  astSetFitsF ( fitschan, "WNDSPDEN", AST__UNDEFF, 
                "[km/h] Wind Speed at obs end", *status );

  astSetFitsF ( fitschan, "WNDDIRST", AST__UNDEFF, 
                "[deg] Wind direction, azimuth at obs start", 
                *status );

  astSetFitsF ( fitschan, "WNDDIREN", AST__UNDEFF, 
                "[deg] Wind direction, azimuth at obs end", 
                *status );

  astSetFitsF ( fitschan, "TAU225ST", gsdVars->tau225,
		"Tau at 225 GHz from CSO at start", *status );

  astSetFitsF ( fitschan, "TAU225EN", gsdVars->tau225,
		"Tau at 225 GHz from CSO at end", *status );

  astSetFitsS ( fitschan, "TAUDATST", tauDatSt,
		"Time of TAU225ST observation", *status );

  astSetFitsS ( fitschan, "TAUDATEN", tauDatSt,
		"Time of TAU225EN observation", *status );

  astSetFitsS ( fitschan, "TAUSRC", "CSO225GHZ",
		"Source of the TAU225 value", *status );

  astSetFitsF ( fitschan, "WVMTAUST", AST__UNDEFF,
		"186GHz Tau from JCMT WVM at start", *status );

  astSetFitsF ( fitschan, "WVMTAUEN", AST__UNDEFF,
		"185GHz Tau from JCMT WVM at end", *status );

  astSetFitsS ( fitschan, "WVMDATST", AST__UNDEFS,
		"Time of WVMTAUST", *status );

  astSetFitsS ( fitschan, "WVMDATEN", AST__UNDEFS,
		"Time of WVMTAUEN", *status );

  astSetFitsF ( fitschan, "SEEINGST", gsdVars->seeing,
		"[arcsec] SAO atmospheric seeing (start)", 
		*status );

  astSetFitsF ( fitschan, "SEEINGSEN", gsdVars->seeing,
		"[arcsec] SAO atmospheric seeing (end)", 
		*status );

  astSetFitsS ( fitschan, "SEEDATST", seeDatSt,
		"Date/Time of SEEINGST", *status );

  astSetFitsS ( fitschan, "SEEDATEN", seeDatSt,
		"Date/Time of SEEINGEN", *status );

  astSetFitsF ( fitschan, "FRLEGTST", AST__UNDEFF,
		"[degC] Mean Front leg temperature - Start", *status );

  astSetFitsF ( fitschan, "FRLEGTEN", AST__UNDEFF,
		"[degC] Mean Front leg temperature - End", *status );

  astSetFitsF ( fitschan, "BKLEGTST", AST__UNDEFF,
		"[degC] Mean Back leg temperature - Start", *status );

  astSetFitsF ( fitschan, "BKLEGTEN", AST__UNDEFF,
		"[degC] Mean Back leg temperature - End", *status );


  /* Switching and Map setup for the observation. */
  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- Switching and Map setup for the observationi ----", 
                 *status );

  astSetFitsS ( fitschan, "SAM_MODE", samMode, 
                "Sampling Mode", *status );  

  astSetFitsS ( fitschan, "SW_MODE", swMode,
                "Switch Mode", *status );

  astSetFitsS ( fitschan, "SKYREFX", skyRefX,
                "X co-ord of Reference Position", *status );

  astSetFitsS ( fitschan, "SKYREFY", skyRefY,
                "Y co-ord of Reference Position", *status );   

  astSetFitsS ( fitschan, "OBS_TYPE", obsType,
		"Type of observation", *status );

  if ( ( strcmp ( samMode, "grid" ) == 0
       && strcmp ( swMode, "chop" ) == 0 ) ||
       strcmp ( samMode, "sample" ) == 0 ) {

    astSetFitsS ( fitschan, "CHOP_CRD", chopCrd,
                  "Chopping co-ordinate system", *status );

    astSetFitsF ( fitschan, "CHOP_FRQ", gsdVars->chopFrequency,
		  "[Hz] Chop frequency", *status );

    astSetFitsF ( fitschan, "CHOP_PA", gsdVars->chopPA,
		  "[deg] Chop PA; 0=in lat, 90=in long", *status );

    astSetFitsF ( fitschan, "CHOP_THR", gsdVars->chopThrow,
		  "[arcsec] Chop throw", *status );

  } else {

    astSetFitsS ( fitschan, "CHOP_CRD", AST__UNDEFS,
                  "Chopping co-ordinate system", *status );

    astSetFitsF ( fitschan, "CHOP_FRQ", AST__UNDEFF,
		  "[Hz] Chop frequency", *status );

    astSetFitsF ( fitschan, "CHOP_PA", AST__UNDEFF,
		  "[deg] Chop PA; 0=in lat, 90=in long", *status );

    astSetFitsF ( fitschan, "CHOP_THR", AST__UNDEFF,
		  "[arcsec] Chop throw", *status );

  }

  astSetFitsI ( fitschan, "JIGL_CNT", AST__UNDEFI,
		"Number of offsets in jiggle pattern", *status );

  astSetFitsS ( fitschan, "JIGL_NAM", AST__UNDEFS,
		"File containing the jiggle offsets", *status );

  astSetFitsF ( fitschan, "JIGL_PA", AST__UNDEFF,
		"[deg] Jiggle PA; 0=in lat, 90=in long", *status );

  astSetFitsS ( fitschan, "JIGL_CRD", AST__UNDEFS,
		"Jiggling co-ordinate system", *status );

  if ( strcmp ( samMode, "raster" ) == 0
       && strcmp ( swMode, "pssw" ) == 0 ) {

    astSetFitsF ( fitschan, "MAP_HGHT", mapHght,
		  "[arcsec] Requested height of map", *status );

    astSetFitsF ( fitschan, "MAP_PA", mapPA,
		  "[deg] Requested PA of map", *status );

    astSetFitsF ( fitschan, "MAP_WDTH", mapWdth,
		  "[arcsec] Requested width of map", *status );

    astSetFitsS ( fitschan, "LOCL_CRD", loclCrd,
		  "Local offset/map PA co-ordinate system",
		  *status );

    astSetFitsF ( fitschan, "MAP_X", gsdVars->centreOffsetX,
		  "[arcsec] Requested map offset from telescope centre",
		  *status );

    astSetFitsF ( fitschan, "MAP_Y", gsdVars->centreOffsetY,
		  "[arcsec] Requested map offset from telescope centre",
		  *status );  

    astSetFitsS ( fitschan, "SCAN_CRD", scanCrd,
		  "Co-ordinate system for scan", *status );

    astSetFitsF ( fitschan, "SCAN_VEL", scanVel,
		  "[arcsec/sec] Scan velocity along scan direction", 
		  *status );

    astSetFitsF ( fitschan, "SCAN_DY", scanDy,
		  "[arcsec] Scan spacing perp. to scan", *status );

    astSetFitsS ( fitschan, "SCAN_PAT", scanPat,
		  "Scan pattern name", *status );

  } else {

    astSetFitsF ( fitschan, "MAP_HGHT", AST__UNDEFF,
		  "[arcsec] Requested height of map", *status );

    astSetFitsF ( fitschan, "MAP_PA", AST__UNDEFF,
		  "[deg] Requested PA of map", *status );

    astSetFitsF ( fitschan, "MAP_WDTH", AST__UNDEFF,
		  "[arcsec] Requested width of map", *status );

    astSetFitsS ( fitschan, "LOCL_CRD", AST__UNDEFS,
		  "Local offset/map PA co-ordinate system",
		  *status );

    astSetFitsF ( fitschan, "MAP_X", AST__UNDEFF,
		  "[arcsec] Requested map offset from telescope centre",
		  *status );

    astSetFitsF ( fitschan, "MAP_Y", AST__UNDEFF,
		  "[arcsec] Requested map offset from telescope centre",
		  *status );  

    astSetFitsS ( fitschan, "SCAN_CRD", AST__UNDEFS,
		  "Co-ordinate system for scan", *status );

    astSetFitsF ( fitschan, "SCAN_VEL", AST__UNDEFF,
		  "[arcsec/sec] Scan velocity along scan direction", 
		  *status );

    astSetFitsF ( fitschan, "SCAN_DY", AST__UNDEFF,
		  "[arcsec] Scan spacing perp. to scan", *status );

    astSetFitsS ( fitschan, "SCAN_PAT", AST__UNDEFS,
		  "Scan pattern name", *status );

  }


  /* SMU */
  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- SMU ----", *status );

  astSetFitsF ( fitschan, "ALIGN_DX", gsdVars->smuDX, 
		"SMU tables X axis focus offset", *status );

  astSetFitsF ( fitschan, "ALIGN_DY", gsdVars->smuDY, 
		"SMU tables Y axis focus offset", *status );

  astSetFitsF ( fitschan, "FOCUS_DZ", gsdVars->smuDZ, 
		"SMU tables Z axis focus offset", *status );

  astSetFitsF ( fitschan, "DAZ", gsdVars->smuOffsEW, 
		"SMU azimuth pointing offset", *status );

  astSetFitsF ( fitschan, "DEL", gsdVars->smuOffsNS, 
		"SMU elevation pointing offset", *status );

  astSetFitsF ( fitschan, "UAZ", gsdVars->errAz, 
		"User azimuth pointing offset", *status );

  astSetFitsF ( fitschan, "UEL", gsdVars->errEl, 
		"User elevation pointing offset", *status );


  /* JOS parameters */
  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- JOS parameters ----", *status );

  astSetFitsF ( fitschan, "STEPTIME", gsdVars->cycleTime, 
                "RTS step time during an RTS sequence", *status ); 

  astSetFitsI ( fitschan, "NUM_CYC", gsdVars->nCycle, 
                "Number of times to repeat entire recipe", *status );  

  astSetFitsI ( fitschan, "NUM_NODS", 1, 
                "Number of times to repeat nod set", *status );

  astSetFitsI ( fitschan, "JOS_MULT", AST__UNDEFI,
		"", *status );//k description

  astSetFitsI ( fitschan, "JOS_MIN", josMin,
                "", *status );//k description

  astSetFitsI ( fitschan, "NCALSTEP", AST__UNDEFI,
		"Number of RTS steps for each CAL", *status );

  astSetFitsF ( fitschan, "NREFSTEP", nRefStep,
		"Mean no. of RTS steps for each REF", *status );

  astSetFitsI ( fitschan, "STBETREF", stBetRef, 
		"Target number of RTS steps between REFs",
		*status );

  astSetFitsI ( fitschan, "STBETCAL", AST__UNDEFI,
		"Target number of RTS steps between CALs",
		*status );

  astSetFitsI ( fitschan, "STARTIDX", startIdx,
		"Index into pattern at start of obs", *status );

  astSetFitsS ( fitschan, "FOCAXIS", AST__UNDEFS,
		"Focus Axis to move (X, Y, Z)", *status );

  astSetFitsI ( fitschan, "NFOCSTEP", AST__UNDEFI,
		"Number of focal position steps", *status );

  astSetFitsF ( fitschan, "FOCSTEP", AST__UNDEFF,
		"Distance between focal steps", *status );


  /* Miscellaneous parameters */
  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- Miscellaneous ----", *status );

  astSetFitsS ( fitschan, "OCSCFG", AST__UNDEFS,
	       "OCS config filename", *status );

  astSetFitsL ( fitschan, "SIM_CORR", 0,
		"True if any CORRTASK is simualted", *status );

  astSetFitsL ( fitschan, "SIM_SMU", 0,
		"True if SMU data is simulated", *status );

  astSetFitsL ( fitschan, "SIM_TCS", 0,
		"True if TCS data is simulated", *status );

  astSetFitsL ( fitschan, "RTS_SMU", 0,
		"True if RTS data is simulated", *status );

  astSetFitsL ( fitschan, "IF_SMU", 0,
		"True if IF data is simulated", *status );

  astSetFitsL ( fitschan, "SIMULATE", 0,
		"True if any data are simulated", *status );

  astSetFitsS ( fitschan, "STATUS", "NORMAL",
		"Status at end of observation", *status );


  /* Rover=specific parameters */
  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- ROVER Specific ----", *status );

  astSetFitsL ( fitschan, "POL_CONN", 0,
		"True if ROVER is connected", *status );

  astSetFitsS ( fitschan, "POL_MODE", AST__UNDEFS,
		"Step-and-integrate (STEPINT) or spinning (SPIN)", 
                *status );

  astSetFitsF ( fitschan, "ROTAFREQ", AST__UNDEFF,
		"[Hz] Spin frequency (if spinning)", *status );


}
