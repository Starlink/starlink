/*
*+
*  Name:
*     "gsdac_putFits.c

*  Purpose:
*     Retrieve values from the GSD headers to fill the FITS headers
*     in the ACSIS file.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_putFits ( const struct gsdac_gsd_struct *gsd, 
*                     const int nSubsys, const int subsysNum, 
*                     const int obsNum, const int utDate, const int nChans, 
*                     const int nSteps, const char *backend, 
*                     const int nRecep, char *recepNames[],
*                     const struct JCMTState *record, 
*                     const AstFitsChan *fitschan, int *status )

*  Arguments:
*     gsd = const struct gsdac_gsd_struct* (Given)
*        GSD file access parameters
*     nSubsys = const int (Given)
*        Number of subsystems
*     subsysNum = const int (Given)
*        Subsystem number
*     obsNum = const int (Given)
*        Observation number
*     utDate = const int (Given)
*        UT observation date
*     nChans = const int (Given)
*        Number of channels used in this subsection
*     nSteps = const int (Given)
*        Number of time steps in observation
*     backend = const char* (Given)
*        Name of the backend
*     nRecep = const int (Given)
*        Number of receptors
*     recepnames = char*[] (Given)
*        Names of receptors
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
#include "gsd.h"
#include "sae_par.h"
#include "mers.h"
#include "cnf.h"

/* GSDAC includes */
#include "gsdac.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define MAXFITS 80

/* Debug level, 0 = quiet, 1 = loud. */
#define DEBUGLVL 0

#define FUNC_NAME "gsdac_putFits"

void gsdac_putFits ( const struct gsdac_gsd_struct *gsd, 
                     const int nSubsys, const int subsysNum, 
                     const int obsNum, const int utDate, const int nChans, 
                     const int nSteps, const char *backend, 
                     const int nRecep, char *recepNames[],
                     const struct JCMTState *record, 
                     const AstFitsChan *fitschan, int *status )

{

  /* Local variables */
  float alignDX;              /* SMU tables X axis alignment offset */
  float alignDY;              /* SMU tables Y axis alignment offset */
  float amEnd;                /* airmass at end of observation */
  float amStart;              /* airmass at start of observation */
  double atEnd;               /* air temp at end of observation (degC) */
  double atStart;             /* air temp at start of observation (degC) */
  double azEnd;               /* Azimuth at observation end (deg) */
  double azStart;             /* Azimuth at observation start (deg) */
  float bandwidth;            /* bandwidth */
  double bpEnd;               /* pressure at observation end (mbar) */
  double bpStart;             /* pressure at observation start (mbar) */
  char bwMode[MAXFITS];       /* ACSIS total bandwidth setup */

  char chopCrd[MAXFITS];      /* chopper coordinate system */
  float chopFrq;              /* chopper frequency (Hz) */
  float chopPA;               /* chopper position angle (deg) */
  float chopThr;              /* chop throw (arcsec) */
  char curChar;               /* character pointer */
  char dateEnd[MAXFITS];      /* UTC datetime of end of observation 
                                 in format YYYY-MM-DDTHH:MM:SS */
  char dateObs[MAXFITS];      /* UTC datetime of start of observation 
                                 in format YYYY-MM-DDTHH:MM:SS */
  int day;                    /* days for time conversion. */
  float dAz;                  /* SMU azimuth pointing offset */
  float dEl;                  /* SMU elevation pointing offset */
  char doppler[MAXFITS];      /* doppler velocity definition */
  double dut1;                /* UT1-UTC correction (day) */
  double elEnd;               /* elevation at observation end (deg) */
  double elStart;             /* elevation at observation start (deg) */
  double etal;                /* telescope efficiency */
  float focusDZ;              /* SMU tables Z axis focus offset */
  int hour;                   /* hours for time conversion. */
  char HSTend[MAXFITS];       /* HST at observation end in format 
                                 YYYY-MM-DDTHH:MM:SS */
  char HSTstart[MAXFITS];     /* HST at observation start in format 
                                 YYYY-MM-DDTHH:MM:SS */
  double humEnd;              /* humidity at observation end (%) */ 
  double humStart;            /* humidity at observation start (%) */
  int i;                      /* loop counter */
  float IFchanSp;             /* TOPO IF channel spacing (Hz) */
  double IFfreq;              /* IF frequency (GHz) */
  char instrume[MAXFITS];     /* front-end receiver */
  double intTime;             /* total time spent integrating (s) */
  int josMin;                 /* ?? */
  char loclCrd[MAXFITS];      /* local offset coordinate system for 
                                 map_x / map_y */
  double LOfreqE;             /* LO frequency at end of observation (GHz) */
  double LOfreqS;             /* LO frequency at start of observation (GHz) */
  char LSTstart[MAXFITS];     /* LST at observation start in format
                                 YYYY-MM-DDTHH:MM:SS */
  char LSTend[MAXFITS];       /* LST at observation end in format
                                 YYYY-MM-DDTHH:MM:SS */
  float mapHght;              /* requested height of rectangle to be mapped 
                                 (arcsec) */
  float mapPA;                /* requested PA of map vertical, +ve towards
                                 +ve long */
  char mapPositiveX;          /* flag for x-direction increase in first row. */
  char mapPositiveY;          /* flag for y-direction increase in first row. */
  double mapStartX;           /* Start map x index. */
  double mapStartY;           /* Start map y index. */
  float mapWdth;              /* requested width of rectangle to be mapped
                                 (arcsec) */
  double mapX;                /* requested map X offset from telescope
                                 centre (arcsec) */
  double mapY;                /* requested map Y offset from telescope
                                 centre (arcsec) */
  float medTSys;              /* median of T_sys across all receptors */
  int min;                    /* minutes for time conversion. */
  char molecule[MAXFITS];     /* target molecular species */
  int month;                  /* months for time conversion. */
  int nMix;                   /* number of mixers */
  double nRefStep;            /* number of nod sets repeated */
  int numCyc;                 /* number of times to repeat entire recipe */
  int numNods;                /* number of nod sets repeated */
  int numPtsX;                /* number of points in x direction */
  int numPtsY;                /* number of points in y direction */
  int numScanPts;             /* number of points in a phase */
  char object[MAXFITS];       /* object of interest */
  char object2[MAXFITS];      /* object of interest (second half of name) */
  char obsDirection[MAXFITS]; /* direction of scan (horizontal or
                                 vertical) */
  char obsID[MAXFITS];        /* unique observation number in format
                                 INSTR_NNNNN_YYYYMMDDTHHMMSS */
  char obsIDs[MAXFITS];       /* unique observation number + subsystem
                                 number in format
                                 INSTR_NNNNN_YYYYMMDDTHHMMSS_N */
  char obsSB[MAXFITS];        /* observed sideband */
  char obsType[MAXFITS];      /* type of observation */
  int parse;                  /* flag for incorrect date string. */
  char project[MAXFITS];      /* observing program */
  char recptors[MAXFITS];     /* active FE receptor IDs for this obs */
  double refChan;             /* reference IF channel no. */
  char refrecep[MAXFITS];     /* receptor with unit sensitivity */
  int remainder;              /* remainder when parsing dates. */
  char samMode[MAXFITS];      /* sampling mode (raster or grid) */
  char sbMode[MAXFITS];       /* sideband mode */
  int sbSign;                 /* sideband sign */
  char scanCrd[MAXFITS];      /* coordinate system of scan */
  float scanDy;               /* scan spacing perpendicular to scan
                                 (arcsec) */
  float scanPA;               /* Scan PA rel. to lat. line; 0=lat, 
                                 90=long in scanCrd system */
  char scanPat[MAXFITS];      /* name of scanning scheme */
  double scanTime;            /* time of one scan. */
  float scanVel;              /* scan velocity (arcsec/sec) */
  char seeDatEn[MAXFITS];     /* time of seeingEn in format
                                 YYYY-MM-DDTHH:MM:SS */
  char seeDatSt[MAXFITS];     /* time of seeingSt in format
                                 YYYY-MM-DDTHH:MM:SS */
  float seeingEn;             /* SAO atmospheric seeing at end (arcsec) */
  float seeingSt;             /* SAO atmospheric seeing at start (arcsec) */
  char skyRefX[MAXFITS];      /* X co-ord of reference position (arcsec) */  
  char skyRefY[MAXFITS];      /* Y co-ord of reference position (arcsec) */  
  char sSysObs[MAXFITS];      /* spectral ref. frame during observation */
  int standard;               /* true for spectral line standards */
  int startIdx;               /* index in pattern at start of observation */
  int stBetRef;               /* max number of steps between refs */
  int stepTime;               /* RTS step time (s) */
  char subBands[MAXFITS];     /* ACSIS sub-band set-up */
  int subRefP1;               /* reference channel for subband1 */
  char swMode[MAXFITS];       /* switch mode (chop, pssw, freq, or NONE) */
  float *tableDat;            /* table data */
  int tableDims = 0;          /* dimensionality of data table */
  int tableSize = 0;          /* number of elements of data table */
  float tau225En;             /* 225 GHz tau from CSO at end */
  float tau225St;             /* 225 GHz tau from CSO at start */
  char tauDatEn[MAXFITS];     /* time of tau225En observation in 
				  format YYYY-MM-DDTHH:MM:SS */
  char tauDatSt[MAXFITS];     /* time of tau225St observation in 
                                 format YYYY-MM-DDTHH:MM:SS */
  char telName[MAXFITS];      /* telescope name */
  char time[MAXFITS];         /* time before converting format. */
  char transiti[MAXFITS];     /* target transition for molecule */
  double uAz;                 /* user azimuth pointing offset */
  double uEl;                 /* user elevation pointing offset */
  int year;                   /* year for time conversion. */  

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

/* NOTE!!!!!! Kludged calcs indicated by //k */

  if ( DEBUGLVL > 0 ) printf ( "Getting general FITS headers...\n" );

  /* Get the telescope name. */
  gsdac_get0c ( gsd, "C1TEL", telName, status );

  /* Get the telescope efficiency and convert from percentage to decimal */
  gsdac_get0d ( gsd, "C8EL", &etal, status ); 

  etal = etal / 100.0; 



  /* OMP and ORAC-DR Specific */
  if ( DEBUGLVL > 0 ) printf ( "Getting OMP FITS headers...\n" );

  /* Get the Project name */
/***** NOTE: possibly CAL for calibration sources (planets) *****/
  gsdac_get0c ( gsd, "C1PID", project, status ); 



  /* Obs Id, Date, Pointing Info */
  if ( DEBUGLVL > 0 ) printf ( "Getting Obs ID FITS headers...\n" );

  /* Get the Object name. */
  gsdac_get0c ( gsd, "C1SNA1", object, status );
  gsdac_get0c ( gsd, "C1SNA2", object2, status );

  if ( *status == SAI__OK ) {

    /* Truncate the object names and concatenate. */
    cnfImprt ( object, 16, object );
    cnfImprt ( object2, 16, object2 );

    if ( strncmp ( object2, " ", 1 ) != 0 ) {
      strcat ( object, ", " ); 
      strcat ( object, object2 );
    }

  }

  /* Determine if this is a spectral line standard. */
  standard = 0;//k

  /* Get the UT1-UTC correction. */
  gsdac_get0d ( gsd, "c3ut1c", &dut1, status );

  /* Get the UTC start and end times and observation IDs. */
  gsdac_getDateVars ( gsd, subsysNum, obsNum, backend, dateObs, 
                      dateEnd, obsID, obsIDs, 
                      HSTstart, HSTend, LSTstart, LSTend, status );
  
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

  if ( DEBUGLVL > 0 ) printf ( "Getting integ-time FITS headers...\n" );

  /* Get the dimensionality of the scan table 2. */
  gsdac_get0i ( gsd, "C3NO_SCAN_VARS2", &tableDims, status );

  /* Get the size of the scan table 1. */
  gsdac_getArraySize ( gsd, "C12SCAN_TABLE_2", &tableSize, status );

  tableDat = smf_malloc ( tableSize, sizeof( float ), 0, status );

  /* Get the data from scan table 1. */
  gsdac_get1r ( gsd, "C12SCAN_TABLE_2", tableDat, status ); 

  /* Get the sum of the integration times. */
  if ( *status == SAI__OK ) {

    i = 0;
    intTime = 0.0;
    while ( i < tableSize ) {
      intTime += tableDat[i];
      i += tableDims;
    }

  }

  smf_free ( tableDat, status );



  /* ACSIS Specific. */
  if ( DEBUGLVL > 0 ) printf ( "Getting ACSIS FITS headers...\n" );

  /* Get the molecule. */
  strcpy ( molecule, "" );//k

  /* Get the transition. */
  strcpy ( transiti, "" );//k

  /* Get the bandwidth setup. */
/***** NOTE: may be different for rxb widebands *****/  
  gsdac_getElemr ( gsd, "C12BW", subsysNum-1, &bandwidth, status );
  
  if ( *status == SAI__OK ) {
  
    sprintf ( bwMode, "%iMHzx%i", (int)bandwidth, nChans );

/***** NOTE: Possibly undef? *****/
    strcpy ( subBands, bwMode );

    /* Get the reference channel. */
    refChan = (double)nChans / 2.0;

  }

  /* Get the IF channel spacing and convert to Hz. */
  gsdac_getElemr ( gsd, "C12FR", subsysNum-1, &IFchanSp, status );

  if ( *status == SAI__OK ) IFchanSp = IFchanSp * 1000000.0;
  


  /* FE Specific. */
  if ( DEBUGLVL > 0 ) printf ( "Getting FE FITS headers...\n" );

  /* Get the name of the frontend. */
  gsdac_get0c ( gsd, "C1RCV", instrume, status ); 

  if ( *status == SAI__OK ) {

    /* Truncate the name of the frontend. */
    cnfImprt ( instrume, 16, instrume );

  }

  /* Get the sideband mode. */
  gsdac_get0c ( gsd, "C3SBMODE", sbMode, status );

  /* Get the IF frequency and make sure it's always positive. */
  gsdac_getElemd ( gsd, "C3BETOTIF", subsysNum-1, &IFfreq, status ); 

  IFfreq = abs( IFfreq );  

  /* Get the number of mixers. */
  nMix = 1;//k

  /* Get the observed sideband (-ve value = LSB, +ve value = USB ). */
  gsdac_getElemi ( gsd, "C3BEFESB", subsysNum-1, &sbSign, status );

  if ( *status == SAI__OK ) {

    if ( sbSign > 0 ) strcpy ( obsSB, "USB" );
    else strcpy ( obsSB, "LSB" );

  }

  /* Get the LO frequency at the start of the observation. */
  gsdac_getElemd ( gsd, "C3BEFENULO", subsysNum-1, &LOfreqS, status );  

  /* Get the LO frequency at the end of the observation. */
  gsdac_getElemd ( gsd, "C3BEFENULO", subsysNum-1, &LOfreqE, status ); 

  /* Get the names of the receptors. */
  strcpy ( recptors, recepNames[0] );
  for ( i = 1; i < nRecep; i++ ) {
    strcat ( recptors, " " );
    strcat ( recptors, recepNames[i] );   
  }

  /* Get the name of the receptor with unit sensitivity. */
  strcpy ( refrecep, recepNames[0] );

  /* Get the median of the T_sys across all receptors. */
  gsdac_getElemr ( gsd, "C12SST", subsysNum-1, &medTSys, status );

  /* Get the Doppler velocity definition. */
  gsdac_get0c ( gsd, "c12vdef", doppler, status ); 

  if ( *status == SAI__OK ) {

    /* Truncate the doppler velocity definition and set
       to lowercase. */
    cnfImprt ( doppler, 16, doppler );
    i = 0;
    curChar = doppler[i];
    while ( curChar != '\0' ) {
      doppler[i] = tolower(curChar);
      i++;
      curChar = doppler[i];
    }  

/***** NOTE : Possibly comes from VEL_REF (c12vref). */
    strcpy ( sSysObs, "TOPOCENT" );

  }  



  /* Environmental data. */
  if ( DEBUGLVL > 0 ) printf ( "Getting Env FITS headers...\n" );

  /* Get the air temp. */
  gsdac_get0d ( gsd, "C5AT", &atStart, status );

  /* Get the relative humidity. */
  gsdac_get0d ( gsd, "C5RH", &humStart, status );

  /* Get the pressure. */
  gsdac_get0d ( gsd, "C5PRS", &bpStart, status );

  /* Get the Tau at 225 GHz. */
  gsdac_get0r ( gsd, "c7tau225", &tau225St, status );

  /* Get the time of the 225 GHz Tau. */
  gsdac_get0c ( gsd, "c7tautime", time, status );

  if ( *status == SAI__OK ) {

    /* Convert dates from YYMMDDHHMMSS to 
       YYYY-MM-DDTHH:MM:SS. */
    parse = sscanf ( time, "%02d%02d%02d%02d%02d", &year, &month, &day, 
                   &hour, &min );

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

  }

  /* Get the seeing. */
  gsdac_get0r ( gsd, "c7seeing", &seeingSt, status );

  /* Get the time of the seeing. */
  gsdac_get0c ( gsd, "c7seetime", time, status );

  if ( *status == SAI__OK ) {

    /* Convert dates from YYMMDDHHMMSS to 
       YYYY-MM-DDTHH:MM:SS. */
    parse = sscanf ( time, "%02d%02d%02d%02d%02d", &year, &month, &day, 
                   &hour, &min );

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

  }

  if ( *status == SAI__OK ) {

    /* Copy end values from start values. */
    atEnd = atStart;
    humEnd = humStart;
    bpEnd = bpStart;
    tau225En = tau225St;
    seeingEn = seeingSt;
    strcpy ( tauDatEn, tauDatSt );
    strcpy ( seeDatEn, seeDatSt );

  }



  /* Switching and Map setup for the observation. */
  if ( DEBUGLVL > 0 ) printf ( "Getting Map FITS headers...\n" );

  gsdac_getMapVars ( gsd, samMode, swMode, skyRefX, skyRefY, 
                     obsType, chopCrd, &chopFrq, &chopPA, &chopThr,
                     &mapHght, &mapPA, &mapWdth, &numPtsX, 
                     &numPtsY, obsDirection, loclCrd, &mapX,
                     &mapY, scanCrd, &scanVel, &scanDy, &scanPA, scanPat, 
                     status );



  /* SMU */
  if ( DEBUGLVL > 0 ) printf ( "Getting SMU FITS headers...\n" );

  /* Get the SMU table alignment offsets. */
  gsdac_get0r ( gsd, "C2FV", &alignDX, status );
  gsdac_get0r ( gsd, "C2FL", &alignDY, status );

  /* Get the SMU tables z axis focus offset. */
  gsdac_get0r ( gsd, "C2FR", &focusDZ, status );

  /* Get the SMU Az/El pointing offsets. */
  gsdac_get0r ( gsd, "c4offs_ew", &dAz, status );
  gsdac_get0r ( gsd, "c4offs_ns", &dEl, status );

  /* Get the SMU Az/El user pointing offsets. */
  gsdac_get0d ( gsd, "c4azerr", &uAz, status );
  gsdac_get0d ( gsd, "c4elerr", &uEl, status );  



  /* JOS parameters */
  if ( DEBUGLVL > 0 ) printf ( "Getting JOS FITS headers...\n" );

  /* Get the steptime. */
  gsdac_get0i ( gsd, "C3CL", &stepTime, status );//k

  /* Get the number of cycles. */
  gsdac_get0i ( gsd, "c3ncycle", &numCyc, status );//k

  /* Get the JOS_MIN (1 for raster, for sample this is the number
     of STEPTIME integrations coadded into a single spectrum. */
  if ( strcmp ( samMode, "raster" ) == 0 )
    josMin = 1;//k
  else if ( strcmp ( samMode, "sample" ) == 0 )
    gsdac_get0i ( gsd, "C3NIS", &josMin, status );//k 
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
    gsdac_get0i ( gsd, "c3ncp", &numScanPts, status );
    gsdac_get0d ( gsd, "C3SRT", &scanTime, status );
    nRefStep = sqrt ( (double)numScanPts ) * ( scanTime / (double)numScanPts );

    stBetRef = numScanPts;
    
  } else { 

    nRefStep = (double)stepTime;

    stBetRef = 1;

  }  

  if ( strcmp ( swMode, "chop" ) == 0 )
    stBetRef = AST__UNDEFI;

  /* Get the starting index into the pattern. */
  gsdac_getStartIdx ( gsd, samMode, &numPtsX, &numPtsY,
                      obsDirection, &startIdx, status );



  /************************************/
  /*      WRITE OUT FITS HEADERS      */
  /************************************/

  if ( DEBUGLVL > 0 ) printf ( "Writing general FITS headers...\n" );

  astSetFitsS ( fitschan, "TELESCOP", telName, 
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
  if ( DEBUGLVL > 0 ) printf ( "Writing OMP FITS headers...\n" );

  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- OMP and ORAC-DR Specific ----", *status );

  astSetFitsS ( fitschan, "PROJECT", project, 
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
  if ( DEBUGLVL > 0 ) printf ( "Writing Obs ID FITS headers...\n" );

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

  astSetFitsF ( fitschan, "DUT1", dut1, 
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
  if ( DEBUGLVL > 0 ) printf ( "Writing integ-time FITS headers...\n" );

  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- Integration time related ----", *status );

  astSetFitsF ( fitschan, "INT_TIME", intTime,
                "Time spent integrating, entire", *status );


  /* ACSIS Specific. */
  if ( DEBUGLVL > 0 ) printf ( "Writing ACSIS FITS headers...\n" );

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

  astSetFitsS ( fitschan, "SUBBANDS", subBands,
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
  if ( DEBUGLVL > 0 ) printf ( "Writing FE FITS headers...\n" );

  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- FE Specific ----", *status );

  astSetFitsS ( fitschan, "INSTRUME", instrume, 
	        "Front-end receiver", *status ); 

  astSetFitsS ( fitschan, "SB_MODE", sbMode, 
	        "Sideband mode", *status ); 

  astSetFitsF ( fitschan, "IFFREQ", IFfreq,
                "[GHz] IF Frequency", *status );

  astSetFitsI ( fitschan, "N_MIX", nMix, 
                "No. of mixers", *status );

  astSetFitsS ( fitschan, "OBS_SB", obsSB, 
		"The observed sideband", *status );

  astSetFitsF ( fitschan, "LOFREQS", LOfreqS,
		"[GHz] LO Frequency at start of obs", *status );

  astSetFitsF ( fitschan, "LOFREQE", LOfreqE,
                "[GHz] LO Frequency at end of obs", *status );

  astSetFitsS ( fitschan, "RECPTORS", recptors,
                "Active FE receptor IDs for this obs", *status );

  astSetFitsS ( fitschan, "REFRECEP", refrecep, 
                "Receptor with unit sensitivity", *status );

  if ( strcmp ( samMode, "sample" ) == 0 ) {

    astSetFitsF ( fitschan, "MEDTSYS", AST__UNDEFF,
		  "[K] Median of the T-sys across all receptors", 
                  *status );

  }

  astSetFitsF ( fitschan, "MEDTSYS", medTSys,
		"[K] Median of the T-sys across all receptors", 
                *status );

  astSetFitsS ( fitschan, "TEMPSCAL", "TA*", 
                "Temperature scale in use", *status );

  astSetFitsS ( fitschan, "DOPPLER", doppler,
                "Doppler velocity definition", *status );

  astSetFitsS ( fitschan, "SSYSOBS", sSysObs,
                "Spectral ref. frame during observation", *status );


  /* Environmental data. */
  if ( DEBUGLVL > 0 ) printf ( "Writing Env FITS headers...\n" );

  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- Environmental Data ----", *status );

  astSetFitsF ( fitschan, "ATSTART", atStart, 
                "[degC] Air temp at start of observation", 
                *status );

  astSetFitsF ( fitschan, "ATEND", atEnd, 
                "[degC] Air temp at end of observation", 
                *status );

  astSetFitsF ( fitschan, "HUMSTART", humStart, 
                "Rel Humidity at observation start", *status );

  astSetFitsF ( fitschan, "HUMEND", humEnd, 
                "Rel Humidity observation end", *status );

  astSetFitsF ( fitschan, "BPSTART", bpStart, 
                "[mbar] Pressure at observation start", 
                *status );

  astSetFitsF ( fitschan, "BPEND", bpEnd, 
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

  astSetFitsF ( fitschan, "TAU225ST", tau225St,
		"Tau at 225 GHz from CSO at start", *status );

  astSetFitsF ( fitschan, "TAU225EN", tau225En,
		"Tau at 225 GHz from CSO at end", *status );

  astSetFitsS ( fitschan, "TAUDATST", tauDatSt,
		"Time of TAU225ST observation", *status );

  astSetFitsS ( fitschan, "TAUDATEN", tauDatEn,
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

  astSetFitsF ( fitschan, "SEEINGST", seeingSt,
		"[arcsec] SAO atmospheric seeing (start)", 
		*status );

  astSetFitsF ( fitschan, "SEEINGSEN", seeingEn,
		"[arcsec] SAO atmospheric seeing (end)", 
		*status );

  astSetFitsS ( fitschan, "SEEDATST", seeDatSt,
		"Date/Time of SEEINGST", *status );

  astSetFitsS ( fitschan, "SEEDATEN", seeDatEn,
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
  if ( DEBUGLVL > 0 ) printf ( "Writing Map FITS headers...\n" );

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

    astSetFitsF ( fitschan, "CHOP_FRQ", chopFrq,
		  "[Hz] Chop frequency", *status );

    astSetFitsF ( fitschan, "CHOP_PA", chopPA,
		  "[deg] Chop PA; 0=in lat, 90=in long", *status );

    astSetFitsF ( fitschan, "CHOP_THR", chopThr,
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

    astSetFitsF ( fitschan, "MAP_X", mapX,
		  "[arcsec] Requested map offset from telescope centre",
		  *status );

    astSetFitsF ( fitschan, "MAP_Y", mapY,
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
  if ( DEBUGLVL > 0 ) printf ( "Writing SMU FITS headers...\n" );

  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- SMU ----", *status );

  astSetFitsF ( fitschan, "ALIGN_DX", alignDX, 
		"SMU tables X axis focus offset", *status );

  astSetFitsF ( fitschan, "ALIGN_DY", alignDY, 
		"SMU tables Y axis focus offset", *status );

  astSetFitsF ( fitschan, "FOCUS_DZ", focusDZ, 
		"SMU tables Z axis focus offset", *status );

  astSetFitsF ( fitschan, "DAZ", dAz, 
		"SMU azimuth pointing offset", *status );

  astSetFitsF ( fitschan, "DEL", dEl, 
		"SMU elevation pointing offset", *status );

  astSetFitsF ( fitschan, "UAZ", uAz, 
		"User azimuth pointing offset", *status );

  astSetFitsF ( fitschan, "UEL", uEl, 
		"User elevation pointing offset", *status );


  /* JOS parameters */
  if ( DEBUGLVL > 0 ) printf ( "Writing JOS FITS headers...\n" );

  astSetFitsCN ( fitschan, "COMMENT", "", 
                 "---- JOS parameters ----", *status );

  astSetFitsF ( fitschan, "STEPTIME", stepTime, 
                "RTS step time during an RTS sequence", *status ); 

  astSetFitsI ( fitschan, "NUM_CYC", numCyc, 
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
  if ( DEBUGLVL > 0 ) printf ( "Writing MISC FITS headers...\n" ); 

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
  if ( DEBUGLVL > 0 ) printf ( "Writing ROVER FITS headers...\n" ); 

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
