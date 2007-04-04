/*
*+
*  Name:
*     sc2sim_ndfwrdata

*  Purpose:
*     Generic digitise/compress and store SC2 data as NDF

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_ndfwrdata ( const struct sc2sim_obs_struct *inx, 
*                        const struct sc2sim_sim_struct *sinx,
*                        double meanwvm, const char file_name[], 
*                        int numsamples, int nflat, const char flatname[], 
*                        const JCMTState *head, const int *dbuf, const int *dksquid, 
*                        const double *fcal, const double *fpar, const char instrume[],
*                        const char filter[], const char dateobs[], const char obsid[],
*                        const double *posptr, int jigsamples, const double jigptr[][2],
*                        int *status )

*  Arguments:
*     inx = const sc2sim_obs_struct* (Given)
*        Pointer to struct with observation parameters
*     sinx = const sc2sim_sim_struct* (Given)
*        Pointer to struct with simulation parameters
*     meanwvm = double (Given)
*        225 GHz tau
*     file_name = const char[] (Given)
*        Output file name 
*     numsamples = int (Given)
*        Number of samples 
*     nflat = int (Given)
*        Number of flat coeffs per bol
*     flatname = const char[] (Given)
*        Name of flatfield algorithm 
*     head = const JCMTState* (Given)
*        Header data for each frame 
*     dbuf = const int* (Given)
*        Simulated data
*     dksquid = const int* (Given)
*        Dark SQUID time stream data 
*     fcal = const double* (Given)
*        Flatfield calibration 
*     fpar = double (Given)
*        Flat-field parameters
*     instrume = const char[] (Given)
*        Instrument name (usually SCUBA-2)
*     filter = const char[] (Given)
*        String representing filter (e.g. "850") 
*     dateobs = const char[] (Given)
*        DATE-OBS FITS string
*     obsid = const char[] (Given)
*        Observation ID string
*     posptr = const double* (Given)
*        Pointing offsets from map centre
*     jigsamples = int (Given)
*        Number of jiggle samples in DREAM pattern
*     jigptr[][2] = double (Given)
*        Array of jiggle X and Y positions
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Create and map a SCUBA-2 NDF file. Scale the data to integers one
*     frame at a time and add a compressed version to the mapped file.
*     Store the per-frame header items and the FITS headers.

*  Authors:
*     E.Chapin (UBC)
*     A.G. Gibb (UBC)
*     J. Balfour (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History :
*     2006-03-29 (EC):
*        dsim_ndfwrdata adapted from dsim_ndfwrpong
*     2006-05-11 (AGG)
*        Added obsmode
*     2006-07-21 (JB):
*        Split from dsim.c
*     2006-07-28 (JB):
*        Changed sc2head to JCMTState
*     2006-08-08 (EC):
*        Added INSTRUME FITS keyword
*     2006-08-18 (AGG):
*        Update API to take:
*        - pointers to inx and sinx structs
*        - DREAM jiggle position parameters
*     2006-09-06 (EC):
*        INSTRUME keyword now taken as argument (to accomodate AzTEC)
*     2006-09-15 (AGG):
*        Write out name of DREAM weights file into FITS header
*     2006-09-22 (JB):
*        Replace dxml_structs with sc2sim_structs
*     2006-10-06 (AGG):
*        Add WAVELEN FITS keyword
*     2006-10-26 (JB):
*        Convert to using AstFitsChans
*     2006-12-01 (AGG):
*        Now takes dateobs string, writes TIMESYS FITS header
*     2006-12-15 (AGG):
*        Write out DUT1 FITS header
*     2006-12-19 (TIMJ):
*        sc2store_wrtstream has additional subnum argument
*     2006-12-21 (AGG):
*        Add instap & instap_x/y FITS headers
*     2007-03-20 (TIMJ):
*        - Write header units in compliance with FITS standard
*        - Use const arguments and add OBSID argument/header
*     2007-04-02 (AGG):
*        Add more FITS headers

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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
#include "ndf.h"
#include "star/kaplibs.h"

/* SC2SIM includes */
#include "sc2sim.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2ast.h"

void sc2sim_ndfwrdata
( 
const struct sc2sim_obs_struct *inx,  /* structure for values from XML (given) */
const struct sc2sim_sim_struct *sinx, /* structure for sim values from XML (given)*/
double meanwvm,          /* 225 GHz tau */
const char file_name[],  /* output file name (given) */
int numsamples,          /* number of samples (given) */
int nflat,               /* number of flat coeffs per bol (given) */
const char flatname[],   /* name of flatfield algorithm (given) */
const JCMTState *head,   /* header data for each frame (given) */
const int *dbuf,         /* simulated data (given) */
const int *dksquid,      /* dark SQUID time stream data (given) */
const double *fcal,      /* flatfield calibration (given) */
const double *fpar,      /* flat-field parameters (given) */
const char instrume[],   /* String representing instrument (e.g. "SCUBA-2") (given) */
const char filter[],     /* String representing filter (e.g. "850") (given) */
const char dateobs[],    /* String representing UTC DATE-OBS */
const char obsid[],      /* unique obsid for this observation (given) */
const double *posptr,    /* Pointing offsets from map centre (given) */
int jigsamples,          /* Number of jiggle samples (given) */
const double jigptr[][2], /* Array of X, Y jiggle positions (given) */
const int obsnum,        /* Observation number (given) */
const int nsubscan,      /* Sub-scan number (given) */
const char obstype[],    /* Observation type, e.g. SCIENCE (given)*/
const char utdate[],     /* UT date in YYYYMMDD form (given) */
const double azstart,    /* Azimuth at start of sub-scan (given) */
const double azend,      /* Azimuth at end of sub-scan (given) */
const double elstart,    /* Elevation at start of sub-scan (given) */
const double elend,      /* Elevation at end of sub-scan (given) */
const char lststart[],   /* LST at start of sub-scan (given) */
const char lstend[],     /* LST at end of sub-scan (given) */
const char loclcrd[],    /* Coordinate frame (given) */
const char scancrd[],    /* SCAN coordinate frame (given) */
const double totaltime,  /* Total integration time (given) */
const double exptime,    /* Subimage exposure time (given) */
const int nimage,        /* Number of subimages within subscan (given) */
int *status              /* Global status (given and returned) */
)

{
   /* Local variables */
   double decd;                     /* Dec of observation in degrees */
   AstFitsChan *fitschan;           /* FITS headers */
   char fitsrec[SC2STORE__MAXFITS][SZFITSCARD]; /* Store for FITS records */
   int i;                           /* Loop counter */
   int nrec;                        /* number of FITS header records */
   int subnum;                      /* sub array index */
   double rad;                      /* RA of observation in degrees */
   double map_hght;   /* Map height in arcsec */
   double map_wdth;   /* Map width in arcsec  */
   double map_pa;     /* Map PA in degrees  */
   double map_x = 0;  /* Map X offset in arcsec */
   double map_y = 0;  /* Map Y offset in arcsec */
   double x_min = 0;  /* Maximum extend of pointing centre offsets */
   double x_max = 0;
   double y_min = 0;
   double y_max = 0;

   char weightsname[81];             /* Name of weights file for DREAM 
					reconstruction */

   /* Check status */
   if ( !StatusOkP(status) ) return;

   /* This calc should go in a higher level routine */
   /* Determine extent of the map from posptr + known size of the arrays */
   for( i=0; i<numsamples; i++ ) {
     /* Initialize extrema */
     if( i == 0 ) {
       x_min = posptr[0];
       x_max = posptr[0];
       y_min = posptr[1];
       y_max = posptr[1];
     }
     if( posptr[i*2] < x_min ) x_min = posptr[i*2];
     if( posptr[i*2] > x_max ) x_max = posptr[i*2];
     if( posptr[i*2+1] < y_min ) y_min = posptr[i*2+1];
     if( posptr[i*2+1] > y_max ) y_max = posptr[i*2+1];
   }
   map_wdth = (x_max - x_min) + 650.0; /* 650 arcsec for array diagonal FOV */
   map_hght = (y_max - y_min) + 650.0; /* 650 arcsec for array diagonal FOV */
   map_pa = 0; /* kludge for now since it is not specified by the user */
   map_x = (x_max + x_min)/2.;
   map_y = (y_max + y_min)/2.;
  

   /* Define the FITS headers to add to the output file */
   fitschan = astFitsChan ( NULL, NULL, "" );

   /* Telescope */
   astSetFitsS ( fitschan, "COMMENT", "", "-- Telescope specific parameters --", 0 );
   astSetFitsS ( fitschan, "TELESCOP", "JCMT", "Name of telescope", 0 );
   astSetFitsS ( fitschan, "ORIGIN", "SMURF SCUBA-2 simulator", 
		 "Origin of file", 0 );
   astSetFitsF ( fitschan, "OBSGEO-X", -1.601185365E+06, 
		 "x,y,z triplet for JCMT", 0 );
   astSetFitsF ( fitschan, "OBSGEO-Y", -5.041977547E+06, 
		 "relative to centre of the Earth", 0 );
   astSetFitsF ( fitschan, "OBSGEO-Z", 3.554875870E+06, "", 0 );
   astSetFitsF ( fitschan, "ALT-OBS", 4092, 
		 "[m] Height of observatory above sea level", 0 );
   astSetFitsF ( fitschan, "LAT-OBS", 19.8258323669, 
		 "[deg] Latitude of observatory", 0 );
   astSetFitsF ( fitschan, "LONG-OBS", 204.520278931, 
		 "[deg] East longitude of observatory", 0 );
   astSetFitsF ( fitschan, "ETAL", 1.0, "Telescope efficiency", 0 );

   /* Observation, date & pointing */
   astSetFitsS ( fitschan, "COMMENT", "", "-- Observation & date parameters --", 0 );
   astSetFitsS ( fitschan, "OBSID", obsid, "Unique observation ID", 0 );
   astSetFitsS ( fitschan, "OBJECT", "Hoonose", "Object Name", 0 );
   astSetFitsL ( fitschan, "STANDARD", 0, "True if source is a calibrator", 0 );
   astSetFitsI ( fitschan, "OBSNUM", obsnum, "Observation Number", 0 );
   astSetFitsI ( fitschan, "NSUBSCAN", nsubscan, "Sub-scan Number", 0 );
   astSetFitsL ( fitschan, "OBSEND", 0, 
		 "True if frame is last in current observation", 0 );
   astSetFitsS ( fitschan, "UTDATE", utdate, 
		 "UT date as a string in yyyymmdd format", 0 );
   astSetFitsS ( fitschan, "DATE-OBS", dateobs, 
		 "Date and time (UTC) of start of sub-scan", 0 );
   astSetFitsS ( fitschan, "DATE-END", dateobs, 
		 "Date and time (UTC) of end of sub-scan", 0 );
   astSetFitsF ( fitschan, "DUT1", inx->dut1, "[d] UT1 - UTC correction", 0 );
   astSetFitsS ( fitschan, "INSTAP", inx->instap, "Instrument aperture", 0 );
   astSetFitsF ( fitschan, "INSTAP_X", inx->instap_x, 
		 "[arcsec] X focal plane offset", 0 );
   astSetFitsF ( fitschan, "INSTAP_Y", inx->instap_y, 
		 "[arcsec] Y focal plane offset", 0 );
   astSetFitsF ( fitschan, "AMSTART", 1./cos(AST__DPIBY2-elstart), 
		 "Air mass at start", 0 );
   astSetFitsF ( fitschan, "AMEND", 1./cos(AST__DPIBY2-elend), 
		 "Air mass at end", 0 );
   astSetFitsF ( fitschan, "AZSTART", AST__DR2D*azstart, 
		 "[deg] Azimuth at sub-scan start", 0 );
   astSetFitsF ( fitschan, "AZEND", AST__DR2D*azend, 
		 "[deg] Azimuth at sub-scan end", 0 );
   astSetFitsF ( fitschan, "ELSTART", AST__DR2D*elstart, 
		 "[deg] Elevation at sub-scan start", 0 );
   astSetFitsF ( fitschan, "ELEND", AST__DR2D*elend, 
		 "[deg] Elevation at sub-scan end", 0 );
   astSetFitsS ( fitschan, "LSTSTART", lststart, "LST at start of sub-scan", 0 );
   astSetFitsS ( fitschan, "LSTEND", lstend, "LST at end of sub-scan", 0 );

   /* Environment */
   astSetFitsS ( fitschan, "COMMENT", "", "-- Environment parameters --", 0 );
   astSetFitsF ( fitschan, "ATSTART", sinx->atstart, 
                 "[deg C] Ambient temperature at start", 0 );
   astSetFitsF ( fitschan, "ATEND", sinx->atend, 
		 "[deg C] Ambient temperature at end", 0 );

   /* OMP & ORAC-DR */
   astSetFitsS ( fitschan, "COMMENT", "", "-- OMP & ORAC-DR parameters --", 0 );
   astSetFitsS ( fitschan, "PROJECT", "M08AC00", 
		 "The proposal ID for the PROJECT", 0 );
   astSetFitsS ( fitschan, "RECIPE", "", "The ORAC-DR recipe", 0 );
   astSetFitsS ( fitschan, "DRGROUP", "", 
		 "Name of group to combine current observation with", 0 );
   astSetFitsS ( fitschan, "MSBID", "", "ID of min schedulable block", 0 );
   astSetFitsS ( fitschan, "MSBTID", "", "Translation ID of MSB", 0 );
   astSetFitsS ( fitschan, "SURVEY", "", "Survey Name", 0 );

   /* SCUBA-2 */
   astSetFitsS ( fitschan, "COMMENT", "", "-- SCUBA-2 specific parameters --", 0 );
   astSetFitsS ( fitschan, "SUBARRAY", sinx->subname, "subarray name", 0 );
   astSetFitsS ( fitschan, "SHUTTER", "", "Shutter position for dark frames", 0 );
   astSetFitsS ( fitschan, "FILTER", filter, "filter used", 0 );
   astSetFitsF ( fitschan, "WAVELEN", inx->lambda, "[m] Wavelength", 0 );

   /* Switching and mapping */
   astSetFitsS ( fitschan, "COMMENT", "", "-- Mapping parameters --", 0 );
   if ( strncmp( inx->obsmode, "DREAM", 5) == 0 || 
	strncmp( inx->obsmode, "STARE", 5) == 0 ) {
     astSetFitsS ( fitschan, "SAM_MODE", inx->obsmode, 
		   "Sample mode: STARE, DREAM or SCAN", 0 );
   } else {
     astSetFitsS ( fitschan, "SAM_MODE", "SCAN", 
		   "Sample mode: STARE, DREAM or SCAN", 0 );
   }
   astSetFitsS ( fitschan, "SW_MODE", "NONE", 
		 "Switch mode: CHOP, PSSW, FREQ, or NONE", 0 );
   astSetFitsS ( fitschan, "OBS_TYPE", obstype, 
		 "Observation type -  Science, Pointing or Focus", 0 );

   if ( strncmp( inx->obsmode, "DREAM", 5) == 0 ) {
     astSetFitsI ( fitschan, "JIGL_CNT", inx->nvert,
		   "Number of points in DREAM pattern", 0 );
     astSetFitsS ( fitschan, "JIGL_NAM", "",
		   "Name containing DREAM jiggle offsets", 0 );
     astSetFitsF ( fitschan, "JIGL_PA", 0,
		   "Number of points in DREAM pattern", 0 );
     astSetFitsS ( fitschan, "JIGL_CRD", "FPLANE",
		   "Coord frame of jiggle pattern", 0 );
     astSetFitsF ( fitschan, "JIG_SCAL", inx->jig_step_x, 
		   "[arcsec] SMU jiggle pattern scale factor", 0 );
     /* Construct weights name from subarray */
     strncat( weightsname, "dreamweights_", 13);
     strncat( weightsname, sinx->subname, 3);
     strncat( weightsname, ".sdf", 4);
     astSetFitsS ( fitschan, "DRMWGHTS", weightsname, 
                   "Name of DREAM weights file", 0 );
   } else {
     astSetFitsI ( fitschan, "JIGL_CNT", 0,
		   "Number of points in DREAM pattern", 0 );
     astSetFitsS ( fitschan, "JIGL_NAM", "",
		   "Name containing DREAM jiggle offsets", 0 );
     astSetFitsF ( fitschan, "JIGL_PA", 0,
		   "Number of points in DREAM pattern", 0 );
     astSetFitsS ( fitschan, "JIGL_CRD", "",
		   "Coord frame of jiggle pattern", 0 );
     astSetFitsF ( fitschan, "JIG_SCAL", 0.0, 
		   "[arcsec] SMU jiggle pattern scale factor", 0 );
     astSetFitsS ( fitschan, "DRMWGHTS", "", 
                   "Name of DREAM weights file", 0 );
   }
   astSetFitsF ( fitschan, "MAP_HGHT", map_hght, "[arcsec] Map height", 0 );
   astSetFitsF ( fitschan, "MAP_PA", map_pa, "[deg] Map PA", 0 );
   astSetFitsF ( fitschan, "MAP_WDTH", map_wdth, "[arcsec] Map width", 0 );
   astSetFitsS ( fitschan, "LOCL_CRD", loclcrd, 
		 "Local offset coordinate system", 0 );
   astSetFitsF ( fitschan, "MAP_X", map_x, "[arcsec] Map X offset", 0 );
   astSetFitsF ( fitschan, "MAP_Y", map_y, "[arcsec] Map Y offset", 0 );
   /* Fill in for obsmode = PONG only */
   if ( strncmp( inx->obsmode, "PONG", 4) == 0 ) {
     astSetFitsS ( fitschan, "SCAN_CRD", scancrd, "Scan coordinate system", 0 );
     astSetFitsF ( fitschan, "SCAN_VEL", inx->vmax, 
		   "[arcsec/s] Requested scanning rate", 0 );
     astSetFitsF ( fitschan, "SCAN_DY", inx->spacing, 
		   "[arcsec] Sample spacing perpendicular to scan", 0 );
     astSetFitsF ( fitschan, "SCAN_PA", inx->scan_angle, 
		   "[deg] Scan PA relative to N in SCAN_CRD system", 0 );
     astSetFitsS ( fitschan, "SCAN_PAT", "PONG", "Scanning pattern", 0 );
   } else {
     astSetFitsS ( fitschan, "SCAN_CRD", "", "Scan coordinate system", 0 );
     astSetFitsF ( fitschan, "SCAN_VEL", 0, 
		   "[arcsec/s] Requested scanning rate", 0 );
     astSetFitsF ( fitschan, "SCAN_DY", 0, 
		   "[arcsec] Sample spacing perpendicular to scan", 0 );
     astSetFitsF ( fitschan, "SCAN_PA", 0, 
		   "[deg] Scan PA relative to N in SCAN_CRD system", 0 );
     astSetFitsS ( fitschan, "SCAN_PAT", "", "Scanning pattern", 0 );
   }

   /* JOS parameters */
   astSetFitsS ( fitschan, "COMMENT", "", "-- JOS parameters --", 0 );
   astSetFitsF ( fitschan, "STEPTIME", inx->steptime, 
		 "[s] Time interval between samples", 0 );

   /* Integration time */
   astSetFitsS ( fitschan, "COMMENT", "", 
		 "-- Integration time-related parameters --", 0 );
   astSetFitsF ( fitschan, "INT_TIME", totaltime, 
		 "[s] Time spent integrating on source", 0 );
   /* Only write exp_time for DREAM and STARE*/
   if ( strncmp( inx->obsmode, "DREAM", 5) == 0 || 
	strncmp( inx->obsmode, "STARE", 5) == 0) {
     astSetFitsF ( fitschan, "EXP_TIME", exptime, 
		   "[s] Mean integration time per output pixel", 0 );
     astSetFitsI ( fitschan, "N_SUB", nimage, 
		   "Number of sub-scans written to file", 0 );
   } else {
     astSetFitsI ( fitschan, "N_SUB", 0, 
		   "Number of sub-scans written to file", 0 );
   }

   /* SMU specific */
   astSetFitsS ( fitschan, "COMMENT", "", "-- SMU-specific parameters --", 0 );

   /* Misc */
   astSetFitsS ( fitschan, "COMMENT", "", "-- Miscellaneous --", 0 );
   astSetFitsS ( fitschan, "OCSCFG", "config.xml", 
		 "Name of OCS Configuration XML file defining the observation", 0 );
   astSetFitsL ( fitschan, "SIMULATE", 1, "True if data produced by simulator", 0 );
   astSetFitsL ( fitschan, "SIM_SMU", 1, "True if SMU data are simulated", 0 );
   astSetFitsL ( fitschan, "SIM_RTS", 1, "True if RTS data are simulated", 0 );
   astSetFitsL ( fitschan, "SIM_TCS", 1, "True if TCS data are simulated", 0 );
   astSetFitsS ( fitschan, "STATUS", "NORMAL", 
		 "Status at obs. end - NORMAL or ABORT", 0 );

   /* Others... */
   astSetFitsL ( fitschan, "POL_CONN", 0, "True if polarimeter is in the beam", 0 );
   astSetFitsL ( fitschan, "FTS_CONN", 0, "True if FTS is used", 0 );

   /* We need to write this - the simulator effectively assumes all
      times are TAI */
   /*   astSetFitsS ( fitschan, "TIMESYS", "UTC", "Time scale for DATE-OBS", 0 );
   rad = inx->ra * AST__DR2D;
   astSetFitsF ( fitschan, "RA", rad, "Right Ascension of observation", 0 );
   decd = inx->dec * AST__DR2D;
   astSetFitsF ( fitschan, "DEC", decd, "Declination of observation", 0 );
   astSetFitsF ( fitschan, "MEANWVM", meanwvm, 
   "Mean zenith tau at 225 GHz from WVM", 0 );*/

   
   /* Convert the AstFitsChan data to a char array */
   smf_fits_export2DA ( fitschan, &nrec, fitsrec, status );

   /* Calculate the sub array index */
   sc2ast_name2num( sinx->subname, &subnum, status );

   /* Store the timestream data */
   sc2store_wrtstream ( file_name, subnum, nrec, fitsrec, inx->nbolx, 
                        inx->nboly, numsamples, nflat, flatname, head, 
                        dbuf, dksquid, fcal, fpar, inx->obsmode, 
                        inx->jig_vert, inx->nvert, jigptr, jigsamples, 
                        status );

   /* Close the file */
   sc2store_free ( status );

}
