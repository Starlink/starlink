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
*     sc2sim_ndfwrdata ( struct sc2sim_obs_struct *inx, 
*                        struct sc2sim_sim_struct *sinx,
*                        double meanwvm, char file_name[], 
*                        int numsamples, int nflat, char *flatname, 
*                        JCMTState *head, int *dbuf, int *dksquid, 
*                        double *fcal, double *fpar, char filter[], 
*                        double *posptr, int jigsamples, double jigptr[][2],
*                        int *status )

*  Arguments:
*     inx = sc2sim_obs_struct* (Given)
*        Pointer to struct with observation parameters
*     sinx = sc2sim_sim_struct* (Given)
*        Pointer to struct with simulation parameters
*     meanwvm = double (Given)
*        225 GHz tau
*     file_name = char[] (Given)
*        Output file name 
*     numsamples = int (Given)
*        Number of samples 
*     nflat = int (Given)
*        Number of flat coeffs per bol
*     flatname = char*
*        Name of flatfield algorithm 
*     head = JCMTState* (Given)
*        Header data for each frame 
*     dbuf = int* (Given)
*        Simulated data
*     dksquid = int* (Given)
*        Dark SQUID time stream data 
*     fcal = double* (Given)
*        Flatfield calibration 
*     fpar = double (Given)
*        Flat-field parameters
*     filter = char[] (Given)
*        String representing filter (e.g. "850") 
*     posptr = double* (Given)
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

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
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
struct sc2sim_obs_struct *inx,  /* structure for values from XML (given) */
struct sc2sim_sim_struct *sinx, /* structure for sim values from XML (given)*/
double meanwvm,   /* 225 GHz tau */
char file_name[], /* output file name (given) */
int numsamples,   /* number of samples (given) */
int nflat,        /* number of flat coeffs per bol (given) */
char *flatname,   /* name of flatfield algorithm (given) */
JCMTState *head,  /* header data for each frame (given) */
int *dbuf,        /* simulated data (given) */
int *dksquid,     /* dark SQUID time stream data (given) */
double *fcal,     /* flatfield calibration (given) */
double *fpar,     /* flat-field parameters (given) */
char instrume[],  /* String representing instrument (e.g. "SCUBA-2") (given) */
char filter[],    /* String representing filter (e.g. "850") (given) */
char *dateobs,    /* String representing UTC DATE-OBS */
double *posptr,   /* Pointing offsets from map centre (given) */
int jigsamples,   /* Number of jiggle samples (given) */
double jigptr[][2], /* Array of X, Y jiggle positions (given) */
int *status       /* global status (given and returned) */
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

   /* Add the FITS data to the output file */
   fitschan = astFitsChan ( NULL, NULL, "" );
   astSetFitsS ( fitschan, "DATE-OBS", dateobs, "observation date", 0 );
   /* We need to write this - the simulator effectively assumes all
      times are TAI */
   astSetFitsS ( fitschan, "TIMESYS", "UTC", "Time scale for DATE-OBS", 0 );
   astSetFitsF ( fitschan, "DUT1", inx->dut1, "UT1 - UTC correction (days)", 0 );

   rad = inx->ra * AST__DR2D;
   astSetFitsF ( fitschan, "RA", rad, "Right Ascension of observation", 0 );
   decd = inx->dec * AST__DR2D;
   astSetFitsF ( fitschan, "DEC", decd, "Declination of observation", 0 );
   astSetFitsI ( fitschan, "ADD_ATM", sinx->add_atm, 
                 "flag for adding atmospheric emission", 0 );
   astSetFitsI ( fitschan, "ADDFNOIS", sinx->add_fnoise, "flag for adding 1/f noise", 0 );
   astSetFitsI ( fitschan, "ADD_PNS", sinx->add_pns, "flag for adding photon noise", 0 );
   astSetFitsI ( fitschan, "FLUX2CUR", sinx->flux2cur, 
                 "flag for converting flux to current", 0 );
   astSetFitsI ( fitschan, "NBOLX", inx->nbolx, "number of bolometers in X direction", 0 );
   astSetFitsI ( fitschan, "NBOLY", inx->nboly, "number of bolometers in Y direction", 0 );
   astSetFitsF ( fitschan, "SAMPLE_T", inx->sample_t, "sample interval in msec", 0 );
   astSetFitsS ( fitschan, "SUBARRAY", sinx->subname, "subarray name", 0 );
   astSetFitsI ( fitschan, "NUMSAMP", numsamples, "number of samples", 0 );
   astSetFitsF ( fitschan, "AMSTART", sinx->airmass, "Air mass at start", 0 );
   astSetFitsF ( fitschan, "AMEND", sinx->airmass, "Air mass at end", 0 );
   astSetFitsF ( fitschan, "MEANWVM", meanwvm, 
                 "Mean zenith tau at 225 GHz from WVM", 0 );
   astSetFitsS ( fitschan, "FILTER", filter, "filter used", 0 );
   astSetFitsF ( fitschan, "WAVELEN", inx->lambda, "Wavelength (m)", 0 );
   astSetFitsF ( fitschan, "ATSTART", sinx->atstart, 
                 "Ambient temperature at start (C)", 0 );
   astSetFitsF ( fitschan, "ATEND", sinx->atend, "Ambient temperature at end (C)", 0 );
   astSetFitsS ( fitschan, "OBSMODE", inx->obsmode, "Observing mode", 0 );
   astSetFitsS ( fitschan, "INSTRUME", instrume, "Instrument name", 0 );
   astSetFitsS ( fitschan, "TELESCOP", "JCMT", "Name of telescope", 0 );

   astSetFitsS ( fitschan, "INSTAP", inx->instap, "Instrument aperture", 0 );
   astSetFitsF ( fitschan, "INSTAP_X", inx->instap_x, "X focal plane offset (arcsec)", 0 );
   astSetFitsF ( fitschan, "INSTAP_Y", inx->instap_y, "Y focal plane offset (arcsec)", 0 );
   
   if ( strncmp( inx->obsmode, "DREAM", 5) == 0 ) {

      astSetFitsI ( fitschan, "JIGL_CNT", inx->nvert, 
                    "Number of positions in DREAM pattern", 0 );
      astSetFitsI ( fitschan, "NJIGLCYC", sinx->ncycle, 
                    "Number of times around DREAM pattern", 0 );
      astSetFitsF ( fitschan, "JIGSTEP", inx->jig_step_x, 
                    "Size of jiggle step (arcsec)", 0 );


     /* Construct weights name from subarray */
     strncat( weightsname, "dreamweights_", 13);
     strncat( weightsname, sinx->subname, 3);
     strncat( weightsname, ".sdf", 4);
     astSetFitsS ( fitschan, "DRMWGHTS", weightsname, 
                   "Name of DREAM weights file", 0 );

  }

   /* Determine extent of the map from posptr + known size of the arrays */
   for( i=0; i<numsamples; i++ ) {
    
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
 
   map_wdth = (x_max - x_min) + 1000; /* 1000 arcsec for array FOV */
   map_hght = (y_max - y_min) + 1000; /* 1000 arcsec for array FOV */
   map_pa = 0; /* kludge */
   map_x = (x_max + x_min)/2.;
   map_y = (y_max + y_min)/2.;
  
   astSetFitsF ( fitschan, "MAP_HGHT", map_hght, "Map height (arcsec)", 0 );
   astSetFitsF ( fitschan, "MAP_WDTH", map_wdth, "Map width (arcsec)", 0 );
   astSetFitsF ( fitschan, "MAP_PA", map_pa, "Map PA (degrees)", 0 );
   astSetFitsF ( fitschan, "MAP_X", map_x, "Map X offset (arcsec)", 0 );
   astSetFitsF ( fitschan, "MAP_Y", map_y, "Map Y offset (arcsec)", 0 );

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
