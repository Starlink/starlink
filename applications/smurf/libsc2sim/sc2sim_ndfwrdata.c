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
*     sc2sim_ndfwrdata ( struct dxml_struct *inx, struct dxml_sim_struct *sinx,
*                        double meanwvm, char file_name[], 
*                        int numsamples, int nflat, char *flatname, 
*                        JCMTState *head, int *dbuf, int *dksquid, 
*                        double *fcal, double *fpar, char filter[], 
*                        double *posptr, int jigsamples, double jigptr[][2],
*                        int *status )

*  Arguments:
*     inx = dxml_struct* (Given)
*        Pointer to struct with observation parameters
*     sinx = dxml_sim_struct* (Given)
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
#include "fitsio.h"
#include "ndf.h"

/* SC2SIM includes */
#include "sc2sim.h"
#include "fhead.h"
#include "fhead_par.h"
#include "sc2da/sc2store.h"

void sc2sim_ndfwrdata
( 
struct dxml_struct *inx,      /* structure for values from XML (given) */
struct dxml_sim_struct *sinx, /* structure for sim values from XML (given)*/
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
char filter[],    /* String representing filter (e.g. "850") (given) */
double *posptr,   /* Pointing offsets from map centre (given) */
int jigsamples,   /* Number of jiggle samples (given) */
double jigptr[][2], /* Array of X, Y jiggle positions (given) */
int *status       /* global status (given and returned) */
)

{
   /* Local variables */
   double decd;                     /* Dec of observation in degrees */
   static char fitsrec[FHEAD__MXREC][81];  /* store for FITS records */
   int i;                           /* Loop counter */
   int nrec;                        /* number of FITS header records */
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

   /* Check status */
   if ( !StatusOkP(status) ) return;

   /* Format the FITS headers */

   fhead_init ( status );
  
   fhead_putfits ( TSTRING,
		   "DATE-OBS", "YYYY-MM-DDThh:mm:ss",
		   "observation date", status );
  
   rad = inx->ra * AST__DR2D;
  
   fhead_putfits ( TDOUBLE,
		   "RA", &rad,
		   "Right Ascension of observation", status );
  
   decd = inx->dec * AST__DR2D;
  
   fhead_putfits ( TDOUBLE,
		   "DEC", &decd,
		   "Declination of observation", status );
  
   fhead_putfits ( TINT,
		   "ADD_ATM", &sinx->add_atm,
		   "flag for adding atmospheric emission", status );
  
   fhead_putfits ( TINT,
		   "ADDFNOIS", &sinx->add_fnoise,
		   "flag for adding 1/f noise", status );
  
   fhead_putfits ( TINT,
		   "ADD_PNS", &sinx->add_pns,
		   "flag for adding photon noise", status );
  
   fhead_putfits ( TINT,
		   "FLUX2CUR", &sinx->flux2cur,
		   "flag for converting flux to current", status );
  
   fhead_putfits ( TINT,
		   "NBOLX", &inx->nbolx,
		   "number of bolometers in X direction", status );

   fhead_putfits ( TINT,
		   "NBOLY", &inx->nboly,
		   "number of bolometers in Y direction", status );
  
   fhead_putfits ( TDOUBLE,
		   "SAMPLE_T", &inx->sample_t,
		   "The sample interval in msec", status );
  
   fhead_putfits ( TSTRING,
		   "SUBARRAY", sinx->subname,
		   "subarray name", status );
  
   fhead_putfits ( TINT,
		   "NUMSAMP", &numsamples,
		   "number of samples", status );
  
   fhead_putfits ( TDOUBLE,
		   "AMSTART", &sinx->airmass,
		   "Air mass at start", status );
  
   fhead_putfits ( TDOUBLE,
		   "AMEND", &sinx->airmass,
		   "Air mass at end", status );
  
   fhead_putfits ( TDOUBLE,
		   "MEANWVM", &meanwvm,
		   "Mean zenith tau at 225 GHz from WVM", status );
  
   fhead_putfits ( TSTRING,
		   "FILTER", filter,
		   "filter used", status );
  
   fhead_putfits ( TDOUBLE,
		   "ATSTART", &sinx->atstart,
		   "Ambient temperature at start (C)", status );
  
   fhead_putfits ( TDOUBLE,
		   "ATEND", &sinx->atend,
		   "Ambient temperature at end (C)", status );

   fhead_putfits ( TSTRING,
		   "OBSMODE", inx->obsmode,
		   "Observing mode", status );

   fhead_putfits ( TSTRING,
		   "INSTRUME", "SCUBA-2",
		   "Instrument type", status );
   
  if ( strncmp( inx->obsmode, "DREAM", 5) == 0 ) {
    fhead_putfits ( TINT,
		    "JIGL_CNT", &inx->nvert,
		    "Number of positions in DREAM pattern", status );
    fhead_putfits ( TINT,
		    "NJIGLCYC", &sinx->ncycle,
		    "Number of times around DREAM pattern", status );
    fhead_putfits ( TDOUBLE,
		    "JIGSTEP", &inx->jig_step_x,
		    "Size of jiggle step (arcsec)", status );
  }

   /* Determine extent of the map from posptr + known size of the arrays */
   for( i=0; i<numsamples; i++ ) {
    
      if( i == 0 ) {
         x_min = posptr[0];
         x_max = posptr[0];
         y_min = posptr[1];
         y_max = posptr[1];
      }//if
    
      if( posptr[i*2] < x_min ) x_min = posptr[i*2];
      if( posptr[i*2] > x_max ) x_max = posptr[i*2];
      if( posptr[i*2+1] < y_min ) y_min = posptr[i*2+1];
      if( posptr[i*2+1] > y_max ) y_max = posptr[i*2+1];

   }//for
 
   map_wdth = (x_max - x_min) + 1000; /* 1000 arcsec for array FOV */
   map_hght = (y_max - y_min) + 1000; /* 1000 arcsec for array FOV */
   map_pa = 0; /* kludge */
   map_x = (x_max + x_min)/2.;
   map_y = (y_max + y_min)/2.;
  
   fhead_putfits ( TDOUBLE,
		   "MAP_HGHT", &map_hght,
		   "Map height (arcsec)", status );
   fhead_putfits ( TDOUBLE,
		   "MAP_WDTH", &map_wdth,
		   "Map height (arcsec)", status );
   fhead_putfits ( TDOUBLE,
		   "MAP_PA", &map_pa,
		   "Map PA (degrees)", status );
   fhead_putfits ( TDOUBLE,
		   "MAP_X", &map_x,
		   "Map X offset (arcsec)", status );
   fhead_putfits ( TDOUBLE,
		   "MAP_Y", &map_y,
		   "Map Y offset (arcsec)", status );
  
   /* Get the accumulated FITS headers */ 
   fhead_getfits ( &nrec, fitsrec, status );
  
   /* Store the timestream data */
   sc2store_wrtstream ( file_name, nrec, fitsrec, inx->nbolx, inx->nboly, 
			numsamples, nflat, flatname, head, dbuf, dksquid, 
			fcal, fpar, inx->obsmode, inx->jig_vert, inx->nvert, 
			jigptr, jigsamples, status );
 
   /* Close the file */
   sc2store_free ( status );

}//sc2sim_ndfwrdata
