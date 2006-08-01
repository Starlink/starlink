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
*     sc2sim_ndfwrdata ( double ra, double dec, int add_atm, int add_fnoise,
*                        int add_pns, int flux2cur, double amstart, double amend,
*                        double meanwvm, double obslam, char file_name[], 
*                        int ncol, int nrow, double sample_t, char subarray[],
*                        int numsamples, int nflat, char *flatname, 
*                        struct sc2head *head, int *dbuf, int *dksquid, 
*                        double *fcal, double *fpar, char filter[], 
*                        double atstart, double atend, double *posptr,
*                        char *obsmode, int *status )

*  Arguments:
*     ra = double (Given)
*        RA of observation in radians
*     dec = double (Given)
*        Dec of observation in radians
*     add_atm = int (Given)
*        Flag for adding atmospheric emission 
*     add_fnoise = int (Given)
*        Flag for adding 1/f noise 
*     add_pns = int (Given)
*        Flag for adding photon noise 
*     flux2cur = int (Given)
*        Flag for converting flux to current
*     amstart = double (Given)
*        Airmass at beginning 
*     amend = double (Given)
*        Airmass at end 
*     meanwvm = double (Given)
*        225 GHz tau
*     obslam = double (Given)
*        Wavelength 
*     file_name = char[] (Given)
*        Output file name 
*     ncol = int (Given)
*        Number of bolometers in column 
*     nrow = int (Given)
*        Number of bolometers in row 
*     sample_t = double (Given)
*        Sample interval in msec 
*     subarray = char[] (Given)
*        Name of the subarray
*     numsamples = int (Given)
*        Number of samples 
*     nflat = int (Given)
*        Number of flat coeffs per bol
*     flatname = char*
*        Name of flatfield algorithm 
*     head = sc2head* (Given)
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
*     atstart = double (Given)
*        Ambient temperature at start (Celsius) 
*     atend = double (Given)
*        Ambient temperature at end (Celsius) 
*     posptr = double* (Given)
*        Pointing offsets from map centre
*     obsmode = char* (Given)
*        Observing mode
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Create and map a SCUBA-2 NDF file. Scale the data to integers one
*     frame at a time and add a compressed version to the mapped file.
*     Store the per-frame header items and the FITS headers.

*  Authors:
*     E.Chapin (UBC)
*     A.Gibb (UBC)
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
double ra,        /* RA of observation in radians (given) */
double dec,       /* Dec of observation in radians (given) */
int add_atm,      /* flag for adding atmospheric emission (given) */
int add_fnoise,   /* flag for adding 1/f noise (given) */
int add_pns,      /* flag for adding photon noise (given) */
int flux2cur,     /* flag for converting flux to current (given) */
double amstart,   /* Airmass at beginning (given) */
double amend,     /* Airmass at end (given) */
double meanwvm,   /* 225 GHz tau */
double obslam,    /* Wavelength */
char file_name[], /* output file name (given) */
int ncol,         /* number of bolometers in column (given) */
int nrow,         /* number of bolometers in row (given) */
double sample_t,  /* sample interval in msec (given) */
char subarray[],  /* name of the subarray */
int numsamples,   /* number of samples (given) */
int nflat,        /* number of flat coeffs per bol (given) */
char *flatname,   /* name of flatfield algorithm (given) */
JCMTState *head,  /* header data for each frame (given) */
int *dbuf,        /* simulated data (given) */
int *dksquid,     /* dark SQUID time stream data (given) */
double *fcal,     /* flatfield calibration (given) */
double *fpar,     /* flat-field parameters (given) */
char filter[],    /* String representing filter (e.g. "850") (given) */
double atstart,   /* Ambient temperature at start (Celsius) (given) */
double atend,     /* Ambient temperature at end (Celsius) (given) */
double *posptr,   /* Pointing offsets from map centre */
char *obsmode,    /* Observing mode */
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
  
   rad = ra * AST__DR2D;
  
   fhead_putfits ( TDOUBLE,
		   "RA", &rad,
		   "Right Ascension of observation", status );
  
   decd = dec * AST__DR2D;
  
   fhead_putfits ( TDOUBLE,
		   "DEC", &decd,
		   "Declination of observation", status );
  
   fhead_putfits ( TINT,
		   "ADD_ATM", &add_atm,
		   "flag for adding atmospheric emission", status );
  
   fhead_putfits ( TINT,
		   "ADDFNOIS", &add_fnoise,
		   "flag for adding 1/f noise", status );
  
   fhead_putfits ( TINT,
		   "ADD_PNS", &add_pns,
		   "flag for adding photon noise", status );
  
   fhead_putfits ( TINT,
		   "FLUX2CUR", &flux2cur,
		   "flag for converting flux to current", status );
  
   fhead_putfits ( TINT,
		   "NBOLX", &ncol,
		   "number of bolometers in X direction", status );

   fhead_putfits ( TINT,
		   "NBOLY", &nrow,
		   "number of bolometers in Y direction", status );
  
   fhead_putfits ( TDOUBLE,
		   "SAMPLE_T", &sample_t,
		   "The sample interval in msec", status );
  
   fhead_putfits ( TSTRING,
		   "SUBARRAY", subarray,
		   "subarray name", status );
  
   fhead_putfits ( TINT,
		   "NUMSAMP", &numsamples,
		   "number of samples", status );
  
   fhead_putfits ( TDOUBLE,
		   "AMSTART", &amstart,
		   "Air mass at start", status );
  
   fhead_putfits ( TDOUBLE,
		   "AMEND", &amend,
		   "Air mass at end", status );
  
   fhead_putfits ( TDOUBLE,
		   "MEANWVM", &meanwvm,
		   "Mean zenith tau at 225 GHz from WVM", status );
  
   fhead_putfits ( TSTRING,
		   "FILTER", filter,
		   "filter used", status );
  
   fhead_putfits ( TDOUBLE,
		   "ATSTART", &atstart,
		   "Ambient temperature at start (C)", status );
  
   fhead_putfits ( TDOUBLE,
		   "ATEND", &atend,
		   "Ambient temperature at end (C)", status );

   fhead_putfits ( TSTRING,
		   "OBSMODE", obsmode,
		   "Observing mode", status );

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
   sc2store_wrtstream ( file_name, nrec, fitsrec, ncol, nrow, numsamples, 
		        nflat, flatname, head, dbuf, dksquid, fcal, fpar, 
		        status );
 
   /* Close the file */
   sc2store_free ( status );

}//sc2sim_ndfwrdata
