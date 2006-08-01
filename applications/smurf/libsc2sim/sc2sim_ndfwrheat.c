/*
*+
*  Name:
*     sc2sim_ndfwrheat

*  Purpose:
*     Digitise and compress the heater simulation and store as NDF

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_ndfwrheat ( int add_atm, int add_fnoise, int add_pns, double heatstart,
                         double heatstep, char file_name[], 
*                        int ncol, int nrow, double sample_t, char subarray[],
*                        int numsamples, int nflat, char *flatname, 
*                        struct sc2head *head, int *dbuf, int *dksquid, 
*                        double *fcal, double *fpar, char filter[], 
*                        double atstart, double atend, int *status )

*  Arguments:
*     add_atm = int (Given)
*        Flag for adding atmospheric emission 
*     add_fnoise = int (Given)
*        Flag for adding 1/f noise 
*     add_pns = int (Given)
*        Flag for adding photon noise 
*     heatstart = double (Given)
*        Initial heater setting in pW
*     heatstep = double (Given)
*        Increment of heater setting in pW
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
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Create and map a SCUBA-2 NDF file. Scale the data to integers one
*     frame at a time and add a compressed version to the mapped file.
*     Store the per-frame header items and the FITS headers.

*  Authors:
*     B.D.Kelly (UKATC)
*     {enter_new_authors_here}

*  History :
*     2005-02-10 (BDK):
*        Original
*     2005-02-11 (BDK)
*        Addeed heatval
*     2005-05-20 (BDK)
*        Added flatcal
*     2005-07-08 (BDK)
*        Change use of fhead library
*     2005-08-19 (BDK)
*        Pass-in digitised data, remove digitisation
*        parameters and unused flags
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

void sc2sim_ndfwrheat
( 
int add_atm,       /* flag for adding atmospheric emission (given) */
int add_fnoise,    /* flag for adding 1/f noise (given) */
int add_pns,       /* flag for adding photon noise (given) */
double heatstart,  /* initial heater setting in pW (given) */
double heatstep,   /* increment of heater setting in pW (given) */
char file_name[],  /* output file name (given) */
int ncol,          /* number of bolometers in column (given) */
int nrow,          /* number of bolometers in row (given) */
double sample_t,   /* sample interval in msec (given) */
char subarray[],   /* name of the subarray */
int numsamples,    /* number of samples (given) */
int nflat,         /* number of flat coeffs per bol (given) */
char *flatname,    /* name of flatfield algorithm (given) */
JCMTState *head,   /* header data for each frame (given) */
int *dbuf,         /* time stream data (given) */
int *dksquid,      /* dark SQUID time stream data (given) */
double *fcal,      /* flat-field calibration (given) */
double *fpar,      /* flat-field parameters (given) */
char filter[],     /* String representing filter (e.g. "850") (given) */
double atstart,    /* Ambient temperature at start (Celsius) (given) */
double atend,      /* Ambient temperature at end (Celsius) (given) */
int *status        /* global status (given and returned) */
)

{
   /* Local variables */
   static char fitsrec[FHEAD__MXREC][81];  /* store for FITS records */
   double fpos;                     /* RA or Dec in degrees */
   int nrec;                        /* number of FITS header records */

   /* Check status */
   if ( !StatusOkP(status) ) return;

   /* Format the FITS headers */

   fhead_init ( status );

   fhead_putfits ( TSTRING,
     "DATE-OBS", "YYYY-MM-DDThh:mm:ss",
     "observation date", status );

   fhead_putfits ( TDOUBLE,
     "RA", &fpos,
     "Right Ascension of observation", status );

   fhead_putfits ( TDOUBLE,
     "DEC", &fpos,
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

   fhead_putfits ( TDOUBLE,
     "HEATVAL", &heatstart,
     "heater setting in pW", status );

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

   fhead_putfits ( TSTRING,
     "FILTER", filter,
     "filter used", status );

   fhead_putfits ( TDOUBLE,
     "ATSTART", &atstart,
     "Ambient temperature at start (C)", status );

   fhead_putfits ( TDOUBLE,
     "ATEND", &atend,
     "Ambient temperature at end (C)", status );

   /* Get the accumulated FITS headers */
   fhead_getfits ( &nrec, fitsrec, status );

   /* Store the timestream data */

   sc2store_wrtstream ( file_name, nrec, fitsrec, ncol, nrow, numsamples, 
                        nflat, flatname, head, dbuf, dksquid, fcal, fpar, status );

   /* Close the file */
   sc2store_free ( status );

}//sc2sim_ndfwrheat
