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
*     sc2sim_ndfwrheat ( struct sc2sim_obs_struct *inx, 
*                        struct sc2sim_sim_struct *sinx,
*                        char file_name[], int numsamples, int nflat, char *flatname, 
*                        JCMTState *head, int *dbuf, int *dksquid, 
*                        double *fcal, double *fpar, char filter[], int *status )

*  Arguments:
*     inx = sc2sim_obs_struct* (Given)
*        Pointer to struct with observation parameters
*     sinx = sc2sim_sim_struct* (Given)
*        Pointer to struct with simulation parameters
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
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Create and map a SCUBA-2 NDF file. Scale the data to integers one
*     frame at a time and add a compressed version to the mapped file.
*     Store the per-frame header items and the FITS headers.

*  Authors:
*     B.D.Kelly (UKATC)
*     A.G. Gibb (UBC)
*     J. Balfour (UBC)
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
*     2006-08-18 (AGG):
*        Update API to take pointers to inx and sinx structs
*     2006-09-22 (JB):
*        Change dxml_structs to sc2sim_structs

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

void sc2sim_ndfwrheat
( 
struct sc2sim_obs_struct *inx,  /* structure for values from XML (given) */
struct sc2sim_sim_struct *sinx, /* structure for sim values from XML (given)*/
char file_name[],  /* output file name (given) */
int numsamples,    /* number of samples (given) */
int nflat,         /* number of flat coeffs per bol (given) */
char *flatname,    /* name of flatfield algorithm (given) */
JCMTState *head,   /* header data for each frame (given) */
int *dbuf,         /* time stream data (given) */
int *dksquid,      /* dark SQUID time stream data (given) */
double *fcal,      /* flat-field calibration (given) */
double *fpar,      /* flat-field parameters (given) */
char filter[],     /* String representing filter (e.g. "850") (given) */
int *status        /* global status (given and returned) */
)

{
   /* Local variables */
   static char fitsrec[FHEAD__MXREC][81]; /* store for FITS records */
   double fpos = 0;                       /* RA or Dec in degrees */
   int nrec;                              /* number of FITS header records */

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
		   "ADD_ATM", &(sinx->add_atm),
		   "flag for adding atmospheric emission", status );

   fhead_putfits ( TINT,
		   "ADDFNOIS", &(sinx->add_fnoise),
		   "flag for adding 1/f noise", status );

   fhead_putfits ( TINT,
		   "ADD_PNS", &(sinx->add_pns),
		   "flag for adding photon noise", status );

   fhead_putfits ( TDOUBLE,
		   "HEATVAL", &(inx->heatstart),
		   "heater setting in pW", status );

   fhead_putfits ( TINT,
		   "NBOLX", &(inx->nbolx),
		   "number of bolometers in X direction", status );

   fhead_putfits ( TINT,
		   "NBOLY", &(inx->nboly),
		   "number of bolometers in Y direction", status );

   fhead_putfits ( TDOUBLE,
		   "SAMPLE_T", &(inx->sample_t),
		   "The sample interval in msec", status );

   fhead_putfits ( TSTRING,
		   "SUBARRAY", sinx->subname,
		   "subarray name", status );

   fhead_putfits ( TINT,
		   "NUMSAMP", &numsamples,
		   "number of samples", status );

   fhead_putfits ( TSTRING,
		   "FILTER", filter,
		   "filter used", status );

   fhead_putfits ( TDOUBLE,
		   "ATSTART", &(sinx->atstart),
		   "Ambient temperature at start (C)", status );

   fhead_putfits ( TDOUBLE,
		   "ATEND", &(sinx->atend),
		   "Ambient temperature at end (C)", status );

   /* Get the accumulated FITS headers */
   fhead_getfits ( &nrec, fitsrec, status );

   /* Store the timestream data */

   /*   sc2store_wrtstream ( file_name, nrec, fitsrec, ncol, nrow, numsamples, 
	nflat, flatname, head, dbuf, dksquid, fcal, fpar, status );*/
   sc2store_wrtstream ( file_name, nrec, fitsrec, inx->nbolx, inx->nboly, 
			numsamples, nflat, flatname, head, dbuf, dksquid, 
			fcal, fpar, inx->obsmode, inx->jig_vert, inx->nvert, 
			NULL, 0, status );


   /* Close the file */
   sc2store_free ( status );

}
