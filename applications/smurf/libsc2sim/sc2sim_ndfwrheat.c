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
*     Tim Jenness (JAC, Hawaii)
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
*     2006-10-26 (JB):
*        Convert to using AstFitsChans
*     2006-12-19 (TIMJ):
*        sc2store_wrtstream has additional subnum argument
*     2007-10-05 (AGG):
*        Rationalize FITS headers, write correct DATE-OBS string

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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
#include "ndf.h"
#include "mers.h"
#include "sae_par.h"

/* SC2SIM includes */
#include "sc2sim.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2ast.h"

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
  char dateobs[SZFITSCARD+1] = "\0"; /* DATE-OBS string for observation */
  AstFitsChan *fitschan;           /* FITS headers */
  const char fitsrec[SC2STORE__MAXFITS][SZFITSCARD]; /* Store for FITS records */  
  int nrec;                        /* number of FITS header records */
  int subnum;                      /* subarray index */

  /* Check status */
  if ( *status != SAI__OK ) return;

  /* Add the FITS data to the output file */
  fitschan = astFitsChan ( NULL, NULL, "" );

  /* Kludged to write generic date */ 
  sc2sim_dateobs( inx->mjdaystart, dateobs, status );
  astSetFitsS ( fitschan, "DATE-OBS", dateobs, "UTC observation date", 0 );

  astSetFitsI ( fitschan, "ADD_HNSE", sinx->add_hnoise, 
		"Flag for adding heater noise", 0 );
  astSetFitsS ( fitschan, "FLATNAME", flatname, "Type of flatfield solution", 0 );
  if ( strncmp(flatname, "TABLE", 5) == 0 ) {
    astSetFitsF ( fitschan, "HEATVAL", inx->heatstart, "Heater setting in pW", 0 );
    astSetFitsF ( fitschan, "HEATSTEP", inx->heatstep, "Heater setting in pW", 0 );
    astSetFitsI ( fitschan, "NFLAT", nflat, "Number of flatfield steps", 0 );
  } else {
    astSetFitsI ( fitschan, "NFLAT", nflat, "Number of polynomial coefficients", 0 );
  } 
  astSetFitsI ( fitschan, "NBOLX", inx->nbolx,
		"Number of bolometers in X direction", 0 );
  astSetFitsI ( fitschan, "NBOLY", inx->nboly, 
		"Number of bolometers in Y direction", 0 );
  astSetFitsS ( fitschan, "SUBARRAY", sinx->subname, "Subarray name", 0 );
  astSetFitsI ( fitschan, "NUMSAMP", numsamples, "Number of samples", 0 );
  astSetFitsS ( fitschan, "FILTER", filter, "Filter used", 0 );

  /* Convert the AstFitsChan data to a char array */
  smf_fits_export2DA ( fitschan, &nrec, fitsrec, status );

  /* Calculate the sub array index */
  sc2ast_name2num( sinx->subname, &subnum, status );

  /* Store the timestream data */
  sc2store_wrtstream ( file_name, subnum, nrec, fitsrec, inx->nbolx, 
		       inx->nboly, numsamples, nflat, flatname, head, 
		       dbuf, dksquid, fcal, fpar, inx->obsmode, 
		       inx->jig_vert, inx->nvert, NULL, 0,
		       status );

  /* Close the file */
  sc2store_free ( status );

}
