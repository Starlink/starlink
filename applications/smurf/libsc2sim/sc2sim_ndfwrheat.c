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
 *     sc2sim_ndfwrheat ( const struct sc2sim_obs_struct *inx,
 *                        const struct sc2sim_sim_struct *sinx,
 *                        const char file_name[], dim_t numsamples,
 *                        dim_t nflat, double refres, const char flatname[],
 *                        const JCMTState *head, const int *dbuf,
 *                        const int *dksquid, const double *fcal,
 *                        const double *fpar, const char filter[], int *status )

 *  Arguments:
 *     inx = const sc2sim_obs_struct* (Given)
 *        Pointer to struct with observation parameters
 *     sinx = const sc2sim_sim_struct* (Given)
 *        Pointer to struct with simulation parameters
 *     file_name = const char[] (Given)
 *        Output file name
 *     numsamples = dim_t (Given)
 *        Number of samples
 *     nflat = dim_t (Given)
 *        Number of flat coeffs per bol
 *     refres = double (Given)
 *        Reference resistance used to calculate flatfield.
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
 *     fpar = const double* (Given)
 *        Flat-field parameters
 *     filter = const char[] (Given)
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
 *     2007-10-22 (TIMJ):
 *        Use new fitsrec definition for sc2store
 *     2007-10-31 (TIMJ):
 *        Use dim_t following sc2store change. Use const.

 *  Copyright:
 *     Copyright (C) 2007 Science and Technology Facilities Council.
 *     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
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
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

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
 const struct sc2sim_obs_struct *inx, /* structure for values from XML (given) */
 const struct sc2sim_sim_struct *sinx, /* structure for sim values from XML (given)*/
 int subindex,            /* index into sinx->subname of subarray being written */
 const char file_name[],  /* output file name (given) */
 dim_t numsamples,    /* number of samples (given) */
 dim_t nflat,         /* number of flat coeffs per bol (given) */
 double refres,        /* reference resistance used to calculate flatfield (given) */
 const char flatname[],    /* name of flatfield algorithm (given) */
 const JCMTState *head,   /* header data for each frame (given) */
 const int *dbuf,         /* time stream data (given) */
 const int *dksquid,      /* dark SQUID time stream data (given) */
 const double *fcal,      /* flat-field calibration (given) */
 const double *fpar,      /* flat-field parameters (given) */
 const char filter[],     /* String representing filter (e.g. "850") (given) */
 int *status        /* global status (given and returned) */
 )

{
  /* Local variables */
  char dateobs[SZFITSTR] = "\0";   /* DATE-OBS string for observation */
  AstFitsChan *fitschan;           /* FITS headers */
  char fitsrec[SC2STORE__MAXFITS*SZFITSCARD+1]; /* Store for FITS records */
  int jigvert[SC2SIM__MXVERT][2]; /* Temp array to jig_vert */
  dim_t nrec = 0;                 /* number of FITS header records */
  sc2ast_subarray_t subnum;        /* subarray index */
  dim_t i, j;

  /* Check status */
  if ( *status != SAI__OK ) return;

  /* Add the FITS data to the output file */
  fitschan = astFitsChan ( NULL, NULL, " " );

  /* Kludged to write generic date */
  sc2sim_dateobs( inx->mjdaystart, dateobs, status );
  astSetFitsS ( fitschan, "DATE-OBS", dateobs, "UTC observation date", 0 );

  astSetFitsI ( fitschan, "ADD_HNSE", sinx->add_hnoise,
                "Flag for adding heater noise", 0 );
  astSetFitsS ( fitschan, "FLATNAME", flatname, "Type of flatfield solution", 0 );
  if ( strncmp(flatname, "TABLE", 5) == 0 ) {
    astSetFitsF ( fitschan, "HEATVAL", inx->heatstart, "Heater setting in pW", 0 );
    astSetFitsF ( fitschan, "HEATSTEP", inx->heatstep, "Heater power increment in pW", 0 );
    astSetFitsI ( fitschan, "NFLAT", nflat, "Number of heater steps", 0 );
  } else {
    astSetFitsI ( fitschan, "NFLAT", nflat, "Number of polynomial coefficients", 0 );
  }
  astSetFitsI ( fitschan, "COLSIZE", inx->colsize,
                "Number of bolometers in a column", 0 );
  astSetFitsI ( fitschan, "ROWSIZE", inx->rowsize,
                "Number of bolometers in a row", 0 );
  astSetFitsS ( fitschan, "SUBARRAY", (sinx->subname)[subindex], "Subarray name", 0 );
  astSetFitsI ( fitschan, "NUMSAMP", numsamples, "Number of samples", 0 );
  astSetFitsS ( fitschan, "FILTER", filter, "Filter used", 0 );

  /* Convert the AstFitsChan data to a char array */
  smf_fits_export2DA ( fitschan, &nrec, fitsrec, status );

  /* Calculate the sub array index */
  sc2ast_name2num( (sinx->subname)[subindex], &subnum, status );

  /* There are "issues" handling const for arrays in call to wrtstream
     partly caused by the input struct being const and the jig_vert
     member therefore also being const. Rather than try to work out how
     to fix it properly we just copy the data from the struct to a local
     variable before calling wrtstream */
  for (i=0; i < inx->nvert; i++) {
    for (j=0; j < 2; j++) {
      jigvert[i][j] = inx->jig_vert[i][j];
    }
  }

  /* Store the timestream data */
  sc2store_wrtstream ( file_name, subnum, nrec, fitsrec, inx->colsize,
                       inx->rowsize, numsamples, nflat, refres, 0, flatname, head, NULL,
                       dbuf, dksquid, fcal, fpar, inx->obsmode,
                       NULL, NULL, jigvert, inx->nvert, NULL, 0,
                       NULL, status );

  /* Close the file */
  sc2store_free ( status );

}
