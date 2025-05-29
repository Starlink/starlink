/*
 *+
 *  Name:
 *     sc2sim_heatrun

 *  Purpose:
 *     Generate a heater flat-field measurement

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     SC2SIM subroutine

 *  Invocation:
 *     sc2sim_heatrun ( struct sc2sim_obs_struct *inx,
 *                      struct sc2sim_sim_struct *sinx,
 *                      double coeffs[], double digcurrent, double digmean,
 *                      double digscale, char filter[], double *heater, int nbol,
 *                      double *pzero, double samptime, int *status);

 *  Arguments:
 *     inx = sc2sim_obs_struct* (Given)
 *        Structure for values from XML
 *     sinx = sc2sim_sim_struct* (Given)
 *        Structure for sim values from XML
 *     coeffs = double[] (Given)
 *        Bolometer response coeffs
 *     digcurrent - double (Given)
 *        Digitisation mean current
 *     digmean = double (Given)
 *        Digitisation mean value
 *     digscale = double (Given)
 *        Digitisation scale factor
 *     filter = char[] (Given)
 *        String to hold filter name
 *     heater - double* (Given)
 *        Bolometer heater ratios
 *     nbol = int (Given)
 *        Total number of bolometers
 *     pzero = double* (Given)
 *        Bolometer power offsets
 *     samptime = double (Given)
 *        Sample time in sec
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     The heatrun method generates a heater flat-field measurement
 *     from simulated data for each of a range of heater settings. Note
 *     that if multiple sub-arrays are requested, the same flatfield
 *     solution is written for each one

 *  Notes:

 *  Authors:
 *     Tim Jenness (JAC, Hawaii)
 *     Andy Gibb (UBC)
 *     Edward Chapin (UBC)
 *     David Berry (JAC, UCLan)
 *     B.D.Kelly (ROE)
 *     Jen Balfour (UBC)
 *     {enter_new_authors_here}

 *  History:
 *     2005-02-16 (BDK):
 *        Original
 *     2005-05-18 (BDK):
 *        Get xbc, ybc from instrinit
 *     2005-05-20 (BDK):
 *        Add flatcal
 *     2005-06-17 (BDK):
 *        Allocate workspace dynamically
 *     2005-08-19 (BDK):
 *        Do calibration fit, remove flux2cur flag check
 *     2005-10-04 (BDK):
 *        Change to new data interface
 *     2006-01-13 (ELC):
 *        Write subarray name
 *     2006-01-24 (ELC):
 *        Write filter/atstart/atend
 *     2006-06-09 (JB):
 *        Added to smurf_sim
 *     2006-07-26 (JB):
 *        Moved into sc2sim_heatrun
 *     2006-07-28 (JB):
 *        Changed sc2head to JCMTState
 *     2006-08-08 (EC):
 *        Removed slaDjcl prototype and instead include star/slalib.h
 *     2006-09-22 (JB):
 *        Replaced dxml_structs with sc2sim_structs and removed
 *        DREAM-specific code.
 *     2007-10-05 (AGG):
 *        Loop over all requested subarrays
 *     2009-10-15 (TJ):
 *        Initialise sc2store in heatrun simulation, otherwise you get
 *        an emsLevel warning on completion.
 *     2009-10-16 (AGG):
 *        Include sc2store functions, remove unused variable
 *     2012-03-06 (TIMJ):
 *         Replace SLA with SOFA.
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2006,2009,2012 Particle Physics and Astronomy Research Council.
 *     Copyright (C) 2009 Science and Technology Facilities Council.
 *     Copyright (C) 2006-2009 University of British Columbia. All
 *     Rights Reserved.

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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "erfa.h"
#include "erfam.h"

#include "sc2da/Dits_Err.h"
#include "sc2da/Ers.h"
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2ast.h"

/* SMURF includes */
#include "sc2sim.h"
#include "smurf_par.h"
#include "libsmurf/smurflib.h"
#include "libsmf/smf.h"

#define FUNC_NAME "sc2sim_heatrun"
#define LEN__METHOD 20

void sc2sim_heatrun ( struct sc2sim_obs_struct *inx,
                      struct sc2sim_sim_struct *sinx,
                      double coeffs[], double digcurrent, double digmean,
                      double digscale, char filter[], double *heater, int nbol,
                      double *pzero, double samptime, int *status) {

  int bol;                        /* counter for indexing bolometers */
  double corner;                  /* corner frequency in Hz */
  double current;                 /* bolometer current in amps */
  int date_da;                    /* day corresponding to MJD */
  double date_df;                 /* day fraction corresponding to MJD */
  int date_mo;                    /* month corresponding to MJD */
  int date_yr;                    /* year corresponding to MJD */
  double *dbuf=NULL;              /* simulated data buffer */
  int *digits=NULL;               /* output data buffer */
  int *dksquid=NULL;              /* dark squid values */
  char filename[SC2SIM__FLEN];    /* name of output file */
  double *flatcal=NULL;           /* flatfield calibration */
  char flatname[SC2STORE_FLATLEN];/* flatfield algorithm name */
  double *flatpar=NULL;           /* flatfield parameters */
  double flux;                    /* flux at bolometer in pW */
  static double fnoise[SC2SIM__MXSIM];    /* instr. noise for 1 bolometer */
  JCMTState *head;                /* per-frame headers */
  double *heatptr;                /* pointer to list of heater settings */
  int j;                          /* loop counter */
  int nflat;                      /* number of flat coeffs per bol */
  int numsamples;                 /* Number of samples in output. */
  double *output;                 /* series of output values */
  int sample;                     /* sample counter */
  double sigma;                   /* instrumental white noise */

  if ( *status != SAI__OK) return;

  /* Do a heatrun simulation */

  /* Force sc2store to think it's initialized */
  sc2store_force_initialised( status );

  /* Calculate year/month/day corresponding to MJD at start */
  (void) eraJd2cal( ERFA_DJM0, inx->mjdaystart, &date_yr, &date_mo,
                    &date_da, &date_df );

  numsamples = inx->heatnum;

  /* Allocate workspace */

  output = astCalloc( numsamples, sizeof(*output) );
  heatptr = astCalloc( numsamples, sizeof(*heatptr) );
  dbuf = astCalloc( numsamples*nbol, sizeof(*dbuf) );
  digits = astCalloc( numsamples*nbol, sizeof(*digits) );
  dksquid = astCalloc( numsamples*inx->rowsize, sizeof(*dksquid) );
  head = astCalloc( numsamples, sizeof(*head) );

  /* Generate the list of heater settings */
  for ( sample=0; sample<numsamples; sample++ ) {
    heatptr[sample] = inx->heatstart + (double)sample * inx->heatstep;
  }

  /*  Generate a full time sequence for one bolometer at a time */
  for ( bol=0; bol<nbol; bol++ ) {

    /* Create an instrumental 1/f noise sequence.
       Simulate a 1/f noise sequence by generating white noise sequence,
       Fourier transforming it, applying a 1/f law, then transforming back
       again. Generate and add a new white noise sequence.
       The buffer fnoise contains the noise pattern. */
    if ( sinx->add_fnoise == 1 ) {
      sigma = 1.0e-9;
      corner = 0.01;
      sc2sim_invf ( sigma, corner, samptime, SC2SIM__MXSIM, fnoise, status );
    }

    /* Generate a measurement sequence for each bolometer. */
    for ( sample=0; sample<numsamples; sample++ ) {
      if ( sinx->add_hnoise == 1 ) {
        flux = heatptr[sample] * heater[bol];
      } else {
        flux = heatptr[sample];
      }

      /* Convert to current with bolometer power offset.
         The bolometer offset in PZERO(BOL) is added to the FLUX, and then
         the power in FLUX is converted to a current in scalar CURRENT with
         help of the polynomial expression with coefficients in COEFFS(*) */
      sc2sim_ptoi ( flux, SC2SIM__NCOEFFS, coeffs, pzero[bol],
                    &current, status);

      /* Store the value. */
      output[sample] = current;
    }

    /* Now output[] contains the values current for all samples in the
       cycles for this bolometer. */
    if ( sinx->add_fnoise == 1 ) {
      /* Add instrumental 1/f noise data in output */
      for ( sample=0; sample<numsamples; sample++ ) {
        output[sample] += fnoise[sample];
      }
    }

    for ( sample=0; sample<numsamples; sample++ ) {
      dbuf[sample*nbol+bol] = output[sample];
    }

  }

  /* Digitise the numbers */
  sc2sim_digitise ( nbol*numsamples, dbuf, digmean, digscale, digcurrent,
                    digits, status );

  /* Overwrite the original simulation with the digitised version */
  for ( sample=0; sample<numsamples*nbol; sample++ ) {
    dbuf[sample] = (double)digits[sample];
  }

  /* Perform the fit */
  if ( strcmp ( inx->flatname, "POLYNOMIAL" ) == 0 ) {
    nflat = 6;
    flatcal = astCalloc( nbol*nflat, sizeof(*flatcal) );
    flatpar = astCalloc( nflat, sizeof(*flatpar) );
    strcpy ( flatname, "POLYNOMIAL" );
    sc2sim_fitheat ( nbol, numsamples, heatptr, dbuf, flatcal, status );
    for ( j=0; j<nflat; j++ ) {
      flatpar[j] = j - 2;
    }

  } else {
    /* Generate a simple lookup table */
    nflat = numsamples;
    flatcal = astCalloc( nbol*nflat, sizeof(*flatcal) );
    flatpar = astCalloc( nflat, sizeof(*flatpar) );
    strcpy ( flatname, "TABLE" );

    for ( j=0; j<nflat; j++ ) {
      flatpar[j] = heatptr[j];
    }
    for ( j=0; j<nflat*nbol; j++ ) {
      flatcal[j] = dbuf[j];
    }
  }

  /* Loop over array of subarrays */
  for ( j = 0; j < sinx->nsubarrays; j++ ) {
    /* Get the name of this flatfield solution */
    sprintf ( filename, "%sheat%04i%02i%02i_00001", (sinx->subname)[j], date_yr,
              date_mo, date_da );

    msgSetc( "FILENAME", filename );
    msgOut(" ", "Writing ^FILENAME", status );

    /* Store the data in output file file_name
       Use a "bad" reference resistance to prevent heater efficiency values
       from being applied to simulated data. */
    sc2sim_ndfwrheat( inx, sinx, j, filename, numsamples, nflat, VAL__BADD, flatname, head,
                      digits, dksquid, flatcal, flatpar, filter, status );

    msgSetc( "FILENAME", filename );
    msgOut(" ", "Done ^FILENAME", status );
  }
  msgOutif(MSG__VERB," ", "Heatrun successful.", status );

}
