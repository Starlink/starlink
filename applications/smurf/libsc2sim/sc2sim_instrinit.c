/*
 *+
 *  Name:
 *     sc2sim_instrinit.c

 *  Purpose:
 *     Initialise instrument parameters

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_instrinit( struct sc2sim_obs_struct *inx,
 *                       struct sc2sim_sim_struct *sinx,
 *                       AstKeyMap *obskeymap, AstKeyMap *simkeymap,
 *                       double coeffs[SC2SIM__NCOEFFS], double *digcurrent,
 *                       double *digmean, double *digscale, double *elevation,
 *                       double weights[], double **heater, double **pzero,
 *                       double **xbc, double **ybc, double **xbolo,
 *                       double **ybolo, int *status )


 *  Arguments:
 *     inx = sc2sim_obs_struct* (Returned)
 *        Structure for values from XML file
 *     sinx = sc2sim_sim_struct* (Returned)
 *        Structure for values from XML file
 *     obskeymap = AstKeyMap* (Given)
 *        Keymap for obs parameters
 *     simkeymap = AstKeyMap* (Given)
 *        Keymap for sim parameters
 *     coeffs = double[] (Returned)
 *        Bolometer respose coeffs
 *     digcurrent = double* (Returned)
 *        Digitisation mean current
 *     digmean = double* (Returned)
 *        Digitisation mean value
 *     digscale = double* (Returned)
 *        Digitisation scale factor
 *     elevation = double* (Returned)
 *        Telescope elevation (radians)
 *     weights = double[] (Returned)
 *        Impulse response
 *     heater = double** (Returned)
 *        Bolometer heater ratios
 *     pzero = double** (Returned)
 *        Bolometer power offsets
 *     xbc = double** (Returned)
 *        X offsets of bolometers in arcsec
 *     ybc = double** (Returned)
 *        Y offsets of bolometers in arcsec
 *     xbolo = double** (Returned)
 *        Native bolo x-offsets
 *     ybolo = double** (Returned)
 *        Native bolo y-offsets
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Initialise instrument parameters.

 *  Authors:
 *     B.D.Kelly (ROE)
 *     J. Balfour (UBC)
 *     E.Chapin (UBC)
 *     Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History :
 *     2005-05-13 (BDK):
 *        Original
 *     2005-05-18 (BDK):
 *        Returned xbc, ybc
 *     2005-06-17 (BDK):
 *        Allocate space for bolometer parameter arrays
 *     2006-02-28 (EC):
 *        Added xbolo/ybolo
 *     2006-07-21 (JB):
 *        Split from dsim.c
 *     2006-08-07 (EC)
 *        Removed dependence on sc2sim_telpos & sc2sim_bolcoords
 *     2006-08-08 (EC)
 *        Include sc2ast.h
 *     2006-08-15 (EC)
 *        Use sc2ast_get_gridcoords instead of sc2sim_bolnatcoords
 *     2006-08-16 (EC)
 *        Fixed mis-handling of bad error status
 *     2006-08-16 (EC)
 *        Call smf_get_gridcoords instead of sc2ast_get_gridcoords
 *     2006-09-07 (EC):
 *        Modified sc2ast_createwcs calls to use new interface.
 *     2006-09-11 (EC):
 *        Fixed pointer problem with callc to smf_calc_telpos
 *     2006-09-22 (JB):
 *        Changed from using XML files to AstKeyMaps
 *     2006-10-18 (EC):
 *        Extra status checking
 *     2007-06-29 (EC):
 *        Hard-wire sky level for digitisation coefficients since they get
 *        updated in sc2sim_simulate
 *     2007-08-20 (TIMJ):
 *        Fix some intel compiler warnings

 *  Copyright:
 *     Copyright (C) 2005-2007 University of British Columbia.
 *     Copyright (C) 2007 Science and Technology Facilities Council.
 *     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
 *     Council. All Rights Reserved.

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
#include <math.h>

/* SC2SIM includes */
#include "sc2sim.h"
#include "sc2sim_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "sc2da/sc2ast.h"
#include "smurf_par.h"
#include "jcmt/state.h"

/* Starlink Includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"

#define FUNC_NAME "sc2sim_instrinit"

void sc2sim_instrinit( struct sc2sim_obs_struct *inx,
                       struct sc2sim_sim_struct *sinx,
                       AstKeyMap *obskeymap, AstKeyMap *simkeymap,
                       double coeffs[SC2SIM__NCOEFFS], double *digcurrent,
                       double *digmean, double *digscale,
                       double *elevation __attribute__((unused)),
                       double weights[], double **heater, double **pzero,
                       double **xbc, double **ybc, double **xbolo,
                       double **ybolo, int *status ) {

  /* Local variables */
  double decay;                  /* bolometer time constant (msec) */
  AstFrameSet *fset=NULL;        /* Frameset to calculate xbc + ybc */
  double instap[2];              /* Focal plane instrument offsets */
  int j;                         /* loop counter */
  int nbol=0;                    /* total number of bolometers */
  double photonsigma;            /* typical photon noise level in pW */
  double startatm;               /* mean expected atmospheric signal (pW) */
  JCMTState state;               /* Telescope state at one time slice */
  sc2ast_subarray_t subnum;      /* subarray number */
  double telpos[3];              /* Geodetic location of the telescope */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  sc2sim_getobspar ( obskeymap, inx, status );
  sc2sim_getsimpar ( simkeymap, sinx, status );

  /* Get the bolometer information */
  if( *status == SAI__OK ) {
    nbol = inx->colsize * inx->rowsize;
  }

  decay = 5.0;
  *heater = astCalloc( nbol, sizeof(**heater) );
  *pzero = astCalloc( nbol, sizeof(**pzero) );
  *xbc = astCalloc( nbol, sizeof(**xbc) );
  *ybc = astCalloc( nbol, sizeof(**ybc) );
  *xbolo = astCalloc( nbol, sizeof(**xbolo) );
  *ybolo = astCalloc( nbol, sizeof(**ybolo) );


  /*  Initialise the standard bolometer response function.
      The routine sets the values for 6 polynomial coefficients which
      translate input power in pico Watts to output current in Amp. */
  sc2sim_response ( inx->lambda, SC2SIM__NCOEFFS, coeffs, status );

  /* Assume a dummy atmospheric loading in pW to calculate digitization
     parameters. This part will get re-calculated in sc2sim_simulate when
     we actually know what the loading is at the observation start */

  startatm = 10;

  /*  Calculate the parameters for simulating digitisation */
  sc2sim_getsigma ( sinx->refload, sinx->refnoise,
                    (sinx->telemission+startatm), &photonsigma, status );

  sc2sim_getscaling ( SC2SIM__NCOEFFS, coeffs, inx->targetpow, photonsigma,
                      digmean, digscale, digcurrent, status );


  /*  Initialise the bolometer impulse response
      WEIGHTS contain 16 coefficients in reversed order given by
      exp(-ti/DECAY) divided by the sum of all 16 values.
      This is practically identical to the fb.exp(-t1.fb) formula
      used in the DREAM software with fb=1/DECAY. */
  sc2sim_getweights ( decay, inx->steptime, SC2SIM__MXIRF, weights, status );

  /* Get the subsystem number */
  sc2ast_name2num( (sinx->subname)[0], &subnum, status );

  /* Get the native x- and y- (GRID) coordinates of each bolometer
     Since sc2sim_simframe still needs xbc & ybc to interpolate values from
     the sky noise image, calculate them here. Get rid of the old call
     to sc2sim_bolcoords. For now just make a dummy scuba2 frameset at
     some fixed elevation. To do this properly we would actually want to
     calculate xbc and ybc on-the-fly as the telescope points at different
     regions of the sky. */

  smf_get_gridcoords( *xbolo, *ybolo, inx->colsize, inx->rowsize, status );

  /* Check to make sure that all the relevant elements of JCMTState are set! */
  state.tcs_az_ac1 = 0;
  state.tcs_az_ac2 = 0;
  state.tcs_az_bc1 = 0;
  state.tcs_az_bc2 = 0;
  state.smu_az_jig_x = 0;
  state.smu_az_jig_y = 0;
  state.smu_az_chop_x = 0;
  state.smu_az_chop_y = 0;
  state.rts_end = 53795.0;
  state.tcs_tai = state.rts_end;
  smf_calc_telpos( NULL, "JCMT", telpos, status );
  instap[0] = 0;
  instap[1] = 0;
  sc2ast_createwcs( subnum, &state, instap, telpos, NO_FTS, &fset, status );

  /* If we do an AzEl projection for each bolometer it is very nearly
     a tangent plane projection because we chose El=0 */

  if( *status == SAI__OK ) {

    astSetC( fset, "SYSTEM", "AzEl" );
    astTran2( fset, nbol, *ybolo, *xbolo, 1, *xbc, *ybc );

    if( !astOK ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "AST error calculating projected bolometer coords.",
             status);
    }

  }

  if( *status == SAI__OK ) {

    /* xbc and ybc are in radians at this point. Convert to arcsec */
    for ( j=0; j<nbol; j++ ) {
      (*xbc)[j] *= DR2AS;
      (*ybc)[j] *= DR2AS;
    }

    sc2sim_getspread ( nbol, *pzero, *heater, status );
  }

}
