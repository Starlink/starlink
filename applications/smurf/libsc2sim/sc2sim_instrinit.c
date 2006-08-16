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
*     sc2sim_instrinit ( int argc, char **argv, struct dxml_struct *inx,
*                        struct dxml_sim_struct *sinx, int *rseed, 
*                        double coeffs[NCOEFFS], double *digcurrent, 
*                        double *digmean, double *digscale, double *elevation,
*                        double weights[], double **heater, double **pzero,
*                        double **xbc, double **ybc, double **xbolo, 
*                        double **ybolo, int *status )

*  Arguments:
*     argc = int (Given)
*        Argument count
*     argv = char** (Given)
*        Argument list
*     inx = dxml_struct* (Returned)
*        Structure for values from XML file
*     sinx = dxml_sim_struct* (Returned)
*        Structure for values from XML file
*     rseed = int* (Returned)
*        Seed for random number generator
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
#include <math.h>

/* SC2SIM includes */
#include "sc2sim.h"
#include "sc2sim_par.h"
#include "dream.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "sc2da/sc2ast.h"
#include "smurf_par.h"

/* Starlink Includes */
#include "ast.h"
#include "sae_par.h"

void sc2sim_instrinit( int argc, char **argv, struct dxml_struct *inx,
                       struct dxml_sim_struct *sinx, int *rseed,
                       double coeffs[NCOEFFS], double *digcurrent,
                       double *digmean, double *digscale, double *elevation,
                       double weights[], double **heater, double **pzero,
                       double **xbc, double **ybc, double **xbolo,
                       double **ybolo, int *status ) {

  /* Local variables */
  double azimuth;                /* Azimuth in radians */
  double decay;                  /* bolometer time constant (msec) */
  AstFrameSet *fset=NULL;        /* Frameset to calculate xbc + ybc */
  int j;                         /* loop counter */
  double lst;                    /* local sidereal time in radians */
  double meanatm;                /* mean expected atmospheric signal (pW) */
  int nboll;                     /* total number of bolometers */
  double p;                      /* parallactic angle (radians) */
  double photonsigma;            /* typical photon noise level in pW */
  double samptime;               /* sample time in sec */
  int savebols;                  /* flag for bol details (unused here) */
  int subnum;                    /* subarray number */
  double trans;                  /* average transmission */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* Initialise tracing */
  dream_traceinit();
   
  /* Get control parameters */
  if ( dream_trace ( 1 ) ) {
    printf ( "staresim : Start of the program\n" );
  }

  sc2sim_getpar ( argc, argv, inx, sinx, rseed, &savebols, status );

  samptime = inx->sample_t / 1000.0;
   
  /* Initialise bolometer characteristics */
  dream_bolinit ( 1, inx->nbolx, inx->nboly, status );
   
  /* Get the bolometer information */
  if( *status == SAI__OK ) {
    nboll = inx->nbolx * inx->nboly;
  }     

  decay = 5.0;
  *heater = smf_malloc ( nboll, sizeof(**heater), 1, status );
  *pzero = smf_malloc ( nboll, sizeof(**pzero), 1, status  );
  *xbc = smf_malloc ( nboll, sizeof(**xbc), 1, status  );
  *ybc = smf_malloc ( nboll, sizeof(**ybc), 1, status  );
  *xbolo = smf_malloc ( nboll, sizeof(**xbolo), 1, status  );
  *ybolo = smf_malloc ( nboll, sizeof(**ybolo), 1, status  );

  /*  Initialise the standard bolometer response function.
      The routine sets the values for 6 polynomial coefficients which
      translate input power in pico Watts to output current in Amp. */
  sc2sim_response ( inx->lambda, NCOEFFS, coeffs, status );

  /*  Calculate the parameters for simulating digitisation */
  sc2sim_calctrans ( inx->lambda, &trans, sinx->tauzen, status );
  sc2sim_atmsky ( inx->lambda, trans, &meanatm, status );   
  sc2sim_getsigma ( inx->lambda, sinx->bandGHz, sinx->aomega,
                    (sinx->telemission+meanatm), &photonsigma, status );
   
  sc2sim_getscaling ( NCOEFFS, coeffs, inx->targetpow, photonsigma,
                      digmean, digscale, digcurrent, status );
  
   
  /*  Initialise the bolometer impulse response
      WEIGHTS contain 16 coefficients in reversed order given by
      exp(-ti/DECAY) divided by the sum of all 16 values.
      This is practically identical to the fb.exp(-t1.fb) formula
      used in the DREAM software with fb=1/DECAY. */
  sc2sim_getweights ( decay, inx->sample_t, DREAM__MXIRF, weights, status );
 
  /* Get the subsystem number */ 
   
  sc2ast_name2num( sinx->subname, &subnum, status );

  /* Get the native x- and y- (GRID) coordinates of each bolometer */
   
  /*sc2sim_bolnatcoords( *xbolo, *ybolo, &nboll, status );*/

  sc2ast_get_gridcoords( *xbolo, *ybolo, status );
   
  /* Since sc2sim_simframe still needs xbc & ybc to interpolate values from
     the sky noise image, calculate them here. Get rid of the old call
     to sc2sim_bolcoords. For now just make a dummy scuba2 frameset at
     some fixed elevation. To do this properly we would actually want to
     calculate xbc and ybc on-the-fly as the telescope points at different
     regions of the sky. */

  sc2ast_createwcs( subnum, 0, 0, 0, 0, 53795.0, &fset, status ); 

  /* If we do an AzEl projection for each bolometer it is very nearly
     a tangent plane projection because we chose El=0 */

  if( *status == SAI__OK ) {

    astSetC( fset, "SYSTEM", "AzEl" );
    astTran2( fset, nboll, *ybolo, *xbolo, 1, *xbc, *ybc );
     
    /* xbc and ybc are in radians at this point. Convert to arcsec */
    for ( j=0; j<nboll; j++ ) {
      (*xbc)[j] *= DR2AS;
      (*ybc)[j] *= DR2AS;
    }
     
    /* Setup world coordinate information and get the bolometer positions in
       Nasmyth and native coordinates */
    /*
      lst = inx->ra;
      sc2sim_telpos ( inx->ra, inx->dec, lst, &azimuth, elevation, &p, 
      status );
    */
     
    /*
      sc2sim_bolcoords ( sinx->subname, inx->ra, inx->dec, *elevation, p,
      "NASMYTH", &nboll, *xbc, *ybc, status );
    */
    /* Convert Nasmyth coordinates from mm to arcsec */
    /*
      for ( j=0; j<nboll; j++ ) {
      (*xbc)[j] *= MM2SEC;
      (*ybc)[j] *= MM2SEC;
      }
    */
     
     
    sc2sim_getspread ( nboll, *pzero, *heater, status );
  }

}
