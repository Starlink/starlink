/*
 *+
 *  Name:
 *     sc2sim_simframe

 *  Purpose:
 *     Simulate a single frame of bolometer data

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_simframe ( struct sc2sim_obs_struct inx,
 *                       struct sc2sim_sim_struct sinx,
 *                       int astnaxes[2], double astscale, double *astsim,
 *                       int atmnaxes[2], double atmscale, double *atmsim,
 *                       double coeffs[], AstFrameSet *fset, double heater[],
 *                       int nbol, double focposn, int frame, int nterms, double *noisecoeffs,
 *                       double *pzero, double samptime, double start_time,
 *                       double *weights, AstMapping *sky2map,
 *                       double *xbolo, double *ybolo, double *xbc, double *ybc,
 *                       double *position, double *dbuf, int *status )

 *  Arguments:
 *     inx = sc2sim_obs_struct (Given)
 *        Structure for values from XML
 *     sinx = sc2sim_sim_struct (Given)
 *        Structure for sim values from XML
 *     astnaxes = int[2] (Given)
 *        Dimensions of simulated image
 *     astscale = double (Given)
 *        Pixel size in simulated image (arcsec/pixel)
 *     astsim= = double* (Given)
 *        Astronomical sky
 *     atmnaxes = int[2] (Given)
 *        Dimensions of simulated atm background
 *     atmscale = double (Given)
 *        Pixel size in simulated atm background (arcsec/pixel)
 *     atmsim = double* (Given)
 *        Atmospheric emission
 *     coeffs = double[] (Given)
 *        Bolometer response coeffs
 *     fset = AstFrameSet* (Given)
 *        World Coordinate transformations
 *     heater = double[] (Given)
 *        Bolometer heater ratios
 *     nbol = int (Given)
 *        Total number of bolometers
 *     focposn = double (Given)
 *        SMU position for FOCUS observation
 *     frame = int (Given)
 *        Number of current frame
 *     nterms = int (Given)
 *        Number of 1/f noise coeffs
 *     noisecoeffs = double* (Given)
 *        1/f noise coeffs
 *     pzero = double* (Given)
 *        Bolometer power offsets
 *     samptime = double (Given)
 *        Sample time in sec
 *     start_time = double (Given)
 *        Time at start of scan in seconds since the simulation started
 *     weights = double* (Given)
 *        Impulse response
 *     sky2map = AstMapping* (Given)
 *        Mapping celestial->map coordinates
 *     xbolo = double* (Given)
 *        Native X offsets of bolometers (GRID coordinates)
 *     ybolo = double* (Given)
 *        Native Y offsets of bolometers (GRID coordinates)
 *     xbc = double* (Given)
 *        Nasmyth X offsets of bolometers (arcsec)
 *     ybc = double* (Given)
 *        Nasmyth Y offsets of bolometers (arcsec)
 *     position = double* (Given)
 *        Nasmyth positions of bolometers (arcsec)
 *     dbuf = double* (Returned)
 *        Generated frame
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Interpolate values of atmospheric background and astronomical
 *     image for each bolometer in the current frame. Simulate photon
 *     and instrumental noise and bolometer responses. If the
 *     simulation is of a FOCUS observation then a simple quadratic
 *     model is used to reduce (defocus) the astronomical signal as a
 *     function of the given focus position.

 *  Authors:
 *     B.D.Kelly (ROE)
 *     E.Chapin (UBC)
 *     J.Balfour (UBC)
 *     A.G.Gibb (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2005-07-08 (BDK):
 *        Original
 *     2006-02-28 (EC)
 *        Modified to use framesets to connect bolos with actual
 *        WCS info for input sky image
 *     2006-03-29 (EC)
 *        Renamed from dsim_pongframe to be used generically
 *     2006-06-29 (JB)
 *        Added per-bolometer atmospheric correction from elevation.
 *     2006-07-21 (JB):
 *        Split from dsim.c
 *     2006-08-18 (EC):
 *        Don't annul fset at the end
 *        Improved status handing
 *     2006-08-21 (EC):
 *        Fixed memory leak: freeing skycoord
 *     2006-09-22 (JB):
 *        Removed DREAM-specific code and replaced dxml_structs
 *        with sc2sim_structs
 *     2006-10-19 (AGG):
 *        Correct error in airmass correction of sky flux (take 2)
 *     2006-12-21 (AGG):
 *        Only calculate sky gradient if atmosphere is to be added
 *     2007-01-10 (AGG):
 *        Set SYSTEM to GAPPT if we're observing a planet
 *     2007-02-28 (AGG):
 *        Fix major memory leak by annulling bolo2azel mapping
 *     2007-06-27 (EC):
 *        - Changed skynoise task so that simulated sky images are
 *        normalized.  Now the atmosphere value sampled from these
 *        images are scaled on-the-fly by the photon noise and mean
 *        atmospheric level added on.
 *     2007-07-04 (EC):
 *        - Added cosmic ray spikes
 *     2007-12-18 (AGG):
 *        Update to use new smf_free behaviour
 *     2008-04-15 (AGG):
 *        Fix time bug
 *     2008-04-18 (AGG):
 *        Use transmission for current wavelength to get atmospheric power
 *     2008-04-24 (AGG):
 *        - Add focus (SMU) position to API
 *        - Remove telemission from API
 *        - Add defocus multiplier
 *     2008-10-10 (AGG):
 *        Allow NOISE observations, set the astronomical signal to zero
 *     2009-11-20 (DSB):
 *        Pass "interp" and "params" to sc2sim_getast_wcs.
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2005-2008 Particle Physics and Astronomy Research
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
#include <math.h>

/* SC2SIM includes */
#include "sc2sim.h"

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "sc2sim_simframe"

void sc2sim_simframe
(
 struct sc2sim_obs_struct inx,  /* structure for values from XML (given) */
 struct sc2sim_sim_struct sinx, /* structure for sim values from XML (given)*/
 int astnaxes[2],             /* dimensions of simulated image (given) */
 double astscale,             /* pixel size in simulated image (given) */
 double *astsim,              /* astronomical sky (given) */
 int atmnaxes[2],             /* dimensions of simulated atm background
                                 (given) */
 double atmscale,             /* pixel size in simulated atm background
                                 (given) */
 double *atmsim,              /* atmospheric emission (given) */
 double coeffs[],             /* bolometer response coeffs (given) */
 AstFrameSet *fset,           /* World Coordinate transformations */
 double heater[],             /* bolometer heater ratios (given) */
 int nbol,                    /* total number of bolometers (given) */
 double focposn,              /* SMU position for FOCUS observation */
 int frame __attribute__((unused)), /* number of current frame (given) */
 int nterms,                  /* number of 1/f noise coeffs (given) */
 double *noisecoeffs,         /* 1/f noise coeffs (given) */
 double *pzero,               /* bolometer power offsets (given) */
 double samptime,             /* sample time in sec (given) */
 double start_time,           /* time at start of scan in sec since start of
                                 simulation (given) */
 double *weights __attribute__((unused)), /* impulse response (given) */
 AstMapping *sky2map,         /* Mapping celestial->map coordinates */
 double *xbolo,               /* native X offsets of bolometers */
 double *ybolo,               /* native Y offsets of bolometers */
 double *xbc,                 /* nasmyth X offsets of bolometers */
 double *ybc,                 /* nasmyth Y offsets of bolometers */
 double *position,            /* nasmyth positions of bolometers (arcsec) */
 double *dbuf,                /* generated frame (returned) */
 int *status                  /* global status (given and returned) */
 )

{
  /* Local variables */
  double a[3];                    /* Quadratic coefficients for defocus multiplier */
  double airmass;                 /* airmass for each bolometer */
  double astvalue;                /* obs. astronomical value in pW */
  double atmvalue;                /* obs. atmospheric emission in pW */
  int bol;                        /* counter for indexing bolometers */
  AstMapping *bolo2azel=NULL;     /* Mapping bolo-azel coordinates */
  AstCmpMap *bolo2map=NULL;       /* Combined mapping bolo->map coordinates */
  AstMapping *bolo2sky=NULL;      /* Mapping bolo->celestial coordinates */
  double current;                 /* bolometer current in amps */
  double defocus = 1.0;           /* Multiplier for `de-focussing' an image */
  double exponent;                /* Exponent of spike power law integral */
  double flux;                    /* flux at bolometer in pW */
  double fnoise;                  /* 1/f noise value */
  int i;                          /* loop counter */
  int lbnd_in[2];                 /* Pixel bounds for astTranGrid */
  double meanatm;                 /* Mean boresight atmosphere level */
  double phase;                   /* 1/f phase calculation */
  int pos;                        /* lookup in noise coefficients */
  double sigma;                   /* photon noise standard deviation */
  double *skycoord=NULL;          /* az & el coordinates */
  double skytrans;                /* sky transmission (%) */
  double spike;                   /* Spike value */
  int ubnd_in[2];
  double xpos;                    /* X measurement position */
  double xsky;                    /* X position on sky screen */
  double ypos;                    /* Y measurement position */
  double ysky;                    /* Y position on sky screen */
  double zenatm;                  /* zenith atmospheric signal (pW) */

  /* Check status */
  if ( *status != SAI__OK ) return;

  /* Concatenate mappings to get bolo->astronomical image coordinates */
  if ( inx.planetnum == -1 ) {
    astSetC( fset, "SYSTEM", "ICRS" );
  } else {
    astSetC( fset, "SYSTEM", "GAPPT" );
  }
  bolo2sky = astGetMapping( fset, AST__BASE, AST__CURRENT );
  if( !astOK ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "AST error extracting bolo->sky mapping",
           status);
  }

  if( *status == SAI__OK ) {
    bolo2map = astCmpMap( bolo2sky, sky2map, 1, " " );
    if( !astOK ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "AST error calculating bolo->image pixel mapping",
             status);
    }
  }

  /* Sample astronomical sky image */
  if( *status == SAI__OK ) {
    sc2sim_getast_wcs( inx.colsize, inx.rowsize, xbolo, ybolo, bolo2map, astsim, astnaxes,
                       sinx.interp, sinx.params, dbuf, status);
  }

  if( *status == SAI__OK ) {
    /* Extract bolo->AzEl mapping */
    astSetC( fset, "SYSTEM", "AZEL" );
    bolo2azel = astGetMapping( fset, AST__BASE, AST__CURRENT );

    if( !astOK ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "AST error calculating bolo->AzEl mapping",
             status);
    }
  }


  /* Allocate space for array */
  if( *status == SAI__OK ) {
    skycoord = astCalloc( nbol*2, sizeof(*skycoord) );

    lbnd_in[0] = 1;
    ubnd_in[SC2STORE__ROW_INDEX] = inx.colsize;
    lbnd_in[1] = 1;
    ubnd_in[SC2STORE__COL_INDEX] = inx.rowsize;

    /* Transform bolo offsets into positions in azel and store in
       skycoord */
    astTranGrid ( bolo2azel, 2, lbnd_in, ubnd_in, 0.1, 1000000, 1,
                  2, nbol, skycoord );

    if( !astOK ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "AST error evaluating bolo->AzEl mapping",
             status);
    }
  }

  /* If a valid focus position has been passed in then artificially
     dim (defocus) the astronomical source by a factor given by a
     simple quadratic. Note that a[2] must be negative or else there
     is no maximum! It must also be `small' so that the quadratic
     expression remains positive for all focus positions. Default
     coefficients of [+0.95,+0.10,-0.05] yield a best-fit focus
     position of +1 mm.
  */
  if ( focposn != VAL__BADD ) {
    a[0] = 0.95;
    a[1] = 0.1;
    a[2] = -0.05;
    defocus = a[0] + a[1]*focposn + a[2]*focposn*focposn;
  }

  /* For a noise observation, set the astronomical signal to zero */
  if ( strncmp(inx.obsmode, "NOISE", 5) == 0 ) {
    defocus = 0.0;
  }

  /* Get the zenith atmospheric signal */
  sc2sim_calctrans( inx.lambda, &skytrans, sinx.tauzen, status );
  sc2sim_atmsky( inx.lambda, skytrans, &zenatm, status );

  if( *status == SAI__OK ) {
    for ( bol=0; bol<nbol; bol++ ) {

      /* Calculate the elevation and airmass */
      airmass = 1.0 / sin ( skycoord[nbol + bol] );
      xpos = position[0] + xbc[bol] + 0.5 * (double)astnaxes[0] * astscale;
      ypos = position[1] + ybc[bol] + 0.5 * (double)astnaxes[1] * astscale;

      /* Get the observed astronomical value in Jy and convert it to pW.
         This value is not yet corrected for atmospheric transmission! */
      astvalue = dbuf[bol];
      astvalue = astvalue * sinx.jy2pw * defocus;

      /* Calculate mean atmospheric emission at current airmass
         from zenith emission */
      meanatm = zenatm * ( 1.0 - pow(0.01*skytrans,airmass) ) /
        ( 1.0 - (0.01*skytrans) );

      /* Calculate the photon noise from this mean loading */
      sc2sim_getsigma( sinx.refload, sinx.refnoise,
                       meanatm + sinx.telemission, &sigma, status );

      /* Add spikes if desired. In a given sample there is approximately
         a inx.steptime/sinx.spike_t0 chance of there being a spike. Spikes
         probably don't correlate with opacity, so the units are
         non-extinction corrected Jy. The brightness distribution
         is a power-law with fixed lower and upper limits */

      spike = 0;
      if( sinx.spike_t0 != 0 ) {

        if( sinx.spike_alpha == -1 ) {
          *status = SAI__ERROR;
          errRep(FUNC_NAME, "Spike power-law alpha can't be -1.",status);
          spike = 0;
          break;
        } else if( (((double) rand())/((double)RAND_MAX)) <
                   inx.steptime/sinx.spike_t0 ) {

          exponent = sinx.spike_alpha+1;
          spike = pow( (((double) rand())/((double) RAND_MAX)) *
                       ( pow(sinx.spike_p1,exponent) -
                         pow(sinx.spike_p0,exponent) ) +
                       pow(sinx.spike_p0,exponent), 1./exponent ) *
            sinx.jy2pw;
        }
      }

      /* Initialize atmvalue to 0 */
      atmvalue = 0;

      /* If the add atmospheric emission flag is set, use bilinear
         interpolation to find the atmvalue from the input sky noise
         image */
      if ( sinx.add_atm == 1 ) {

        /* Lookup atmospheric emission - offset to near centre of the
           atm frame.  A typical windspeed moves the sky screen at
           equivalent to 5000 arcsec per sec.  The scalar ATMVALUE
           contains the atmosphere map value for the current position
           (XPOS,YPOS). */

        xsky = xpos + sinx.atmxvel * start_time + sinx.atmzerox;
        ysky = ypos + sinx.atmyvel * start_time + sinx.atmzeroy;

        sc2sim_getbilinear ( xsky, ysky, atmscale, atmnaxes[0], atmsim,
                             &atmvalue, status );

        if ( !StatusOkP(status) ) {
          printf( "sc2sim_simframe: failed to interpolate sky bol=%d x=%e y=%e\n",
                  bol, xpos, ypos );

          break;
        } else {

          /* If we successfully sampled a brightness from the sky
             noise simulation, and we have the normalization of the
             noise power spectrum at the knee frequency (sigma),
             scale the normalized sky noise value by sigma and add on
             the mean level */

          atmvalue = atmvalue*sigma + meanatm;
        }

      } else {
        /* Otherwise, set atmvalue to smooth meanatm value */
        atmvalue = meanatm;
      }

      /* Calculate atmospheric transmission for this bolometer */
      sc2sim_atmtrans ( inx.lambda, meanatm, &skytrans, status );

      if( *status == SAI__OK ) {
        /*  Add atmospheric and telescope emission.
            TELEMISSION is a constant value for all bolometers.
            The 0.01 is needed because skytrans is a % */
        flux = 0.01 * skytrans * astvalue + atmvalue + sinx.telemission + spike;
      }
      /*  Add photon noise */
      if ( sinx.add_pns == 1 ) {
        sc2sim_addpnoise( sinx.refload, sinx.refnoise, samptime, &flux,
                          status );
      }

      /* Add heater, assuming mean heater level is set to add onto meanatm and
         TELEMISSION to give targetpow */
      if( *status == SAI__OK ) {
        if ( sinx.add_hnoise == 1 ) {
          flux = flux + ( inx.targetpow - meanatm - sinx.telemission ) *
            heater[bol];
        } else {
          flux = flux + ( inx.targetpow - meanatm - sinx.telemission );
        }
      }

      /* Convert to current with bolometer power offset.
         The bolometer offset in PZERO(BOL) is added to the FLUX, and then
         the power in FLUX is converted to a current in scalar CURRENT with
         help of the polynomial expression with coefficients in COEFFS(*) */
      if ( sinx.flux2cur == 1 ) {

        sc2sim_ptoi ( flux, SC2SIM__NCOEFFS, coeffs, pzero[bol], &current,
                      status );

      } else {
        current = flux;
      }

      if ( (sinx.add_fnoise == 1) && (*status == SAI__OK) ) {

        /*  Add instrumental 1/f noise to the smoothed data in output */
        pos = bol * nterms * 3;
        fnoise = 0.0;

        for ( i=0; i<nterms*3; i+=3 ) {
          phase = fmod ( start_time, noisecoeffs[pos+i] ) / noisecoeffs[pos+i];
          fnoise += noisecoeffs[pos+i+1] * cos ( 2.0 * AST__DPI * phase )
            + noisecoeffs[pos+i+2] * sin ( 2.0 * AST__DPI * phase );
        }

        current += fnoise;
      }

      dbuf[bol] = current;

      /* Break out of loop if bad status */
      if( *status != SAI__OK ) {
        bol = nbol;
      }
    }
  }

  /* Free resources */
  if( bolo2sky) bolo2sky = astAnnul(bolo2sky);
  if( bolo2map ) bolo2map = astAnnul(bolo2map);
  if( bolo2azel ) bolo2azel = astAnnul(bolo2azel);
  skycoord = astFree( skycoord );
}

