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
*                       int nboll, int frame, int nterms, double *noisecoeffs, 
*                       double *pzero, double samptime, double start_time, 
*                       double telemission, double *weights, AstMapping *sky2map,
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
*        Time at start of scan in sec
*     telemission = double (Given)
*        Power from telescope emission
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
*     Interpolate values of atmospheric background and astronomical image
*     for each bolometer in the current frame. Simulate photon and
*     instrumental noise and bolometer responses.

*  Authors:
*     B.D.Kelly (ROE)
*     E.Chapin (UBC)
*     J.Balfour (UBC)
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

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"

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
int frame,                   /* number of current frame (given) */
int nterms,                  /* number of 1/f noise coeffs (given) */
double *noisecoeffs,         /* 1/f noise coeffs (given) */
double *pzero,               /* bolometer power offsets (given) */
double samptime,             /* sample time in sec (given) */
double start_time,           /* time at start of scan in sec  (given) */
double telemission,          /* power from telescope emission (given) */
double *weights,             /* impulse response (given) */
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
   double airmass;                 /* airmass for each bolometer */
   double astvalue;                /* obs. astronomical value in pW */
   double atmvalue;                /* obs. atmospheric emission in pW */
   int bol;                        /* counter for indexing bolometers */
   AstMapping *bolo2azel=NULL;     /* Mapping bolo-azel coordinates */
   AstCmpMap *bolo2map=NULL;       /* Combined mapping bolo->map coordinates */
   AstMapping *bolo2sky=NULL;      /* Mapping bolo->celestial coordinates */
   double current;                 /* bolometer current in amps */
   double flux;                    /* flux at bolometer in pW */
   double fnoise;                  /* 1/f noise value */
   int i;                          /* loop counter */
   double meanatm;                 /* mean expected atmospheric signal (pW) */
   double phase;                   /* 1/f phase calculation */
   int pos;                        /* lookup in noise coefficients */
   double skytrans;                /* sky transmission (%) */
   double time;                    /* time from start of observation */
   double xpos;                    /* X measurement position */
   double xsky;                    /* X position on sky screen */
   double ypos;                    /* Y measurement position */
   double ysky;                    /* Y position on sky screen */
   double *skycoord=NULL;          /* az & el coordinates */
   int lbnd_in[2];                 /* Pixel bounds for astTranGrid */
   int ubnd_in[2];
  
    /* Check status */
   if ( !StatusOkP(status) ) return;

   /* Concatenate mappings to get bolo->astronomical image coordinates */
   astSetC( fset, "SYSTEM", "icrs" );
   bolo2sky = astGetMapping( fset, AST__BASE, AST__CURRENT );
   if( !astOK ) {
     *status = SAI__ERROR;
     errRep(FUNC_NAME, "AST error extracting bolo->sky mapping", 
	    status);
   }

   if( *status == SAI__OK ) {
     bolo2map = astCmpMap( bolo2sky, sky2map, 1, "" );  
     if( !astOK ) {
       *status = SAI__ERROR;
       errRep(FUNC_NAME, "AST error calculating bolo->image pixel mapping", 
	      status);
     }
   }

   /* Sample astronomical sky image */
   if( *status == SAI__OK ) {
     sc2sim_getast_wcs( nbol, xbolo, ybolo, bolo2map, astsim, astnaxes, dbuf,
			status);
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
     skycoord = smf_malloc ( nbol*2, sizeof(*skycoord), 1, status  );
     
     lbnd_in[0] = 1;
     ubnd_in[0] = BOLROW;
     lbnd_in[1] = 1;
     ubnd_in[1] = BOLCOL;

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

   /* Get time when frame taken in seconds */
   if( *status == SAI__OK ) {
     time = start_time + frame * samptime;
   }

   /* Get the mean atmospheric signal */
   sc2sim_calctrans ( inx.lambda, &skytrans, sinx.tauzen, status );
   sc2sim_atmsky ( inx.lambda, skytrans, &meanatm, status );

   if( *status == SAI__OK ) {
     for ( bol=0; bol<nbol; bol++ ) {

       /* Calculate the elevation correction. */
       airmass = 1.0 / sin ( skycoord[nbol + bol] );

       xpos = position[0] + xbc[bol] + 0.5 * (double)astnaxes[0] * astscale;
       ypos = position[1] + ybc[bol] + 0.5 * (double)astnaxes[1] * astscale;
       
       /* Get the observed astronomical value in Jy and convert it to pW. */
       astvalue = dbuf[bol];
       astvalue = astvalue * 1.0e-5 * AST__DPI * 0.25 * DIAMETER 
	 * DIAMETER * sinx.bandGHz;
       
       /* Lookup atmospheric emission - offset to near centre of the atm frame.
	  A typical windspeed moves the sky screen at equivalent to 5000 arcsec
	  per sec.
	  The scalar ATMVALUE contains the atmosphere map value for the 
	  current position (XPOS,YPOS). */
       
       xsky = xpos + sinx.atmxvel * time + sinx.atmzerox;
       ysky = ypos + sinx.atmyvel * time + sinx.atmzeroy;
       
       /* If the add atmospheric emission flag is set, use bilinear interpolation
	  to find the atmvalue from the input file. */
       if ( sinx.add_atm == 1 ) {
	 
         sc2sim_getbilinear ( xsky, ysky, atmscale, atmnaxes[0], atmsim, 
			      &atmvalue, status );
	 
         if ( !StatusOkP(status) ) {
	   printf ( "sc2sim_simframe: failed to interpolate sky bol=%d x=%e y=%e\n",
		    bol, xpos, ypos );
	   break;
         }
	 
       } else {
         /* Otherwise, calculate the atmvalue from the tau */
         atmvalue = meanatm;
       }
       
       /* Apply the elevation correction */
       atmvalue = atmvalue * airmass;
       
       /* Calculate atmospheric transmission for this bolometer */
       sc2sim_atmtrans ( inx.lambda, atmvalue, &skytrans, status );
       
       if( *status == SAI__OK ) {
	 atmvalue = atmvalue / airmass;
	 
	 /*  Add atmospheric and telescope emission.
	     TELEMISSION is a constant value for all bolometers. */
	 flux = 0.01 * skytrans * astvalue + atmvalue + telemission;
       
	 /*  Add offset due to photon noise */
       }	 

       if ( sinx.add_pns == 1 ) {
         sc2sim_addpnoise ( inx.lambda, sinx.bandGHz, sinx.aomega, 
		            samptime, &flux, status );
	 
       }
       
       /* Add heater, assuming mean heater level is set to add onto meanatm and
	  TELEMISSION to give targetpow */
       
       if( *status == SAI__OK ) {
	 if ( sinx.add_hnoise == 1 ) {
	   flux = flux + ( inx.targetpow - meanatm - telemission ) * 
	     heater[bol];
	 } else {
	   flux = flux + ( inx.targetpow - meanatm - telemission );
	 }
       }

       /* Convert to current with bolometer power offset.
	  The bolometer offset in PZERO(BOL) is added to the FLUX, and then
	  the power in FLUX is converted to a current in scalar CURRENT with 
	  help of the polynomial expression with coefficients in COEFFS(*) */
       if ( sinx.flux2cur == 1 ) {
	 
         sc2sim_ptoi ( flux, SC2SIM__NCOEFFS, coeffs, 
                       pzero[bol], &current, status );
	 
       } else {
         current = flux;
       }
       
       if ( (sinx.add_fnoise == 1) && (*status == SAI__OK) ) {
	 
         /*  Add instrumental 1/f noise to the smoothed data in output */
         pos = bol * nterms * 3;
         fnoise = 0.0;
         time = start_time + frame * samptime;
	 
         for ( i=0; i<nterms*3; i+=3 ) {
	   phase = fmod ( time, noisecoeffs[pos+i] ) / noisecoeffs[pos+i];
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
   smf_free( skycoord, status );
   
}

