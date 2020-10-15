/*
*+
*  Name:
*     smf_calc_wvm

*  Purpose:
*     Function to calculate the optical depth from the raw WVM data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_calc_wvm( const smfHead *hdr, double approxam, AstKeyMap * extpars,
*                   int *status);

*  Arguments:
*     hdr = const smfHead* (Given)
*        Header struct from data struct. TCS_AIRMASS value will be
*        read from JCMTSTATE.
*     approxam = double (Given)
*        If the Airmass value stored in the header is bad, this
*        value is used as an approximation. If it is VAL__BADD
*        the AMSTART and AMEND headers will be read from the header.
*     extpars = AstKeyMap * (Given)
*        AST keymap containing the parameters required to convert
*        the tau (on the CSO scale) to the current filter. Must
*        contain the "taurelation" key which itself will be a keymap
*        containing the parameters for the specific filter. If NULL
*        the 225GHz tau will be returned.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine returns the optical depth for the given SCUBA-2
*     filter based on the raw temperature info from the WVM.

*  Notes:
*     - Returns a value of VAL__BADD if status is set bad on entry
*     - See also smf_scale_tau.c for scaling between filters/wavelengths
*     - If the TCS_AIRMASS value is missing "approxam" is used instead.
*       If this is a bad value then AMSTART or AMEND will be examined.
*     - Returns VAL__BADD if any of the temperature values are the bad value.
*     - A cache is implemented to allow previously calculated values to be
*       returned. Calling the function with NULL hdr and a negative airmass
*       will free the cache. The cache should be cleared between calls to the
*       monolith. Each thread has its own cache, stored in thread-specific
*       data .

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-02-03 (AGG):
*        Initial test version
*     2006-07-26 (TIMJ):
*        sc2head no longer used. Use JCMTState instead.
*     2008-04-29 (AGG):
*        Code tidy, add status check
*     2008-12-16 (TIMJ):
*        - Ambient temperature should be in kelvin
*        - PWV should be converted to zenith value before calculating tau.
*        - use smf_cso2filt_tau
*     2009-11-04 (TIMJ):
*        Add fallback airmass value in case we have bad TCS data.
*     2010-01-18 (TIMJ):
*        Check for bad values in the WVM temperature readings.
*     2010-06-03 (TIMJ):
*        Add keymap containing tau scaling parameters.
*     2010-12-21 (TIMJ):
*        Trap 0 airmass in same manner as VAL__BADD airmass. Fall back to using
*        TCS_AZ_AC2.
*     2011-04-12 (TIMJ):
*        Return 225 GHz tau if not extinction parameters supplied.
*     2012-03-06 (TIMJ):
*        Use PAL instead of SLA.
*     2013-02-04 (TIMJ):
*        Include date in WVM conversion.
*     2013-03-18 (DSB):
*        Ensure VAL__BADD is returned if airmass cannot be determined.
*     2013-07-22 (TIMJ):
*        Add a cache so that previously calculate values can be returned
*        immediately.
*     2013-08-16 (DSB):
*        Exempt the cache from AST context handling so that it is not
*        annulled when the current AST context ends.
*     2013-09-13 (TIMJ):
*        Add ability to disable cache explicitly. The cache doesn't work
*        in threads and causes problems when we let it simply trigger
*        AST__OBJIN and annul it. So now when you know you are in threads
*        you can explicitly disable it.
*     2013-09-24 (DSB):
*        Changed so that each thread has its own cache.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2013 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council.  Copyright (C) 2006-2008 University of British
*     Columbia.  All Rights Reserved.

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

#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "ast_err.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "star/pal.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

/* WVM includes */
#include "wvm/wvmCal.h"
#include "wvm/wvmTau.h"

#define CACHE_NAME "SMF_CALC_WVM_CACHE"

double smf_calc_wvm( const smfHead *hdr, double approxam, AstKeyMap * extpars,
                     int *status ) {

  /* Local variables */
  double airmass;           /* Airmass of current observation */
  AstKeyMap *cache = NULL;  /* The caceh for the running thread */
  float pwv;                /* Precipitable water vapour in mm */
  const JCMTState *state = NULL; /* STATE struct containing TCS info */
  double tau;               /* Zenith tau at current wavelength */
  double tamb;              /* Effective ambient temperature */
  float tau0;               /* Optical depth along line of sight */
  double tau225 = 0.0;      /* 225 GHz zenith optical depth */
  float twater;             /* Effective temperature of water vapour */
  float wvm[3];             /* WVM temperature in the 3 channels */
  double wvmtime;           /* Date of WVM reading */

  /* Initialise returned value */
  tau = VAL__BADD;

  /* Routine */
  if ( *status != SAI__OK) return VAL__BADD;

  /* Get a pointer to the cache used by the running thread. This will be
     NULL if no cache has yet been created. */
  if( !astMapGet0A( thrThreadData( status ), CACHE_NAME, &cache ) ) cache = NULL;

  /* See if we are required to clear any cache. If so, remove the cache from
     the thread specific data KeyMap, and then annul the cache object. */
  if (!hdr && approxam != VAL__BADD && approxam < 0.0 ) {
    if (cache) {
      astMapRemove( thrThreadData( status ), CACHE_NAME );
      cache = astAnnul( cache );
    }
    return VAL__BADD;
  }

  /* do not allow null pointer */
  if (!hdr) {
    *status = SAI__ERROR;
    errRep( " ", "hdr is NULL for calc_wvm. Possible programming error.", status );
    return VAL__BADD;
  }

  /* Sane airmass */
  if (approxam < 1.000 && approxam != VAL__BADD) {
    *status = SAI__ERROR;
    errRep( " ", "approxam has an invalid value. Must be >= 1.00 or BAD."
            " Possible programming error.", status );
    return VAL__BADD;
  }

  /* Initialise the cache and store it in the thread specific data
     KeyMap. */
  if (!cache) {
     astBeginPM;
     cache = astKeyMap( " " );
     astMapPut0A( thrThreadData( status ), CACHE_NAME, cache, " " );
     astEndPM;
  }

  /* Store TCS info */
  state = hdr->state;

  /* Retrieve WVM brightness temperatures and the airmass from the
     header */
  wvm[0] = state->wvm_t12;
  wvm[1] = state->wvm_t42;
  wvm[2] = state->wvm_t78;
  wvmtime = state->wvm_time;

  if (wvm[0] == VAL__BADR || wvm[1] == VAL__BADR || wvm[2] == VAL__BADR ) {
    cache = astAnnul( cache );
    return VAL__BADD;
  }

  airmass = state->tcs_airmass;

  if (airmass == VAL__BADD || airmass == 0.0 ) {
    /* try the tcs elevation value */
    if ( state->tcs_az_ac2 != VAL__BADD ) {
      airmass = palAirmas( M_PI_2 - state->tcs_az_ac2 );
    } else if ( approxam != VAL__BADD && approxam > 0) {
      airmass = approxam;
    } else {
      double amstart = VAL__BADD;
      double amend = VAL__BADD;
      smf_getfitsd( hdr, "AMSTART", &amstart, status );
      smf_getfitsd( hdr, "AMEND", &amend, status );
      if (amstart != VAL__BADD && amend != VAL__BADD) {
        airmass = (amstart + amend) / 2.0;
      } else if (amstart != VAL__BADD) {
        airmass = amstart;
      } else {
        airmass = amend;
      }
    }
  }

  /* Retrieve the ambient temperature and convert to kelvin */
  /* FUTURE: interpolate to current timeslice */
  smf_fits_getD( hdr, "ATSTART", &tamb, status );
  tamb += 273.15;

  if ( *status == SAI__OK ) {

    if (airmass == VAL__BADD) {
      *status = SAI__ERROR;
      errRep( " ", "Unable to determine airmass so can not calculate WVM opacity at zenith",
              status);
    } else {
      float rms;
      char cachekey[100];

      /* This is quite a costly operation but is predictable given the inputs.
         Therefore use a cache */
      sprintf( cachekey, "%.4f-%.4f-%.4f-%.4f-%.4f",
               airmass, tamb, wvm[0], wvm[1], wvm[2] );

      if (astMapHasKey( cache, cachekey )) {
        /* Get the value */
        astMapGet0D( cache, cachekey, &tau225 );

      } else {

        /* Get the pwv for this airmass */
        wvmOpt( (float)airmass, (float)tamb, wvm, &pwv, &tau0, &twater, &rms);

        /* Convert to zenith pwv */
        pwv /= airmass;

        /* convert zenith pwv to zenith tau */
        tau225 = pwv2tau_bydate( wvmtime, pwv );

        /* Store the result in the cache */
        astBeginPM;
        astMapPut0D( cache, cachekey, tau225, " " );
        astEndPM;

      }
    }
  }

  /* Scale from CSO to filter tau if necessary */
  if ( *status == SAI__OK ) {
    if (extpars) {
      tau = smf_cso2filt_tau( hdr, tau225, extpars, status );
    } else {
      tau = tau225;
    }
  }

  /*  printf("A = %g tau225 = %g tau = %g\n",airmass,tau225,tau);*/

  cache = astAnnul( cache );

  return tau;
}
