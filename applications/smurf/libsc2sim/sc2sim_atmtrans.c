/*
 *+
 *  Name:
 *     sc2sim_atmtrans

 *  Purpose:
 *     Calculate sky transmission from flux.

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_atmtrans (double lambda, double flux, double *trans, int *status)

 *  Arguments:
 *     lambda = double (Given)
 *        Wavelength in metres
 *     flux = double (Given and Returned)
 *        Flux value in pW
 *     trans = double* (Given)
 *        % atmospheric transmission
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Calculate the atmospheric transmission equivalent to the given
 *     flux for the indicated wavelength band using a simple relation
 *     deduced from modelling.

 *  Authors:
 *     B.D.Kelly (ROE)
 *     A.G. Gibb (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2005-01-20 (BDK):
 *        Original
 *     2005-02-05 (BDK):
 *        Revised constants
 *     2005-04-14 (AGG):
 *        Alternative method
 *     2005-04-15 (BDK):
 *        Corrected 850 constants
 *     2006-07-19 (JB):
 *        Split from dsim.c
 *     2006-10-18 (AGG):
 *        Use global status for reporting errors, improve smurfification

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
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */

/* Standard includes */
#include <stdlib.h>
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "prm_par.h"

/* SC2SIM includes */
#include "sc2sim.h"

#define FUNC_NAME "sc2sim_atmtrans"

void sc2sim_atmtrans
(
 double lambda,       /* wavelength in metres (given) */
 double flux,         /* flux per bolometer in pW (given) */
 double *trans,       /* % atmospheric transmission (returned) */
 int *status          /* global status (given and returned) */
 )

{

  /* Local variables */
  double zero;                 /* constant offset */
  double slope;                /* slope of relation */
  double maxflux;              /* Maximum sky flux */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* Check whether 450 or 850 microns */
  if ( fabs ( lambda - 0.45e-3 ) < 0.1e-3 ) {
    zero = 95.818;
    slope = -0.818;
    /*     slope = -8.772e-3;*/
  } else {
    zero = 102.0;
    slope = -5.0;
    /*     slope = -0.0465;*/
  }

  /*   zero = 1.0;*/
  maxflux = -zero/slope;

  if ( flux > maxflux ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSetd("F", flux);
      msgSetd("M",maxflux);
      errRep(FUNC_NAME, "Assumed sky flux, ^F pW, is greater than maximum value ^M pW", status);
    }
    *trans = 0;
  } else {
    /* agg's version */
    /*   *trans = 100.0*(zero + slope * flux);*/
    *trans = zero + slope * flux;
  }

}



