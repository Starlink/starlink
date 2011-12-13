/*
 *+
 *  Name:
 *     sc2sim_atmsky

 *  Purpose:
 *     Calculate sky flux given sky transmission.

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_atmsky ( double lambda, double trans, double *flux, int *status )

 *  Arguments:
 *     lambda = double (Given)
 *        Wavelength in metres
 *     trans = double (Given)
 *        % atmospheric transmission
 *     flux = double* (Returned)
 *        Flux per bolometer in pW
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Calculate the sky noise power given the atmospheric
 *     transmission. This routine is basically the inverse of
 *     dsim_atmtrans.

 *  Authors:
 *     A.G.Gibb (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2005-04-20 (AGG):
 *        Original
 *     2006-07-19 (JB):
 *        Split from dsim.c
 *     2006-10-18 (AGG):
 *        Use global status to report errors, update prologue

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
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "prm_par.h"

/* SC2SIM includes */
#include "sc2sim.h"

#define FUNC_NAME "sc2sim_atmsky"

void sc2sim_atmsky
(
 double lambda,       /* wavelength in metres (given) */
 double trans,        /* % atmospheric transmission (given) */
 double *flux,        /* flux per bolometer in pW (returned) */
 int *status          /* global status (given and returned) */
 )

{
  /* Local variables */
  double zero;                 /* constant offset */
  double slope;                /* slope of relation */
  double maxflux;              /* Maximum sky flux */

  /* Check status. */
  if ( !StatusOkP(status) ) return;

  /* Check whether 450 or 850 microns. */
  if ( fabs ( lambda - 0.45e-3 ) < 0.1e-3 ) {
    zero = 95.818;
    slope = -0.818;
    /*     slope = -8.772e-3;*/
  } else {
    zero = 102.0;
    slope = -5.0;
    /*     slope = -0.0465;*/
  }

  /* zero = 1.0;*/
  maxflux = -zero/slope;

  *flux = ( trans - zero ) / slope;

  if ( *flux > maxflux ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSetd("F", *flux);
      msgSetd("M",maxflux);
      errRep(FUNC_NAME, "Derived sky flux, ^F pW, is greater than maximum value ^M pW", status);
    }
  } else if ( *flux < 0 ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSetd("F", *flux);
      errRep(FUNC_NAME, "Derived sky flux, ^F pW, is negative", status);
    }
  }
}


