/*
 *+
 *  Name:
 *     sc2sim_calctime

 *  Purpose:
 *     Calculate UT1 + LAST arrays given a start time

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_calctime ( double lon, double mjdaystart, double dut1,
 *                       double samptime, int nsamp,
 *                       double *ut, double *last, int *status )

 *  Arguments:
 *     lon = double (Given)
 *        Geodetic longitude of observer in radians (+ve E)
 *     mjdaystart = double (Given)
 *        UTC start time as a modified juldate
 *     dut1 = double (Given)
 *        DUT1 value in seconds
 *     samptime = double (Given)
 *        Length of sample in seconds
 *     nsamp = int (Given)
 *        Number of samples
 *     ut = double* (Returned)
 *        UT at each sample (mod. juldate)
 *     last = double* (Returned)
 *        LAST at each sample (radians)
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Given a start time and number of samples, calculate the UT (UT1)
 *     and local apparent sidereal time (LAST) at each sample.

 *  Notes:
 *     - The ut & last pointers must be allocated externally to this routine
 *     - The input time is a UTC MJD
 *     - Expressing the UTC as MJD means that there cannot be any leap seconds
 *       during the simulation

 *  Authors:
 *     E. Chapin (UBC)
 *     J. Balfour (UBC)
 *     A.G. Gibb (UBC)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History :
 *     2006-02-23 (EC):
 *        Original
 *     2006-07-20 (JB):
 *        Split from dsim.c
 *     2006-07-08 (EC)
 *        Replace cut-and-pasted slaGmst with library call
 *     2006-09-05 (JB)
 *        Included smurf_par.h
 *     2006-09-08 (EC)
 *        Made Longitude an argument
 *     2006-12-12 (AGG):
 *        - Update to return local apparent sidereal time (see SUN/67)
 *        - Calculate equation of equinoxes
 *        - Check input pointers are not NULL
 *     2006-12-15 (AGG):
 *        Fix bug in calculating LAST due to misplaced eqeqx.
 *     2006-12-18 (AGG):
 *        Add DUT1 as argument, call slaDtt, update prologue
 *     2012-03-06 (TIMJ):
 *        Replace SLA with PAL.

 *  Copyright:
 *     Copyright (C) 2012 Science & Technology Facilities Council.
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
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "star/pal.h"
#include "mers.h"
#include "sae_par.h"

/* SMURF includes */
#include "sc2sim.h"
#include "smurf_par.h"

#define FUNC_NAME "sc2sim_calctime"

void sc2sim_calctime
(
 double lon,          /* Geodetic W Lon (radians) */
 double mjdaystart,   /* start time as modified juldate (UTC) */
 double dut1,         /* DUT1 in seconds */
 double samptime,     /* length of a sample in seconds */
 int nsamp,           /* number of samples */
 double *ut,          /* returned UT at each sample (mod. juldate) */
 double *last,        /* returned LAST at each sample (radians) */
 int *status          /* global status (given and returned) */
 )

{

  /* Local variables */
  double dttd;                   /* TT-UTC in days */
  double eqeqx;                  /* Equation of the equinoxes in radians */
  double gmst;                   /* Greenwich Mean Sidereal Time (radians) */
  int i;                         /* Loop counter */
  double sampday;                /* Sample time in days */
  double start_ut1;              /* UT1 (MJD) corresponding to start of scan */

  /* Check status */
  if ( *status != SAI__OK ) return;

  /* Check input pointers are not NULL */
  if ( ut == NULL || last == NULL ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Input pointers to sc2sim_calctime are NULL", status);
    }
  }

  if ( *status == SAI__OK ) {

    /* Length of a single sample in days */
    sampday = samptime / SPD;

    /* Convert start time from UTC to UT1 */
    start_ut1 = mjdaystart + dut1/SPD;

    /* Calculate the TT-UTC difference in days, assume TT is equivalent
       to TDB (needed below). Assume it doesn't change significantly
       over an observation. */
    dttd = palDtt( mjdaystart ) / SPD;

    /* Calculate the equation of the equinoxes: input time must be TDB
       (= TT above). This only needs to be done once per simulation as
       the change over a minute is of order microarcsec.
       TDB = TT = START_TIME (UTC) + (TT-UTC) */
    eqeqx = palEqeqx( mjdaystart + dttd );

    /* Loop over each time step, calculate UT1 and then calculate
       Greenwich mean sidereal time using slalib routine slaGmst and
       convert to local apparent sidereal time */
    for(i=0; i<nsamp; i++) {
      /* The following is OK because we assume dut1 does not change
         significantly over the course of a simulation */
      ut[i] = start_ut1 + ((double) i)*sampday;

      gmst = palGmst( ut[i] );

      /* Calculate LAST from GMST using telescope longitude and equation
         of equinoxes */
      last[i] = fmod( gmst - lon + eqeqx + ERFA_D2PI, ERFA_D2PI );
    }
  }
}

