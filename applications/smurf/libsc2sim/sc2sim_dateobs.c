/*
 *+
 *  Name:
 *     sc2sim_dateobs

 *  Purpose:
 *     Calculate the DATE-OBS string

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     SC2SIM subroutine

 *  Invocation:
 *     sc2sim_dateobs ( double mjdaystart, char *dateobs, int *status);

 *  Arguments:
 *     mjdaystart = double (Given)
 *        MJD corresponding to first sample in file
 *     dateobs = char* (Given and Returned)
 *        Dateobs string
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *      This routine constructs the DATE-OBS string for the FITS header
 *      in the output files. The necessary input is the MJD at the
 *      start of the file. The DATE-OBS returned is on the same time
 *      scale as the input so it is the caller's responsibility to
 *      ensure that the returned string corresponds to the time they expect.

 *  Notes:
 *      - It's likely that this routine has re-invented the wheel so it may
 *        not be here forever.

 *  Authors:
 *     Andy Gibb (UBC)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     2006-12-01 (AGG):
 *        Original version
 *     2006-12-01 (AGG):
 *        String written with a single sprintf
 *     2006-12-08 (JB):
 *        Date for current chunk is precalculated by calling function.
 *     2012-03-06 (TIMJ):
 *        Replace SLA with SOFA.
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2012 Science & Technology Facilities Council.
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
 *     University of British Columbia. All Rights Reserved.

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

/* Standard includes */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/* STARLINK includes */
#include "sae_par.h"
#include "sofa.h"
#include "sofam.h"

/* Simulator includes */
#include "sc2sim.h"

#define FUNC_NAME "sc2sim_dateobs"
#define LEN__METHOD 20

void sc2sim_dateobs ( double mjdaystart, char *dateobs, int *status ) {

  int yy;                     /* Year for output file start time */
  int mm;                     /* Month for output file start time */
  int dd;                     /* Day for output file start time */
  double df;                  /* Day fraction for output file start time */
  int ihmsf[4];               /* Array containing H, M, S and .SS */
  char sign[2];               /* Sign of day fraction */

  if ( *status != SAI__OK) return;

  /* Convert this MJD to something more readable */
  (void) iauJd2cal( DJM0, mjdaystart, &yy, &mm, &dd, &df );

  /* Convert day fraction to hh:mm:ss */
  iauD2tf( 3, df, sign, ihmsf );

  /* Store this in dateobs as YYYY-MM-DDThh:mm:ss.sss */
  sprintf( dateobs, "%d-%02d-%02dT%02d:%02d:%02d.%03d",
           yy, mm, dd, ihmsf[0], ihmsf[1], ihmsf[2], ihmsf[3]);
}
