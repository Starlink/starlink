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
*     sc2sim_dateobs ( double mjdaystart, int maxwrite, double sample_t,
*                      int outscan, char *dateobs, int *status);

*  Arguments:
*     mjdaystart = double (Given)
*        MJD corresponding to first sample in file (TAI)
*     maxwrite = int (Given)
*        Maximum number of samples to write per output file
*     sample_t = double (Given)
*        Sample integration time in milliseconds
*     outscan = int (Given)
*        Sequence number for current output file
*     dateobs = char* (Given and Returned)
*        Dateobs string
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*      This routine constructs the DATE-OBS string for the FITS header
*      in the output files. The necessary inputs are the MJD at the
*      start of the file, the integration time per sample and the
*      sequence number of the current file. Note that the MJD is on
*      the TAI time scale.

*  Notes:
*      - MJD is a TAI time, not UT
*      - It's likely that this routine has re-invented the wheel so it may
*        not be here forever.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-12-01 (AGG): 
*        Original version 
*     2006-12-01 (AGG):
*        String written with a single sprintf
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia. All Rights Reserved.

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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Standard includes */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

/* STARLINK includes */
#include "mers.h"
#include "sae_par.h"
#include "star/slalib.h"
#include "f77.h"

/* Simulator includes */
#include "sc2sim.h"

#define FUNC_NAME "sc2sim_dateobs"
#define LEN__METHOD 20

void sc2sim_dateobs ( double mjdaystart, int maxwrite, double sample_t, 
		      int outscan, char *dateobs, int *status ) {

  double filestarttime;       /* MJD corresponding to first sample in output file */
  int yy;                     /* Year for output file start time */
  int mm;                     /* Month for output file start time */
  int dd;                     /* Day for output file start time */
  double df;                  /* Day fraction for output file start time */
  int ihmsf[4];               /* Array containing H, M, S and .SS */
  char sign[2];               /* Sign of day fraction */
  int date_status;            /* status of mjd->calendar date conversion*/

  if ( *status != SAI__OK) return;

  /* Calculate DATE-OBS for current file */
  /* Remember sample_t is in milliseconds - hence the 1e-3
     factor - and convert to a day fraction by dividing by the
     number of seconds in 24 hours */
  filestarttime = mjdaystart + (double)(maxwrite*sample_t*outscan*1.0e-3)/86400.0;

  /* Convert this MJD to something more readable */
  slaDjcl( filestarttime, &yy, &mm, &dd, &df, &date_status );

  /* Convert day fraction to hh:mm:ss */
  slaDd2tf( 3, df, sign, ihmsf );

  /* Store this in dateobs as YYYY-MM-DDThh:mm:ss.sss */
  sprintf( dateobs, "%d-%02d-%02dT%02d:%02d:%02d.%03d", 
	   yy, mm, dd, ihmsf[0], ihmsf[1], ihmsf[2], ihmsf[3]);



}
