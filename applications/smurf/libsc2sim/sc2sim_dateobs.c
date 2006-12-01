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
*        MJD corresponding to first sample in file
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
*      This routine calculates the DATE-OBS string for the FITS header
*      in the output files. The necessary inputs are the MJD at the
*      start of the file, the integration time per sample and the
*      sequence number of the current file.

*  Notes:
*      - It's likely that this routine has re-invented the wheel so it may
*        not be here forever.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-12-01 (AGG): 
*        Original version 
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
  char str1[5];               /* Temp string */
  int date_status;                /* status of mjd->calendar date conversion*/

  if ( *status != SAI__OK) return;

  /* Calculate DATE-OBS for current file */
  /* Remember sample_t is in milliseconds - hence the 1e-3
     factor - and convert to a day fraction by dividing by the
     number of seconds in 24 hours */
  filestarttime = mjdaystart + (double)(maxwrite*sample_t*outscan*1.0e-3)/86400.0;

  /* Convert this MJD to something more readable */
  slaDjcl( filestarttime, &yy, &mm, &dd, &df, &date_status );

  /* Convert day fraction to hh:mm:ss */
  slaDd2tf( 0, df, sign, ihmsf );

  /* Convert numbers to strings */

  /* Four digit year */
  sprintf ( str1 ,"%d-", yy );
  strncat ( dateobs, str1, 5 ); 

  /* 1 or 2 digit month */
  sprintf ( str1 ,"%d-", mm );
  if ( mm < 10 ) {
    strncat( dateobs, "0", 1);
    strncat ( dateobs, str1, 2 ); 
  } else {
    strncat ( dateobs, str1, 3 );
  }

  /* 1 or 2 digit day */
  sprintf ( str1 ,"%dT", dd );
  if ( dd < 10 ) {
    strncat( dateobs, "0", 1);
    strncat ( dateobs, str1, 2 ); 
  } else {
    strncat ( dateobs, str1, 3 );
  }

  /* 1 or 2 digit hours */
  sprintf ( str1 ,"%d:", ihmsf[0] );
  if ( ihmsf[0] < 10 ) {
    strncat( dateobs, "0", 1);
    strncat ( dateobs, str1, 2 ); 
  } else {
    strncat ( dateobs, str1, 3 );
  }

  /* 1 or 2 digit minutes */
  sprintf ( str1 ,"%d:", ihmsf[1] );
  if ( ihmsf[1] < 10 ) {
    strncat( dateobs, "0", 1);
    strncat ( dateobs, str1, 2 ); 
  } else {
    strncat ( dateobs, str1, 3 );
  }

  /* 1 or 2 digit seconds */
  sprintf ( str1 ,"%d", ihmsf[2] );
  if ( ihmsf[2] < 10 ) {
    strncat( dateobs, "0", 1);
    strncat ( dateobs, str1, 1 ); 
  } else {
    strncat ( dateobs, str1, 2 );
  }

}
