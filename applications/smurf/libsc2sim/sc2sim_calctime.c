/*
*+
*  Name:
*     sc2sim_calctime

*  Purpose:
*     Calculte UT + LST arrays given a start time

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_calctime ( double mjdaystart, double samptime, int nsamp, 
                        double *ut, double *lst, int *status )

*  Arguments:
*     mjdaystart = double (Given)
*        Start time as amodified juldate 
*     samptime = double (Given)
*        Length of sample in seconds 
*     nsamp = int (Given)
*        Number of samples
*     ut = double* (Returned)
*        UT at each sample (mod. juldate)
*     lst = double* (Returned)
*        LST at each sample (radians)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Given a start time and number of samples, calculate the UT and LST at
*     each sample.

*  Authors:
*     E.Chapin (UBC)
*     {enter_new_authors_here}

*  History :
*     2006-02-23 (EC):
*        Original
*     2006-07-20 (JB):
*        Split from dsim.c
*     2006-07-08 (EC)
*        Replace cut-and-pasted slaGmst with library call 

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

/* Starlink includes */
#include "ast.h"
#include "star/slalib.h"

/* SC2SIM includes */
#include "sc2sim.h"

void sc2sim_calctime
( 
double mjdaystart,   /* start time as modified juldate */
double samptime,     /* length of a sample in seconds */
int nsamp,           /* number of samples */
double *ut,          /* returned UT at each sample (mod. juldate) */
double *lst,         /* returned LST at each sample (radians) */
int *status          /* global status (given and returned) */
)

{

   /* Local variables */
   double gst;
   int i;
   double lon;
   double tu;
   double sampday;

   /* Check status */
   if ( !StatusOkP(status) ) return;
  
   /* JCMT longitude in radians */
   lon = ( 155.0 + (28.0/60.0) + (0.0/3600.0) ) / AST__DR2D;

   /* Length of a single sample in days */
   sampday = samptime/(3600. * 24.);
 
   /* Loop over each time step, calculate UT and then calculate 
      Greenwich sidereal time using slalib routine slaGmst and convert
      to local sidereal time */
  
   for(i=0; i<nsamp; i++) {
      ut[i] = mjdaystart + ((double) i)*sampday;

      gst = slaGmst( ut[i] );

      /* Calculate LST from GMST using telescope longitude */
      lst[i] = fmod(gst - lon + D2PI, D2PI);
   }

}

