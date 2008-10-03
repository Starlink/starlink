/*
*+
*  Name:
*     smf_reportprogress

*  Purpose:
*     Report progress.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_reportprogress( int max, int *status )

*  Arguments:
*     max = int (Given)
*        Should be supplied non-zero only on the first call to this 
*        function. It gives the number of calls to this function that
*        corresponds to 100% of the job done. On subsequent calls it
*        should be supplied as zero. Supplying a non-zero value causes 
*        the internal counter to be reset to zero.
*     status = int * (Given and Returned)
*        Inherited status value. 

*  Description:
*     This function counts the number of times it has been invoked since
*     the previous invocation with a non-zero "max" value. On each
*     invocation it display this count as a percentage of the "max" value.
*     It uses VT100 escape sequences to update the previously displayed
*     figure.
*
*     All text is displayed at the "verbose" MSG information level.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     20-MAR-2008 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* System includes */
#include <stdio.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

#include "smf.h"

void smf_reportprogress( int max, int *status ){

/* Local Variables */
   char buf[20];
   static int count = 0.0;
   static int maxcount = 1;
   static int perc_last = 0;
   int perc;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* If a non-zero max is supplied, record it and reset the counter to
   zero. Also print a blank line. */
   if( max > 0 ) {
      maxcount = max;
      count = 0.0;
      msgOutif( MSG__VERB, "", " ", status );
      perc_last = -1;
   }   

/* Do nothing if we have already displayed "100 %" */
   if( perc_last < 100 ) {

/* Calculate the current percentage of the job. This also increments the
   count of invocations. */
      perc = (int) ( 100.0*( ((float) count++)/((float) maxcount ) ) + 0.5 );
      if( perc > 100 ) perc = 100;

/* If the percentage has changed, display the current progress, including 
   vt100 escape sequences that result in  each displayed percentage 
   over-printing the previous displayed percentage. Set the MSG "STREAM"
   tuning parameter non-zero so that the escape characters are left in place. */ 
      if( perc != perc_last ) {
         if( perc < 100 ) {
            sprintf( buf, "%3d %% done[9D[A", perc );
            msgTune( "STREAM", 1, status );
            msgOutif( MSG__VERB, "", buf, status );
            msgTune( "STREAM", 0, status );

         } else {
            sprintf( buf, "%3d %% done", perc );
            msgOutif( MSG__VERB, "", buf, status );
            msgOutif( MSG__VERB, "", " ", status );
         }
      } 

/* Record the previous percentage for next time. */
      perc_last = perc;
   }
}

