/*
*+
*  Name:
*     smf_calc_wvm_clear

*  Purpose:
*     Clear all the caches used by smf_calc_wvm.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_calc_wvm_clear( int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function clears the cache used by smf_calc_wvm. Each thread
*     (including the main thread) has its own cache, and all these caches
*     are cleared.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-SEP-2013 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "sae_par.h"
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Prototypes for private functions defined in this file. */
static void smf1_calc_wvm_clear_job( void *job_data, int *status );

void smf_calc_wvm_clear( int *status ) {

/* Local Variables */
   int iw;
   int nw;
   ThrWorkForce *wf;

/* Clear the cache used by the main thread. */
   smf_calc_wvm( NULL, -1.0, NULL, status );

/* Get a pointer to the singleton workforce used by smurf. */
   wf = thrGetWorkforce( -1, status );

/* If no workforce exists, we have nothing more to do. */
   if( wf ) {

/* Submit jobs to the workforce to clear the cache used by each
   worker thread. */
      nw = wf ? wf->nworker : 1;
      for( iw = 0; iw < nw; iw++ ) {
         thrAddJob( wf, 0, NULL, smf1_calc_wvm_clear_job, 0, NULL,
                    status );
      }

/* Wait for the workforce to complete all jobs. */
      thrWait( wf, status );
   }
}


static void smf1_calc_wvm_clear_job( void *job_data __attribute__((unused)),
                                     int *status ) {
/*
*  Name:
*     smf1_calc_wvm_clear_job

*  Purpose:
*     Clear the smf_calc_wvm cache in a worker thread.

*  Invocation:
*     void smf1_calc_wvm_clear_job( void *job_data, int *status )

*  Arguments:
*     job_data = void * (Given)
*        Pointer to the data needed by the job.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine clears the smf_calc_wvm cache in a worker thread.

*/
   smf_calc_wvm( NULL, -1.0, NULL, status );
}



