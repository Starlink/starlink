/*
*+
*  Name:
*     smf_rebinseq_thread

*  Purpose:
*     Invoke astRebinSeqF/D from within a thread.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_rebinseq_thread( void *data, int *status )

*  Arguments:
*     data = void * (Given)
*        A pointer to a data structure (a smfRebinSeqArgs) holding parameter
*        values for astRebinSeq.
*     status = int * (Given and Returned)
*        A pointer to the inherited status value.

*  Description:
*     This function invokes astRebinSeqF or astRebinSeqF from within a
*     new thread. The API for this function is dictated by the
*     thrAddJob function, so information needed for astRebinSeq
*     is passed into this function within a smfRebinSeqArgs structure.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     16-JUN-2008 (DSB):
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"

/* Smurf includes */
#include "smf_typ.h"
#include "smf.h"

#pragma GCC diagnostic ignored "-Wcast-qual"

void smf_rebinseq_thread( void *data_ptr, int *status ){

/* Local Variables: */
   int *old_status;
   smfRebinSeqArgs *data = (smfRebinSeqArgs *) data_ptr;

/* Check the inherited status */
   if( *status != SAI__OK ) return;

/* Tell AST to use the inherited status value as its "global" status variable */
   old_status = astWatch( status );

/* The Mapping pointer in the data structure should have been unlocked by
   the parent thread, so we can now lock it for exclusive use by this
   thread. In order to enforce carefull usage of Objects, AST insists that
   Objects be locked before they are used */
   astLock( data->this, 0 );

/* And finally call astRebinSeqF or astRebinSeqD to paste the input data
   into the output array. */
   if( data->is_double ) {
      astRebinSeq8D( data->this, data->wlim, data->ndim_in, data->lbnd_in,
                     data->ubnd_in, (double *)data->in, (double *)data->in_var,
                     data->spread, data->params, data->flags, data->tol,
                     data->maxpix, data->badval_d, data->ndim_out,
                     data->lbnd_out, data->ubnd_out, data->lbnd, data->ubnd,
                     (double *) data->out, (double *) data->out_var,
                     data->weights, &(data->nused) );
   } else {
      astRebinSeq8F( data->this, data->wlim, data->ndim_in, data->lbnd_in,
                     data->ubnd_in, (float *)data->in, (float *)data->in_var,
                     data->spread, data->params, data->flags, data->tol,
                     data->maxpix, data->badval_f, data->ndim_out,
                     data->lbnd_out, data->ubnd_out, data->lbnd, data->ubnd,
                     (float *) data->out, (float *) data->out_var,
                     data->weights, &(data->nused) );
   }

/* Unlock the Mapping so that the parent thread can lock it and then
   annul it. */
   astUnlock( data->this, 1 );

/* Tell AST to use the original status variable */
   (void) astWatch( old_status );

}

