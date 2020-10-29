/*
*+
*  Name:
*     smf_rebincube_paste_thread

*  Purpose:
*     Invoke smf_rebincube_paste2d or smf_rebincube_paste3d from within a
*     thread.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_rebincube_paste_thread( void *data, int *status )

*  Arguments:
*     data = void * (Given)
*        A pointer to a data structure (a smfRebincubeNNArgs2) holding
*        parameter values for smf_rebincube_paste2d/3d.
*     status = int * (Given and Returned)
*        A pointer to the inherited status value.

*  Description:
*     This function invokes smf_rebincube_paste2d or smf_rebincube_paste3d
*     from within a new thread. See smf_rebincube_nn. The API for this
*     function is dictated by the thrAddJob function, so information
*     needed for smf_rebincube_paste2d/3d is passed into this function
*     within a smfRebincubeNNArgs2 structure.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     11-JUN-2008 (DSB):
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

/* Smurf includes */
#include "smf.h"
#include "smf_typ.h"

void smf_rebincube_paste_thread( void *data_ptr, int *status ){

/* Local Variables: */
   smfRebincubeNNArgs2 *data = (smfRebincubeNNArgs2 *) data_ptr;
   smfRebincubeNNArgs1 *cdata;

/* Check the inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer to the data that is common to all detectors. */
   cdata = data->common;

/* 2D algorithm... */
   if( cdata->is2d ) {
      data->used = smf_rebincube_paste2d( cdata->badmask, cdata->nchan, cdata->nchanout,
                                          cdata->spectab, cdata->specpop, data->iv0,
                                          cdata->nxy, data->wgt, cdata->genvar,
                                          data->invar, data->ddata, cdata->data_array,
                                          cdata->var_array, cdata->wgt_array,
                                          cdata->pop_array, &(data->nused),
                                          &(data->nreject), &(data->naccept), data->work,
                                          status );

/* 3D algorithm... */
   } else {
      data->used = smf_rebincube_paste3d( cdata->nchan, cdata->nout, cdata->spectab,
                                          data->iv0, cdata->nxy, data->wgt,
                                          cdata->genvar, data->invar, data->ddata,
                                          cdata->data_array, cdata->var_array,
                                          cdata->wgt_array, &(data->nused), status );
      data->naccept++;
   }
}

