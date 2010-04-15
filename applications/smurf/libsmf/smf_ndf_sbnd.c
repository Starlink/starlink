/*
*+
*  Name:
*     smf_ndf_sbnd

*  Purpose:
*     Set new pixel-index bounds for an NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_ndf_sbnd( int ndim, int *lbnd, int *ubnd, smfData *data,
*                        int *status )

*  Arguments:
*     ndim = int (Given)
*        New number of NDF dimensions.
*     lbnd = int * (Given)
*        Pointer to array of new lower pixel-index bounds.
*     ubnd = int * (Given)
*        Pointer to array of new upper pixel-index bounds.
*     data = smfData *
*        Pointer to the structure holding the NDF identifier.
*     status = int * (Given and Returned)
*        Inherited status value.

*  Description:
*     This function unmaps the NDF array components, calls ndfSbnd to
*     change the NDF pixel-index bounds, and then re-maps the array
*     components. It also updates other bounds-related information within
*     the smfData structure.
*
*     It returns without action if the smfData has no associated NDF.

*  Authors:
*     David S Berry (JAC, UCLan)
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "sae_par.h"
#include "ndf.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"

void smf_ndf_sbnd( int ndim, int *lbnd, int *ubnd, smfData *data,
                   int *status ){

/* Local Variables */
   const char *type;    /* Mapped data type */
   int dm;              /* Is the "Data" component mapped? */
   int el;              /* Nu,mber of array elements mapped */
   size_t i;               /* Axis index */
   int qm;              /* Is the "Quality" component mapped? */
   int vm;              /* Is the "Variance" component mapped? */

/* Check the inherited status */
   if( *status != SAI__OK ) return;

/* Do nothing if no NDF identifier is available. */
   if( data->file && data->file->ndfid != NDF__NOID ) {

/* Note which components are mapped. */
      dm = ( data->pntr[ 0 ] != NULL );
      vm = ( data->pntr[ 1 ] != NULL );
      qm = ( data->pntr[ 2 ] != NULL );

/* Unmap all NDF array components. */
      ndfUnmap( data->file->ndfid, "*", status );
      data->pntr[ 0 ] = NULL;
      data->pntr[ 1 ] = NULL;
      data->pntr[ 2 ] = NULL;

/* Modify the NDF bounds. */
      ndfSbnd( ndim, lbnd, ubnd, data->file->ndfid, status );

/* Map the required components again. */
      type = smf_dtype_string( data, status );
      if( dm ) ndfMap( data->file->ndfid, "Data", type, "UPDATE",
                       &( data->pntr[ 0 ] ), &el, status );
      if( vm ) ndfMap( data->file->ndfid, "Variance", type, "UPDATE",
                       &( data->pntr[ 1 ] ), &el, status );
      if( qm ) ndfMap( data->file->ndfid, "Quality", "_UBYTE",
                       "UPDATE", &( data->pntr[ 2 ] ), &el, status );

/* Modify the bounds information in the smfData. */
      for( i = 0; i < (size_t)ndim; i++ ) {
         data->dims[ i ] = ubnd[ i ] - lbnd[ i ] + 1;
      }
      for( ; i < data->ndims; i++ ) {
         data->dims[ i ] = 1;
      }
      data->ndims = ndim;

   }
}

