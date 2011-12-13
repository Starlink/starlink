/*
*+
*  Name:
*     smf_puthistory

*  Purpose:
*     Complete the history information in an output NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_puthistory( smfData *data, const char *appn, int *status );

*  Arguments:
*     data = smfData *  (Given)
*        Pointer to the structure describing the output NDF.
*     appn = const char * (Given)
*        Pointer to a null-terminated string holding the name of the
*        application that created the NDF.
*     status = int * (Given and Returned)
*        Inherited status value.

*  Description:
*     This function should be called immediately before closing an output
*     NDF. It re-reads the Provenance information from the NDF, clears
*     its creator hash code, and then writes the Provenance back out to
*     the NDF. This causes any remaining GRP history information to be
*     appended to the current history record in the NDF, and updates the
*     creator hash code accordingly in the PROVENANCE extension of the NDF.
*
*     Calling this function is only really necessary if Provenance
*     information was written to the output NDF before all GRP group
*     parameters had been accessed. For instance, smf_open_and_flatfield
*     causes Provenance information to be written to the output NDF, but
*     is usually called fairly early in an application. If any group
*     parameters are accessed after calling smf_open_and_flatfield, then
*     they will not be included in the NDF history unless this function
*     is called.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     18-AUG-2010 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
#include "star/ndg.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"

void smf_puthistory( smfData *data, const char *appn, int *status ){

/* Local Variables */
   int indf;
   NdgProvenance *prv;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get the NDF identifier. */
   indf = ( data && data->file ) ? data->file->ndfid : NDF__NOID;

/* Check we have a valid NDF identifier. */
   if( indf != NDF__NOID ) {

/* Read provenance from the NDF. */
      prv = ndgReadProv( indf, appn, status );

/* Clear the creator hash code in the Provenance so that ndgWriteProv
   will use the current history record in the NDF as the creator record. */
      ndgUnhashProv( prv, status );

/* Write out the provenance to the NDF. Since the creator hash code has
   been cleared, a new one will be found by first writing default history
   to the NDF (if not already done), then appending any remaining GRP
   history information to the current history record, and then
   calculating a new creator hash code on the basis of this expanded
   current history record. */
      ndgWriteProv( prv, indf, 1, status );

/* Release the memory holding the Provenance structure. */
      ndgFreeProv( prv, status );
   }
}

