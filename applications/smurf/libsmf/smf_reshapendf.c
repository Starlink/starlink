/*
*+
*  Name:
*     smf_reshapendf

*  Purpose:
*     Free a smfData and reshape the NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_reshapendf( smfData **data, smfTile *tile, int *status )

*  Arguments:
*     data = smfData ** (Given and Returned)
*        Pointer to the structure describing the NDF to be closed and
*        re-shaped. NULL is returned.
*     tile = smfTile * (Given)
*        Pointer to the structure defining the required shape for the
*        re-shaped NDF. 
*     status = int * (Given and Returned)
*        Inherited status value. This function attempts to execute even
*        if status is set to an error value on entry.

*  Description:
*     This function closes the specified NDF and assocaited resources,
*     then changes the pixel bounds of the NDF to match the unextended
*     bounds of the specified tile.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     4-SEP-2007 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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

void smf_reshapendf( smfData **data, smfTile *tile, int *status ){

/* Local Variables */
   int tndf;

/* Do nothing if no data was supplied. */
   if( *data ) {

/* If the NDF identifier is available, clone it. */
      if( (*data)->file && tile ) {
         ndfClone( (*data)->file->ndfid, &tndf, status ); 
      } else {
         tndf = NDF__NOID;
      }

/* Close the smfData structure holding the NDF description. */
      smf_close_file( data, status );

/* Change the shape of the NDF, and free the clonded NDF identifier. */
      if( tndf != NDF__NOID ) {
         ndfSbnd( 3, tile->lbnd, tile->ubnd, tndf, status ); 
         ndfAnnul( &tndf, status );
      }
   }          
}

