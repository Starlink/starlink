/*
*+
*  Name:
*     smf_updateprov

*  Purpose:
*     Update the output NDF provenance to include a new input NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_updateprov( int ondf, smfData *data, int *status )

*  Arguments:
*     ondf = int (Given)
*        The output NDF identifier.
*     data = smfData * (Given)
*        Pointer to the structure describing the current input NDF.
*     status = int * (Given and Returned)
*        Inherited status value. 

*  Description:
*     This function records the current input NDF as a parent of the
*     output NDF. It includes the input OBSIDSS value in the output
*     provenance information.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     23-NOV-2007 (DSB):
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
#include "star/hds.h"
#include "star/ndg.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_updateprov( int ondf, smfData *data, int *status ){

/* Local Variables */
   HDSLoc *cloc = NULL;         /* Locator for HDS component */
   HDSLoc *tloc = NULL;         /* Locator for temp HDS storage */
   char *obsidss = NULL;        /* OBSIDSS value in input file */

/* Check the inherited status */
   if( *status != SAI__OK ) return;

/* Get the OBSIDSS keyword value from the input FITS header. If found,
   put it in an HDS structure that will be stored with the output
   provenance information. */
   if( astGetFitsS( data->hdr->fitshdr, "OBSIDSS", &obsidss ) ) {
      datTemp( "MORE", 0, NULL, &tloc, status );
      datNew0C( tloc, "OBSIDSS", strlen( obsidss ), status );
      datFind( tloc, "OBSIDSS", &cloc, status );
      datPut0C( cloc, obsidss, status );
      datAnnul( &cloc, status );
   }

/* Update the provenance for the output NDF to include the input NDF as
   an ancestor. Indicate that each input NDF is a root NDF (i.e. has no 
   parents). */
   ndgPtprv( ondf, data->file->ndfid, tloc, 1, "SMURF:MAKECUBE", status );

/* Free resources. */
   if( tloc ) datAnnul( &tloc, status );

}

