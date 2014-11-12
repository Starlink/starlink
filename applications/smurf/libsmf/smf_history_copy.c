/*
*+
*  Name:
*     smf_history_copy

*  Purpose:
*     Copy the History component from an input NDF to the output NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_history_copy( Grp *igrp,  int ifile, int ondf, int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Group of input NDFs.
*     ifile = int (Given)
*        The one-based index of the file within "igrp" from which the
*        History component is to be copied.
*     ondf = int (Given)
*        Identifier for the NDF in which the copied History component is
*        to be placed. Any pre-existing History component is replaced.
*     status = int* (Given and Returned)
*        Pointer to inherited status.

*  Description:
*     This function copies the History component from the specified input
*     NDF to the supplied output NDF, replacing any existing History component.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-NOV-2014 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Science & Technology Facilities Council.
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
#include "mers.h"
#include "sae_par.h"

/* SMURF includes */
#include "smf.h"

#define FUNC_NAME "smf_history_copy"

void smf_history_copy( Grp *igrp,  int ifile, int ondf, int *status ){

/* Local Variables */
   smfData *data = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Obtain information about the specified input NDF. */
   smf_open_file( NULL, igrp, ifile, "READ", 0, &data, status );

/* Notrhing to do if there is no input NDF. */
   if( data && data->file ) {

/* Ensure the History component is defined in the output NDF. */
      ndfHcre( ondf, status );

/* Copy any history information from input to output. */
      ndfHcopy( data->file->ndfid, ondf, status );
   }

/* Close the input data file. */
   smf_close_file( NULL, &data, status );

/* Issue a context message if anything went wrong. */
   if( *status != SAI__OK ) errRep( FUNC_NAME, "Unable to copy History "
                                    "information to output NDF.", status );
}
