/*
*+
*  Name:
*     smf_check_pol2

*  Purpose:
*     Check that all supplied files hold POL2 data.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_check_pol2( Grp *igrp,  int size, int raw, int *status );

*  Arguments:
*     igrp = Grp * (Given)
*        Group of NDF data files to check.
*     size = int (Given)
*        Number of elements in igrp
*     raw = int (Given)
*        If 0, check that the files hold Q or U values. If non-zero,
*        check they contain raw analysed intensoty.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function checks that all the supplied input files hold POL2
*     data - either analysed intensity or Q/U as indicated by argument
*     "raw". An error is reported if any non-POL2 files, or POL2 files
*     that are not of the requested type, are found.

*  Authors:
*     DSB: David S Berry (EAO):
*     {enter_new_authors_here}

*  History:
*     8-JUL-2016 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2016 East Asian Observatory.
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
#include "libsmf/smf.h"

#define FUNC_NAME "smf_check_pol2"

void smf_check_pol2( Grp *igrp,  int size, int raw, int *status ) {

/* Local Variables */
   smfData *data = NULL;        /* pointer to  SCUBA2 data struct */
   int i;                       /* Loop counter */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Loop over all files in the Grp */
   for( i = 1; i <= size && *status == SAI__OK; i++ ) {

/* Read header info  from the ith input file in the group */
      smf_open_file( NULL, igrp, i, "READ", SMF__NOCREATE_DATA, &data,
                     status );

/* Check there is a file. */
      if(  *status == SAI__OK && data->file == NULL ) {
          *status = SAI__ERROR;
          errRep( FUNC_NAME, "No smfFile associated with smfData.",
                  status );

/* Check there is a header. */
      } else if( *status == SAI__OK && data->hdr == NULL ) {
          smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );
          *status = SAI__ERROR;
          errRep( FUNC_NAME, "No smfHead associated with ^FILE.",
                  status );
      }

/* Check that there are 3 pixel axes. */
      if(  *status == SAI__OK && data->ndims != 3) {
         smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );
         msgSeti( "NDIMS", data->ndims );
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "^FILE has ^NDIMS pixel axes, should be 3.",
                 status );
      }

/* Check that POL2 is in the beam. */
      if( *status == SAI__OK && !(data->hdr->inbeam & SMF__INBEAM_POL) ) {
          smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );
          *status = SAI__ERROR;
          errRep( FUNC_NAME, "^FILE does not contain POL2 data.",
                  status );
      }

/* For raw data check that the NDF Label component is "Q" or "U". */
      if( *status == SAI__OK ) {
         if( raw  ) {
            if( strcmp( data->hdr->dlabel, "Signal" ) ) {
               smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );
               msgSetc( "L", data->hdr->dlabel );
               *status = SAI__ERROR;
               errRep( FUNC_NAME, "File ^FILE does not contain POL2 "
                       "analysed intensity data (NDF label is '^L' "
                       "but should be 'Signal').", status );
            }

/* For Q/U data check that the NDF Label component is "Q" or "U". */
         } else {
            if( strcmp( data->hdr->dlabel, "Q" ) &&
                strcmp( data->hdr->dlabel, "U" ) ) {
               smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );
               msgSetc( "L", data->hdr->dlabel );
               *status = SAI__ERROR;
               errRep( FUNC_NAME, "File ^FILE does not contain POL2 "
                       "Q or U data (NDF label is '^L' but should be "
                       "'Q' or 'U').", status );
            }
         }
      }

/* Close the data file */
      smf_close_file( NULL, &data, status);
   }
}



