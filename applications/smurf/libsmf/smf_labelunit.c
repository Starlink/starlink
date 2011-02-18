/*
*+
*  Name:
*     smf_labelunit

*  Purpose:
*     Store Label and Unit components in the output NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_labelunit( Grp *igrp,  int size, smfData *odata, int *status );

*  Arguments:
*     igrp = Grp * (Given)
*        Group of input NDFs.
*     size = int (Given)
*        Number of elements in igrp
*     odata = smfData * (Given)
*        The output smfData object.
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Description:
*     This function gets the Label and Units components from the first
*     input NDF, and assigns them to the output NDF. It also checks that
*     all input NDFs have the same values for the Label and Units components.
*     If not, it issues a warning.

*  Authors:
*     David S Berry (JAC, UCLan)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     28-NOV-2006 (DSB):
*        Initial version.
*     29-OCT-2007 (EC):
*        Modified interface to smf_open_file.
*     7-MAY-2009 (DSB):
*        Use SMF__CHARLABEL to define lengths of NDF character components.
*     2010-03-16 (TIMJ):
*        Use smf_smfFile_msg instead of msgSetc
*     2010-03-18 (TIMJ):
*        Make use of the fact that a smfData now include data label and unit
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2009-2010 Science & Technology Facilities Council.
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

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "star/grp.h"
#include "star/one.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"

#define FUNC_NAME "smf_labelunit"

void smf_labelunit( Grp *igrp,  int size, smfData *odata, int *status ){

/* Local Variables */
   char label1[ SMF__CHARLABEL ];/* Label from first NDF */
   char unit1[ SMF__CHARLABEL ]; /* Unit from first NDF */
   int ifile;            /* Index of current input file */
   smfData *data = NULL; /* Pointer to data struct for current input file */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Loop round all the input NDFs. */
   for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++ ) {
     char * unit = NULL;
     char * label = NULL;

/* Obtain information about the current input NDF. */
     smf_open_file( igrp, ifile, "READ", 0, &data, status );

     if (*status == SAI__OK) {
       unit = data->hdr->units;
       label = data->hdr->dlabel;

/* If this is the first input NDF, copy the Label and Unit string to the
   output NDF, and save them for later use. */
       if( ifile == 1 ) {
         if( strlen( label ) ) {
            ndfCput( label, odata->file->ndfid, "Label", status );
         }

         if( strlen( unit ) ) {
            ndfCput( unit, odata->file->ndfid, "Unit", status );
         }

         one_strlcpy( label1, label, sizeof(label1), status );
         one_strlcpy( unit1, unit, sizeof(unit1), status );

/* Otherwise, compare the Label and Unit strings for this input NDF with
   those from the first input NDF. If any difference is found issue a
   warning. */
       } else if( *status == SAI__OK && strcmp( label, label1 ) ) {
         msgSeti( "I", ifile );
         msgSetc( "L1", label1 );
         msgSetc( "L", label );
         smf_smfFile_msg( data->file, "N", 1, "<unknown file>" );
         msgOutif( MSG__NORM, " ", "   WARNING: Input ^I (^N) has Label "
                   "'^L' but the first input had Label '^L1'.", status );

       } else if( *status == SAI__OK && strcmp( unit, unit1 ) ) {
         msgSeti( "I", ifile );
         msgSetc( "U1", unit1 );
         msgSetc( "U", unit );
         smf_smfFile_msg( data->file, "N", 1, "<unknown file>" );
         msgOutif( MSG__NORM, " ", "   WARNING: Input ^I (^N) has Unit "
                   "'^U' but the first input had Unit '^U1'.", status );

       }
     }

/* Close the current input data file. */
      smf_close_file( &data, status );
      data = NULL;

   }
}


