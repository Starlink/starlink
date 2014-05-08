/*
*+
*  Name:
*     FITSMERGE

*  Purpose:
*     Merge FITS headers.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_fitsmerge(int *status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine reads the FITS headers of the files mentioned in the
*     IN Parameter and merges them using the smf_fits_outhdr function.
*     The merged headers are then written into the file specified by the
*     NDF Parameter.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Files containing input FITS headers.
*     NDF = NDF (Read and Write)
*          File to receive merged FITS headers.

*  Authors:
*     Graham Bell (JAC)
*     {enter_new_authors_here}

*  History:
*     07-MAY-2014 (GSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2014 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"

#define FUNC_NAME "smurf_fitsmerge"
#define TASK_NAME "FITSMERGE"
#define LEN__METHOD 20

void smurf_fitsmerge(int* status) {
   AstFitsChan *fc = NULL;    /* Merged FITS headers */
   Grp* igrp = NULL;          /* Group of input files */
   int ondf;                  /* NDF to update */
   int flag;                  /* Was the group expression flagged? */
   size_t i;                  /* Input file index */
   size_t isize;              /* Number of files in input group */
   smfData* data = NULL;      /* Pointer to input data struct */
   smfHead *hdr = NULL;       /* Pointer to data header */

/* Check inherited status */
   if (*status != SAI__OK) return;

/* Begin an AST context */
   astBegin;

/* Begin an NDF context. */
   ndfBegin();

/* Get the group of input files. */
   ndgAssoc("IN", 1, &igrp, &isize, &flag, status);

/* Get the group of files to update. */
   ndfAssoc("NDF", "UPDATE", &ondf, status);

/* Loop round all the input files. */
   for (i = 0; i < isize && *status == SAI__OK; i ++) {
      ndfBegin();

      smf_open_file(NULL, igrp, i + 1, "READ",
         SMF__NOTTSERIES | SMF__NOCREATE_DATA |
         SMF__NOCREATE_FTS | SMF__NOFIX_METADATA,
         &data, status);

      if (*status != SAI__OK) {
         errRep(FUNC_NAME, "Could not open input file.", status);
         break;
      }
      else if ((hdr = data->hdr) == NULL) {
         *status = SAI__ERROR;
         errRep(FUNC_NAME, "No smfHead associated with smfData.", status);
         break;
      }

      smf_fits_outhdr(hdr->fitshdr, &fc, status);

      smf_close_file(NULL, &data, status);

      ndfEnd(status);
   }

/* Close any input data file that is still open due to an early exit from
   the above loop. */
   if (data != NULL) smf_close_file(NULL, &data, status);

/* Write the merged FITS headers into the output file. */
   kpgPtfts(ondf, fc, status);

/* Free remaining resources. */
   if (igrp != NULL) grpDelet(&igrp, status);
   ndfAnnul(&ondf, status);
   fc = astAnnul(fc);

/* End the NDF context. */
   ndfEnd(status);

/* End the AST context. */
   astEnd;

/* Issue a status indication.*/
   if (*status == SAI__OK) {
      msgOutif(MSG__VERB," ",TASK_NAME " succeeded.", status);
   }
   else {
      msgOutif(MSG__VERB," ",TASK_NAME " failed.", status);
   }
}

/* vim: set sw=3 sts=3 : */
