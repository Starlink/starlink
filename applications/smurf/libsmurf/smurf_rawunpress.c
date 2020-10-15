/*
*+
*  Name:
*     RAWUNPRESS

*  Purpose:
*     Uncompress raw data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_rawunpress( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Uncompress the raw time series data from 16-bit to 32-bit integers.
*     Does not flatfield and so the data are still in integer format. If the
*     data are not compressed they will be copied without change.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be uncompressed.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Output file.

*  Related Applications:
*     SMURF: FLATFIELD

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2007-10-19 (TIMJ):
*        Clone from smurf_flatfield.c
*     2007-11-28 (TIMJ):
*        Copy the right number of elements!
*     2008-04-30 (TIMJ):
*        Write out NDF title.
*     2008-05-23 (TIMJ):
*        Add provenance propagation.
*     2008-07-22 (TIMJ):
*        Use kaplibs for param IN/OUT
*     2009-10-20 (TIMJ):
*        Write units and label from smfData to output file.
*     2010-09-21 (COBA):
*        Add SMF__NOCREATE_FTS
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2006-2007 University of British Columbia.
*     Copyright (C) 2007-2008 Science and Technology Facilities Council.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smurf_rawunpress"
#define TASK_NAME "RAWUNPRESS"

void smurf_rawunpress( int *status ) {

  smfData *data = NULL;     /* Pointer to input data struct */
  size_t i = 0;             /* Counter, index */
  int indf;                 /* NDF identifier for input file */
  size_t nout;              /* Number of data points in output data file */
  Grp *igrp = NULL;         /* Input group of files */
  Grp *ogrp = NULL;         /* Output group of files */
  void *outdata[1];         /* Pointer to array of output mapped pointers*/
  int outndf;               /* Output NDF identifier */
  size_t outsize;           /* Total number of NDF names in the output group */
  size_t size;              /* Number of files in input group */

  /* Main routine */
  ndfBegin();

  /* Get input file(s) */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Get output file(s) */
  kpg1Wgndf( "OUT", igrp, size, size, "More output files required...",
             &ogrp, &outsize, status );

  for (i=1; i<=size; i++ ) {

    /* Open output file as type _INTEGER */
    ndgNdfas( igrp, i, "READ", &indf, status );
    ndgNdfpr( indf, "WCS", ogrp, i, &outndf, status );

    /* Update provenance in the output before we close the input
       because sc2store will not give us access to an NDF identifier
       from the raw time series */
    smf_updateprov( outndf, NULL, indf, "SMURF:RAWUNPRESS", NULL, status );
    ndfAnnul( &indf, status);

    /* Set parameters of the DATA array in the output file */
    ndfStype( "_INTEGER", outndf, "DATA", status);
    /* We need to map this so that the DATA_ARRAY is defined on exit */
    ndfMap( outndf, "DATA", "_INTEGER", "WRITE", &(outdata[0]), &nout, status );

    /* Open the input file using standard routine */
    smf_open_file( NULL, igrp, i, "READ", SMF__NOCREATE_DA | SMF__NOCREATE_FTS, &data, status );

    /* Quick sanity check to make sure we are raw data */
    if (*status == SAI__OK && data->dtype != SMF__INTEGER ) {
      *status = SAI__ERROR;
      errRep(" ", "Can only uncompress raw data file(s)", status);
    }

    /* Copy all the data from uncmpressed smfData to output. All are integer types. */
    if (*status == SAI__OK) {
      nout *= sizeof(int); /* number of bytes to copy */
      memcpy( (outdata)[0], (data->pntr)[0], nout );
    }

    /* Set labels (sc2store may have changed them on read) */
    ndfCput( "Uncompressed, not flatfielded", outndf, "TITLE", status);
    if (*status == SAI__OK) {
      ndfCput( data->hdr->units, outndf, "UNITS", status );
      ndfCput( data->hdr->dlabel, outndf, "LABEL", status );
    }


    /* Free resources for files */
    ndfAnnul( &outndf, status);
    smf_close_file( NULL, &data, status );
  }

  /* Tidy up after ourselves: release the resources used by the grp routines  */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}
