/*
*+
*  Name:
*     smf_open_and_flatfield

*  Purpose:
*     Open files and apply flatfield correction as necessary

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_open_and_flatfield( Grp *igrp, Grp *ogrp, int index,
*                             smfData *data, int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Pointer to an input group
*     ogrp = Grp* (Given)
*        Pointer to an output group
*     index = int (Given)
*        Index into the group
*     data = smfData** (Returned)
*        Pointer to a smfData struct containing flatfielded data.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is a handler routine for opening files specified in the
*     input group, propagating them to output files and determining if
*     the lower-level FLATFIELD task needs to be run.

*  Notes:

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-01-23 (AGG):
*        Initial version, stripped out code from old version of smurf_flatfield.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia. All Rights
*     Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

void smf_open_and_flatfield ( Grp *igrp, Grp *ogrp, int index, smfData *ffdata, int *status) {

  smfData *data;            /* Pointer to input data struct */
  smfFile *file;            /* Pointer to input file struct */
  int indf;                 /* NDF identifier for input file */
  int nout;                 /* Number of data points in output data file */
  double *outdata = NULL;   /* Pointer to output DATA array */
  int outndf;               /* Output NDF identifier */
  char *pname;              /* Pointer to input filename */

  if ( *status != SAI__OK ) return;

  /* Open the input file solely to propagate it to the output file */
  ndgNdfas( igrp, i, "READ", &indf, status );
  ndgNdfpr( indf, " ", ogrp, i, &outndf, status );
  ndfAnnul( &indf, status);

  smf_open_file( igrp, i, "READ", &data, status);
  /* Should check status here to make sure that the file was opened OK */
  if ( *status != SAI__OK) {
    errRep("", "Unable to open input file", status);
  }

  /* Check whether the data are flatfielded */
  smf_check_flat( data, status);

  if (*status == SAI__OK) {
    file = data->file;
    pname = file->name;
    msgSetc("FILE", pname);
    msgOutif(MSG__VERB, " ", "Flatfielding file ^FILE", status);
  }

  /* Set parameters of the DATA array in the output file */
  ndfStype( "_DOUBLE", outndf, "DATA", status);
  ndfMap( outndf, "DATA", "_DOUBLE", "WRITE", &outdata, &nout, status );

  /* Close and reopen output file, populate output struct */
  ndfAnnul( &outndf, status);
  smf_open_file( ogrp, i, "WRITE", &ffdata, status);
  if ( *status == SAI__ERROR) {
    errRep("", "Unable to open output file", status);
  }

  /* Flatfield the data */
  smf_flatfield( data, &ffdata, status );

  /* Free resources for input data */
  smf_close_file( &data, status );
}
