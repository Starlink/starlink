/*
*+
*  Name:
*     FLATFIELD

*  Purpose:
*     Top-level FLATFIELD implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_flatfield( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine implementing the FLATFIELD task. Each
*     input file is propagated to a corresponding output file which is
*     identical but contains flatfielded data. If the input data are
*     already flatfielded, the user will be informed and no action
*     will be performed on the data.

*  Notes:
*     At the moment an output file is propagated regardless of whether
*     the input data are already flatfield or not. This is a waste of
*     disk space :)

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be uncompressed and flatfielded
*     FLAT = NDF (Read)
*          Optional file containing flatfield solution
*     OUT = NDF (Write)
*          Output file

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-11-04 (AGG):
*        Initial test version. Copy from smurf_extinction
*     2005-11-05 (AGG):
*        Factor out I/O code to smf_open_file
*     2005-11-07 (TIMJ):
*        Replace fits example code with call to smf_fits_getI
*     2005-11-28 (TIMJ):
*        Use smf_close_file
*     2005-12-12 (AGG):
*        Use smf_flatfield, remove need to include sc2da info, fix
*        loop counter bug
*     2005-12-14 (AGG/TIMJ):
*        Use smf_open_file on the output data. Note that
*        smf_close_file has been temporarily commented out until it is
*        also updated to take account of reference counting.
*     2006-01-24 (AGG):
*        Updated to use new smf_open_and_flatfield routine.
*     2006-01-27 (TIMJ):
*        Can now smf_close_file
*     2008-07-18 (TIMJ):
*        Use kaplibs routine to get in and out files so that the counts match properly.
*        Locate darks in input list.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2006 University of British Columbia.
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

#define FUNC_NAME "smurf_flatfield"
#define TASK_NAME "FLATFIELD"

void smurf_flatfield( int *status ) {

  smfArray *darks = NULL;   /* Dark data */
  smfData *ffdata = NULL;   /* Pointer to output data struct */
  Grp *fgrp = NULL;         /* Filtered group, no darks */
  size_t i = 0;             /* Counter, index */
  Grp *igrp = NULL;         /* Input group of files */
  Grp *ogrp = NULL;         /* Output group of files */
  size_t outsize;           /* Total number of NDF names in the output group */
  size_t size;              /* Number of files in input group */

  /* Main routine */
  ndfBegin();

  /* Get input file(s) */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Filter out darks */
  smf_find_darks( igrp, &fgrp, NULL, 1, &darks, status );

  /* input group is now the filtered group so we can use that and
     free the old input group */
  size = grpGrpsz( fgrp, status );
  grpDelet( &igrp, status);
  igrp = fgrp;
  fgrp = NULL;

  if (size > 0) {
    /* Get output file(s) */
    kpg1Wgndf( "OUT", igrp, size, size, "More output files required...",
               &ogrp, &outsize, status );
  } else {
    msgOutif(MSG__NORM, " ","All supplied input frames were DARK,"
       " nothing to flatfield", status );
  }

  for (i=1; i<=size; i++ ) {

    if (*status != SAI__OK) break;

    /* Call flatfield routine */
    smf_open_and_flatfield(igrp, ogrp, i, NULL, &ffdata, status);

    /* Check status to see if data are already flatfielded */
    if (*status == SMF__FLATN) {
      errAnnul( status );
      msgOutif(MSG__VERB," ",
	     "Data are already flatfielded", status);
    } else if ( *status == SAI__OK) {
      msgOutif(MSG__VERB," ", "Flat field applied", status);
    } else {
      /* Tell the user which file it was... */
      /* Would be user-friendly to trap 1st etc... */
      /*errFlush( status );*/
      msgSeti("I",i);
      msgSeti("N",size);
      errRep(FUNC_NAME,	"Unable to flatfield data from file ^I of ^N", status);
    }
    /* Free resources for output data */
    smf_close_file( &ffdata, status );
  }

  /* Tidy up after ourselves: release the resources used by the grp routines  */
  if (igrp) grpDelet( &igrp, status);
  if (ogrp) grpDelet( &ogrp, status);
  smf_close_related( &darks, status );
  ndfEnd( status );
}

