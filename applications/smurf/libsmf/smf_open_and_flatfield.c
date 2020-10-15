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
*     didflat = smf_open_and_flatfield( ThrWorkForce *wf, const Grp *igrp,
*                             const Grp *ogrp, dim_t index, const smfArray* darks,
*                             const smfArray* flatramps, AstKeyMap * heateffmap,
*                             smfData **data, int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     igrp = const Grp* (Given)
*        Pointer to an input group
*     ogrp = const Grp* (Given)
*        Pointer to an output group
*     index = dim_t (Given)
*        Index into the group
*     darks = const smfArray* (Given)
*        Set of darks that could be applied. Can be NULL.
*     flatramps = const smfArray * (Given)
*        Set of flatfield ramps to be assigned to any relevant data files.
*        Can be NULL.
*     heateffmap = AstKeyMap * (Given)
*        Details of heater efficiency data to be applied during flatfielding.
*     data = smfData** (Returned)
*        Pointer to a pointer to smfData struct containing flatfielded data.
*        Will be created by this routine, or NULL on error.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is a handler routine for opening files specified in the
*     input group, propagating them to output files and determining if
*     the lower-level FLATFIELD task needs to be run. For the case
*     where there is no output file, a copy is made of the relevant
*     input data only, and the smfData is created with no smfFile
*     struct.

*  Returned Value:
*     int = boolean
*       True if data were flatfielded, false if the data were simply copied.

*  Notes:

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-01-23 (AGG):
*        Initial version, stripped out code from old version of
*        smurf_flatfield.
*     2006-01-24 (TIMJ):
*        Fix i vs index and calling arguments
*     2006-01-25 (AGG):
*        Copies input data to output file when passed flatfielded data
*     2006-03-03 (AGG):
*        Trap the case that ogrp is NULL
*     2006-03-23 (AGG):
*        Creates copy of smfData with smf_deepcopy_smfData
*     2006-03-24 (AGG):
*        Set data type correctly for case of NULL ffdata
*     2006-03-29 (AGG):
*        Check ffdata on return from astRealloc
*     2006-04-21 (AGG):
*        - update subroutine calls due to changed APIs
*        - set SMF_NOCREATE_FILE flag for NULL ffdata
*     2006-05-02 (AGG):
*        Set SMF_NOCREATE_FILE & SMF__NOCREATE_DA flags for
*        NULL ffdata when input data are flatfielded
*     2006-12-20 (TIMJ):
*        Open related files in UPDATE mode to prevent overwrite of propogated
*        components
*     2007-10-29 (EC):
*        Modified interface to smf_open_file.
*     2008-03-11 (AGG):
*        Propagate QUALITY from input file
*     2008-05-07 (EC):
*        Use smf_realloc instead of astRealloc
*     2008-05-23 (TIMJ):
*        Enable provenance propagation.
*     2008-05-28 (TIMJ):
*        Initialise pointers.
*     2008-05-29 (TIMJ):
*        Use smf_accumulate_prov. Do not propagate provenance extension.
*     2008-06-06 (TIMJ):
*        Store the input filename in the smfFile even if there is no
*        output group. This simplifies filename retrieval enormously
*     2008-07-18 (TIMJ):
*        Use dim_t and const.
*        Pass in smfArray of darks.
*     2008-11-24 (TIMJ):
*        Pass in smfArray of bad pixel masks.
*     2008-11-26 (TIMJ):
*        - Factor out dark subtraction code into smf_apply_dark
*        - Call smf_apply_mask
*     2008-12-11 (TIMJ):
*        Decide that masking should not be done by this routine since it does
*        not have to be done prior to flatfielding and does not save much
*        code in the caller (who just calls smf_apply_mask).
*     2008-12-12 (TIMJ):
*        Return boolean indicating whether the data were flatfielded or not.
*     2009-10-06 (TIMJ):
*        Do not need to malloc _DOUBLE for raw if smf_deepcopy_smfData
*        is being called with rawconvert true.
*     2010-03-15 (TIMJ):
*        Assign flatfield overrides.
*     2010-08-19 (DSB):
*        Stop default NDF history being written when the output NDF is closed.
*     2014-01-10 (DSB):
*        Added argument wf.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2010 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2006-2008 University of British Columbia.
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
#include "star/thr.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "par.h"

#include "smf.h"
#include "smurf_par.h"
#include "smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smf_open_and_flatfield"

int smf_open_and_flatfield ( ThrWorkForce *wf, const Grp *igrp, const Grp *ogrp, dim_t index,
                             const smfArray *darks, const smfArray * flatramps,
                             AstKeyMap * heateffmap, smfData **ffdata, int *status) {

  smfData *data = NULL;     /* Pointer to input data struct */
  smfFile *file = NULL;     /* Pointer to input file struct */
  int indf;                 /* NDF identifier for input file */
  size_t nout;              /* Number of data points in output data file */
  void *outdata[1] = { NULL };/* Pointer to array of output mapped pointers*/
  int outndf;               /* Output NDF identifier */
  dim_t npts = 0;           /* Number of data points */
  int flags = 0;            /* Flags for creating smfFile and smfHead */
  char prvname[2*PAR__SZNAM+1]; /* provenance ID string */
  int retval = 0;           /* True if flatfielded */

  if ( *status != SAI__OK ) return retval;

  /* might be useful later on for provenance */
  smf_get_taskname( NULL, prvname, status );

  if ( ogrp != NULL ) {
    /* Open the input file solely to propagate it to the output file */
    ndgNdfas( igrp, (int) index, "READ", &indf, status );
    /* We want QUALITY too if it's available */
    ndgNdfpr( indf, "WCS,QUALITY,UNITS,TITLE,LABEL,NOEXTENSION(PROVENANCE)",
              ogrp, (int) index, &outndf, status );
    ndfAnnul( &indf, status);

    /* Set parameters of the DATA array in the output file */
    ndfStype( "_DOUBLE", outndf, "DATA", status);
    /* We need to map this so that the DATA_ARRAY is defined on exit */
    ndfMap( outndf, "DATA", "_DOUBLE", "WRITE", &(outdata[0]), &nout, status );

    /* Close output file, suppressing the writing of default history
       information since it will be written again when the application
       finally closes the completed output NDF. */
    ndfHsmod( "SKIP", outndf, status );
    ndfAnnul( &outndf, status);
  }

  /* Open the input without header information. This is required
     because sc2store can not open two files at once
     22-Mar-2006: no longer true? hdr needed anyway */
  if (*status == SAI__OK) {
    smf_open_file( wf, igrp, index, "READ", 0, &data, status);
    if ( *status != SAI__OK) {
      errRep("", FUNC_NAME ": Unable to open input file(s)", status);
    }
  }

  /* Open the output file for write. If the output grp is NULL then
     ffdata is returned NULL. Use UPDATE mode to retain components
     such as WCS */

  if (*status == SAI__OK) {
    /* Returns without action if ogrp is not defined */
    smf_open_file( wf, ogrp, index, "UPDATE", 0, ffdata, status);
    if ( *status == SAI__ERROR) {
      errRep("", FUNC_NAME ": Unable to open output flatfielded file(s)",
             status);
    }

    /* Propagate provenance to the output. We always have to run this
       because automatic provenance propagation becomes really slow
       for raw data with many INCOMPS NDFs. */
    if (ogrp != NULL) {
      /* need an ndfid */
      smf_accumulate_prov( data, igrp, index, (*ffdata)->file->ndfid,
                           prvname, NULL, status );
    }

  }

  /* how many elements in the data array? */
  if (*status == SAI__OK) {
    npts = (data->dims)[0] * (data->dims)[1];
    if ( data->ndims == 3) {
      npts *= (data->dims)[2];
    }
  }

  /* Check whether the input data are flatfielded */
  smf_check_flat( data, status);

  if (*status == SAI__OK) {
    file = data->file;
    if ( file != NULL ) {
      smf_smfFile_msg( file, "FILE", 0, "<unknown>" );
      msgOutif(MSG__VERB, " ", "Flatfielding file '^FILE'", status);
    }
    /* If ffdata is NULL then populate a struct to work with */
    if ( *ffdata == NULL ) {
      /* Note that we don't need to create a smfFile but we ask
       the new smfData to be a _DOUBLE using the rawconvert flag */
      flags |= SMF__NOCREATE_FILE;
      *ffdata = smf_deepcopy_smfData( wf, data, 1, flags, 0, 0, status );
      if (*status != SAI__OK) {
        errRep( "", FUNC_NAME
                ": Error, unable to allocate memory for new smfData",
               status);
      }
    }

    /* Handle darks - note that "data" read from a raw file
     is actually a malloced data array and not a mmapped array. This means
     that it is not read-only despite the call to smf_open_file above. */
    smf_apply_dark( data, darks, status );

    /* Flatfield the data */
    flags |= SMF__NOCREATE_FTS;
    smf_flatfield( wf, data, flatramps, heateffmap, ffdata, flags, status );

    if (*status == SAI__OK) retval = 1;

    /* synchronize units and label with the new file */
    smf_write_clabels( *ffdata, status );

  } else if ( *status == SMF__FLATN ) {
    /* Just copy input to output file */
    errAnnul( status );

    /* What if ffdata is NULL? */
    msgOutif(MSG__DEBUG," ",
             "Data already flatfielded. Copying to output file ", status);
    if ( *ffdata == NULL ) {
      /* Don't need the smfFile */
      flags |= SMF__NOCREATE_FILE;
      *ffdata = smf_deepcopy_smfData( wf, data, 0, flags, 0, 0, status );
    } else {
      memcpy( ((*ffdata)->pntr)[0], (data->pntr)[0], npts * sizeof (double) );
    }

  }

  /* For convenience, if we have no output group we still
     store the name of the input file in the output smfData
     so that we can tell what is happening in the caller */
  if (!ogrp && *status == SAI__OK) {
    file = data->file;
    if (file) {
      (*ffdata)->file = smf_construct_smfFile( NULL, NDF__NOID, 0, 1,
                                               file->name, status);
    }
  }

  /* Free resources for input data */
  smf_close_file( wf, &data, status );

  return retval;
}
