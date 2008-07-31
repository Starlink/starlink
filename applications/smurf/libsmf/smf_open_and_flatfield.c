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
*     smf_open_and_flatfield( const Grp *igrp, const Grp *ogrp, size_t index,
*                             const smfArray* darks,smfData **data,
*                             int *status );

*  Arguments:
*     igrp = const Grp* (Given)
*        Pointer to an input group
*     ogrp = const Grp* (Given)
*        Pointer to an output group
*     index = size_t (Given)
*        Index into the group
*     darks = const smfArray* (Given)
*        Set of darks that could be applied. Can be NULL.
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
*        Use size_t and const.
*        Pass in smfArray of darks.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
#include "par.h"

#include "smf.h"
#include "smurf_par.h"
#include "libsmurf/smurflib.h"
#include "smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smf_open_and_flatfield"

void smf_open_and_flatfield ( const Grp *igrp, const Grp *ogrp, size_t index,
                              const smfArray *darks, smfData **ffdata, int *status) {

  smfData *data = NULL;     /* Pointer to input data struct */
  smfFile *file = NULL;     /* Pointer to input file struct */
  int indf;                 /* NDF identifier for input file */
  int nout;                 /* Number of data points in output data file */
  void *outdata[1] = { NULL };/* Pointer to array of output mapped pointers*/
  int outndf;               /* Output NDF identifier */
  char *pname;              /* Pointer to input filename */
  size_t npts = 0;          /* Number of data points */
  int flags = 0;            /* Flags for creating smfFile and smfHead */
  char prvname[2*PAR__SZNAM+1]; /* provenance ID string */

  if ( *status != SAI__OK ) return;

  /* might be useful later on for provenance */
  smf_get_taskname( NULL, prvname, status );

  if ( ogrp != NULL ) {
    /* Open the input file solely to propagate it to the output file */
    ndgNdfas( igrp, index, "READ", &indf, status );
    /* We want QUALITY too if it's available */
    ndgNdfpr( indf, "WCS,QUALITY,UNITS,TITLE,LABEL,NOEXTENSION(PROVENANCE)", 
              ogrp, index, &outndf, status );
    ndfAnnul( &indf, status);

    /* Set parameters of the DATA array in the output file */
    ndfStype( "_DOUBLE", outndf, "DATA", status);
    /* We need to map this so that the DATA_ARRAY is defined on exit */
    ndfMap( outndf, "DATA", "_DOUBLE", "WRITE", &(outdata[0]), &nout, status );

    /* Close output file */
    ndfAnnul( &outndf, status);
  } 

  /* Open the input without header information. This is required
     because sc2store can not open two files at once 
     22-Mar-2006: no longer true? hdr needed anyway */
  if (*status == SAI__OK) {
    smf_open_file( igrp, index, "READ", 0, &data, status);
    if ( *status != SAI__OK) {
      errRep("", "Unable to open input file(s)", status);
    }
  }

  /* Open the output file for write. If the output grp is NULL then
     ffdata is returned NULL. Use UPDATE mode to retain components such as WCS */
  if (*status == SAI__OK) {
    /* Returns without action if ogrp is not defined */
    smf_open_file( ogrp, index, "UPDATE", 0, ffdata, status);
    if ( *status == SAI__ERROR) {
      errRep("", "Unable to open output flatfielded file(s)", status);
    }

    /* Propagate provenance to the output. We always have to run this
       because automatic provenance propagation becomes really slow
       for raw data with many INCOMPS NDFs. */
    if (ogrp != NULL) {
      /* need an ndfid */
      smf_accumulate_prov( data, igrp, index, (*ffdata)->file->ndfid,
                           prvname, status );
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
      pname = file->name;
      msgSetc("FILE", pname);
      msgOutif(MSG__VERB, " ", "Flatfielding file ^FILE", status);
    }
    /* If ffdata is NULL then populate a struct to work with */
    if ( *ffdata == NULL ) {
      /* Note that we don't need to create a smfFile */
      flags |= SMF__NOCREATE_FILE;
      *ffdata = smf_deepcopy_smfData( data, 1, flags, status );
      /* Change data type to DOUBLE */
      if ( *status == SAI__OK) {
        ((*ffdata)->pntr)[0] = smf_realloc( ((*ffdata)->pntr)[0], npts, 
                                            sizeof(double), status );
        (*ffdata)->dtype = SMF__DOUBLE;
      } else {
        errRep(FUNC_NAME, "Error: unable to allocate memory for new smfData", 
               status);
      }
    }

    if (darks) {
      size_t dark1;
      size_t dark2;
      smfData * dkdata1 = NULL;
      smfData * dkdata2 = NULL;

      /* work out which darks are suitable */
      smf_choose_darks( darks, *ffdata, &dark1, &dark2, status );

      /* and correct for dark */
      if (dark1 != SMF__BADIDX) dkdata1 = darks->sdata[dark1];
      if (dark2 != SMF__BADIDX) dkdata2 = darks->sdata[dark2];
      if (dkdata1 || dkdata2) {
        smf_subtract_dark( data, dkdata1, dkdata2, SMF__DKSUB_PREV, status );
      } else {
        msgSetc( "FILE", file->name );
        msgOutif(MSG__QUIET, " ",
                 "Warning: File ^FILE has no suitable dark frame",
                 status);
      }
    }

    /* Flatfield the data */
    smf_flatfield( data, ffdata, flags, status );

    /* synchronize units and label with the new file */
    smf_write_clabels( *ffdata, status );

  } else if ( *status == SMF__FLATN ) {
    /* Just copy input to output file */
    errAnnul( status );

    /* What if ffdata is NULL? */
    msgOutif(MSG__DEBUG," ", "Data FF: Copying to output file ", status);
    if ( *ffdata == NULL ) {
      /* Don't need the smfFile or smfDA components */
      flags |= SMF__NOCREATE_FILE;
      flags |= SMF__NOCREATE_DA;
      *ffdata = smf_deepcopy_smfData( data, 0, flags, status );
    } else {    msgOutif(MSG__NORM, " ","All supplied input frames were DARK,"
       " nothing to do", status );
      /*      printf("using memcpy\n");*/
      memcpy( ((*ffdata)->pntr)[0], (data->pntr)[0], npts * sizeof (double) );
    }

  }

  /* For convenience, if we have no output group we still
     store the name of the input file in the output smfData
     so that we can tell what is happening in the caller */
  if (!ogrp && *status == SAI__OK) {
    file = data->file;
    if (file) {
      pname = file->name;
      (*ffdata)->file = smf_construct_smfFile( NULL, NDF__NOID, 0, 1,
                                               pname, status);
    }
  }

  /* Free resources for input data */
  smf_close_file( &data, status );
}
