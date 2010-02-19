/*
*+
*  Name:
*     smf_find_science

*  Purpose:
*     Extract science data from an input group

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_find_science(const Grp * ingrp, Grp **outgrp, Grp **darkgrp,
*                     int reduce, smf_dtype dtype, smfArray ** darks,
*                     int * status );

*  Arguments:
*     ingrp = const Grp* (Given)
*        Input group consisting of science and non-science observations.
*     outgrp = Grp ** (Returned)
*        Output group consisting of all the science observations. Can be
*        NULL.
*     darkgrp = Grp ** (Returned)
*        If non-null, will contain the group of dark files. Will not be
*        created if no darks were found.
*     reduce = int (Given)
*        Logical, if true the darks are reduced (if needed) and converted
*        to 2d images from the time series. If false, the darks are not
*        touched. Only accessed if "darks" is true.
*     dtype = smf_dtype (Given)
*        Data type to use for reduced dark. Only accessed if "reduce" is
*        true. SMF__NULL will indicate that the data type should be the
*        same as the input type.
*     darks = smfArray ** (Returned)
*        Pointer to smfArray* to be created and filled with darks. Must
*        be freed using smf_close_related. Can be NULL pointer if the darks
*        do not need to be used. *darks will be NULL if no darks were found.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine opens each of the files in the input group to read
*     the header and determine whether it is science data or non-science
*     (a dark or a fast flat) data. If it is a dark the data are read
*     and stored in the output smfArray. If it is science data
*     the group entry is copied to the output group and the
*     file is closed.

*  Notes:
*     - use smf_close_related to free the smfArray.
*     - if "outgrp" is NULL then only dark information will be returned.
*     - if both "darkgrp" and "darks" are NULL on entry, this routine
*       will simply filter out the dark frames.
*     - it is an error for "darkgrp", "outgrp" and "darks" to all be NULL.
*     - The darks will be returned in time order.
*     - Observations which have a SEQ_TYPE header and whose value differs
*       OBS_TYPE will be filtered out and not copied to outgrp.
*     - Observations with inconsistent SEQSTART and RTS_NUM entries will be
*       filtered out. These are commonly caused by the DA system using a
*       header from the previous file.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-07-17 (TIMJ):
*        Initial version
*     2008-07-29 (TIMJ):
*        Did not realise that ndgCpsup should be used to copy entries.
*     2008-07-31 (TIMJ):
*        Report observation details.
*     2008-12-09 (TIMJ):
*        Do not react badly if a fits header is missing.
*     2009-04-24 (TIMJ):
*        Factor out observation summary reporting.
*     2009-05-25 (TIMJ):
*        Use new smf_obsmap_report API.
*     2009-09-02 (TIMJ):
*        Use smf_find_dateobs rather than directly accessing jcmtstate
*        to be a little more robust with junk files.
*     2009-09-25 (TIMJ):
*        Move sort routine externally.
*     2009-11-24 (TIMJ):
*        Quick hack to filter out SEQ_TYPE ne OBS_TYPE observations. Proper
*        solution is for an additional smfArray to be returned with these
*        items.
*     2010-01-14 (TIMJ):
*        Ignore files that have corrupt FITS header by seeing if the first RTS_NUM
*        matches the SEQSTART value.
*     2010-02-18 (TIMJ):
*        Rename to smf_find_science from smf_find_darks.

*  Copyright:
*     Copyright (C) 2008-2010 Science and Technology Facilities Council.
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

/* System includes */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "msg_par.h"
#include "star/one.h"
#include "ast.h"
#include "star/one.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_find_science"

void smf_find_science( const Grp * ingrp, Grp **outgrp, Grp **darkgrp,
                     int reduce, smf_dtype dtype, smfArray ** darks,
                     int * status ) {

  smfSortInfo *alldarks; /* array of sort structs */
  Grp * dgrp = NULL;  /* Internal dark group */
  size_t dkcount = 0; /* Dark counter */
  size_t i;           /* loop counter */
  smfData *infile = NULL; /* input file */
  size_t insize;     /* number of input files */
  AstKeyMap * obsmap = NULL; /* Info from all observations */
  AstKeyMap * objmap = NULL; /* All the object names used */
  Grp *ogrp = NULL;   /* local copy of output group */
  smfSortInfo *sortinfo; /* individual struct in array */

  if (outgrp) *outgrp = NULL;
  if (darkgrp) *darkgrp = NULL;
  if (darks) *darks = NULL;

  if (*status != SAI__OK) return;

  /* Sanity check to make sure we return some information */
  if ( outgrp == NULL && darkgrp == NULL && darks == NULL) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME ": Must have some non-NULL arguments"
            " (possible programming error)", status);
    return;
  }

  /* Create new group for output files */
  ogrp = grpNew( "NonDark", status );

  /* and a new group for darks */
  dgrp = grpNew( "DarkFiles", status );

  /* and also create a keymap for the observation description */
  obsmap = astKeyMap( " " );

  /* and an object map */
  objmap = astKeyMap( " " );

  /* Work out how many input files we have and allocate sufficient sorting
     space */
  insize = grpGrpsz( ingrp, status );
  alldarks = smf_malloc( insize, sizeof(*alldarks), 1, status );

  /* check each file in turn */
  for (i = 1; i <= insize; i++) {

    /* open the file but just to get the header */
    smf_open_file( ingrp, i, "READ", SMF__NOCREATE_DATA, &infile, status );
    if (*status != SAI__OK) break;

    /* Fill in the keymap with observation details */
    smf_obsmap_fill( infile, obsmap, objmap, status );

    if (smf_isdark( infile, status )) {
      /* Store the entry in the output group */
      ndgCpsup( ingrp, i, dgrp, status );
      if (*status == SAI__OK) { /* protect against smf_isdark problem */
        /* store the name and time in the struct */
        sortinfo = &(alldarks[dkcount]);
        one_strlcpy( sortinfo->name, infile->file->name, sizeof(sortinfo->name),
                     status );
        smf_find_dateobs( infile->hdr, &(sortinfo->mjd), NULL, status );
        msgSetc("F", infile->file->name);
        msgOutif(MSG__DEBUG, " ", "Dark file: ^F",status);
        dkcount++;
      }
    } else {
      /* compare sequence type with observation type and drop it (for now)
         if they differ */
      if ( infile->hdr->obstype == infile->hdr->seqtype ) {
        /* Sanity check the header for corruption. Compare RTS_NUM with SEQSTART */
        int seqstart = 0;
        JCMTState *tmpState = NULL;
        smf_getfitsi( infile->hdr, "SEQSTART", &seqstart, status );
        tmpState = infile->hdr->allState;
        msgSetc("F", infile->file->name);
        if ( (tmpState[0]).rts_num == seqstart ) {
          /* store the file in the output group */
          ndgCpsup( ingrp, i, ogrp, status );
          msgOutif(MSG__DEBUG, " ", "Non-dark file: ^F",status);
        } else {
          msgOutif( MSG__QUIET, "", "File ^F has a corrupt FITS header. Ignoring it.", status );
        }
      } else {
        msgSetc("F", infile->file->name);
        msgOutif(MSG__DEBUG, " ", "Sequence type mismatch with observation type: ^F",status);
      }
    }

    /* close the file */
    smf_close_file( &infile, status );
  }

  /* Store output group in return variable or else free it */
  if (outgrp) {
    *outgrp = ogrp;
  } else {
    grpDelet( &ogrp, status );
  }

  /* no need to do any more if neither darks nor darkgrp are defined */
  if (dkcount > 0 && (darks || darkgrp) ) {

    /* sort darks into order */
    qsort( alldarks, dkcount, sizeof(*alldarks), smf_sort_bytime);

    /* now open the darks and store them if requested */
    if (darks) {
      smfArray * array = smf_create_smfArray( status );
      if (*status == SAI__OK) {
        for (i = 1; i <= dkcount; i++ ) {
          smf_open_file( dgrp, i, "READ", 0, &infile, status );

          /* do we have to process these darks? */
          if (reduce) {
            smfData *outfile = NULL;
            smf_reduce_dark( infile, dtype, &outfile, status );
            if (outfile) {
              smf_close_file( &infile, status );
              infile = outfile;
            }
          }

          smf_addto_smfArray( array, infile, status );
        }
      }
      *darks = array;
    }
  }

  /* free memory */
  alldarks = smf_free( alldarks, status );

  /* Store the output dark group in the return variable or free it */
  if (darkgrp) {
    *darkgrp = dgrp;
  } else {
    grpDelet( &dgrp, status);
  }

  msgSeti( "ND", (insize - dkcount) );
  msgSeti( "DK", dkcount );
  msgSeti( "TOT", insize );
  if ( insize == 1 ) {
    if (dkcount == 1) {
      msgSetc( "TXT", " " );
    } else {
      msgSetc( "TXT", "not" );
    }
    msgOutif( MSG__VERB, " ", "Single input file was ^TXT a dark",
              status);
  } else {
    if (dkcount == 1) {
      msgSetc( "DKTXT", "was a dark");
    } else {
      msgSetc( "DKTXT", "were darks");
    }
    if ((insize-dkcount) == 1) {
      msgSetc( "NDTXT", "was not a dark");
    } else {
      msgSetc( "NDTXT", "were not darks");
    }

    /* This might be a useful message */
    msgOutif( MSG__NORM, " ", "Out of ^TOT input files, ^DK ^DKTXT and "
              "^ND ^NDTXT", status );
  }

  /* Now report the details of the observation */
  smf_obsmap_report( MSG__NORM, obsmap, objmap, status );

  obsmap = astAnnul( obsmap );
  objmap = astAnnul( objmap );

  return;
}
