/*
*+
*  Name:
*     smf_find_darks

*  Purpose:
*     Extract darks from an input group

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_find_darks(const Grp * ingrp, Grp **outgrp, Grp **darkgrp,
*                     int reduce, smf_dtype dtype, smfArray ** darks,
*                     int * status );

*  Arguments:
*     ingrp = const Grp* (Given)
*        Input group consisting of dark and non-dark observations.
*     outgrp = Grp ** (Returned)
*        Output group consisting of all the non-dark observations. Can be
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
*     the header and determine whether it is associated with a dark. If it
*     is a dark the data are read and stored in the output smfArray. If it
*     is not a dark the group entry is copied to the output group and the
*     file is closed.

*  Notes:
*     - use smf_close_related to free the smfArray.
*     - if "outgrp" is NULL then only dark information will be returned.
*     - if both "darkgrp" and "darks" are NULL on entry, this routine
*       will simply filter out the dark frames.
*     - it is an error for "darkgrp", "outgrp" and "darks" to all be NULL.
*     - The darks will be returned in time order.

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

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* Define a simple struct to contain the filename and the
   timestamp for sorting. We will store these in an array
   and pass them to qsort so that we can ensure that the darks
   are in time order.
*/

typedef struct {
  char name[GRP__SZNAM+1];
  double tai;
} smfSortInfo;

/* local qsort sort routine */
static int sortbytime( const void *in1, const void *in2);

#define FUNC_NAME "smf_find_darks"

void smf_find_darks( const Grp * ingrp, Grp **outgrp, Grp **darkgrp,
                     int reduce, smf_dtype dtype, smfArray ** darks,
                     int * status ) {

  smfSortInfo *alldarks; /* array of sort structs */
  Grp * dgrp = NULL;  /* Internal dark group */
  size_t dkcount = 0; /* Dark counter */
  size_t i;           /* loop counter */
  size_t nobs;        /* number of distinct observations */
  size_t nobj;        /* number of distinct objects */
  smfData *infile = NULL; /* input file */
  size_t insize;     /* number of input files */
  char obsid[SZFITSCARD]; /* Observation ID */
  AstKeyMap * obsinfo = NULL; /* observation information */
  AstKeyMap * obsmap = NULL; /* Info from all observations */
  char object[SZFITSCARD]; /* Object name */
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

    /* Get observation ID and see if we already have an entry in the map */
    (void)smf_getobsidss( infile->hdr->fitshdr, obsid, sizeof(obsid), NULL, 0, status );
    /* As of 20080718 OBSID is not unique per obs so chop off date*/
    obsid[22] = '\0';

    if (!astMapHasKey(obsmap, obsid )) {
      int itemp;

      /* Create a sub keymap and populate it */
      obsinfo = astKeyMap( " " );
      smf_fits_getS( infile->hdr, "OBJECT", object, sizeof(object), status );
      astMapPut0C( obsinfo, "OBJECT", object, NULL );
      astMapPut0I( obsinfo, "OBSMODE", infile->hdr->obsmode, NULL );
      astMapPut0I( obsinfo, "OBSTYPE", infile->hdr->obstype, NULL );
      smf_fits_getI( infile->hdr, "OBSNUM", &itemp, status );
      astMapPut0I( obsinfo, "OBSNUM", itemp, NULL );
      smf_fits_getI( infile->hdr, "UTDATE", &itemp, status );
      astMapPut0I( obsinfo, "UTDATE", itemp, NULL );

      /* store information in the global observatio map
	 and also track how many distinct objects we have */
      astMapPut0A( obsmap, obsid, obsinfo, NULL );
      astMapPut0I( objmap, object, 0, NULL );
    }

    if (smf_isdark( infile, status ) ) {
      /* Store the entry in the output group */
      ndgCpsup( ingrp, i, dgrp, status );
      /* store the name and time in the struct */
      sortinfo = &(alldarks[dkcount]);
      one_strlcpy( sortinfo->name, infile->file->name, sizeof(sortinfo->name),
                   status );
      sortinfo->tai = (infile->hdr->allState)[0].tcs_tai;
      msgSetc("F", infile->file->name);
      msgOutif(MSG__DEBUG, " ", "Dark file: ^F",status);
      dkcount++;
    } else {
      /* store the file in the output group */
      ndgCpsup( ingrp, i, ogrp, status );
      msgSetc("F", infile->file->name);
      msgOutif(MSG__DEBUG, " ", "Non-dark file: ^F",status);
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
    qsort( alldarks, dkcount, sizeof(*alldarks), sortbytime);

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

  /* Now report the details of the observations */
  nobj = astMapSize( objmap );
  nobs = astMapSize( obsmap );
  if (nobs > 0) {
    if (nobs == 1) {
      msgSetc("S", "");
    } else {
      msgSetc("S", "s");
    }
    /* if we only have on object name report it now */
    if (nobj == 1) {
      msgSetc( "OBJ", "for object '");
      msgSetc( "OBJ", astMapKey( objmap, 0 ) );
      msgSetc( "OBJ", "'");
    } else {
      msgSetc( "OBJ", " ");
    }
    msgOutif(MSG__NORM, " ", "Processing data ^OBJ from the following observation^S :", status);
    for (i = 0; i < nobs; i++) {
      if (astMapGet0A( obsmap, astMapKey(obsmap, i ), &obsinfo )) {
        const char * ctemp;
        int itemp;

        /* only display object if we have not already done so */
        if (nobj > 1) {
          astMapGet0C( obsinfo, "OBJECT", &ctemp );
          msgSetc( "OBJ", ctemp);
        } else {
          msgSetc( "OBJ", " ");
        }

        /* do not display "SCIENCE" as it is the default */
        astMapGet0I( obsinfo, "OBSTYPE", &itemp );
        if (itemp != SMF__TYP_SCIENCE) {
          msgSetc( "OT", "(");
          msgSetc( "OT", smf_obstype_str( itemp, status) );
          msgSetc( "OT", ")");
        } else {
          msgSetc( "OT", " ");
        }
        astMapGet0I( obsinfo, "OBSMODE", &itemp );
        msgSetc( "OM", smf_obsmode_str( itemp, status) );
        astMapGet0I( obsinfo, "OBSNUM", &itemp );
        msgSeti( "ON", itemp);
        astMapGet0I( obsinfo, "UTDATE", &itemp );
        msgSeti( "UT", itemp);
        msgOutif(MSG__NORM, "", "  ^UT #^ON ^OM ^OBJ ^OT", status);
      }
    }
    msgBlank( status );
  }


  obsmap = astAnnul( obsmap );
  objmap = astAnnul( objmap );

  return;
}

/* This routine can be used to sort the darks */
static int sortbytime ( const void *in1, const void *in2 ) {
  const smfSortInfo * sort1;
  const smfSortInfo * sort2;
  double tai1;
  double tai2;

  sort1 = in1;
  sort2 = in2;

  tai1 = sort1->tai;
  tai2 = sort2->tai;

  if (tai1 < tai2) {
    return -1;
  } else if (tai1 > tai2) {
    return 1;
  } else {
    /* least likely case last */
    return 0;
  }
}
