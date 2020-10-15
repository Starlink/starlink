/*
*+
*  Name:
*     RAWPRESS

*  Purpose:
*     Compress raw data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_rawpress( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Compress the raw time series data. Currently two compression schemes
*     are available - see parameter "METHOD". This task is intended to be
*     a test bed of compression algorithms.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be compressed.
*     METHOD = _CHAR (Read)
*          The compression scheme to use:
*
*          "OLD" - converts 32-bit integers to 16-bit integers
*          by calculating a common mode signal at each time slice and a
*          multiplicative value (BZERO and BSCALE) after removing the first
*          measurement (STACKZERO).
*
*          "DELTA" - stores the differences between adjacent bolometer samples
*          as 16-bit integers. Any values for which the differences are too
*          big to be stored in 16 bits are stored explicitly in 32 bit
*          integers (see SUN/11 for full details).
*
*          [DELTA]
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Output file(s).

*  Related Applications:
*     SMURF: RAWUNPRESS

*  Notes:
*     - Data will be uncompressed automatically by any SMURF routine.
*     - Files may well be larger when compressed.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     COBA: Coskun Oba (UoL)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-09-23 (TIMJ):
*        Original version.
*     2010-10-11 (DSB):
*        Added delta compression option.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2010 Science and Technology Facilities Council.
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
#include "par.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"

static void smf__copy_comp( const HDSLoc * inxloc, HDSLoc * outxloc,
                            const char * comp, int *status );

#define FUNC_NAME "smurf_rawpress"
#define TASK_NAME "RAWPRESS"
#define LEN_METHOD 6

void smurf_rawpress( int *status ) {

  size_t i = 0;             /* Counter, index */
  Grp *igrp = NULL;         /* Input group of files */
  char method[LEN_METHOD];  /* String for compression method */
  Grp *ogrp = NULL;         /* Output group of files */
  size_t outsize;           /* Total number of NDF names in the output group */
  size_t size;              /* Number of files in input group */
  sc2store_cmptype cmptype; /* Compression method */

  /* Main routine */
  ndfBegin();
  sc2store_force_initialised( status );

  /* Get input file(s) */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Get output file(s) */
  kpg1Wgndf( "OUT", igrp, size, size, "More output files required...",
             &ogrp, &outsize, status );

  /* Get compression method. */
  parChoic( "METHOD", "DELTA", "OLD, DELTA", 1,  method, LEN_METHOD,
            status);
  if( !strcmp( method, "OLD" ) ) {
     cmptype = SC2STORE__BDK;
  } else {
     cmptype = SC2STORE__DELTA;
  }

  for (i=1; i<=size; i++ ) {
    dim_t colsize;
    smfData *data = NULL;     /* Pointer to input data struct */
    char filename[GRP__SZNAM+1]; /* Input filename, derived from GRP */
    char fitsrec[SC2STORE__MAXFITS*SZFITSCARD+1];
    int jig_vert[1][2];       /* dummy jiggle vertices */
    double jig_path[1][2];    /* dummy jiggle path */
    int * idksquid = NULL;
    int indf;                 /* Input NDF identifier */
    int isthere = 0;
    dim_t ncards = 0;
    dim_t nframes;
    int outndf;               /* Output NDF identifier */
    char *pname;              /* Pointer to input filename */
    dim_t rowsize;
    sc2ast_subarray_t subnum;

    /* This is an experimental routine so we just use wrtstream
       to create the output file */

    /* Open the input file */
    smf_open_file( NULL, igrp, i, "READ", 0, &data, status );
    if (*status != SAI__OK || !data) break;

    /* Get the subarray id */
    smf_find_subarray( data->hdr, NULL, 0, &subnum, status );

    /* dimensions */
    smf_get_dims( data, &colsize, &rowsize, NULL, &nframes, NULL, NULL,
                  NULL, status);

    /* Convert the dark squids back to integers */
    idksquid = astMalloc( (nframes*rowsize)*sizeof(*idksquid) );
    if (idksquid) {
      dim_t j;
      double * dkpntr = (data->da->dksquid->pntr)[0];
      for (j=0; j<(nframes*rowsize); j++) {
        idksquid[j] = dkpntr[j];
      }
    }

    /* FITS header */
    smf_fits_export2DA( data->hdr->fitshdr, &ncards, fitsrec, status );

    /* Get the output file name from the group */
    pname = filename;
    grpGet( ogrp, i, 1, &pname, SMF_PATH_MAX, status );

    /* Use sc2store to write out compressed information to the temp file*/
    sc2store_setcompflag( cmptype, status );
    sc2store_wrtstream( filename,       /* Filename */
                        subnum,         /* Sub array number */
                        ncards,         /* Number of fits records */
                        fitsrec,        /* Fits char * array */
                        colsize, rowsize, nframes,
                        data->da->nflat,/* nflat */
                        data->da->refres, /* refres */
                        0,              /* ntrack */
                        smf_flat_methstring( data->da->flatmeth, status ), /* flatname */
                        data->hdr->allState, /* JCMTSTATE */
                        NULL,           /* telpar */
                        data->pntr[0],  /* data */
                        idksquid,       /* dark squids are _DOUBLE in smfDA */
                        data->da->flatcal,/* flatcal */
                        data->da->flatpar,/* flatpar */
                        "",             /* obsmode ! DREAM at present*/
                        NULL,           /* MCE HEAD */
                        NULL,           /* Trackinfo */
                        jig_vert,       /* jig_vert */
                        0,              /* nvert */
                        jig_path,       /* jig_path */
                        0,              /* npath */
                        NULL,           /* xmlfile name not the config string */
                        status );
    sc2store_free(status);
    sc2store_setcompflag( SC2STORE__NONE, status );

    /* We now need to copy over any components that are not
       read by smf_open_file. We now have to deal with NDF identifiers. */
    smf_close_file( NULL, &data, status );

    /* Re-open this file to get the NDF ID for provenance propagation
       and to copy missing items. We do not use smf_open_file since
       we are now in NDF identifier land. */
    ndgNdfas( igrp, i, "READ", &indf, status );
    ndgNdfas( ogrp, i, "UPDATE", &outndf, status );

    /* Provenance updating */
    smf_updateprov( outndf, NULL, indf, "SMURF:" TASK_NAME, NULL, status );

    /* Need to copy some missing extensions:
          .MORE.SCUBA2.MCEHEAD
          .MORE.SCUBA2.TRACKINFO
          .MORE.JCMTOCS
     */
    ndfXstat( indf, "SCUBA2", &isthere, status );
    if (isthere) {
      HDSLoc * inxloc = NULL;
      HDSLoc * outxloc = NULL;

      /* get the input SCUBA2 component */
      ndfXloc( indf, "SCUBA2", "READ", &inxloc, status );

      /* Get the output SCUBA2 component (it should be present!) */
      ndfXloc( outndf, "SCUBA2", "UPDATE", &outxloc, status );

      smf__copy_comp( inxloc, outxloc, "MCEHEAD", status );
      smf__copy_comp( inxloc, outxloc, "TRACKINFO", status );

      datAnnul( &inxloc, status );
      datAnnul( &outxloc, status );
    }

    ndfXstat( indf, "JCMTOCS", &isthere, status );
    if (isthere) {
      HDSLoc *inxloc = NULL;
      HDSLoc *outxloc = NULL;
      ndfXloc( indf, "JCMTOCS", "READ", &inxloc, status );

      ndfXstat( outndf, "JCMTOCS", &isthere, status );
      if (isthere) {
        ndfXloc( outndf, "JCMTOCS", "UPDATE", &outxloc, status );
      } else {
        ndfXnew ( outndf, "JCMTOCS", "OCSINFO", 0, NULL, &outxloc, status );
      }
      smf__copy_comp( inxloc, outxloc, "CONFIG", status );

      datAnnul( &inxloc, status );
      datAnnul( &outxloc, status );
    }


    /* Free resources for files */
    ndfAnnul( &outndf, status);
    ndfAnnul( &indf, status );
    idksquid = astFree( idksquid );

  }

  /* Tidy up after ourselves: release the resources used by the grp routines  */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);

  ndfEnd( status );
}


/* Helper function: Copies a component from one HDS structure to another */

static void smf__copy_comp( const HDSLoc * inxloc, HDSLoc * outxloc,
                            const char * comp, int *status ) {
  HDSLoc * srcloc = NULL;
  int isthere = 0;

  if (*status != SAI__OK) return;

  datThere( inxloc, comp, &isthere, status );
  if (isthere) {
    datFind( inxloc, comp, &srcloc, status );

    /* Remove destination if present */
    datThere( outxloc, comp, &isthere, status );
    if (isthere) {
      datErase( outxloc, comp, status );
    }

    datCopy( srcloc, outxloc, comp, status );
  }

}
