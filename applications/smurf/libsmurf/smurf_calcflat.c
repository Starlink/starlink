/*
*+
*  Name:
*     CALCFLAT

*  Purpose:
*     Calculate a flatfield solution

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_calcflat( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculate flatfield solution from a flatfield observation.

*  Notes:

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be processed. Must all be from the same
*          observation.
*     OUT = NDF (Write)
*          Output flatfield file.
*     RESIST = GROUP (Read)
*          Resistor settings for each bolometer.
*     RESP = NDF (Write)
*          Responsivity image with variance. No image is written
*          if Null [!]

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-08-26 (TIMJ):
*        Original version.
*     {enter_further_changes_here}

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

#include <string.h>
#include <stdio.h>

#include "par.h"
#include "ast.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "star/one.h"

#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smurf_calcflat"
#define TASK_NAME "CALCFLAT"

void smurf_calcflat( int *status ) {

  smfArray * bbhtframe = NULL; /* Data (non-reference) frames */
  double *bolref = NULL;    /* response of each bolometer to powref */
  smfArray * darks = NULL;  /* Darks */
  smfData *ddata = NULL;    /* A heater file */
  char defname[GRP__SZNAM+1]; /* default output file name */
  char flatname[GRP__SZNAM+1]; /* Actual output file name */
  Grp *flatgrp = NULL;      /* Output flatfield group */
  size_t flatsize;          /* Size ouf output flatfield group */
  Grp * fgrp = NULL;        /* Filtered group */
  size_t i = 0;             /* Counter, index */
  size_t j;                 /* Counter */
  size_t ksize;             /* Size of key map group */
  size_t nbols;             /* Number of bolometers */
  int ncols;                /* Number of columns */
  int nrows;                /* Number of rows */
  Grp *igrp = NULL;         /* Input group of files */
  int obsnum;               /* Observation number */
  Grp *ogrp = NULL;         /* Output group of files */
  double *pixheat = NULL;   /* Pixel heater settings for each input file */
  char *pname = NULL;       /* Temporary pointer */
  double *powref = NULL;    /* Reference input powers */
  double refohms;           /* reference pixel heater resistance */
  double *resistance = NULL; /* Bolometer resistance settings */
  Grp *resgrp =  NULL;       /* Resistor group */
  AstKeyMap * resmap = NULL; /* Resistor map */
  smfData * respmap = NULL;  /* Responsivity map */
  int respmask = 0;          /* Mask bolometers that have bad responsivity? */
  size_t size;               /* Number of files in input group */
  char subarray[9];          /* subarray name */
  int utdate;                /* UTdate of observation */

  /* Main routine */
  ndfBegin();

  /* Get input file(s) */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Find darks (might be all) */
  smf_find_darks( igrp, &fgrp, NULL, 1, SMF__DOUBLE, &darks, status );

  if (!darks || darks->ndat <= 2) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( " ","Flatfield observation must include at least two darks",
              status);
    }
    goto CLEANUP;
  }

  /* input group is now the filtered group so we can use that and
     free the old input group */
  size = grpGrpsz( fgrp, status );
  grpDelet( &igrp, status);
  igrp = fgrp;
  fgrp = NULL;

  /* get the reference pixel heater resistance */
  parGdr0d("REFRES", 2.0, 0, VAL__MAXD, 1, &refohms, status );

  /* Get the bolometer resistor settings */
  kpg1Gtgrp( "RESIST", &resgrp, &ksize, status );
  kpg1Kymap( resgrp, &resmap, status );
  if( resgrp ) grpDelet( &resgrp, status );  

  if (*status != SAI__OK) goto CLEANUP;

  /* Parse the file and generate a 2d array of resistor settings */
  if (!astMapGet0I( resmap, "NROWS", &nrows ) ) {
    *status = SAI__ERROR;
    errRep(" ", "Resistor file did not have an nrows entry", status );
    goto CLEANUP;
  }
  if (!astMapGet0I( resmap, "NCOLS", &ncols ) ) {
    *status = SAI__ERROR;
    errRep(" ", "Resistor file did not have an ncols entry", status );
    goto CLEANUP;
  }

  nbols = ncols * nrows;
  resistance = smf_malloc( nbols, sizeof(*resistance),
                           0, status);

  if (*status == SAI__OK) {
    for (j = 0; j < (size_t)ncols; j++) {
      int ngot;
      char colname[6];
      sprintf( colname, "COL%-d", (int)(j+1) );
      astMapGet1D( resmap, colname, nrows, &ngot, &(resistance[nrows*j]));
      if (ngot != nrows) {
        if (*status == SAI__OK) {
          msgSetc( "COL", colname );
          msgSeti( "NR", nrows );
          errRep(" ", "Did not read ^NR resistor values from ^COL", status );
        }
        goto CLEANUP;
      }
    }

    /* Replace small values with bad */
    for (j = 0; j < nbols; j++) {
      if (resistance[j] < 0.1) {
        resistance[j] = VAL__BADD;
      }
    }
  }


  /* a full dark flatfield will be obvious */
  if ( size == 0 ) {

    /* check that we are all from the same observation */
    /* whilst we are at it, read the PIXHEAT header */
    for (i = 1; i < darks->ndat; i++) {
      if (strcmp( (darks->sdata)[0]->hdr->obsidss,
                  (darks->sdata)[i]->hdr->obsidss ) != 0 ) {
        *status = SAI__ERROR;
        errRep(" ", "Flatfield can not be calculated from multiple observations",
               status);
        goto CLEANUP;
      }
    }

    /* Okay, single observation, darks in time order */

    /* get some memory for pixel heater settings */
    pixheat = smf_malloc( darks->ndat, sizeof(*pixheat), 0, status );

    /* and some memory for non-reference frames */
    bbhtframe = smf_create_smfArray( status );
    if (*status != SAI__OK) goto CLEANUP;

    /* this smfArray does not own the data */
    bbhtframe->owndata = 0;

    /* Loop over every other frame. Assumes start and end on dark
       but note that this branch assumes all files are darks but with
       varying PIXHEAT */
    for (i = 1; i < darks->ndat; i+=2) {

      /* Subtract darks using MEAN */
      smf_subtract_dark( (darks->sdata)[i], (darks->sdata)[i-1],
                         (darks->sdata)[i+1], SMF__DKSUB_MEAN, status);


      /* Store the frame for later */
      smf_addto_smfArray( bbhtframe, (darks->sdata)[i], status );

      /* get the pixel heater settings */
      smf_fits_getD( (darks->sdata)[i]->hdr, "PIXHEAT",
                     &(pixheat[bbhtframe->ndat - 1]),
                     status );
      
    }

    /* We now have data for the various pixel heater settings.
       Generate a set of reference heater power settings in pW, and calculate the
       expected measurement from each bolometer at each power setting.
     */

    bolref = smf_malloc( bbhtframe->ndat * nbols, sizeof(*bolref), 0,
                         status );
    powref = smf_malloc( bbhtframe->ndat, sizeof(*powref), 0, status );

    smf_flat_standardpow( bbhtframe, refohms, pixheat, resistance,
                          powref, bolref, status );


    /* See if we need an output file for responsivities or some temporary
       memory */
    if (*status == SAI__OK) {
      Grp *rgrp = NULL;
      size_t rsize = 0;

      kpg1Wgndf( "RESP", NULL, 1, 1, "", &rgrp, &rsize, status );

      if (*status == PAR__NULL) {
        void *pntr[] = {NULL, NULL, NULL};
        dim_t mydims[2];
        errAnnul( status );
        mydims[SMF__ROW_INDEX] = nrows;
        mydims[SMF__COL_INDEX] = ncols;
        
        pntr[0] = smf_malloc( nrows*ncols, sizeof(double), 0, status );
        pntr[1] = smf_malloc( nrows*ncols, sizeof(double), 0, status );

        respmap = smf_construct_smfData( NULL, NULL, NULL, NULL, SMF__DOUBLE,
                                         pntr, 0, mydims, 2, 0, 0, NULL,
                                         NULL, status );

      } else {
        int lbnd[] = { 1, 1 };
        int ubnd[2];

        /* open the file */
        ubnd[SMF__ROW_INDEX] = nrows - lbnd[SMF__ROW_INDEX] + 1;
        ubnd[SMF__COL_INDEX] = ncols - lbnd[SMF__COL_INDEX] + 1;
        smf_open_newfile( rgrp, 1, SMF__DOUBLE, 2, lbnd, ubnd,
                          SMF__MAP_VAR, &respmap, status );
      }
      if (rgrp) grpDelet( &rgrp, status );
    }


    /* Calculate the responsivity in Amps/Watt */

    smf_flat_responsivity( respmap, bbhtframe->ndat, powref, bolref, status );

    /* Optionally discard the calibration if the responsivity is bad */
    parGet0l( "RESPMASK", &respmask, status );
    if (respmask) {
      double *respdata = (respmap->pntr)[0];
      for (i = 0; i < nbols; i++ ) {
        if ( respdata[i] == VAL__BADD) {
          for (j=0; j<bbhtframe->ndat; j++) {
            bolref[j*nrows*ncols+i] = VAL__BADD;
          } 
        }
      }
    }

    /* Work out the output filename  - provide a default */
    ddata = (bbhtframe->sdata)[0];
    smf_find_subarray( ddata->hdr, subarray, sizeof(subarray), NULL, status );
    smf_fits_getI( ddata->hdr, "UTDATE", &utdate, status );
    smf_fits_getI( ddata->hdr, "OBSNUM", &obsnum, status );
    snprintf( defname, sizeof(defname), "%s%08d_%05d_flat",
              subarray, utdate, obsnum );
    parDef0c( "OUT", defname, status );
    kpg1Wgndf( "OUT", NULL, 1, 1, "", &flatgrp, &flatsize, status);
    pname = flatname;
    grpGet( flatgrp, 1, 1, &pname, sizeof(flatname), status );
    if (flatgrp) grpDelet( &flatgrp, status );

    /* write out the flatfield */
    smf_flat_write( flatname, bbhtframe, powref, bolref, status );

    if (respmap) smf_close_file( &respmap, status );

  } else {
    *status = SAI__ERROR;
    errRep( " ", "Blackbody flatfield not yet supported", status );
  }


  /* Tidy up after ourselves: release the resources used by the grp routines  */
 CLEANUP:
  if (bbhtframe) smf_close_related( &bbhtframe, status );
  if (darks) smf_close_related( &darks, status );
  if (igrp) grpDelet( &igrp, status);
  if (ogrp) grpDelet( &ogrp, status);
  if (resistance) resistance = smf_free( resistance, status );
  if (pixheat) pixheat = smf_free( pixheat, status );
  if (bolref) bolref = smf_free( bolref, status );
  if (powref) powref = smf_free( powref, status );

  ndfEnd( status );
}

