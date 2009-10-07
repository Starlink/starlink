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
*     This routine can calculate a flatfield solution from a flatfield observation.
*
*     The flatfield observation consists of a series of dark measurements taken at 
*     various pixel heater settings. One standard SCUBA-2 raw data file is stored for
*     each measurement.
*
*     Some optimum pixel heater setting is chosen. The procedure is to record
*     measurements at heater settings around this optimum value, continually returning
*     to the optimum which is used as a reference to subtract pixel zero-point drifts.

*  Notes:
*     - Does not yet enforce the observation to be of type FLATFIELD.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be processed. Must all be from the same
*          observation and the same subarray.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     NGOOD = _INTEGER (Write)
*          Number of bolometers with good responsivities.
*     OUT = NDF (Write)
*          Output flatfield file. The primary data array contains the
*          dark subtracted measurements for each heater setting. The
*          flatfield itself is stored in the .MORE.SCUBA2.FLATCAL 
*          extension. A default output filename based on the date of observation
*          number, subarray name and observation number will be suggested.
*     REFRES = _DOUBLE (Read)
*          Reference pixel heat resistance. Defines the mean power scale to
*          be used. Defaults to 2.0.
*     RESIST = GROUP (Read)
*          Resistor settings for each bolometer. This is a text file, an
*          example can be found in $STARLINK_DIR/share/smurf/resist.cfg
*     RESP = NDF (Write)
*          Responsivity image with variance. No image is written
*          if Null [!]
*     RESPMASK = _LOGICAL (Read)
*          If true, responsivity data will be used to mask bolometer data
*          when calculating the flatfield [TRUE]

*  Related Applications:
*     SMURF: CALCRESP, FLATFIELD

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-08-26 (TIMJ):
*        Original version.
*     2008-09-03 (TIMJ):
*        First version ready for testing.
*     2008-11-18 (TIMJ):
*        Trap erroneous final dark.
*     2009-03-27 (TIMJ):
*        Use new smf_create_respfile routine for creating the responsivity image.
*     2009-05-28 (AGG):
*        Explicitly check for good status after requesting RESP parameter
*     2009-09-02 (TIMJ):
*        Write RESP provenance after OUT parameter has been defaulted. This
*        was causing odd problems in the pipeline.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
*     Copyright (C) 2009 University of British Columbia.
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
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smurf_calcflat"
#define TASK_NAME "CALCFLAT"

void smurf_calcflat( int *status ) {

  smfArray * bbhtframe = NULL; /* Data (non-reference) frames */
  double *bolref = NULL;    /* response of each bolometer to powref */
  smfArray * darks = NULL;  /* Darks */
  smfData *ddata = NULL;    /* A heater file */
  Grp * dkgrp = NULL;       /* Group of darks */
  char defname[GRP__SZNAM+1]; /* default output file name */
  char flatname[GRP__SZNAM+1]; /* Actual output file name */
  Grp *flatgrp = NULL;      /* Output flatfield group */
  size_t flatsize;          /* Size ouf output flatfield group */
  Grp * fgrp = NULL;        /* Filtered group */
  double heatref;           /* Reference heater setting */
  size_t i = 0;             /* Counter, index */
  size_t j;                 /* Counter */
  size_t ksize;             /* Size of key map group */
  size_t nbols;             /* Number of bolometers */
  int ncols;                /* Number of columns */
  int nrows;                /* Number of rows */
  size_t ndarks;            /* Number of darks to process */
  size_t ngood;             /* Number of good responsivities */
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
  int subnum;                /* subarray number */
  int utdate;                /* UTdate of observation */

  /* Main routine */
  ndfBegin();

  /* Get input file(s) */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Find darks (might be all) */
  smf_find_darks( igrp, &fgrp, &dkgrp, 1, SMF__DOUBLE, &darks, status );

  if (!darks || darks->ndat < 3) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( " ","Flatfield observation must include at least three darks",
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
          *status = SAI__ERROR;
          msgSeti( "NG", ngot);
          msgSetc( "COL", colname );
          msgSeti( "NR", nrows );
          errRep(" ", "Did not read ^NR resistor values from column ^COL, read ^NG", status );
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

    /* Get reference subarray */
    smf_find_subarray( (darks->sdata)[0]->hdr, subarray, sizeof(subarray), &subnum, status );
    if (*status != SAI__OK) goto CLEANUP;

    /* Check row vs column count */
    if ( ((darks->sdata)[0]->dims)[SC2STORE__COL_INDEX] != (size_t)ncols ||
         ((darks->sdata)[0]->dims)[SC2STORE__ROW_INDEX] != (size_t)nrows ) {
      *status = SAI__ERROR;
      msgSeti( "RC", ncols );
      msgSeti( "RR", nrows );
      msgSeti( "DC", ((darks->sdata)[0]->dims)[SC2STORE__COL_INDEX]);
      msgSeti( "DR", ((darks->sdata)[0]->dims)[SC2STORE__ROW_INDEX]);
      errRep( " ", "Dimensions of subarray from resistor file (^RC x ^RR)"
              " do not match those of data file (^DC x ^DR)", status );
      goto CLEANUP;
    }

    /* check that we are all from the same observation and same subarray */
    for (i = 1; i < darks->ndat; i++) {
      int nsub;

      if (strcmp( (darks->sdata)[0]->hdr->obsidss,
                  (darks->sdata)[i]->hdr->obsidss ) != 0 ) {
        *status = SAI__ERROR;
        errRep(" ", "Flatfield can not be calculated from multiple observations",
               status);
        goto CLEANUP;
      }

      smf_find_subarray( (darks->sdata)[i]->hdr, NULL, 0, &nsub, status );
      if (nsub != subnum) {
        *status = SAI__ERROR;
        errRep( " ", "Flatfield command does not yet handle multiple subarrays in a single call",
                status );
        goto CLEANUP;
      }

    }

    /* Okay, single observation, darks in time order */

    /* Report reference heater setting */
    smf_fits_getD( (darks->sdata)[0]->hdr, "PIXHEAT", &heatref,
                   status );
    msgSetd( "PX", heatref );
    msgOutif( MSG__NORM, " ", "Reference heater setting: ^PX", status );

    /* get some memory for pixel heater settings */
    pixheat = smf_malloc( darks->ndat, sizeof(*pixheat), 0, status );

    /* and some memory for non-reference frames */
    bbhtframe = smf_create_smfArray( status );
    if (*status != SAI__OK) goto CLEANUP;

    /* this smfArray does not own the data */
    bbhtframe->owndata = 0;

    /* Need odd number of darks */
    ndarks = darks->ndat;
    if (darks->ndat % 2 == 0) {
      msgOutif( MSG__NORM, " ",
                "Observed an even number of darks. Dropping last dark from processing.",
                status);
      ndarks--;
    }

    /* Loop over every other frame. Assumes start and end on dark
       but note that this branch assumes all files are darks but with
       varying PIXHEAT */
    for (i = 1; i < ndarks; i+=2) {
      double heater;
      double ref1;
      double ref2;

      /* get the pixel heater settings and make sure they are consistent */
      smf_fits_getD( (darks->sdata)[i]->hdr, "PIXHEAT", &heater,
                     status );

      msgSetd( "PX", heater );
      msgOutif( MSG__NORM, " ", "Processing heater setting ^PX", status );

      /* Get reference */
      smf_fits_getD( (darks->sdata)[i-1]->hdr, "PIXHEAT", &ref1,
                     status );
      smf_fits_getD( (darks->sdata)[i+1]->hdr, "PIXHEAT", &ref2,
                     status );

      if (ref1 != heatref || ref2 != heatref) {
        if (*status == SAI__OK) {
          *status = SAI__ERROR;
          msgSetd( "REF", heatref );
          msgSetd( "R1", ref1 );
          msgSetd( "R2", ref2 );
          errRep( " ", "Bracketing darks have inconsistent heater settings"
                  " (^REF ref cf ^R1 and ^R2)", status );
          break;
        }
      }

      /* Subtract darks using MEAN */
      smf_subtract_dark( (darks->sdata)[i], (darks->sdata)[i-1],
                         (darks->sdata)[i+1], SMF__DKSUB_MEAN, status);

      /* Store the frame for later */
      smf_addto_smfArray( bbhtframe, (darks->sdata)[i], status );

      pixheat[bbhtframe->ndat - 1] = heater;

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
      smfData *refdata = (bbhtframe->sdata)[0];

      kpg1Wgndf( "RESP", NULL, 1, 1, "", &rgrp, &rsize, status );

      if (*status == SAI__OK) {
        /* Create the file on disk (units will normalise so no need for prefix) */
        smf_create_bolfile( rgrp, 1, refdata, "Responsivity",
                            "A/W", &respmap, status );
      } else if (*status == PAR__NULL) {
        void *pntr[] = {NULL, NULL, NULL};
        dim_t mydims[2];
        dim_t lbnd[2];
        errAnnul( status );
        mydims[SC2STORE__ROW_INDEX] = nrows;
        mydims[SC2STORE__COL_INDEX] = ncols;

        pntr[0] = smf_malloc( nbols, sizeof(double), 0, status );
        pntr[1] = smf_malloc( nbols, sizeof(double), 0, status );
        lbnd[SC2STORE__ROW_INDEX] = (refdata->lbnd)[SC2STORE__ROW_INDEX];
        lbnd[SC2STORE__COL_INDEX] = (refdata->lbnd)[SC2STORE__COL_INDEX];

        respmap = smf_construct_smfData( NULL, NULL, NULL, NULL, SMF__DOUBLE,
                                         pntr, 0, mydims, lbnd, 2, 0, 0, NULL,
                                         NULL, status );
      }
      if (rgrp) grpDelet( &rgrp, status );
    }


    /* Calculate the responsivity in Amps/Watt */

    ngood = smf_flat_responsivity( respmap, bbhtframe->ndat, powref, bolref,
                                   status );

    /* Report the number of good responsivities */
    msgSeti( "NG", ngood );
    msgSeti( "NTOT", nbols );
    msgOutif( MSG__NORM, "",
              "Number of good responsivities: ^NG out of ^NTOT", status);
    parPut0i( "NGOOD", ngood, status );

    /* Optionally discard the calibration if the responsivity is bad */
    parGet0l( "RESPMASK", &respmask, status );
    if (respmask) {
      size_t nmask = 0;
      size_t thisbol = 0;
      double *respdata = (respmap->pntr)[0];
      for (i = 0; i < nbols; i++ ) {
        if ( respdata[i] == VAL__BADD) {
          thisbol = 0;
          for (j=0; j<bbhtframe->ndat; j++) {
            if (bolref[j*nbols+i] != VAL__BADD) {
              bolref[j*nbols+i] = VAL__BADD;
              thisbol++;
            }
          }
          if (thisbol > 0) nmask++;
        }
      }
      msgSeti( "NM", nmask);
      msgOutif( MSG__NORM, "",
                "Responsivity mask has removed an additional ^NM bolometers",
                status);
    }

    /* Work out the output filename  - provide a default */
    ddata = (bbhtframe->sdata)[0];
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
    smf_flat_write( flatname, bbhtframe, pixheat, powref, bolref, dkgrp, status );

    if (respmap) {
      /* write the provenance at the end since we have some problems with A-tasks
         in the pipeline causing trouble if the OUT parameter has not yet been set */
      if (respmap->file) smf_accumulate_prov( NULL, dkgrp, 1, respmap->file->ndfid, "SMURF:CALCFLAT", status );
      smf_close_file( &respmap, status );
    }

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
  if (dkgrp) grpDelet( &dkgrp, status );
  if (resistance) resistance = smf_free( resistance, status );
  if (pixheat) pixheat = smf_free( pixheat, status );
  if (bolref) bolref = smf_free( bolref, status );
  if (powref) powref = smf_free( powref, status );

  ndfEnd( status );
}

