/*
*+
*  Name:
*     CALCFLAT

*  Purpose:
*     Calculate a flatfield solution from a flatfield observation.

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
*     This routine calculates a flatfield solution from a flatfield
*     observation.
*
*     The flatfield observation consists of a series of measurements
*     taken at various pixel heater settings. One standard SCUBA-2 raw
*     data file is stored for each measurement.
*
*     An optimum pixel heater setting is chosen at the time of
*     observation. The procedure is to record measurements at heater
*     settings around this optimum value, continually returning to the
*     optimum, which is used as a reference to subtract pixel
*     zero-point drifts.

*  Notes:
*     Works with Dark and Sky flatfields but not with black-body flatfields.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be processed. Must all be from the same
*          observation and the same subarray.
*     METHOD = _CHAR (Read)
*          Method to use to calculate the flatfield solution. Options
*          are POLYNOMIAL and TABLE. Polynomial fits a polynomial to
*          the measured signal. Table uses an interpolation scheme
*          between the measurements to determine the power. [POLYNOMIAL]
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
*          extension. A default output filename based on the date of
*          observation number, subarray name and observation number
*          will be suggested.
*     ORDER = _INTEGER (Read)
*          The order of polynomial to use when choosing POLYNOMIAL method.
*          [1]
*     REFRES = _DOUBLE (Read)
*          Reference pixel heat resistance. Defines the mean power scale to
*          be used. [2.0]
*     RESIST = GROUP (Read)
*          A group expression containing the resistor settings for each bolometer.
*          Usually specified as a text file using "^" syntax. An example can be found in
*          $STARLINK_DIR/share/smurf/resist.cfg [$STARLINK_DIR/share/smurf/resist.cfg]
*     RESP = NDF (Write)
*          Responsivity image with variance. No image is written
*          if NULL. [!]
*     RESPMASK = _LOGICAL (Read)
*          If true, responsivity data will be used to mask bolometer data
*          when calculating the flatfield. [TRUE]
*     SNRMIN = _DOUBLE (Read)
*          Signal-to-noise ratio threshold to use when filtering the
*          responsivity data to determine valid bolometers for the
*          flatfield. [3.0]

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
*     2009-11-13 (TIMJ):
*        Add support for sky flatfields
*     2009-11-24 (TIMJ):
*        Handle case where no darks are supplied.
*     2010-01-28 (TIMJ):
*        Flatfield routines now use smfData
*     2010-02-03 (TIMJ):
*        Update smf_flat_responsivity and smf_flat_write API
*     2010-02-04 (TIMJ):
*        Add METHOD and ORDER to allow POLYNOMIAL mode.
*     2010-02-08 (TIMJ):
*        Remove obvious problems with data prior to fitting.
*     2010-03-05 (TIMJ):
*        Use smf_flat_mergedata and new smf_flat_standardpow API
*     2010-03-10 (TIMJ):
*        Process fast flatfield ramps.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2010 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

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

/* minimum number of files for a good flatfield */
#define MINFLAT 7

void smurf_calcflat( int *status ) {

  smfArray * bbhtframe = NULL; /* Data (non-reference) frames */
  smfData *bolval = NULL;   /* merged flatfield values */
  smfArray * darks = NULL;  /* Darks */
  Grp * dkgrp = NULL;       /* Group of darks */
  char defname[GRP__SZNAM+1]; /* default output file name */
  int isfastramp = 0;       /* are we processing a fast ramp? */
  smfArray * fflats = NULL; /* Fast flatfield ramps */
  char flatname[GRP__SZNAM+1]; /* Actual output file name */
  smfArray * flatfiles = NULL; /* Flatfield data from all files */
  Grp *flatgrp = NULL;      /* Output flatfield group */
  size_t flatsize;          /* Size ouf output flatfield group */
  Grp * ffgrp = NULL;       /* Fast flatfield group */
  Grp * fgrp = NULL;        /* Filtered group */
  double heatref;           /* Reference heater setting */
  size_t i = 0;             /* Counter, index */
  dim_t j = 0;              /* Counter, index */
  Grp *igrp = NULL;         /* Input group of files */
  dim_t nflatfiles;         /* Number of flatfield files to process */
  dim_t ngood;              /* Number of good responsivities */
  int obsnum;               /* Observation number */
  double *pixheat = NULL;   /* Pixel heater settings for each input file */
  char *pname = NULL;       /* Temporary pointer */

  size_t size;              /* Number of files in input group */
  char subarray[9];         /* subarray name */
  sc2ast_subarray_t subnum; /* subarray number */
  int utdate;               /* UTdate of observation */

  /* Main routine */
  ndfBegin();

  /* Get input file(s)  MINFLAT needed for non-fast ramp */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Find darks (might be all) */
  smf_find_science( NULL, igrp, &fgrp, 0, &dkgrp, &ffgrp, 1, 0, SMF__DOUBLE, &darks,
                    &fflats, NULL, NULL, status );

  /* input group is now the filtered group so we can use that and
     free the old input group */
  size = grpGrpsz( fgrp, status );
  grpDelet( &igrp, status);
  igrp = fgrp;
  fgrp = NULL;

  /* Sanity check */
  if ( *status == SAI__OK && grpGrpsz(ffgrp, status ) == 0 && grpGrpsz( dkgrp, status ) == 0
       && grpGrpsz( igrp, status ) == 0 ) {
    *status = SAI__ERROR;
    errRep("", "No good files supplied to routine for flatfielding", status );
    goto CLEANUP;
  }

  /* See whether we had all darks or science + dark or fast flatfields. For some reason
     grpGrpsz returns 1 if status is bad */
  if ( *status == SAI__OK && grpGrpsz( ffgrp, status ) > 0 && fflats ) {
    dim_t fsize = grpGrpsz( ffgrp, status );

    if (fsize > 1) {
      if (*status == SAI__OK) {
        *status = SAI__ERROR;
        errRep( "", "CALCFLAT can only process one ramp at a time",
                status );
      }
    }

    /* Clear igrp and reassign ffgrp to that */
    grpDelet( &igrp, status );
    igrp = ffgrp;
    ffgrp = NULL;

    /* this is a fast ramp */
    isfastramp = 1;

    /* and assign the smfData */
    bolval = (fflats->sdata)[0];

    /* and find the subarray */
    smf_find_subarray( bolval->hdr, subarray, sizeof(subarray), NULL, status );

  } else if ( size == 0 && darks && darks->ndat > 0 ) {
    /* everything is in the dark */
    flatfiles = darks;
    darks = NULL;

    /* make the input group be the dark group for later provenance handling */
    if (igrp) grpDelet( &igrp, status );
    igrp = dkgrp;
    dkgrp = NULL;
  } else {
    const float clip[] = { 3.0 };
    flatfiles = smf_create_smfArray( status );
    if (*status == SAI__OK) {
      for (i = 1; i <= size; i++ ) {
        smfData *outfile = NULL;
        smfData *infile = NULL;
        if (*status != SAI__OK) break;
        smf_open_file( NULL, igrp, (int) i, "READ", 0, &infile, status );

        if (*status == SAI__OK && infile
            && infile->hdr->obstype != SMF__TYP_FLATFIELD) {
          *status = SAI__ERROR;
          errRep( "", "Attempting to run calcflat on a non-flatfield observation",
                  status );
        }

        /* calculate mean and standard deviation and throw out
           S/N < 1 and constant signal data. Also clip at 3sigma */
        smf_collapse_tseries( infile, 1, clip, 1.0, 1, SMF__DOUBLE,
                              &outfile, status );
        smf_close_file( NULL, &infile, status );
        smf_addto_smfArray( flatfiles, outfile, status );
      }
    }
  }

  if ( *status == SAI__OK ) {

    /* slow mode */
    if (!isfastramp) {

      if (*status == SAI__OK && flatfiles->ndat < MINFLAT ) {
        *status = SAI__ERROR;
        errRepf( "", "Discrete flatfield requires at least %d files",
                status, MINFLAT );
      }

      /* Get reference subarray */
      smf_find_subarray( (flatfiles->sdata)[0]->hdr, subarray, sizeof(subarray), &subnum, status );
      if (*status != SAI__OK) goto CLEANUP;

      /* check that we are all from the same observation and same subarray */
      for (j = 1; j < flatfiles->ndat; j++) {
        int nsub;

        if (strcmp( (flatfiles->sdata)[0]->hdr->obsidss,
                    (flatfiles->sdata)[j]->hdr->obsidss ) != 0 ) {
          *status = SAI__ERROR;
          errRep(" ", "Flatfield can not be calculated from multiple observations",
                 status);
          goto CLEANUP;
        }

        smf_find_subarray( (flatfiles->sdata)[j]->hdr, NULL, 0, &nsub, status );
        if (nsub != subnum) {
          *status = SAI__ERROR;
          errRep( " ", "Flatfield command does not yet handle multiple subarrays in a single call",
                  status );
          goto CLEANUP;
        }

      }

      /* Okay, single observation, flatfield files in time order */

      /* Report reference heater setting */
      smf_fits_getD( (flatfiles->sdata)[0]->hdr, "PIXHEAT", &heatref,
                     status );
      msgSetd( "PX", heatref );
      msgOutif( MSG__NORM, " ", "Reference heater setting: ^PX", status );

      /* get some memory for pixel heater settings */
      pixheat = astMalloc( (flatfiles->ndat)*sizeof(*pixheat) );

      /* and some memory for non-reference frames */
      bbhtframe = smf_create_smfArray( status );
      if (*status != SAI__OK) goto CLEANUP;

      /* this smfArray does not own the data */
      bbhtframe->owndata = 0;

      /* Need odd number of flatfiles */
      nflatfiles = flatfiles->ndat;
      if (flatfiles->ndat % 2 == 0) {
        msgOutif( MSG__NORM, " ",
                  "Observed an even number of sequences. Dropping last one from processing.",
                  status);
        nflatfiles--;
      }

      /* Loop over every other frame. Assumes start and end on dark
         but note that this branch assumes all files are flatfield observations but with
         varying PIXHEAT */
      for (j = 1; j < nflatfiles; j+=2) {
        double heater;
        double ref1;
        double ref2;

        /* get the pixel heater settings and make sure they are consistent */
        smf_fits_getD( (flatfiles->sdata)[j]->hdr, "PIXHEAT", &heater,
                       status );

        msgSetd( "PX", heater );
        msgOutif( MSG__NORM, " ", "Processing heater setting ^PX", status );

        /* Get reference */
        smf_fits_getD( (flatfiles->sdata)[j-1]->hdr, "PIXHEAT", &ref1,
                       status );
        smf_fits_getD( (flatfiles->sdata)[j+1]->hdr, "PIXHEAT", &ref2,
                       status );

        if (ref1 != heatref || ref2 != heatref) {
          if (*status == SAI__OK) {
            *status = SAI__ERROR;
            msgSetd( "REF", heatref );
            msgSetd( "R1", ref1 );
            msgSetd( "R2", ref2 );
            errRep( " ", "Bracketing sequences have inconsistent heater settings"
                    " (^REF ref cf ^R1 and ^R2)", status );
            break;
          }
        }

        /* Subtract bracketing files using MEAN */
        smf_subtract_dark( (flatfiles->sdata)[j], (flatfiles->sdata)[i-1],
                           (flatfiles->sdata)[j+1], SMF__DKSUB_MEAN, status);

        /* Store the frame for later */
        smf_addto_smfArray( bbhtframe, (flatfiles->sdata)[j], status );

        pixheat[bbhtframe->ndat - 1] = heater;

      }

      /* Merge the data into standard form */
      smf_flat_mergedata( bbhtframe, pixheat, &bolval, status );

    }

    /* Work out the output filename  - provide a default */
    if (bolval) {
      smf_fits_getI( bolval->hdr, "UTDATE", &utdate, status );
      smf_fits_getI( bolval->hdr, "OBSNUM", &obsnum, status );

      /* FastRamps need to preserve the subscan number */
      if (isfastramp) {
        int subscan;
        smf_fits_getI( bolval->hdr, "NSUBSCAN", &subscan, status );
        snprintf( defname, sizeof(defname), "%s%08d_%05d_%04d_flat",
                  subarray, utdate, obsnum, subscan );
      } else {
        snprintf( defname, sizeof(defname), "%s%08d_%05d_flat",
                  subarray, utdate, obsnum );
      }

    } else {
      defname[0] = '\0';
    }
    parDef0c( "OUT", defname, status );
    kpg1Wgndf( "OUT", NULL, 1, 1, "", &flatgrp, &flatsize, status);
    pname = flatname;
    grpGet( flatgrp, 1, 1, &pname, sizeof(flatname), status );
    if (flatgrp) grpDelet( &flatgrp, status );

    /* Calculate the flatfield. We now have data for the various pixel heater settings.
       Generate a set of reference heater power settings in pW, and calculate the
       expected measurement from each bolometer at each power setting.
     */

    ngood = smf_flat_calcflat( NULL, MSG__NORM, flatname, "RESIST", "METHOD", "ORDER",
                               "RESP", "RESPMASK", "SNRMIN", igrp, bolval, NULL, status );
    parPut0k( "NGOOD", ngood, status );

  }

  /* Tidy up after ourselves: release the resources used by the grp routines  */
 CLEANUP:
  if (bbhtframe) smf_close_related( NULL, &bbhtframe, status );
  if (darks) smf_close_related( NULL, &darks, status );
  if (fflats) smf_close_related( NULL, &fflats, status );
  if (flatfiles) smf_close_related( NULL, &flatfiles, status );
  if (igrp) grpDelet( &igrp, status);
  if (fgrp) grpDelet( &fgrp, status);
  if (ffgrp) grpDelet( &ffgrp, status);
  if (dkgrp) grpDelet( &dkgrp, status );
  if (pixheat) pixheat = astFree( pixheat );

  /* bolval is a simple pointer copy in fast ramp mode and will be freed when fflats is freed */
  if (!isfastramp) {
    if (bolval) smf_close_file( NULL, &bolval, status );
  }

  ndfEnd( status );

}

