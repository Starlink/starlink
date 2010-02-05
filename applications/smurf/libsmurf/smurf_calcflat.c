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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA.

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
#define MINFLAT 5

void smurf_calcflat( int *status ) {

  smfArray * bbhtframe = NULL; /* Data (non-reference) frames */
  smfData *bolref = NULL;   /* response of each bolometer to powref */
  smfArray * darks = NULL;  /* Darks */
  smfData *ddata = NULL;    /* A heater file */
  Grp * dkgrp = NULL;       /* Group of darks */
  char defname[GRP__SZNAM+1]; /* default output file name */
  char flatname[GRP__SZNAM+1]; /* Actual output file name */
  smfArray * flatfiles = NULL; /* Flatfield data from all files */
  Grp *flatgrp = NULL;      /* Output flatfield group */
  smfData * flatpoly = NULL;/* Polynomial expansion of fit */
  size_t flatsize;          /* Size ouf output flatfield group */
  Grp * fgrp = NULL;        /* Filtered group */
  double heatref;           /* Reference heater setting */
  size_t i = 0;             /* Counter, index */
  int istable = 0;          /* Are we using TABLE mode? */
  size_t j;                 /* Counter */
  size_t ksize;             /* Size of key map group */
  char method[SC2STORE_FLATLEN]; /* Flatfield method to use */
  size_t nbols;             /* Number of bolometers */
  int ncols;                /* Number of columns */
  int nrows;                /* Number of rows */
  size_t nflatfiles;        /* Number of flatfield files to process */
  size_t ngood;             /* Number of good responsivities */
  Grp *igrp = NULL;         /* Input group of files */
  int obsnum;               /* Observation number */
  Grp *ogrp = NULL;         /* Output group of files */
  double *pixheat = NULL;   /* Pixel heater settings for each input file */
  char *pname = NULL;       /* Temporary pointer */
  smfData *powref = NULL;    /* Reference input powers */
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
  double snrmin = 3;         /* Minimum allowed signal-to-noise ratio for responsivity */

  /* Main routine */
  ndfBegin();

  /* Get input file(s) */
  kpg1Rgndf( "IN", 0, MINFLAT, "", &igrp, &size, status );

  /* Find darks (might be all) */
  smf_find_darks( igrp, &fgrp, &dkgrp, 1, SMF__DOUBLE, &darks, status );

  /* input group is now the filtered group so we can use that and
     free the old input group */
  size = grpGrpsz( fgrp, status );
  grpDelet( &igrp, status);
  igrp = fgrp;
  fgrp = NULL;

  /* See whether we had all darks or science + dark */
  if ( size == 0 ) {
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
        smf_open_file( igrp, i, "READ", 0, &infile, status );

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
        smf_close_file( &infile, status );
        smf_addto_smfArray( flatfiles, outfile, status );
      }
    }
  }

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


  if ( *status == SAI__OK ) {

    /* Get reference subarray */
    smf_find_subarray( (flatfiles->sdata)[0]->hdr, subarray, sizeof(subarray), &subnum, status );
    if (*status != SAI__OK) goto CLEANUP;

    /* Check row vs column count */
    if ( ((flatfiles->sdata)[0]->dims)[SC2STORE__COL_INDEX] != (size_t)ncols ||
         ((flatfiles->sdata)[0]->dims)[SC2STORE__ROW_INDEX] != (size_t)nrows ) {
      *status = SAI__ERROR;
      msgSeti( "RC", ncols );
      msgSeti( "RR", nrows );
      msgSeti( "DC", ((flatfiles->sdata)[0]->dims)[SC2STORE__COL_INDEX]);
      msgSeti( "DR", ((flatfiles->sdata)[0]->dims)[SC2STORE__ROW_INDEX]);
      errRep( " ", "Dimensions of subarray from resistor file (^RC x ^RR)"
              " do not match those of data file (^DC x ^DR)", status );
      goto CLEANUP;
    }

    /* check that we are all from the same observation and same subarray */
    for (i = 1; i < flatfiles->ndat; i++) {
      int nsub;

      if (strcmp( (flatfiles->sdata)[0]->hdr->obsidss,
                  (flatfiles->sdata)[i]->hdr->obsidss ) != 0 ) {
        *status = SAI__ERROR;
        errRep(" ", "Flatfield can not be calculated from multiple observations",
               status);
        goto CLEANUP;
      }

      smf_find_subarray( (flatfiles->sdata)[i]->hdr, NULL, 0, &nsub, status );
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
    pixheat = smf_malloc( flatfiles->ndat, sizeof(*pixheat), 0, status );

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
    for (i = 1; i < nflatfiles; i+=2) {
      double heater;
      double ref1;
      double ref2;

      /* get the pixel heater settings and make sure they are consistent */
      smf_fits_getD( (flatfiles->sdata)[i]->hdr, "PIXHEAT", &heater,
                     status );

      msgSetd( "PX", heater );
      msgOutif( MSG__NORM, " ", "Processing heater setting ^PX", status );

      /* Get reference */
      smf_fits_getD( (flatfiles->sdata)[i-1]->hdr, "PIXHEAT", &ref1,
                     status );
      smf_fits_getD( (flatfiles->sdata)[i+1]->hdr, "PIXHEAT", &ref2,
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
      smf_subtract_dark( (flatfiles->sdata)[i], (flatfiles->sdata)[i-1],
                         (flatfiles->sdata)[i+1], SMF__DKSUB_MEAN, status);

      /* Store the frame for later */
      smf_addto_smfArray( bbhtframe, (flatfiles->sdata)[i], status );

      pixheat[bbhtframe->ndat - 1] = heater;

    }

    /* We now have data for the various pixel heater settings.
       Generate a set of reference heater power settings in pW, and calculate the
       expected measurement from each bolometer at each power setting.
     */
    smf_flat_standardpow( bbhtframe, refohms, pixheat, resistance,
                          &powref, &bolref, status );

    /* See if we want to use TABLE or POLYNOMIAL mode */
    parChoic( "METHOD", "POLYNOMIAL", "POLYNOMIAL, TABLE", 1,
              method, sizeof(method), status );

    /* only need to do something if we have a POLYNOMIAL
       since TABLE is what we get straight out of standardpow */
    if (strcmp( method, "POLYNOMIAL" ) == 0 ) {
      int order = 1;
      smfData * coeffs = NULL;

      /* need an order for the polynomial */
      parGdr0i( "ORDER", 1, 1, 3, 1, &order, status );

      smf_flat_fitpoly ( powref, bolref, order, &coeffs,
                         &flatpoly, status );

      /* now coeffs is in fact the new bolval */
      if (*status == SAI__OK && coeffs) {
        smf_close_file( &bolref, status );
        bolref = coeffs;
      }

    } else {
      istable = 1;
    }


    /* See if we need an output file for responsivities or some temporary
       memory */
    if (*status == SAI__OK) {
      Grp *rgrp = NULL;
      size_t rsize = 0;
      smfData *refdata = (bbhtframe->sdata)[0];

      kpg1Wgndf( "RESP", NULL, 1, 1, "", &rgrp, &rsize, status );

      if (*status == PAR__NULL) {
        rgrp = NULL;
        errAnnul( status );
      }
      /* Create the file on disk or malloc it as required.
         (units will normalise so no need for prefix) */
      smf_create_bolfile( rgrp, 1, refdata, "Responsivity",
                          "A/W", 0, &respmap, status );
      if (rgrp) grpDelet( &rgrp, status );
    }


    /* Calculate the responsivity in Amps/Watt (using the supplied
       signal-to-noise ratio minimum */
    parGet0d( "SNRMIN", &snrmin, status );
    ngood = smf_flat_responsivity( method, respmap, snrmin, 1, powref, bolref,
                                   (istable ? &flatpoly : NULL), status );

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
          for (j=0; j<(bolref->dims)[2]; j++) {
            double * dpntr = (bolref->pntr)[0];
            double * vpntr = (bolref->pntr)[1];
            if (dpntr[j*nbols+i] != VAL__BADD) {
              dpntr[j*nbols+i] = VAL__BADD;
              if (vpntr) vpntr[j*nbols+i] = VAL__BADD;
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
    if (ddata) {
      smf_fits_getI( ddata->hdr, "UTDATE", &utdate, status );
      smf_fits_getI( ddata->hdr, "OBSNUM", &obsnum, status );
      snprintf( defname, sizeof(defname), "%s%08d_%05d_flat",
                subarray, utdate, obsnum );
    } else {
      defname[0] = '\0';
    }
    parDef0c( "OUT", defname, status );
    kpg1Wgndf( "OUT", NULL, 1, 1, "", &flatgrp, &flatsize, status);
    pname = flatname;
    grpGet( flatgrp, 1, 1, &pname, sizeof(flatname), status );
    if (flatgrp) grpDelet( &flatgrp, status );

    /* write out the flatfield */
    smf_flat_write( method, flatname, bbhtframe, pixheat, powref, bolref, flatpoly, igrp, status );

    if (respmap) {
      /* write the provenance at the end since we have some problems with A-tasks
         in the pipeline causing trouble if the OUT parameter has not yet been set */
      if (respmap->file) smf_accumulate_prov( NULL, igrp, 1, respmap->file->ndfid, "SMURF:CALCFLAT", status );
      smf_close_file( &respmap, status );
    }

  }

  /* Tidy up after ourselves: release the resources used by the grp routines  */
 CLEANUP:
  if (bbhtframe) smf_close_related( &bbhtframe, status );
  if (darks) smf_close_related( &darks, status );
  if (flatfiles) smf_close_related( &flatfiles, status );
  if (igrp) grpDelet( &igrp, status);
  if (ogrp) grpDelet( &ogrp, status);
  if (fgrp) grpDelet( &fgrp, status);
  if (dkgrp) grpDelet( &dkgrp, status );
  if (resistance) resistance = smf_free( resistance, status );
  if (pixheat) pixheat = smf_free( pixheat, status );
  if (bolref) smf_close_file( &bolref, status );
  if (powref) smf_close_file( &powref, status );
  if (flatpoly) smf_close_file( &flatpoly, status );

  ndfEnd( status );
}

