/*
*+
*  Name:
*     SUPERCAM2ACSIS

*  Purpose:
*     Convert a Supercam SDFITS format data file to an ACSIS format NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_supercam2acsis( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Opens Supercam SDFITS files for reading, and writes out the spectra
*     in ACSIS format. Metadata are converted to appropriate
*     FITS headers. The Supercam spectra must have been calibrated.

*  ADAM Parameters:
*     DIRECTORY = _CHAR (Read)
*          Directory for output ACSIS files. A NULL value will use the
*          current working directory. This command will create a subdir
*          in this directory named after the observation number.
*     IN = GROUP (Read)
*          Name of the input SDFITS files to be converted.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]

*  Related Applications:
*     SMURF: MAKECUBE, GSD2ACSIS;

*  Notes:
*     - Whilst this command does a reasonable job of converting common
*     data to ACSIS format it still has to undergo extensive testing
*     to ensure that it is always doing the correct thing.
*     - The ORAC-DR recipe defaults to REDUCE_SCIENCE.
*     - SUPERCAM data are written as one SDFITS file for every on-the-fly
*       spectrum. Each file has one spectrum from each of the 64 receptors.
*       A full observation can therefore consist of many files.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  History:
*     2014-03-19 (TIMJ):
*        Initial version.

*  Copyright:
*     Copyright (C) 2014 Cornell University
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/


#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Standard includes */
#include <string.h>
#include <stdio.h>
#include <math.h>

/* STARLINK includes */
#include "mers.h"
#include "sae_par.h"
#include "par.h"
#include "star/one.h"
#include "ast.h"
#include "fitsio.h"
#include "star/grp.h"
#include "star/palmac.h"
#include "star/atl.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "smurflib.h"
#include "jcmt/state.h"
#include "libsmfftsio/smfftsio.h"
#include "libsupercam/supercam.h"
#include "libacsis/specwrite/specwrite.h"

#define FUNC_NAME "smurf_supercam2acsis"
#define TASK_NAME "SUPERCAM2ACSIS"

#define MAXNAME 132
#define NBEAM 64      /* Number of Supercam beams */

/* Number of subsystems always 1 */
#define NSUBSYS 1

static void supcam__close_ts ( AstFitsChan * fc, int * status );

#pragma GCC diagnostic ignored "-Wcast-qual"

void smurf_supercam2acsis( int *status ) {

  /* Local variables */
  char directory[MAXNAME];        /* directory to write the file */


  size_t nBeam = NBEAM;           /* Number of beams in receiver array */
  size_t numRows = 0;
  size_t numChans = 0;

  size_t nfiles = 0;

  size_t i;
  Grp *grp = NULL;
  Grp *grp2 = NULL;
  size_t ifile = 0;
  int added = 0;
  int flag = 0;
  AstFitsChan * outhdr = NULL;    /* Collated header to use for output file */
  int * recepmask = NULL;         /* Receptor mask: 1 used 0 unused for each receptor */
  AstFrameSet * wcsframe = NULL;  /* For conversion of offset to absolute coordinates */
  double * xin = NULL;            /* Space to hold input X coordinates */
  double * yin = NULL;            /* Space to hold input Y coordinates */
  double * xout = NULL;           /* Space to hold output X coordinates */
  double * yout = NULL;           /* Space to hold output Y coordinates */

  dim_t obsnum = 0;               /* Current observation number */
  int isopen = 0;                 /* Is an output data file open? */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the input FITS file names. We need to get them as a Grp
     since a single observation consists of multiple FITS files. */
  grp = grpNew( "FITS files", status );
  grpGroup( "IN", NULL, grp, &nfiles, &added, &flag, status );

  /* This grp will probably include file wildcards so we have to expand those
     to get a full list of input files */
  grp2 = smf_expand_filegroup( grp, status );
  grpDelet( &grp, status );

  nfiles = grpGrpsz( grp2, status );
  msgOutiff(MSG__NORM, "", "Found %zu file%s to read", status,
            nfiles, (nfiles == 1 ? "" : "s") );

  /* Allow "!" to indicate the current directory */
  if ( *status != SAI__OK ) return;
  parGet0c ( "DIRECTORY", directory, MAXNAME, status );
  if ( *status == PAR__NULL ) {
    errAnnul ( status );
    one_strlcpy ( directory, ".", sizeof(directory), status );
  }
  if ( *status != SAI__OK ) goto cleanup;

  /* Read each Supercam file in turn */
  /* Be cognizant of observation number changes so that we can start a new
     observation correctly */

  for (ifile = 1; ifile <= nfiles; ifile++ ) {
    char filename[GRP__SZNAM+1];   /* name of FITS file to read */
    char * pname;                  /* Temporary pointer to name */
    int hduType = 0;               /* Type code for selected HDU */
    int fitsStatus = 0;            /* CFITSIO status - used by CALLCFITSIO macro) */
    fitsfile *fits = NULL;         /* CFITSIO handle */
    AstFitsChan *fitschan = NULL;  /* FITS header from this file */
    AstFitsChan *acsisfits = NULL; /* FITS header from this file in ACSIS format */
    float * data = NULL;           /* Spectral data */
    SupercamSpecHdr * hdr = NULL;  /* Per spectrum header information */
    size_t thisRows = 0;           /* Number of rows from this file */
    size_t thisChans = 0;          /* Number of channels from this file */
    dim_t thisobsnum = 0;          /* Observation number of this file */
    const char **recepnames = NULL;/* All the receptor names for this observation */
    dim_t nrecep = 0;              /* Number of active receptors */
    float * fplanex = NULL;        /* Focal plany X coordinates */
    float * fplaney = NULL;        /* Focal plane Y coordinates */
    JCMTState record;              /* Per-time slice information */

    pname = filename;
    grpGet( grp2, ifile, 1, &pname, GRP__SZNAM, status );

    msgOutiff(MSG__NORM," ",
              "Opening SuperCam file %s", status, filename);

    /* Open the FITS file */
    fitsStatus = 0;
    CALLCFITSIO(fits_open_file( &fits, filename, READONLY, &fitsStatus ), NULL);
    if (*status != SAI__OK) {
      errRepf("", "Could not open FITS file %s", status, filename);
      break; /* Nothing to clean up */
    }

    /* Move to the binary table extension */
    CALLCFITSIO( fits_movabs_hdu( fits, 2, &hduType, &fitsStatus ), "Error moving to spectral HDU");
    if ( *status == SAI__OK && hduType != BINARY_TBL ) {
      *status = SAI__ERROR;
      errRepf("", "Bad FITS file %s: No Binary Table Extension", status, filename);
      goto loopcleanup;
    }

    /* Read the HDU and store it in a FitsChan for easy parsing  */
    /* Read early so that we can get the observation number */
    fitschan = astFitsChan ( NULL, NULL, " " );
    smfftsio_read_fitshdr( fits, fitschan, status );

    /* Form ACSIS header from this Supercam header */
    acsisfits = astFitsChan( NULL, NULL, " " );
    supcam_fill_acsis_hdr( fitschan, acsisfits, status );

    {
      int itemp;
      astGetFitsI( acsisfits, "OBSNUM", &itemp);
      thisobsnum = itemp;
    }

    if (obsnum != thisobsnum) {
      /* Change in observation number so need to clean up previous
         data */
      if (isopen) {
        supcam__close_ts( outhdr, status );
        isopen = 0;
        /* Free all resources (just in case receptor count changes between observations) */
        outhdr = astAnnul( outhdr );
        recepmask = astFree( recepmask );
        recepnames = astFree( recepnames );
        fplanex = astFree( fplanex );
        fplaney = astFree( fplaney );
        xin = astFree( xin );
        yin = astFree( yin );
        xout = astFree( xout );
        yout = astFree( yout );
        numRows = 0;
        numChans = 0;
        wcsframe = astAnnul(wcsframe);
      }

      /* Now ensure that we know what the current observation number is */
      obsnum = thisobsnum;

    }

    /* Read the data */
    data = supcam_read_data( fits, &thisRows, &thisChans, status );
    if (*status == SAI__OK && thisRows != nBeam) {
      *status = SAI__ERROR;
      errRepf("", "%s has %zu rows/detectors but expected %zu", status, filename, thisRows, nBeam);
      goto loopcleanup;
    }

    /* If this is the first time through we have to store the row/channel count
       for comparison with subsequent files */
    if (*status == SAI__OK) {
      if ( !isopen ) {
        numRows = thisRows;
        numChans = thisChans;

        /* Allocate memory for the receptor mask */
        recepmask = astCalloc( numRows, sizeof(*recepmask) );

        /* Allocate memory for the receptor names */
        recepnames = astCalloc( numRows, sizeof(*recepnames) );

        /* Focal plane coordinates */
        fplanex = astCalloc( numRows, sizeof(*fplanex) );
        fplaney = astCalloc( numRows, sizeof(*fplaney) );

        /* Storage for coordinate transforms */
        xin = astCalloc( numRows, sizeof(*xin) );
        yin = astCalloc( numRows, sizeof(*yin) );
        xout = astCalloc( numRows, sizeof(*xout) );
        yout = astCalloc( numRows, sizeof(*yout) );

      } else {
        if (numRows != thisRows) {
          *status = SAI__ERROR;
          errRepf( "", "%s has different detector count to previous file(s) [%zu != %zu]",
                   status, filename, thisRows, numRows);
        }
        if (numChans != thisChans && *status == SAI__OK ) {
          *status = SAI__ERROR;
          errRepf( "", "%s has different channel count to previous file(s) [%zu != %zu]",
                   status, filename, thisChans, numChans);
        }
      }
    }

    /* Get per-row (aka spectra aka receptor) information.
       We want offra, offdec, TSYS and exposure time with one number per row. */
    hdr = supcam_read_tabmetadata( fits, numRows, status );

    /* Now merge into ongoing header */
    smf_fits_outhdr( acsisfits, &outhdr, status );

    /* If this is the first time through we want to create a WCS frameset
       to allow us to do the calculations for offsets */
    if ( !wcsframe ) {
      /* Copy the FITS chan so we can be destructive -- note we copy the
         ACSIS version as it has telescope position in it. */
      AstFitsChan * tmpfs = NULL;
      AstFrameSet * tmpwcs = NULL;

      tmpfs = astCopy( acsisfits );
      astClear(tmpfs, "Card"); /* Rewind */
      tmpwcs = astRead( tmpfs );
      if (tmpwcs == AST__NULL && *status == SAI__OK) {
        *status = SAI__ERROR;
        errRep("", "Could not extract WCS from table HDU",
               status );
      }

      /* Get the SKY part of the FrameSet */
      wcsframe = atlFrameSetSplit( tmpwcs, "SKY", NULL, NULL, status );

      astAnnul(tmpfs);
      astAnnul(tmpwcs);
    }

    /* Populate the ACSIS structure and determine how many active
       receptors we have. */

    if (*status != SAI__OK) goto loopcleanup;
    nrecep = 0;
    for (i = 0; i <numRows; i++) {
      SupercamSpecHdr * rowhdr = &(hdr[i]);
      int userecep = 0;
      if (
          rowhdr->ifpower > 0
          //rowhdr->inttime > 0.00001
          //         && rowhdr->tsys > rowhdr->trx
          ) {
        nrecep++;
        userecep = 1;
      }

      if (!isopen) {
        recepmask[i] = userecep;

        if (userecep) {
          recepnames[nrecep-1] = rowhdr->recepname;
          /* These coordinates are not known */
          fplanex[nrecep-1] = (float)(rowhdr->offx * PAL__DD2R * PAL__DR2AS);
          fplaney[nrecep-1] = (float)(rowhdr->offy * PAL__DD2R * PAL__DR2AS);
        }
      } else {
        if (recepmask[i] != userecep) {
          *status = SAI__ERROR;
          errRepf(" ", "Receptor %zu [aka %s] was in different state in this file from previous file",
                  status, i+1, rowhdr->recepname );
          goto loopcleanup;
        }
      }
    }

    msgOutiff( MSG__NORM, "", "File %s has %zu working receptors", status, filename, nrecep);

    /* Open the ACSIS file: acsSpecOpenTS
       We need observation number, UT date, receptor list,
       number of subsystems (always 1), receptor names,
       focal plane coordinate system, focal plane coordinates.
       The OCS config is always empty.
       We should probably only write active observations but we can not
       tell in advance that each file we open will only have the same
       receptors. We can guess that.
       Focal plane coordinates should probably be dummy to start
       with as the input files give the receptor position in
       sky coordinates.
     */
    if (!isopen) {
      int obsnum;
      int utdate;
      astGetFitsI( acsisfits, "OBSNUM", &obsnum );
      astGetFitsI( acsisfits, "UTDATE", &utdate );

      acsSpecOpenTS( directory, utdate, obsnum, nrecep, NSUBSYS,
                     (char**)recepnames, "DIRECT", fplanex, fplaney,
                     "<OCSCONFIG />", status );
      if (*status == SAI__OK) isopen = 1;
    }

    /* Fill in the JCMTState information */
    memset(&record, 0, sizeof(record));

    record.rts_num = ifile;

    { /* Need to convert to TAI */
      smfHead ihdr;
      double dateend;
      double inttime;
      memset(&ihdr, 0, sizeof(ihdr)); /* Need to null out jcmtstate info */
      ihdr.fitshdr = acsisfits;
      smf_find_dateobs( &ihdr, NULL, &dateend, status );
      AstTimeFrame * tf = astTimeFrame( "TimeScale=UTC, TimeOrigin=%.*g", DBL_DIG, dateend );
      astSet( tf, "TimeScale=TAI" );
      record.rts_end = astGetD( tf, "TimeOrigin" );
      tf = astAnnul(tf);

      /* Exposure time in seconds */
      ihdr.fitshdr = fitschan;
      smf_fits_getD( &ihdr, "OBSTIME", &inttime, status );

      /* Not exactly sure which is the relevant time for the TCS positions*/
      /* XXX */
      record.tcs_tai = dateend - (inttime / SPD );
      record.acs_exposure = inttime;
      record.acs_offexposure = inttime;  /* XXXX - guess */
    }

    strcpy( record.rts_tasks, "SUPERCAM" );

    record.smu_x = 0.0;
    record.smu_y = 0.0;
    record.smu_z = 0.0;
    strcpy( record.smu_chop_phase, " ");
    record.smu_jig_index = 0;
    record.smu_az_jig_x = 0.0;
    record.smu_az_jig_y = 0.0;
    record.smu_az_chop_x = 0.0;
    record.smu_az_chop_y = 0.0;
    record.smu_tr_jig_x = 0.0;
    record.smu_tr_jig_y = 0.0;
    record.smu_tr_chop_x = 0.0;
    record.smu_tr_chop_y = 0.0;

    strcpy( record.tcs_tr_sys, "GALACTIC" );
    strcpy( record.tcs_source, "SCIENCE" );
    strcpy( record.tcs_beam, "M" );
    strcpy( record.acs_source_ro, "SPECTRUM_RESULT" );

    /* Now we need to loop through the data array dumping all numRows spectra */
    /* For efficiency transform all the coordinates at once */
    {
      smfHead ihdr;
      dim_t counter = 0;
      double cdelt2;
      double cdelt3;

      /* Feed position is given as an offset in the CTYPE2/3 coordinate
         frame and projection. In our example GLON--GLS & GLAT--GLS. */
      /* I think this means that CDELTx and OFFRA/OFFDEC are offsets in
         GLS projection and scaling them by the CDELTx in the WCS header
         will convert them to "pixels" */
      ihdr.fitshdr = fitschan;
      smf_fits_getD( &ihdr, "CDELT2", &cdelt2, status );
      smf_fits_getD( &ihdr, "CDELT3", &cdelt3, status );

      for (i=0; i<numRows; i++) {
        SupercamSpecHdr * rowhdr = &(hdr[i]);
        xin[i] = (rowhdr->offx / cdelt2);
        yin[i] = (rowhdr->offy / cdelt3);
      }

      astTran2( wcsframe, numRows, xin, yin, 1, xout, yout );

      /*
      for (i=0;i<numRows;i++) {
        printf("%zu (%.*g, %.*g) -> (%.*g, %.*g)\n", i,
               DBL_DIG, xin[i], DBL_DIG, yin[i],
               DBL_DIG, xout[i], DBL_DIG, yout[i]
               );
      }
      */

      /* Coordinate of tracking centre comes from the fiducial receptor
         which happens to be in INSTAP. We now know its coordinate. */
      {
        char * stemp = NULL;
        size_t fidnum;
        double coords[2];
        AstFrameSet * tmpwcs = NULL;
        double azin[1];
        double azout[1];
        double elin[1];
        double elout[1];

        astGetFitsS( acsisfits, "INSTAP", &stemp );
        fidnum = supcam_name_to_beam_num( stemp, status );

        /* Actual position: TRACKING */
        coords[0] = xout[fidnum];
        coords[1] = yout[fidnum];
        astNorm(wcsframe, coords);
        record.tcs_tr_ac1 = coords[0];
        record.tcs_tr_ac2 = coords[1];
        record.tcs_tr_dc1 = coords[0];
        record.tcs_tr_dc2 = coords[1];

        /* Base position comes out of wcsframe: TRACKING */
        coords[0] = astGetD( wcsframe, "SkyRef(1)");
        coords[1] = astGetD( wcsframe, "SkyRef(2)");
        astNorm(wcsframe, coords);
        record.tcs_tr_bc1 = coords[0];
        record.tcs_tr_bc2 = coords[1];

        /* Just ask for the AZEL from the FrameSet */
        tmpwcs = astCopy( wcsframe );
        astSet( tmpwcs, "system=AZEL" );

        /* Actual position: AZEL */
        azin[0] = xin[fidnum];
        elin[0] = yin[fidnum];
        astTran2( tmpwcs, 1, azin, elin, 1, azout, elout );
        coords[0] = azout[0];
        coords[1] = elout[0];

        astNorm(tmpwcs, coords);
        record.tcs_az_ac1 = coords[0];
        record.tcs_az_ac2 = coords[1];
        record.tcs_az_dc1 = coords[0];
        record.tcs_az_dc2 = coords[1];

        /* Base position: AZEL */
        coords[0] = astGetD( tmpwcs, "SkyRef(1)");
        coords[1] = astGetD( tmpwcs, "SkyRef(2)");
        astNorm(tmpwcs, coords);
        record.tcs_az_bc1 = coords[0];
        record.tcs_az_bc2 = coords[1];

        astAnnul(tmpwcs);
      }

      /* No polarimeter or FTS */

      record.pol_ang = VAL__BADD;
      record.fts_pos = VAL__BADR;


      /* Now finally get each spectrum */

      for (i=0; i<numRows; i++) {
        SupercamSpecHdr * rowhdr = &(hdr[i]);
        ACSISSpecHdr spechdr;

        /* Only if this receptor is used */
        if (recepmask[i]) {
          double coords[2];

          counter++;

          /* We assume that there is a sequence per input file so this is just
             the total number of files */
          spechdr.rts_endnum = nfiles;
          spechdr.acs_feed = counter - 1;  /* 0-based */
          spechdr.acs_tsys = rowhdr->tsys;
          spechdr.acs_trx = rowhdr->trx;

          /* Normalise the coordinates for output */
          coords[0] = xout[i];
          coords[1] = yout[i];
          astNorm( wcsframe, coords );

          /* These have to be in degrees though */
          spechdr.acs_feedx = coords[0] * AST__DR2D;
          spechdr.acs_feedy = coords[1] * AST__DR2D;

          acsSpecWriteTS( 1 /* Subsystem */, numChans, &(data[i*numChans]),
                          &record, &spechdr, acsisfits, status );
        }
      }

    }



    //    astShow(fitschan);

  loopcleanup:
    data = astFree(data);
    hdr = astFree(hdr);
    fplanex = astFree(fplanex);
    fplaney = astFree(fplaney);
    if (recepnames) recepnames = astFree(recepnames);
    if (fitschan) fitschan = astAnnul( fitschan );
    if (acsisfits) acsisfits = astAnnul( acsisfits );

    /* Always try to close the file */
    fits_close_file( fits, &fitsStatus );

    if (*status != SAI__OK) break;
  }

  /* Close the data file -- pass in the FITS header */
  if (isopen) supcam__close_ts( outhdr, status );

  if ( *status == SAI__OK ) {
    msgOutif(MSG__VERB," ",
	     "Conversion completed successfully", status);
  }

  //  astShow(outhdr);

 cleanup:
  grpDelet(&grp2, status);
  outhdr = astAnnul( outhdr );
  wcsframe = astAnnul( wcsframe );
  recepmask = astFree( recepmask );
  xin = astFree(xin);
  yin = astFree(yin);
  xout = astFree(xout);
  yout = astFree(yout);
}

/* Helper function to close a time series file */

static void supcam__close_ts ( AstFitsChan * fc, int * status ) {
  AstFitsChan * allfits[NSUBSYS];
    char flagfile[132];
  if (*status != SAI__OK) return;
  allfits[0] = fc;
  acsSpecCloseTS( allfits, 0, sizeof(flagfile),
                  flagfile, status );
}
