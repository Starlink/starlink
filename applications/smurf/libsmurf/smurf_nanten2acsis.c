/*
*+
*  Name:
*     NANTEN2ACSIS

*  Purpose:
*     Convert NANTEN2 FITS format data files to an ACSIS format NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_nanten2acsis( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Opens NANTEN2 SDFITS-style files for reading, and writes out the spectra
*     in ACSIS format. Metadata are converted to appropriate
*     FITS headers. The NANTEN2 spectra must have been calibrated.

*  ADAM Parameters:
*     DIRECTORY = _CHAR (Read)
*          Directory for output ACSIS files. A NULL value will use the
*          current working directory. This command will create a subdir
*          in this directory named after the observation number.
*     IN = GROUP (Read)
*          Name of the input NANTEN2 FITS files to be converted.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]

*  Related Applications:
*     SMURF: MAKECUBE, GSD2ACSIS, SUPERCAM2ACSIS;

*  Notes:
*     - Whilst this command does a reasonable job of converting common
*     data to ACSIS format it still has to undergo extensive testing
*     to ensure that it is always doing the correct thing.
*     - The ORAC-DR recipe defaults to REDUCE_SCIENCE.
*     - This routine works with NANTEN data exported from CLASS as a single
*     SDFITS-style file per observation with the data in a binary table.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  History:
*     2014-04-01 (TIMJ):
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
#include "libnanten/nanten.h"
#include "libacsis/specwrite/specwrite.h"

#define FUNC_NAME "smurf_nanten2acsis"
#define TASK_NAME "NANTEN2ACSIS"

#define NBEAM 12
#define MAXNAME 132

/* Number of subsystems always 1 */
#define NSUBSYS 1

static void nanten__close_ts ( AstFitsChan * fc, int * status );

void smurf_nanten2acsis( int *status ) {

  /* Local variables */
  char directory[MAXNAME];        /* directory to write the file */

  size_t nfiles = 0;

  dim_t i;
  Grp *grp = NULL;
  Grp *grp2 = NULL;
  size_t ifile = 0;
  int added = 0;
  int flag = 0;
  AstFitsChan * outhdr = NULL;    /* Collated header to use for output file */


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

  /* Read each file in turn.
     Assume each file is a new observation and that we are reading
     a single SDFITS-style file per observation.
   */


  for (ifile = 1; ifile <= nfiles; ifile++ ) {
    char filename[GRP__SZNAM+1];   /* name of FITS file to read */
    char * pname;                  /* Temporary pointer to name */
    int hduType = 0;               /* Type code for selected HDU */
    int fitsStatus = 0;            /* CFITSIO status - used by CALLCFITSIO macro) */
    fitsfile *fits = NULL;         /* CFITSIO handle */
    AstFitsChan *fitschan = NULL;  /* FITS header from this file */
    AstFitsChan *acsisfits = NULL; /* FITS header from this file in ACSIS format */
    float * data = NULL;           /* Spectral data */
    //    SupercamSpecHdr * hdr = NULL;  /* Per spectrum header information */

    char ** recepnames = NULL; /* All the receptor names for this observation */
    dim_t nrecep = 0;             /* Number of active receptors */
    AstKeyMap * detectors = NULL;
    long nRows = 0;
    long nChans = 0;          /* Number of channels from this file */
    int isopen = 0;                 /* Is an output data file open? */

    pname = filename;
    grpGet( grp2, ifile, 1, &pname, GRP__SZNAM, status );

    msgOutiff(MSG__NORM," ",
              "Opening NANTEN2 file %s", status, filename);

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

    /* Get the number of rows aka number of output spectra */
    CALLCFITSIO( fits_get_num_rows(fits, &nRows, &fitsStatus),
                 "Error getting number of rows" );


    /* Read the HDU and store it in a FitsChan for easy parsing  */
    /* Read early so that we can get the observation number */
    fitschan = astFitsChan ( NULL, NULL, " " );
    smfftsio_read_fitshdr( fits, fitschan, status );

    /* Need to generate an output header that is derived from data
       from the first and last row in the file */
    {
      int rownum; /* Row 1 and then Row 0 */
      /* The AST CLASS encoding detection requires that we have DELTAV
         and IMAGFREQ so we need to ensure that if those items are in a time
         varying column that we retain the first value */
      double imagfreq = VAL__BADD;
      double deltav = VAL__BADD;
      for (rownum = 1; rownum >= 0; rownum-- ) {
        AstFitsChan *rowhdr = NULL;
        AstFitsChan *mergedhdr = NULL; /* Header merge of HDU and table row */

        rowhdr = astFitsChan ( NULL, NULL, " " );
        smfftsio_table_to_fitschan( fits, rownum, rowhdr, status );

        /* First time through we store IMAGFREQ / DELTAV and then we put it back */
        if ( rownum == 1 ) {
          astGetFitsF( rowhdr, "IMAGFREQ", &imagfreq );
          astGetFitsF( rowhdr, "DELTAV", &deltav );
        } else {
          if (imagfreq != VAL__BADD) atlPtftd( rowhdr, "IMAGFREQ", imagfreq, "", status );
          if (deltav != VAL__BADD) atlPtftd( rowhdr, "DELTAV", deltav, "", status );
        }

        /* Augment the standard FITS header with the table data to allow
           us to seed the header with the start and end information */
        atlMgfts( 4, fitschan, rowhdr, &mergedhdr, status );
        rowhdr = astAnnul(rowhdr);

        /* Form preliminary ACSIS header from this header. Some data has to come
           from the table rows. */
        acsisfits = astFitsChan( NULL, NULL, " " );
        nanten_fill_acsis_hdr( mergedhdr, acsisfits, status );

        /* Merge into the output header */
        smf_fits_outhdr( acsisfits, &outhdr, status );
        mergedhdr = astAnnul(mergedhdr);
      }
    }

    /* We need to work out how many detectors we have and what they are called. We have
       to go through the table retrieving the receptor names and storing them in a hash
    */
    if (*status == SAI__OK) {
      int colnum = 0;
      long rownum = 0;
      float * fplanex = NULL;        /* Focal plany X coordinates */
      float * fplaney = NULL;        /* Focal plane Y coordinates */

      detectors = astKeyMap( "SortBy=KeyUp,KeyCase=0" );

      #pragma GCC diagnostic ignored "-Wcast-qual"
      CALLCFITSIO( fits_get_colnum(fits, CASEINSEN, (char*)"TELESCOP", &colnum, &fitsStatus),
                   "Error getting number of TELESCOP column" );
      #pragma GCC diagnostic pop

      for (rownum=1; rownum<=nRows; rownum++) {
        char telname[12];
        char *svalue[1];
        char nullarray[1];
        int anynul[1];
        svalue[0] = telname;

        CALLCFITSIO( fits_read_colnull( fits, TSTRING, colnum, rownum, 1, 1,
                                        svalue, nullarray, anynul, &fitsStatus),
                     "Error reading TELESCOP row" );
        astMapPut0I(detectors, telname, 1, "" );
        //        printf("Storing %s\n",telname);
      }

      /* Allocate some memory for the receptor names */
      nrecep = astMapSize(detectors);
      recepnames = astCalloc( nrecep+1, sizeof(*recepnames) );

      /* Focal plane coordinates */
      fplanex = astCalloc( nrecep, sizeof(*fplanex) );
      fplaney = astCalloc( nrecep, sizeof(*fplaney) );

      for (i=0; i<nrecep; i++) {
        const char * key = NULL;
        const dim_t buflen = 9;
        fplanex[0] = 0.0;
        fplaney[0] = 0.0;
        key = astMapKey( detectors, (int) i );
        printf("Key %d = %s\n", (int)i, key);
        /* we know that these will be NANTEN2-xxx */
        recepnames[i] = astCalloc( buflen, sizeof(*recepnames) );
        one_strlcpy( recepnames[i], &(key[8]), buflen, status );

        /* Set the VALUE in the hash to be the index number
           so we can remember it */
        astMapPut0I( detectors, key, (int) i, "" );
        //        printf("Storing %d in slot %s [%s]\n", (int)i, key, recepnames[i] );

      }
      astShow(detectors);
      recepnames[nrecep] = NULL;

      /* Now open the ACSIS data file */
      if (*status == SAI__OK) {
        int obsnum;
        int utdate;
        astGetFitsI( outhdr, "OBSNUM", &obsnum );
        astGetFitsI( outhdr, "UTDATE", &utdate );

        acsSpecOpenTS( directory, utdate, obsnum, nrecep, NSUBSYS,
                       recepnames, "DIRECT", fplanex, fplaney,
                       "<OCSCONFIG />", status );
        if (*status == SAI__OK) isopen = 1;
      }

      /* No longer need the focal plane coordinates */
      astFree(fplanex);
      astFree(fplaney);

    }

    /* Work out how many channels there are and then write each spectrum */
    if (*status == SAI__OK) {
      int colnum = 0;
      long dummy = 0;
      long rownum = 0;
      AstFitsChan * rowhdr = NULL;
      AstKeyMap * spectracker = NULL;
      AstFrameSet * wcsframe = NULL;
      float * specdata = NULL;
      int typecode = 0;
      double cdelt2;
      double cdelt3;

      #pragma GCC diagnostic ignored "-Wcast-qual"
      CALLCFITSIO( fits_get_colnum(fits, CASEINSEN, (char*)"SPECTRUM", &colnum, &fitsStatus ),
                   "Error getting SPECTRUM column number");
      #pragma GCC diagnostic pop
      CALLCFITSIO( fits_get_coltype(fits, colnum, &typecode, &nChans, &dummy, &fitsStatus ),
                   "Error getting SPECTRUM column information" );

      specdata = astCalloc( nChans, sizeof(*specdata) );

      /* Get a WCS from the FITS header for the spatial offseting calculations.
         We are assuming that the offsets are projected offsets and not angular offsets.
         It would be more efficient to read in the entire CDELT2 and CDELT3 columns
         and transform them all at once.
      */
      if (*status == SAI__OK) {
        /* Copy the FITS chan so we can be destructive */
        AstFitsChan * tmpfs = NULL;
        AstFrameSet * tmpwcs = NULL;

        tmpfs = astCopy( outhdr );
        astClear(tmpfs, "Card"); /* Rewind */
        tmpwcs = astRead( tmpfs );
        if (tmpwcs == AST__NULL && *status == SAI__OK) {
          *status = SAI__ERROR;
          errRep("", "Could not extract WCS from FITS HDU",
                 status );
        }

        /* Get the SKY part of the FrameSet */
        wcsframe = atlFrameSetSplit( tmpwcs, "SKY", NULL, NULL, status );

        astAnnul(tmpfs);
        astAnnul(tmpwcs);
      }

      if (*status == SAI__OK) {
        smfHead ihdr;
        ihdr.fitshdr = outhdr;
        smf_fits_getD( &ihdr, "CDELT2", &cdelt2, status );
        smf_fits_getD( &ihdr, "CDELT3", &cdelt3, status );
      }


      /* Now we have to loop over each row in the table to,
         get the spectrum and store it in the output */

      rowhdr = astFitsChan ( NULL, NULL, " " );
      spectracker = astKeyMap( "KeyCase=0" );
      for (rownum = 1; rownum <= nRows; rownum++ ) {
        JCMTState record;
        ACSISSpecHdr spechdr;
        smfHead ihdr;
        smfHead rhdr;
        double utoffset;
        double obstime;
        double mjdobs;
        double dvalue;
        char detname[12];
        int detnum;
        char trackkey[20];
        int samplecounter = 0;

        memset(&record, 0, sizeof(record));
        memset(&spechdr, 0, sizeof(spechdr));
        memset(&ihdr, 0, sizeof(ihdr));
        memset(&rhdr, 0, sizeof(rhdr));

        /* For header retrieval routines - these should all be done outside the loop */
        ihdr.fitshdr = outhdr;

        /* Get the scalar items */
        astEmptyFits( rowhdr );
        smfftsio_table_to_fitschan( fits, rownum, rowhdr, status );
        rhdr.fitshdr = rowhdr;

        /* JCMT State information */
        strcpy( record.rts_tasks, "SMART" );

        /* The UT column tells us the time stamp in seconds of the first
           sample in the row. We need to track how many we have had for this
           detector + UT combination */
        smf_fits_getS( &rhdr, "TELESCOP", detname, sizeof(detname), status );
        smf_fits_getD( &rhdr, "UT", &utoffset, status );
        one_snprintf( trackkey, sizeof(trackkey), "%s-%d", status, detname, (int)utoffset );

        /* samplecounter is 0 for first spectrum in row */
        if ( astMapHasKey( spectracker, trackkey ) ) {
          astMapGet0I( spectracker, trackkey, &samplecounter );
          samplecounter++;
        } else {
          samplecounter = 0;
        }
        astMapPut0I( spectracker, trackkey, samplecounter, "Number of samples in row" );

        /* We assume the rts counter is samplecounter + utoffset */
        record.rts_num = (unsigned int)floor(utoffset) + samplecounter;

        /* Now calculate the DATE-OBS for this spectrum and convert to TAI */
        smf_find_dateobs( &ihdr, &mjdobs, NULL, status );

        /* This header has been corrected for the UT column but we need to re-add
           that offset to be consistent */
        mjdobs = floor(mjdobs);

        /* Correct by number of integrations. Include an extra samplecounter
           as we asume the UT column is referring to
           the start of integration. */
        smf_fits_getD( &ihdr, "OBSTIME", &obstime, status );
        utoffset += (samplecounter + 1) * obstime;

        /* Correct for UT offset -- end of sample */
        mjdobs += utoffset / SPD;

        { /* to TAI using a TimeFrame */
          AstTimeFrame * tf = astTimeFrame( "TimeScale=UTC, TimeOrigin=%.*g", DBL_DIG, mjdobs );
          astSet(tf, "TimeScale=TAI" );
          record.rts_end = astGetD( tf, "TimeOrigin" );
          record.tcs_tai = record.rts_end;
          tf = astAnnul(tf);
        }

       /* Base position comes out of wcsframe: TRACKING */
        {
          AstFrameSet * tmpwcs = NULL;
          double coords[2];
          coords[0] = astGetD( wcsframe, "SkyRef(1)");
          coords[1] = astGetD( wcsframe, "SkyRef(2)");
          astNorm(wcsframe, coords);
          record.tcs_tr_bc1 = coords[0];
          record.tcs_tr_bc2 = coords[1];


          /* We do not actually know where the telescope is pointing
             since we are only given the coordinates of the individual receptors */

          /* Just ask for the AZEL from the FrameSet */
          tmpwcs = astCopy( wcsframe );
          astSet( tmpwcs, "system=AZEL" );

          /* Base position: AZEL */
          coords[0] = astGetD( tmpwcs, "SkyRef(1)");
          coords[1] = astGetD( tmpwcs, "SkyRef(2)");
          astNorm(tmpwcs, coords);
          record.tcs_az_bc1 = coords[0];
          record.tcs_az_bc2 = coords[1];

          astAnnul(tmpwcs);
        }


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

        strcpy( record.tcs_tr_sys, "J2000" );
        strcpy( record.tcs_source, "SCIENCE" );
        strcpy( record.tcs_beam, "M" );
        strcpy( record.acs_source_ro, "SPECTRUM_RESULT" );

        /* No polarimeter or FTS */
        record.pol_ang = VAL__BADD;
        record.fts_pos = VAL__BADR;

        record.acs_exposure = obstime;
        record.acs_offexposure = obstime;

        /* Spectrum information */

        spechdr.rts_endnum = nRows;

        smf_fits_getD( &rhdr, "TSYS", &dvalue, status );
        spechdr.acs_tsys = dvalue;

        /* Feed number comes from KeyMap */
        astMapGet0I( detectors, detname, &detnum );
        spechdr.acs_feed = detnum;

        /* Coordinates of receptor */
        if (*status == SAI__OK) {
          double xin[1];             /* Space to hold input X coordinates */
          double yin[1];             /* Space to hold input Y coordinates */
          double xout[1];            /* Space to hold output X coordinates */
          double yout[1];            /* Space to hold output Y coordinates */
          double thiscdelt2;
          double thiscdelt3;
          double coords[2];

          smf_fits_getD( &rhdr, "CDELT2", &thiscdelt2, status );
          smf_fits_getD( &rhdr, "CDELT3", &thiscdelt3, status );

          xin[0] = thiscdelt2 / cdelt2;
          yin[0] = thiscdelt3 / cdelt3;

          astTran2( wcsframe, 1, xin, yin, 1, xout, yout );
          coords[0] = xout[0];
          coords[1] = yout[0];
          astNorm(wcsframe, coords );

          /* Coordinates are in degrees */
          spechdr.acs_feedx = coords[0] * AST__DR2D;
          spechdr.acs_feedy = coords[1] * AST__DR2D;

        }

        /* Get the spectrum */
        CALLCFITSIO( fits_read_col( fits, TFLOAT, colnum, rownum, 1,
                                    nChans, NULL, specdata, NULL, &fitsStatus ),
                     "Error reading spectrum from Binary column SPECTRUM" );

        /* Clean up the data a bit */
        if (*status == SAI__OK) {
          long n;
          for (n=0; n<nChans; n++) {
            if (!isfinite(specdata[n]) ||
                fabs(specdata[n]) > (float)(VAL__MAXI-1)) {
              specdata[n] = VAL__BADR;
            }
          }
        }


        /* Write out the spectrum */
        acsSpecWriteTS( 1 /* Subsystem */, nChans, specdata,
                        &record, &spechdr, outhdr, status );

      }

      /* clean up */
      rowhdr = astAnnul( rowhdr );
      specdata = astFree(specdata);
      if (wcsframe) wcsframe = astAnnul( wcsframe );

    }

    astShow(outhdr);

    //    astShow(fitschan);

  loopcleanup:
    data = astFree(data);

    if (detectors) detectors = astAnnul(detectors);
    if (recepnames) {
      i = 0;
      while (recepnames[i]) {
        recepnames[i] = astFree(recepnames[i]);
        i++;
      }
      recepnames = astFree(recepnames);
    }
    if (fitschan) fitschan = astAnnul( fitschan );
    if (acsisfits) acsisfits = astAnnul( acsisfits );

    /* Close the data file -- pass in the FITS header */
    if (isopen) {
      nanten__close_ts( outhdr, status );
      isopen = 0;
    }

    /* Always try to close the file */
    fits_close_file( fits, &fitsStatus );

    if (*status != SAI__OK) break;
  }

  if ( *status == SAI__OK ) {
    msgOutif(MSG__VERB," ",
	     "Conversion completed successfully", status);
  }

  //  astShow(outhdr);

 cleanup:
  grpDelet(&grp2, status);
  outhdr = astAnnul( outhdr );
}

/* Helper function to close a time series file */

static void nanten__close_ts ( AstFitsChan * fc, int * status ) {
  AstFitsChan * allfits[NSUBSYS];
    char flagfile[132];
  if (*status != SAI__OK) return;
  allfits[0] = fc;
  acsSpecCloseTS( allfits, 0, sizeof(flagfile),
                  flagfile, status );
}

