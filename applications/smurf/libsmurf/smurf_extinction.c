/*
*+
*  Name:
*     EXTINCTION

*  Purpose:
*     Extinction correct SCUBA-2 data.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_extinction( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This application can be used to extinction correct data in a number
*     of ways.

*  ADAM Parameters:
*     BBM = NDF (Read)
*          Group of files to be used as bad bolometer masks. Each data file
*          specified with the IN parameter will be masked. The corresponding
*          previous mask for a subarray will be used. If there is no previous
*          mask the closest following will be used. It is not an error for
*          no mask to match. A NULL parameter indicates no mask files to be
*          supplied. [!]
*     CSOTAU = _REAL (Read)
*          Value of the 225 GHz zenith optical depth. Only used if
*          TAUSRC equals `CSOTAU' or 'AUTO'. If a NULL (!) value is given, the task
*          will use the appropriate value from the FITS header of each
*          file. Note that if a value is entered by the user, that
*          value is used for all input files. In AUTO mode the value
*          might not be used.
*     FILTERTAU = _REAL (Read)
*          Value of the zenith optical depth for the current
*          wavelength. Only used if TAUSRC equals `FILTERTAU'. Note that no
*          check is made to ensure that all the input files share the same
*          filter.
*     FLATMETH = _CHAR (Read)
*          Method to use to calculate the flatfield solution. Options
*          are POLYNOMIAL and TABLE. Polynomial fits a polynomial to
*          the measured signal. Table uses an interpolation scheme
*          between the measurements to determine the power. [POLYNOMIAL]
*     FLATORDER = _INTEGER (Read)
*          The order of polynomial to use when choosing POLYNOMIAL method.
*          [1]
*     FLATSNR = _DOUBLE (Read)
*          Signal-to-noise ratio threshold to use when filtering the
*          responsivity data to determine valid bolometers for the
*          flatfield. [3.0]
*     FLATUSENEXT = _LOGICAL (Read)
*          If true the previous and following flatfield will be used to
*          determine the overall flatfield to apply to a sequence. If false
*          only the previous flatfield will be used. A null default will
*          use both flatfields for data when we did not heater track
*          at the end, and will use a single flatfield when we did heater
*          track. The parameter value is not sticky and will revert to
*          the default unless explicitly over-ridden. [!]
*     HASSKYREM = _LOGICAL (Read)
*          Indicate that the data have been sky removed even if the
*          fact can not be verified. This is useful for the case where
*          the sky background has been removed using an application
*          other than SMURF REMSKY. [FALSE]
*     IN = NDF (Read)
*          Input file(s). The input data must have had the sky signal
*          removed
*     METHOD = _CHAR (Read)
*          Method to use for airmass calculation. Options are:
*          - ADAPTIVE  - Determine whether to use QUICK or FULL
*          based on the elevation of the source and the opacity.
*          - FULL      - Calculate the airmass of each bolometer.
*          - QUICK     - Use a single airmass for each time slice.
*
*          [ADAPTIVE]
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OUT = NDF (Write)
*          Output file(s)
*     OUTFILES = LITERAL (Write)
*          The name of text file to create, in which to put the names of
*          all the output NDFs created by this application (one per
*          line). If a null (!) value is supplied no file is created. [!]
*     RESIST = GROUP (Read)
*          A group expression containing the resistor settings for
*          each bolometer.  Usually specified as a text file using "^"
*          syntax. An example can be found in
*          $STARLINK_DIR/share/smurf/resist.cfg
*          [$STARLINK_DIR/share/smurf/resist.cfg]
*     RESPMASK = _LOGICAL (Read)
*          If true, responsivity data will be used to mask bolometer data
*          when calculating the flatfield. [TRUE]
*     TAUREL = GROUP (Read)
*          Specifies values to be used for scaling the 225 GHz tau
*          to the specific filter. These values will only be used if
*          CSOTAU or WVMRAW methods are used to determine the tau.
*          The group should have the form:
*
*             ext.taurelation.<FILT> = (a,b,c)
*
*          where <FILT> is the filter name and "a", "b" and "c" are the
*          coefficients for a relationship of the form
*
*             tau_filt = a ( tau_cso + b + c sqrt(tau_cso) )
*
*          A null value will use the default relations. [!]
*     TAUSRC = _CHAR (Read)
*          Source of optical depth data. Options are:
*          - WVMRAW    - use the Water Vapour Monitor time series data
*          - WVMFIT    - use a fit to the Water Vapor Monitor data
*          - CSOFIT    - use a fit to the CSO 225 GHz tau data
*          - CSOTAU    - use a single 225 GHz tau value
*          - FILTERTAU - use a single tau value for this wavelength
*          - AUTO      - Use WVM if available and reliable, else a WVM or CSO fit.
*
*          [AUTO]
*     WVMLOG = FILENAME (Write)
*          Name of a text file into which to write raw WVM data with
*          columns for each stage of processing.  Multiple blocks of data
*          may be written to the file if the WVM data are processed in
*          chunks.  New data are appended to the file if it already exists.
*          If a null (!) value is supplied, no file is written. [!]

*  Notes:
*     - The iterative map-maker will extinction correct the data itself
*     and this command will not be necessary.
*     - QLMAKEMAP automatically applies an extinction correction.

*  Related Applications:
*     SMURF: REMSKY, MAKEMAP;
*     SURF: EXTINCTION

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (TIMJ):
*        Initial test version
*     2005-09-27 (AGG):
*        Now uses smurf_par.h
*        Factored out extinction correction
*     2005-09-27 (EC)
*        Trap memmove when status is bad
*     2005-11-09 (AGG)
*        Allow user to specify the optical depth at the start and end
*        of a scan - could be useful if we supply a group of files
*     2005-11-10 (AGG)
*        Perform check for dimensionality of input file and prompt
*        user only for 2-D image data. Now uses Grp interface for
*        setting input/output files and stores data in a smfData struct.
*     2005-12-20 (AGG):
*        Calls smf_flatfield to automatically flatfield data if necessary.
*     2005-12-21 (AGG):
*        Now deals with timeseries data
*     2006-01-10 (AGG):
*        Now reads the tau from the header for timeseries data
*     2006-01-24 (AGG):
*        Tau is now double, rather than float.
*     2006-01-25 (AGG):
*        Mode keyword changed to Method.
*     2006-02-03 (AGG):
*        Filter now a string
*     2006-02-07 (AGG):
*        Can now use the WVMRAW method for getting tau
*     2006-04-21 (AGG):
*        Add call to smf_subtract_plane for sky removal
*     2008-03-05 (EC):
*        Changed smf_correct_extinction interface
*     2008-04-29 (AGG):
*        Remove sky subtraction call, remove placeholder code for future
*        methods
*     2008-05-01 (TIMJ):
*        Add title to output file.
*     2008-05-09 (TIMJ):
*        Enable ability to override remsky nannying.
*     2008-06-10 (AGG):
*        Allow null value for CSO tau to use MEANWVM from current header
*     2008-07-22 (TIMJ):
*        Use kaplibs for group param in/out. Handle darks.
*     2008-07-28 (TIMJ):
*        Use smf_calc_meantau. use parGdr0d to handle NULL easier.
*     2008-12-12 (TIMJ):
*        Add bad pixel masking.
*     2008-12-16 (TIMJ):
*        Rename METHOD to TAUSRC. METHOD now controls airmass calculation
*        method. Use new API for smf_correct_extinction.
*     2009-03-30 (TIMJ):
*        Add OUTFILES parameter.
*     2009-09-29 (EC):
*        Move parsing of tausrc and extmeth into smf_get_extpar
*     2010-01-08 (AGG):
*        Change BPM to BBM.
*     2010-02-16 (TIMJ):
*        Add AUTO option.
*     2010-03-11 (TIMJ):
*        Support flatfield ramps.
*     2010-08-03 (TIMJ):
*        Extract correct part of EXT keymap
*     2013-04-04 (TIMJ):
*        Ask for CSOFIT.
*     2013-07-20 (TIMJ):
*        Add an output message indicating which tau source was actually used.
*        This is important for AUTO mode.
*     2013-08-21 (AGG):
*        Do not call grpList if no output files are generated. This
*        avoids a GRP__INVID error in such cases.
*     2015-11-19 (GSB):
*        Add WVMFIT TAUSRC option.
*     2019-03-19 (GSB):
*        Mention WVMLOG parameter in the documentation.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2010, 2013 Science and Technology Facilities Council.
*     Copyright (C) 2005 Particle Physics and Astronomy Research
*     Council. Copyright (C) 2005-2010,2013 University of British
*     Columbia. All Rights Reserved.

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

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* Starlink includes */
#include "prm_par.h"
#include "sae_par.h"
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"
#include "star/ndg.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "smurflib.h"
#include "smurf_par.h"
#include "libsmf/smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smurf_extinction"
#define TASK_NAME "EXTINCTION"
#define LEN__METHOD 20

void smurf_extinction( int * status ) {

  /* Local Variables */
  smfArray *bbms = NULL;     /* Bad bolometer masks */
  smfArray *darks = NULL;    /* Dark data */
  AstKeyMap *extpars = NULL; /* Tau relation keymap */
  Grp *fgrp = NULL;          /* Filtered group, no darks */
  smfArray *flatramps = NULL;/* Flatfield ramps */
  int has_been_sky_removed = 0;/* Data are sky-removed */
  AstKeyMap *heateffmap = NULL;    /* Heater efficiency data */
  size_t i;                  /* Loop counter */
  Grp *igrp = NULL;          /* Input group */
  AstKeyMap *keymap=NULL;    /* Keymap for storing parameters */
  smf_tausrc tausrc;         /* enum value of optical depth source */
  smf_extmeth extmeth;       /* Extinction correction method */
  char tausource[LEN__METHOD];  /* String for optical depth source */
  char method[LEN__METHOD];  /* String for extinction airmass method */
  smfData *odata = NULL;     /* Output data struct */
  Grp *ogrp = NULL;          /* Output group */
  size_t outsize;            /* Total number of NDF names in the output group */
  size_t size;               /* Number of files in input group */
  double tau = 0.0;          /* Zenith tau at this wavelength */
  ThrWorkForce *wf = NULL;   /* Pointer to a pool of worker threads */

  if (*status != SAI__OK) return;

  /* Main routine */
  ndfBegin();

  /* Find the number of cores/processors available and create a pool of
     threads of the same size. */
  wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

  /* Read the input file */
  kpg1Rgndf( "IN", 0, 1, "", &igrp, &size, status );

  /* Filter out darks */
  smf_find_science( wf, igrp, &fgrp, 0, NULL, NULL, 1, 1, SMF__NULL, &darks,
                    &flatramps, &heateffmap, NULL, status );

  /* input group is now the filtered group so we can use that and
     free the old input group */
  size = grpGrpsz( fgrp, status );
  grpDelet( &igrp, status);
  igrp = fgrp;
  fgrp = NULL;

  if (size > 0) {
    /* Get output file(s) */
    kpg1Wgndf( "OUT", igrp, size, size, "More output files required...",
               &ogrp, &outsize, status );
  } else {
    msgOutif(MSG__NORM, " ","All supplied input frames were DARK,"
             " nothing to extinction correct", status );
  }

  /* Get group of pixel masks and read them into a smfArray */
  smf_request_mask( wf, "BBM", &bbms, status );

  /* Read the tau relations from config file or group. We do not
     allow sub instrument overloading because these are all values
     based on filter name. */
  keymap = kpg1Config( "TAUREL", "$SMURF_DIR/smurf_extinction.def", NULL,
                       1, status );

  /* and we need to use the EXT entry */
  astMapGet0A( keymap, "EXT", &extpars );
  keymap = astAnnul( keymap );

  /* Get tau source */
  parChoic( "TAUSRC", "Auto",
            "Auto,CSOtau,CSOFit, Filtertau, WVMraw, WVMfit", 1,
            tausource, sizeof(tausource), status);

  /* Decide how the correction is to be applied - convert to flag */
  parChoic( "METHOD", "ADAPTIVE",
            "Adaptive,Quick,Full,", 1, method, sizeof(method), status);

  /* Place parameters into a keymap and extract values */
  if( *status == SAI__OK ) {
    keymap = astKeyMap( " " );
    if( astOK ) {
      astMapPut0C( keymap, "TAUSRC", tausource, NULL );
      astMapPut0C( keymap, "TAUMETHOD", method, NULL );
      smf_get_extpar( keymap, &tausrc, &extmeth, NULL, status );
    }
  }

  for (i=1; i<=size && ( *status == SAI__OK ); i++) {

    /* Flatfield - if necessary */
    smf_open_and_flatfield( wf, igrp, ogrp, (int) i, darks, flatramps, heateffmap,
                            &odata, status );

    if (*status != SAI__OK) {
      /* Error flatfielding: tell the user which file it was */
      msgSetk("I",i);
      errRep(TASK_NAME, "Unable to open the ^I th file", status);
    }

    /* Mask out bad pixels - mask data array not quality array */
    smf_apply_mask( wf, odata, bbms, SMF__BBM_DATA, 0, status );

    /* Now check that the data are sky-subtracted */
    if ( !smf_history_check( odata, "smf_subtract_plane", status ) ) {

      /* Should we override remsky check? */
      parGet0l("HASSKYREM", &has_been_sky_removed, status);

      if ( !has_been_sky_removed && *status == SAI__OK ) {
        *status = SAI__ERROR;
        msgSetk("I",i);
        errRep("", "Input data from file ^I are not sky-subtracted", status);
      }
    }

    /* If status is OK, make decisions on source keywords the first
       time through. */
    if ( *status == SAI__OK && i == 1 ) {
      if (tausrc == SMF__TAUSRC_CSOTAU ||
          tausrc == SMF__TAUSRC_AUTO ||
          tausrc == SMF__TAUSRC_TAU) {
        double deftau;
        const char * param = NULL;
        smfHead *ohdr = odata->hdr;

        /* get default CSO tau -- this could be calculated from CSO fits */
        deftau = smf_calc_meantau( ohdr, status );

        /* Now ask for desired CSO tau */
        if ( tausrc == SMF__TAUSRC_CSOTAU || tausrc == SMF__TAUSRC_AUTO) {
          param = "CSOTAU";
        } else if (tausrc == SMF__TAUSRC_TAU) {
          param = "FILTERTAU";
          deftau = smf_cso2filt_tau( ohdr, deftau, extpars, status );
        }
        parGdr0d( param, deftau, 0.0,1.0, 1, &tau, status );
      } else if ( tausrc == SMF__TAUSRC_CSOFIT ||
                  tausrc == SMF__TAUSRC_WVMRAW ||
                  tausrc == SMF__TAUSRC_WVMFIT ) {
        /* Defer a message until after extinction correction */
      } else {
        *status = SAI__ERROR;
        errRep("", "Unsupported opacity source. Possible programming error.",
               status);
      }
    }

    /* Apply extinction correction - note that a check is made to
       determine whether the data have already been extinction
       corrected */
    smf_correct_extinction( wf, odata, &tausrc, extmeth, extpars, tau, NULL, NULL, status );

    if ( tausrc == SMF__TAUSRC_WVMRAW ) {
      msgOutif(MSG__VERB," ", "Used Raw WVM data for extinction correction", status);
    } else if ( tausrc == SMF__TAUSRC_WVMFIT ) {
      msgOutif(MSG__VERB," ", "Used fit to WVM data for extinction correction", status);
    } else if ( tausrc == SMF__TAUSRC_CSOFIT ) {
      msgOutif(MSG__VERB," ", "Used fit to CSO data for extinction correction", status);
    } else if ( tausrc == SMF__TAUSRC_CSOTAU ) {
      msgOutif(MSG__VERB," ", "Used an explicit CSO tau value for extinction correction", status);
    } else if ( tausrc == SMF__TAUSRC_TAU ) {
      msgOutif(MSG__VERB," ", "Used an explicit filter tau value for extinction correction", status);
    } else {
      if (*status == SAI__OK) {
        const char * taustr = smf_tausrc_str( tausrc, status );
        *status = SAI__ERROR;
        errRepf( "", "Unexpected opacity source used for extinction correction of %s."
                 " Possible programming error.", status, taustr );
      }
    }

    /* Set character labels */
    smf_set_clabels( "Extinction corrected",NULL, NULL, odata->hdr, status);
    smf_write_clabels( odata, status );

    /* Free resources for output data */
    smf_close_file( wf, &odata, status );
  }

  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. */
  if( *status == SAI__OK && ogrp ) {
    grpList( "OUTFILES", 0, 0, NULL, ogrp, status );
    if( *status == PAR__NULL ) errAnnul( status );
  }

  /* Tidy up after ourselves: release the resources used by the grp routines  */
  if (darks) smf_close_related( wf, &darks, status );
  if (bbms) smf_close_related( wf, &bbms, status );
  if( flatramps ) smf_close_related( wf, &flatramps, status );
  if (heateffmap) heateffmap = smf_free_effmap( heateffmap, status );
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);
  if( keymap ) keymap = astAnnul( keymap );
  if (extpars) extpars = astAnnul( extpars );
  ndfEnd( status );
}
