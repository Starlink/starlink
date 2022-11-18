/*
*+
*  Name:
*     smf_fix_metadata_acsis

*  Purpose:
*     Fix observation metadata for ACSIS data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_fix_metadata_acsis( msglev_t msglev, smfData * data, int have_fixed,
*                                 int * ncards, int * status );

*  Arguments:
*     msglev = msglev_t (Given)
*        Messaging level to be used for output information. This allows a
*        task whose entire job is to fix up meta data to report information
*        at default level whereas a task that simply wants the data fixed before
*        proceeding could use a debug level.
*     data = smfData * (Given)
*        smfData to be examined and fixed.
*     have_fixed = int (Given)
*        Current status of have_fixed. Modified value will be returned.
*     ncards = int * (Given & Returned)
*        Number of fits cards in the smfData on entry. Can be modified
*        if FITS headers are removed by this routine but should not be
*        modified if cards are added.
*     status = int * (Given & Returned)
*        Pointer to global status

*  Returned Value:
*     Returns int indicating whether the meta data were modified. 0 indicates
*     no modifications were made. Bits corresponding to the smf_metadata_fixups
*     enum will be used to indicate which parts of the meta data were modified.

*  Description:
*     Analyzes the smfData struct and determines whether meta data
*     should be modified. If necessary the FitsChan and JCMTSTATE components
*     will be updated. In some cases it is necessary to open the data file to
*     read information from the XML configuration content. If that XML is unavailable
*     some modifications will not be possible.
*
*     The disk file associated with the smfData will not be updated. The data
*     component is not required to be mapped.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     EC: Ed Chapin (UBC)
*     BRADC: Brad Cavanagh (JAC, Hawaii)
*     DSB: David Berry (JAC, Hawaii)

*  Notes:
*     o This function should not be called directly but should be
*     called solely from smf_fix_metadata().
*     o OFF exposure calculation for scan observations is an estimate
*     due to the difficulty of working out exactly where in the scan a
*     particular spectrum is from. Accurate determination requires all files
*     from a single observation.
*     o OFF exposure calculation for continuum jiggle observations will
*     be incorrect.

*  History:
*     2009-11-27 (TIMJ):
*        Split from smf_fix_metadata
*     2010-03-12 (TIMJ):
*        Fix brokenness since split. "steptime" was not being set so any fixups
*        involving steptime were broken.
*     2011-03-04 (TIMJ):
*        Report the unsupported mode string
*        Ensure that we have obsmode information
*     2013-05-08 (DSB):
*        Change bad TCS_AZ_JIG_X/Y values to zero in PSSW mode.
*     2022-10-14 (GSB):
*        Added call to smf_validate_tcs_position based on TEL_POS_TOLERANCE
*        value in global keymap.

*  Copyright:
*     Copyright (C) 2009-2013 Science & Technology Facilities Council.
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

#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "prm_par.h"
#include "mers.h"
#include "star/one.h"

#include "smf.h"
#include "smf_err.h"

#include <stdio.h>
#include <strings.h>

/* Local struct containing header information that may be accessed multiple times
   without having to use many variables */

struct FitsHeaderStruct {
  double obsgeox;
  double obsgeoy;
  double obsgeoz;
  double chop_thr;
  double chop_pa;
  double rot_pa;
  int obsnum;
  int utdate;
  int jigl_cnt;
  int num_cyc;
  int jos_mult;
  int jos_min;
  int num_nods;
  int ncalstep;
  int nrefstep;
  int stbetref;
  int stbetcal;
  char focaxis[2];
  int nfocstep;
  int focstep;
  double instap_x;
  double instap_y;
  char chop_crd[81];
  char instrume[81];
  char rot_crd[81];
  char obsid[81];
  char obsidss[81];
  char fft_win[81];
};

/* Struct defining dut1 information */
typedef struct Dut1 {
  double mjd;
  double dut1;
} Dut1;

/* Struct defining information based on OBSID. We can add to this if information
   is needed to be updated rather than creating new similar structs each time.
   Note that the utdate and observation number are there to minimize string
   comparisons.
 */
typedef struct {
  int utdate;           /* YYYYMMDD utdate */
  int obsnum;           /* Observation number */
  const char obsid[81]; /* OBSID */
  int isrover;          /* This observation was a rover observation */
} ObsIdLUT;

/* Private routines */

static const ObsIdLUT * smf__find_obsidlut( const struct FitsHeaderStruct * fitsvals, int * status );
static int smf__fix_wrap( double *start, size_t stride, size_t nel, int have_fixed, int *status );

#define FUNC_NAME "smf_fix_metadata_acsis"

/* Indent for informational messages */
#define INDENT "   "

int smf_fix_metadata_acsis ( msglev_t msglev, smfData * data, int have_fixed, int * ncards,
                             int * status ) {

  int cardisdef;             /* FITS card is defined */
  int cardthere;             /* FITS card is present */
  double dateobs;            /* MJD UTC of observation start */
  AstFitsChan * fits = NULL; /* FITS header (FitsChan) */
  struct FitsHeaderStruct fitsvals; /* Quick access Fits header struct */
  int has_dhsver = 0;        /* Do we have DHSVER header? */
  smfHead *hdr = NULL;       /* Data header struct */
  size_t i;
  int missing_exp = 0;       /* Are we missing ACS_EXPOSURE? */
  int missing_off = 0;       /* Are we missing ACS_OFFEXPOSURE? */
  double posn_tolerance;     /* Tolerance for TCS position validation */
  int posn_valid = 1;        /* Did all TCS positions pass validation? */
  AstKeyMap * obsmap = NULL; /* Info from all observations */
  AstKeyMap * objmap = NULL; /* All the object names used */
  double steptime = 0.0;     /* Step time */
  JCMTState * tmpState = NULL; /* Pointer to allState */

  const Dut1 dut1[] = {
    { 54009.0, 0.1468530 },
    { 54010.00, .1460040 },
    { 54011.00, .1448820 },
    { 54012.00, .1433980 },
    { 54013.00, .1415600 },
    { 54014.00, .1394330 },
    { 54015.00, .1371970 },
    { 54016.00, .1350090 },
    { 54017.00, .1331230 },
    { 54018.00, .1315820 },
    { 54019.00, .1303480 },
    { 54020.00, .1293180 },
    { 54021.00, .1283980 },
    { 54022.00, .1275790 },
    { 54023.00, .1267040 },
    { 54024.00, .1256860 },
    { 54025.00, .1245070 },
    { 54026.00, .1232040 },
    { 54027.00, .1217940 },
    { 54028.00, .1203130 },
    { 54029.00, .1188250 },
    { 54030.00, .1173700 },
    { 54031.00, .1160180 },
    { 54032.00, .1147960 },
    { 54033.00, .1137480 },
    { 54034.00, .1128540 },
    { 54035.00, .1120660 },
    { 54036.00, .1113160 },
    { 54037.00, .1104940 },
    { 54038.00, .1095100 },
    { 54039.00, .1082780 },
    { 54040.00, .1067410 },
    { 54041.00, .1049070 },
    { 54042.00, .1028990 },
    { 54043.00, .1009120 },
    { 54044.00, .0991360 },
    { 54045.00, .0977100 },
    { 54046.00, .0966370 },
    { 54047.00, .0958320 },
    { 54048.00, .0951680 },
    { 54049.00, .0944790 },
    { 54050.00, .0937460 },
    { 54051.00, .0928160 },
    { 54052.00, .0917430 },
    { 54053.00, .0905550 },
    { 54054.00, .0892880 },
    { 54055.00, .0880820 },
    { 54056.00, .0868430 },
    { 54057.00, .0856560 },
    { 54058.00, .0845750 },
    { 54059.00, .0836160 },
    { 54060.00, .0827780 },
    { 54061.00, .0820680 },
    { 54062.00, .0814600 },
    { 54063.00, .0809210 },
    { 54064.00, .0803240 },
    { 54065.00, .0795140 },
    { 54066.00, .0784160 },
    { 54067.00, .0770260 },
    { 54068.00, .0753910 },
    { 54069.00, .0736090 },
    { 54070.00, .0718080 },
    { 54071.00, .0701210 },
    { 54072.00, .0687500 },
    { 54073.00, .0676460 },
    { 54074.00, .0667520 },
    { 54075.00, .0659980 },
    { 54076.00, .0652980 },
    { 54077.00, .0645190 },
    { 54078.00, .0635560 },
    { 54079.00, .0624400 },
    { 54080.00, .0611550 },
    { 54081.00, .0597460 },
    { 54082.00, .0582970 },
    { 54083.00, .0569040 },
    { 54084.00, .0556470 },
    { 54085.00, .0544270 },
    { 54086.00, .0533970 },
    { 54087.00, .0525650 },
    { 54088.00, .0518740 },
    { 54089.00, .0512610 },
    { 54090.00, .0507230 },
    { 54091.00, .0501840 },
    { 54092.00, .0494700 },
    { 54093.00, .0484970 },
    { 54094.00, .0472490 },
    { 54095.00, .0457680 },
    { 54096.00, .0441750 },
    { 54097.00, .0425710 },
    { 54098.00, .0410280 },
    { 54099.00, .0395870 },
    { 54100.00, .0384190 },
    { 54101.00, .0375630 },
    { 54102.00, .0369300 },
    { 54103.00, .0363920 },
    { 54104.00, .0358440 },
    { 54105.00, .0352570 },
    { 54106.00, .0344690 },
    { 54107.00, .0334880 },
    { 54108.00, .0323430 },
    { 54109.00, .0310680 },
    { 54110.00, .0297240 },
    { 54111.00, .0283390 },
    { 54112.00, .0270660 },
    { 54113.00, .0258270 },
    { 54114.00, .0247970 },
    { 54115.00, .0238550 },
    { 54116.00, .0230210 },
    { 54117.00, .0222030 },
    { 54118.00, .0214000 },
    { 54119.00, .0204540 },
    { 54120.00, .0192240 },
    { 54121.00, .0176760 },
    { 54122.00, .0158480 },
    { 54123.00, .0138150 },
    { 54124.00, .0116830 },
    { 54125.00, .0096260 },
    { 54126.00, .0077870 },
    { 54127.00, .0062080 },
    { 54128.00, .0049030 },
    { 54129.00, .0038300 },
    { 54130.00, .0028320 },
    { 54131.00, .0018320 },
    { 54132.00, .0007270 },
    { 54133.00, -.0005290 },
    { VAL__BADD, VAL__BADD }
  };

  if (*status != SAI__OK) return have_fixed;

  /* Validate arguments - need smfFile and smfHead */
  smf_validate_smfData( data, 1, 1, status );
  if (*status != SAI__OK) return have_fixed;

  hdr = data->hdr;
  smf_validate_smfHead( hdr, 1, 1, status );
  if (*status != SAI__OK) return have_fixed;

  fits = hdr->fitshdr;
  tmpState = hdr->allState;

  if (hdr->instrument != INST__ACSIS) {
    if (*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep("", " Attempting to fix metadata using ACSIS algorithms but this is not ACSIS data",
             status );
    }
    return have_fixed;
  }

  /* Get the MJD of the observation. Does not need to be accurate so do not care whether
     it is from DATE-OBS or JCMTSTATE */
  smf_find_dateobs( hdr, &dateobs, NULL, status );

  /* Determine that we have a DHSVER header. This can be used to decide whether
     we have already fixed up this header. Some things can be determined from the values,
     but others are just fixups involving change of sign convention. */
  has_dhsver = astTestFits( fits, "DHSVER", NULL);

  /* Read some header values that are likely to be useful. Use a struct for the results
     to stop bloat of variable names. KeyMap or FitsChan are too cumbersone for multiple
     accesses.*/

  fitsvals.obsnum = VAL__BADI;
  fitsvals.utdate = VAL__BADI;
  fitsvals.num_cyc = VAL__BADI;
  fitsvals.jos_min = VAL__BADI;
  fitsvals.nrefstep = VAL__BADI;
  fitsvals.jigl_cnt = VAL__BADI;
  fitsvals.obsgeox = VAL__BADD;
  fitsvals.obsgeoy = VAL__BADD;
  fitsvals.obsgeoz = VAL__BADD;
  fitsvals.instap_x = VAL__BADD;
  fitsvals.instap_y = VAL__BADD;
  fitsvals.chop_thr = VAL__BADD;
  fitsvals.chop_pa = VAL__BADD;
  fitsvals.rot_pa = VAL__BADD;
  *(fitsvals.chop_crd) = '\0';
  *(fitsvals.rot_crd) = '\0';
  *(fitsvals.instrume) = '\0';
  *(fitsvals.obsid) = '\0';
  *(fitsvals.obsidss) = '\0';

  smf_getfitsi( hdr, "OBSNUM", &(fitsvals.obsnum), status );
  smf_getfitsi( hdr, "UTDATE", &(fitsvals.utdate), status );
  smf_getfitsi( hdr, "NUM_CYC", &(fitsvals.num_cyc), status );
  smf_getfitsi( hdr, "JOS_MULT", &(fitsvals.jos_mult), status );
  smf_getfitsi( hdr, "JOS_MIN", &(fitsvals.jos_min), status );
  smf_getfitsi( hdr, "NREFSTEP", &(fitsvals.nrefstep), status );
  smf_getfitsi( hdr, "JIGL_CNT", &(fitsvals.jigl_cnt), status );
  smf_getfitsd( hdr, "OBSGEO-X", &(fitsvals.obsgeox), status );
  smf_getfitsd( hdr, "OBSGEO-Y", &(fitsvals.obsgeoy), status );
  smf_getfitsd( hdr, "OBSGEO-Z", &(fitsvals.obsgeoz), status );
  smf_getfitsd( hdr, "INSTAP_X", &(fitsvals.instap_x), status );
  smf_getfitsd( hdr, "INSTAP_Y", &(fitsvals.instap_y), status );
  smf_getfitsd( hdr, "CHOP_THR", &(fitsvals.chop_thr), status );
  smf_getfitsd( hdr, "CHOP_PA", &(fitsvals.chop_pa), status );
  smf_getfitss( hdr, "CHOP_CRD", fitsvals.chop_crd, sizeof(fitsvals.chop_crd), status );
  smf_getfitss( hdr, "INSTRUME", fitsvals.instrume, sizeof(fitsvals.instrume), status );
  smf_getobsidss( hdr->fitshdr, fitsvals.obsid, sizeof(fitsvals.obsid), fitsvals.obsidss,
                  sizeof(fitsvals.obsidss), status );

  smf_getfitsd( hdr, "STEPTIME", &steptime, status );

  /* Do ROVER before printing out the obs description */

  /* ROVER fix ups. For observations before 20090201 we sometimes
     reported ROVER observations when there were no ROVER observations
     occurring (see JCMT fault 20081119.001). There is a chance that
     some eSMA observations will erroneously have their INBEAM
     header nulled. Assume everything is fine after 20090201. */
  cardisdef = astTestFits( fits, "INBEAM", &cardthere );
  if ( fitsvals.utdate < 20080201 ) {
    /* No known POL observations */
    if ( !cardthere || cardisdef ) {
      smf_fits_updateU( hdr, "INBEAM", "Hardware in the beam", status );
      cardthere = 1;
      have_fixed |= SMF__FIXED_FITSHDR;
    }
  } else if ( fitsvals.utdate < 20090201 ) {
    /* see if we have a matching LUT entry */
    const ObsIdLUT * lut = smf__find_obsidlut( &fitsvals, status );
    if (lut && lut->isrover) {
      /* this is definitely a ROVER observation - we know during this period
         that it will either say POL or it will be wrong. It will never be
         multi-values.
       */
      char inbeam[81];
      if (cardisdef) {
        smf_getfitss( hdr, "INBEAM", inbeam, sizeof(inbeam), status );
        msgSetc( "PREV", inbeam);
      } else {
        inbeam[0] = '\0';
        msgSetc( "PREV", "blank");
      }
      if (strcmp( inbeam, "POL") != 0 ) {
        msgOutif( msglev, "", INDENT "This is a POL observation. Updating INBEAM (was ^PREV).", status );
        smf_fits_updateS( hdr, "INBEAM", "POL", "Hardware in the beam", status );
        cardthere = 1;
        have_fixed |= SMF__FIXED_FITSHDR;
      }
    } else {
      /* not a ROVER observation. So INBEAM should be undef. */
      if ( !cardthere || cardisdef ) {
        /* should be undef, not defined */
        have_fixed |= SMF__FIXED_FITSHDR;
        smf_fits_updateU( hdr, "INBEAM", "Hardware in the beam", status );
        cardthere = 1;
        msgOutif( msglev, "",  INDENT "This is not a POL observation. Forcing INBEAM to undef.", status);
      }
    }

  }
  /* For early ACSIS observations INBEAM was not present so we put one in */
  if (!cardthere) {
    smf_fits_updateU( hdr, "INBEAM", "Hardware in the beam", status );
    have_fixed |= SMF__FIXED_FITSHDR;
  }

  /* Print out summary of this observation - this may get repetitive if multiple files come
     from the same observation in one invocation but it seems better to describe each fix up
     separately and in context. */
  obsmap = astKeyMap( " " );
  objmap = astKeyMap( " " );
  smf_obsmap_fill( data, obsmap, objmap, status );
  smf_obsmap_report( msglev, obsmap, objmap, status );
  obsmap = astAnnul( obsmap );
  objmap = astAnnul( objmap );

  /* FITS header fix ups */

  /* LOFREQS and LOFREQE can come from FE_LOFREQ. HARP had incorrect header tables
     for quite a long period of time. */
  if (!astTestFits( fits, "LOFREQS", NULL ) ) {
    /* undef or missing makes no difference */
    smf_fits_updateD( hdr, "LOFREQS", tmpState[0].fe_lofreq, "[GHz] LO Frequency at start of obs.", status );
    have_fixed |= SMF__FIXED_FITSHDR;
  }
  if (!astTestFits( fits, "LOFREQE", NULL ) ) {
    /* undef or missing makes no difference */
    smf_fits_updateD( hdr, "LOFREQE", tmpState[hdr->nframes - 1].fe_lofreq,
                      "[GHz] LO Frequency at end of obs.", status );
    have_fixed |= SMF__FIXED_FITSHDR;
  }

  /* DUT1 */
  if (*status == SAI__OK && !astTestFits( fits, "DUT1", NULL ) ) {
    /* inefficient for loop  - no fancy binary chop */
    int found = 0;
    i = 0;
    while ( dut1[i].mjd != VAL__BADD ) {
      if (dateobs > dut1[i].mjd && dateobs < dut1[i+1].mjd) {
        smf_fits_updateD( hdr, "DUT1", dut1[i].dut1 / SPD,
                          "[d] UT1-UTC correction", status );
        have_fixed |= SMF__FIXED_FITSHDR;
        found = 1;
      }
      i++;
    }
    if (*status == SAI__OK && !found) {
      *status = SAI__ERROR;
      errRepf("", "Could not determine DUT1 for MJD %g", status, dateobs );
    }
  }

  /*
   * Telescope position
   *  Before 20061013 observations had inaccurate telescope position.
   *  Needs to be replaced with the version used at the time by the telescope.
   *  Sometimes an old position was used for testing even after 20061013 since the coordinates
   *  were not obtained directly from the telescope.
   *  After 20080323 the telescope position was updated again but those files are all correct.
   *  We do not adopt the new position since we have to be consistent with the AZEL numbers
   *  stored in JCMTSTATE.
   */
  if (fitsvals.utdate < 20080323) {
    if ( fitsvals.obsgeox == -5464545.0 ) {
      /* hard-coded OBSGEO-X implies -Y and -Z also wrong */
      msgOutif( msglev, "", INDENT "Correcting telescope coordinates.", status );
      smf_fits_updateD( hdr, "OBSGEO-X", -5464594.335493, NULL, status );
      smf_fits_updateD( hdr, "OBSGEO-Y", -2492695.151639, NULL, status );
      smf_fits_updateD( hdr, "OBSGEO-Z", 2150964.058506, NULL, status );
      smf_fits_updateD( hdr, "LONG-OBS", 19.82583335521, NULL, status );
      smf_fits_updateD( hdr, "LAT-OBS", -155.4797222301, NULL, status );
      have_fixed |= SMF__FIXED_FITSHDR;
    }
  }

  /* Instrument aperture changed sign convention when we started reading it from the TCS.
     See JCMT fault 20090330.009 */
  if (!has_dhsver && fitsvals.utdate < 20080508
      && ( fitsvals.instap_x != 0.0 || fitsvals.instap_y != 0.0) ) {
    int flip = 1; /* assume we are going to flip the sign */

    /* there were some test observations done between 20080501 and 20080508 so for those
       we have to dig a little deeper and look in the ocsconfig */
    if ( fitsvals.utdate >= 20080501 && hdr->ocsconfig ) {
      char * match = astChrSub( hdr->ocsconfig, "AXIS_XY.X_AS", NULL, 0 );
      if (match) {
        flip = 0;
        match = astFree( match );
      }
    }

    if (flip) {
      msgOutif( msglev, "", INDENT "Fixing instrument aperture sign convention.", status );
      smf_fits_updateD( hdr, "INSTAP_X", -1.0 * fitsvals.instap_x, NULL, status );
      smf_fits_updateD( hdr, "INSTAP_Y", -1.0 * fitsvals.instap_y, NULL, status );
      have_fixed |= SMF__FIXED_FITSHDR;
    }
  }

  /* RxW data did some strange things to aperture */
  if ( fitsvals.utdate < 20090601 && strncasecmp( fitsvals.instrume, "RxW", 3 ) == 0 &&
       ( fitsvals.instap_x != 0.0 || fitsvals.instap_y != 0.0) ) {
    msgOutiff( msglev, "", INDENT "Fixing instrument aperture to 0,0 for %s.", status, fitsvals.instrume );
    smf_fits_updateD( hdr, "INSTAP_X", 0.0, NULL, status );
    smf_fits_updateD( hdr, "INSTAP_Y", 0.0, NULL, status );
  }

  /* Store OBSIDSS */
  if (!astTestFits( fits, "OBSIDSS", NULL ) ) {
    smf_fits_updateS( hdr, "OBSIDSS", fitsvals.obsidss, "Unique observation subsys identifier", status );
    have_fixed |= SMF__FIXED_FITSHDR;
  }

  /* Make BASEC1, BASEC2 and TRACKSYS available */
  if (!astTestFits( fits, "TRACKSYS", NULL ) ) {
    /* undef or missing makes no difference */
    msgOutiff( msglev, "", INDENT "Missing TRACKSYS - setting to '%s'", status, tmpState[0].tcs_tr_sys);
    smf_fits_updateS( hdr, "TRACKSYS", tmpState[0].tcs_tr_sys, "TCS Tracking coordinate system", status );
    have_fixed |= SMF__FIXED_FITSHDR;
  }
  if (!astTestFits( fits, "BASEC1", NULL ) ) {
    /* undef or missing makes no difference */
    double basedeg = tmpState[0].tcs_tr_bc1 * AST__DR2D;
    msgOutiff( msglev, "", INDENT "Missing BASEC1 - setting to %g deg", status, basedeg);
    smf_fits_updateD( hdr, "BASEC1", basedeg, "[deg] TCS BASE position (longitude) in TRACKSYS", status );
    have_fixed |= SMF__FIXED_FITSHDR;
  }
  if (!astTestFits( fits, "BASEC2", NULL ) ) {
    /* undef or missing makes no difference */
    double basedeg = tmpState[0].tcs_tr_bc2 * AST__DR2D;
    msgOutiff( msglev, "", INDENT "Missing BASEC2 - setting to %g deg", status, basedeg);
    smf_fits_updateD( hdr, "BASEC2", basedeg, "[deg] TCS BASE position (latitude) in TRACKSYS", status );
    have_fixed |= SMF__FIXED_FITSHDR;
  }

  /* New nomenclature for "raster". It is now a "scan" since 20080610. */
  if (fitsvals.utdate <= 20080610) {
    char sam_mode[81];
    smf_getfitss( hdr, "SAM_MODE", sam_mode, sizeof(sam_mode), status );
    if ( strncasecmp( "raster", sam_mode, 6 ) == 0 ) {
      smf_fits_updateS( hdr, "SAM_MODE", "scan", NULL, status );
      have_fixed |= SMF__FIXED_FITSHDR;
    }
  }

  /* We need general information on observing modes so at this point we assume
     the header information describing the mode is correct. */
  smf_calc_mode( hdr, status );

  /* JIG_SCAL */
  if ( hdr->obsmode == SMF__OBS_JIGGLE ) {
    if (!astTestFits(fits, "JIG_SCAL", NULL) ) {
      if (hdr->ocsconfig) {
        double jigscal = VAL__BADD;
        int found;
        found = smf_pattern_extract( hdr->ocsconfig,
                                      "<JIGGLE .*SCALE=\"([0123456789\\.]+)\"", &jigscal, NULL, 0, status );
        if (found) {
          smf_fits_updateD( hdr, "JIG_SCAL", jigscal, "[arcsec] Scale of jiggle pattern", status );
          msgOutiff( msglev, "", INDENT "Missing JIG_SCAL - setting to %10g arcsec", status, jigscal );
          have_fixed |= SMF__FIXED_FITSHDR;
        } else {
          msgOutiff( msglev, "", INDENT "** Could not determine JIG_SCAL in XML configuration", status );
        }
      } else {
        msgOutiff( msglev, "", INDENT "** Could not determine JIG_SCAL. No XML configuration available", status );
      }
    }
  }

  /* POL_CONN is deprecated so we should remove it completely */
  if (astTestFits( fits, "POL_CONN", NULL ) ) {
    astClear( fits, "Card" );
    if (astFindFits( fits, "POL_CONN", NULL, 0 ) ) {
      astDelFits( fits );
      have_fixed |= SMF__FIXED_FITSHDR;
      (*ncards)--;  /* Adjust the target number of cards */
    }
  }

  /* FFT_WIN */
  if( ! astTestFits( fits, "FFT_WIN", NULL ) ) {
    if( hdr->ocsconfig ) {
      int found;
      found = smf_pattern_extract( hdr->ocsconfig, "window type=\"(\\w+)\"", NULL, fitsvals.fft_win,
                                    sizeof( fitsvals.fft_win ), status );
      if( found ) {
        smf_fits_updateS( hdr, "FFT_WIN", fitsvals.fft_win, "Type of window used for FFT", status );
        msgOutiff( msglev, "", INDENT "Missing FFT_WIN - setting to '%s'", status, fitsvals.fft_win );
        have_fixed |= SMF__FIXED_FITSHDR;
      } else {
        smf_fits_updateS( hdr, "FFT_WIN", "truncate", "Type of window used for FFT", status );
        msgOutiff( msglev, "", INDENT "Missing FFT_WIN - not found in ocsconfig - setting to 'truncate'", status );
        have_fixed |= SMF__FIXED_FITSHDR;
      }
    } else {
      smf_fits_updateS( hdr, "FFT_WIN", "truncate", "Type of window used for FFT", status );
      msgOutiff( msglev, "", INDENT "Missing FFT_WIN - not found in ocsconfig - setting to 'truncate'", status );
      have_fixed |= SMF__FIXED_FITSHDR;
    }
  }

  /* HARP specific fixes */
  if ( strncasecmp( fitsvals.instrume, "HARP", 4 ) == 0 ) {
    /* ROT_CRD - released in 20080111 */
    if (!astTestFits( fits, "ROT_CRD", NULL ) ) {
      if (hdr->ocsconfig) {
        int found;
        found = smf_pattern_extract( hdr->ocsconfig, "ROTATOR SYSTEM=\"([AZELFIXEDTRACKING]+)\"",
                                      NULL, fitsvals.rot_crd, sizeof(fitsvals.rot_crd), status );
        if (found) {
          smf_fits_updateS( hdr, "ROT_CRD", fitsvals.rot_crd, "K-mirror coordinate system", status );
          msgOutiff( msglev, "", INDENT "Missing ROT_CRD - setting to '%s'", status, fitsvals.rot_crd );
          have_fixed |= SMF__FIXED_FITSHDR;
        } else {
          msgOutif( msglev, "", INDENT "** Could not find ROT_CRD in XML configuration", status );
        }
      } else {
        msgOutif( msglev, "", INDENT "** Could not determine ROT_CRD. No XML configuration available", status);
      }
    } else {
      /* make sure we have ROT_CRD */
      smf_getfitss( hdr, "ROT_CRD", fitsvals.rot_crd, sizeof(fitsvals.rot_crd), status );
    }

    /* ROT_PA was released in 20080111 but not fixed until much later */
    if (!astTestFits( fits, "ROT_PA", NULL ) ) {
      if (strlen( fitsvals.rot_crd ) ) {
        if (strncmp( fitsvals.rot_crd, "TRACKING", 8 ) == 0 ||
            strncmp( fitsvals.rot_crd, tmpState[0].tcs_tr_sys, sizeof(tmpState[0].tcs_tr_sys) ) == 0 ) {
          /* mean angle. Note the sign convention to match current ROT_PA values */
          fitsvals.rot_pa = -1.0 * AST__DR2D *
            ( tmpState[0].tcs_tr_ang + tmpState[hdr->nframes - 1].tcs_tr_ang ) / 2.0;
        } else if ( strncmp( fitsvals.rot_crd, "AZEL", 4 ) == 0) {
          fitsvals.rot_pa = -1.0 * AST__DR2D *
            ( tmpState[0].tcs_az_ang + tmpState[hdr->nframes - 1].tcs_az_ang ) / 2.0;
        } else if ( strncmp( fitsvals.rot_crd, "FIXED", 5 ) == 0) {
          /* need to look in the config */
          if (hdr->ocsconfig) {
            int found;
            found = smf_pattern_extract( hdr->ocsconfig, "<ROTATOR.*<PA>([0123456789\\.]+)</PA>.*</ROTATOR>",
                                          &(fitsvals.rot_pa), NULL, 0, status );
            if (!found) {
              if ( hdr->obstype == SMF__TYP_SKYDIP ) {
                /* that is fine. Skydips do not generaly have a ROT_PA */
                smf_fits_updateU( hdr, "ROT_PA", "[deg] K-mirror angle", status );
              } else {
                *status = SAI__ERROR;
                errRepf("", "Need to find a test for FIXED ROT_PA. Report this observation to software group", status);
              }
            }
          } else {
            /* sky dips tend not to have a PA specified - just using whatever it is at */
            if (hdr->obstype != SMF__TYP_SKYDIP) {
              msgOutif( msglev, "", INDENT "** Could not determine FIXED ROT_PA. No XML configuration available",
                        status);
            }
          }
        } else {
          if (*status == SAI__OK) {
            *status = SAI__ERROR;
            errRepf( "", "Unrecognized rotator coordinate system ('%s')", status, fitsvals.rot_crd );
          }
        }
        if (fitsvals.rot_pa != VAL__BADD) {
          msgOutiff( msglev, "", INDENT "Missing ROT_PA - setting to %g deg", status, fitsvals.rot_pa );
          smf_fits_updateD( hdr, "ROT_PA", fitsvals.rot_pa, "[deg] K-mirror angle", status );
          have_fixed |= SMF__FIXED_FITSHDR;
        } else {
          /* clear it but force it to exist */
          smf_fits_updateU( hdr, "ROT_PA", "[deg] K-mirror angle", status );
        }
      }
    }

  } else {
    /* Need to blank the ROT_CRD and ROT_PA headers */
    smf_fits_updateU( hdr, "ROT_CRD",  "K-mirror coordinate system", status );
    smf_fits_updateU( hdr, "ROT_PA", "[deg] K-mirror angle", status );
  }

  /* JCMTSTATE fix ups */

  /* TCS_TAI is missing before 20061013 */

  if (*status == SAI__OK) {
    /* TCS_TAI can be missing with old data files */
    if (tmpState[0].tcs_tai == VAL__BADD) {
      /* need the step time - if we do not have it set
       tcs_tai to rts_end */
      double step = steptime;
      if (step == VAL__BADD) {
        msgOutif(MSG__DEBUG," ","Could not determine step time when correcting TCS_TAI from RTS_END", status );
        step = 0.0;
      }

      /* correct TCS_TAI by half step time corrected to days */
      msgOutiff( msglev, "", INDENT "Missing TCS_TAI. Estimating using RTS_END and STEPTIME of %g sec.",
                 status, step);
      step = 0.5 * step / SPD;
      if (*status == SAI__OK) {
        for (i=0; i < hdr->nframes; i++) {
          tmpState[i].tcs_tai = tmpState[i].rts_end - step;
        }
      }
      have_fixed |= SMF__FIXED_JCMTSTATE;
    }
  }

  /* Older data have BAD values for chop entries. For AZEL chopping we can easily fill in an estimate,
     but for TRACKING chops we will have to do some work. Only bother for CHOP observations */
  if (*status == SAI__OK && hdr->swmode == SMF__SWM_CHOP &&
      (tmpState[0].smu_az_chop_x == VAL__BADD || tmpState[0].smu_az_chop_y == VAL__BADD )) {
    msgOutif( msglev, "", INDENT "Blanked SMU_AZ_CHOP entries.", status );

    if (strcmp( fitsvals.chop_crd, "AZEL" ) == 0) {
      /* we need AZEL numbers and these are already in AZEL so this is easy */
      double pa = fitsvals.chop_pa * AST__DD2R;
      double chop_x = - (fitsvals.chop_thr / 2.0) * sin( pa );
      double chop_y = (fitsvals.chop_thr / 2.0) * cos( pa );

      for (i = 0; i < hdr->nframes; i++ ) {
        if (tmpState[i].smu_az_chop_x == VAL__BADD) tmpState[i].smu_az_chop_x = chop_x;
        if (tmpState[i].smu_az_chop_y == VAL__BADD) tmpState[i].smu_az_chop_y = chop_y;
      }
      have_fixed |= SMF__FIXED_JCMTSTATE;
    } else {
      *status = SAI__ERROR;
      errRepf( "", "Missing SMU_AZ_CHOP entry but not yet able to fix for %s chop coordinate system",
               status, fitsvals.chop_crd );
    }
  }

  /* Older data have BAD values for SMU position entries. Use zero instead. */
  if (*status == SAI__OK && hdr->swmode == SMF__SWM_PSSW &&
      ( tmpState[0].smu_az_jig_x == VAL__BADD ||
        tmpState[0].smu_az_jig_y == VAL__BADD )) {
    msgOutif( msglev, "", INDENT "Blanked SMU_AZ_JIG entries.", status );
    for (i = 0; i < hdr->nframes; i++ ) {
      if (tmpState[i].smu_az_jig_x == VAL__BADD) tmpState[i].smu_az_jig_x = 0.0;
      if (tmpState[i].smu_az_jig_y == VAL__BADD) tmpState[i].smu_az_jig_y = 0.0;
    }
    have_fixed |= SMF__FIXED_JCMTSTATE;
  }

  /* Off exposure time - depends on observing mode.

     o ACS_EXPOSURE and ACS_NO_ONS pops up occassionally
     before 20070123 but should not be trusted.

        - jiggle/chop stores STEPTIME rather than actual
          exposure time.
        - ACS_NO_ONS is filled in for SCAN but reflects
          the number of steps in each scan.

     o Everything seems to be complete by 20070123.

     o Off exposure will be an estimate in some cases.
   */

  /* Assumes that STEPTIME is correct... */
  if ( (tmpState[0].acs_exposure == VAL__BADR) || (tmpState[0].acs_exposure < (0.80 * steptime)) ) {
    missing_exp = 1;
    msgOutif( msglev, "", INDENT "Missing ACS_EXPOSURE", status );
  }
  /* Skydips do not have off exposures */
  if ( hdr->obstype != SMF__TYP_SKYDIP &&
       ((tmpState[0].acs_offexposure == VAL__BADR) || (tmpState[0].acs_offexposure < (0.80 * steptime))) ) {
    missing_off = 1;
    msgOutif( msglev, "", INDENT "Missing ACS_OFFEXPOSURE", status );
  }

  if ( (missing_exp || missing_off) && (*status == SAI__OK) ) {
    float exp_time = 0.0;
    float off_time = 0.0;
    int utmax = 20070130;

    /* on 20070507 obs #45 and #47 have bad exposure information */
    if (fitsvals.utdate > utmax && fitsvals.utdate != 20070507) {
      *status = SAI__ERROR;
      errRepf( "", "ACS_*EXPOSURE missing or BAD but data are newer than expected (%d > %d)",
               status, fitsvals.utdate, utmax);
    }

    /* if we are missing OFF exposure but on exposure seems to be present
       we do not trust it (See above). Some of the above logic becomes
       superfluous once we assume this since we are declaring that presence
       of OFFEXPOSURE controls everything. */
    if (missing_off && !missing_exp ) {
      msgOutiff( msglev, "", INDENT "ACS_EXPOSURE is present but untrustworthy", status );
      missing_exp = 1;
    }

    /* On exposure is assumed to be identical for all spectra */
    switch (hdr->obsmode) {
      case SMF__OBS_SCAN:
        /* On exposure is simply the step time */
        exp_time = steptime;

        if (hdr->swmode == SMF__SWM_PSSW) {
          int nrefstep = fitsvals.nrefstep;
          /* off exposure time depends on the position of the spectrum
             in the row, and varies from 1 to 2 times the actual off time.
             This information is hard to obtain in the general case where a row
             is split between multiple files. The only reliable way to do it
             would be to run this command on multiple files at once and effectively
             timesort all the samples looking for the row boundaries. For now
             the important thing is to get an esimate, rather than a perfect value,
             so choose 1.5.

             Note that in more recent observations the JOS calculates NREFSTEP
             dynamically for each row.
          */
          /* If nrefstep is 0 we either make up a number or hope that the old
             acs_no_prev_ref is okay. eg for 20070507. A 0.0 second
             off time causes problems in the weighted coadd. */
          if (nrefstep == 0) {
            int nrefstate = tmpState[0].acs_no_prev_ref;
            if ( nrefstate != VAL__BADI && nrefstep > 0 ) {
              nrefstep = nrefstate;
            } else {
              nrefstep = 5;
            }
          }

          off_time = 1.5 * nrefstep * steptime;
          if (missing_off) {
            msgOutiff( MSG__QUIET, "", "WARNING: %d #%d: OFF exposure time has been estimated as %g sec",
                       status, fitsvals.utdate, fitsvals.obsnum, off_time );
          }
        } else {
          *status = SAI__ERROR;
          errRepf( "", "Unsupported scan switch mode (%s).", status,
                   smf_swmode_str( hdr->swmode, status ) );
        }
        break;
      case SMF__OBS_GRID:
        /* depends on switch mode */
        if (hdr->swmode == SMF__SWM_CHOP) {
          /* this is really jiggle/chop (2.0 is for nod) */
          exp_time = 2.0 * fitsvals.jos_mult * steptime;
          off_time = exp_time;
        } else if (hdr->swmode == SMF__SWM_PSSW) {
          /* For older data we use JOS_MIN and coadd online. For newer data we actually write out
             each spectrum separately and coadd off line. See jos_dr_control for whether this has been
             done. We should not really be dealing with cases where the latter is being done and we
              are missing exposure time information so ignore for now. */
          exp_time = fitsvals.jos_min * steptime;
          off_time = exp_time;
        } else {
          *status = SAI__ERROR;
          errRepf( "", "Unsupported grid switch mode (%s).", status,
                   smf_swmode_str( hdr->swmode, status ) );
        }
        break;
      case SMF__OBS_JIGGLE:
        /* Since this is for old data, we assume that we are talking about jiggle/chop here
           but add an explicit trap. */
        if (hdr->swmode == SMF__SWM_CHOP) {
          /* 2.0 is for nod */
          exp_time = 2.0 * fitsvals.jos_mult * steptime;

          /* off time depends on how the pattern is broken up since it is actually
           done as 2 * N_CYC_OFF per N_JIGS_ON. In the vast majority of cases
          N_JIGS_ON = JIGL_CNT and N_CYC_OFF = ceil(sqrt(JIGL_CNT)/2)
          In all cases observed in 2006 and Jan 2007 this is true.

          In continuum mode on time and off time are the same.

          Unfortunately the only way to tell if continuum mode is enabled is to
          look at the XML. Assume that POINTING is always continuum.

          */
          if (hdr->obstype == SMF__TYP_POINTING) {
            off_time = exp_time;
          } else {
            /* First 2.0 is for Nodding. Second 2.0 is for spreading the off jiggle over both
               sides of the on */
            off_time = 2.0 * ceil(sqrt((double)fitsvals.jigl_cnt)/2.0) * 2.0 * steptime;
          }
        } else if (hdr->swmode == SMF__SWM_PSSW) {
          *status = SAI__ERROR;
          errRep( "", "Not expecting to have to deal with missing exposure time in jiggle/pssw mode", status);
        } else {
          *status = SAI__ERROR;
          errRepf( "", "Unsupported jiggle switch mode (%s).", status,
                   smf_swmode_str( hdr->swmode, status ) );
        }
        break;
      default:
        *status = SAI__ERROR;
        errRepf( "", "Unsupported observing mode (%s).", status,
                smf_obsmode_str( hdr->obsmode, status ) );
    }
    msgOutiff( msglev, "", INDENT "Calculating ON exposure = %g sec and OFF exposure = %g sec", status, exp_time,
               off_time);

    /* fix up the state structure - assume all values identical */
    for (i = 0; i < hdr->nframes; i++) {
      if (missing_exp) tmpState[i].acs_exposure = exp_time;
      if (missing_off) tmpState[i].acs_offexposure = off_time;
    }
    have_fixed |= SMF__FIXED_JCMTSTATE;

  }


  /* Observations that pass through zero azimuth sometimes have
     a few spurious TCS_AZ_AC1 values in the middle of the rwap-around
     from zero to 2.PI. */
  if( fitsvals.utdate == 20070116 && fitsvals.obsnum == 28 ) {
     have_fixed = smf__fix_wrap( &(tmpState[0].tcs_az_ac1), sizeof(*tmpState),
                                 hdr->nframes, have_fixed, status );
     have_fixed = smf__fix_wrap( &(tmpState[0].tcs_az_bc1), sizeof(*tmpState),
                                 hdr->nframes, have_fixed, status );
     have_fixed = smf__fix_wrap( &(tmpState[0].tcs_az_dc1), sizeof(*tmpState),
                                 hdr->nframes, have_fixed, status );
  }

  /* Check for invalid TCS position (i.e. discrepancy between demand and
     actual position. */
  posn_tolerance = smf_get_global0D( "TEL_POS_TOLERANCE", 0.0, status );
  if( posn_tolerance != 0.0 ) {
    posn_valid = smf_validate_tcs_position( hdr, posn_tolerance, 1, status );

    if( ! posn_valid ) {
      have_fixed |= SMF__FIXED_JCMTSTATE;
    }
  }

  return have_fixed;
}


/* internal routines to simplify pattern extraction */


/* Observation lookups */

/* OBSIDs relating to ROVER observations.
   Could do with these being pre-copied into a static AstKeyMap once at startup
   rather than continuing to walk through the array doing pattern matches. If it is
   to slow we could create a struct that had the UT date as an integer and only check
   the string if the integer matches.

   These items are assumed to be sorted.

 */
const ObsIdLUT obsidlut[] = {
    { 20080209,   18, "acsis_00018_20080209T055628", 1 },
    { 20080209,   19, "acsis_00019_20080209T055938", 1 },
    { 20080209,   20, "acsis_00020_20080209T060553", 1 },
    { 20080209,   21, "acsis_00021_20080209T061339", 1 },
    { 20080209,   22, "acsis_00022_20080209T062211", 1 },
    { 20080209,   23, "acsis_00023_20080209T062538", 1 },
    { 20080209,   24, "acsis_00024_20080209T062944", 1 },
    { 20080209,   25, "acsis_00025_20080209T063340", 1 },
    { 20080209,   26, "acsis_00026_20080209T064202", 1 },
    { 20080209,   27, "acsis_00027_20080209T064542", 1 },
    { 20080209,   28, "acsis_00028_20080209T064928", 1 },
    { 20080209,   29, "acsis_00029_20080209T065315", 1 },
    { 20080209,   30, "acsis_00030_20080209T070726", 1 },
    { 20080209,   31, "acsis_00031_20080209T071031", 1 },
    { 20080209,   32, "acsis_00032_20080209T072036", 1 },
    { 20080209,   35, "acsis_00035_20080209T074918", 1 },
    { 20080209,   36, "acsis_00036_20080209T075248", 1 },
    { 20080209,   37, "acsis_00037_20080209T075628", 1 },
    { 20080209,   38, "acsis_00038_20080209T075956", 1 },
    { 20080209,   39, "acsis_00039_20080209T080335", 1 },
    { 20080209,   40, "acsis_00040_20080209T080658", 1 },
    { 20080209,   41, "acsis_00041_20080209T081005", 1 },
    { 20081129,   55, "acsis_00055_20081129T131119", 1 },
    { 20081129,   57, "acsis_00057_20081129T131651", 1 },
    { 20081129,   58, "acsis_00058_20081129T131942", 1 },
    { 20081129,   59, "acsis_00059_20081129T132243", 1 },
    { 20081129,   60, "acsis_00060_20081129T132544", 1 },
    { 20081129,   61, "acsis_00061_20081129T132849", 1 },
    { 20081129,   62, "acsis_00062_20081129T133207", 1 },
    { 20081129,   63, "acsis_00063_20081129T133459", 1 },
    { 20081129,   64, "acsis_00064_20081129T133747", 1 },
    { 20081129,   65, "acsis_00065_20081129T134034", 1 },
    { 20081129,   66, "acsis_00066_20081129T134337", 1 },
    { 20081129,   67, "acsis_00067_20081129T134658", 1 },
    { 20081129,   68, "acsis_00068_20081129T135005", 1 },
    { 20081129,   69, "acsis_00069_20081129T135306", 1 },
    { 20081129,   70, "acsis_00070_20081129T135612", 1 },
    { 20081129,   71, "acsis_00071_20081129T135903", 1 },
    { 20081129,   72, "acsis_00072_20081129T140254", 1 },
    { 20081129,   73, "acsis_00073_20081129T140558", 1 },
    { 20081129,   74, "acsis_00074_20081129T140859", 1 },
    { 20081129,   75, "acsis_00075_20081129T141202", 1 },
    { 20081129,   76, "acsis_00076_20081129T141450", 1 },
    { 20081129,   78, "acsis_00078_20081129T142017", 1 },
    { 20081129,   80, "acsis_00080_20081129T142534", 1 },
    { 20081129,   81, "acsis_00081_20081129T142834", 1 },
    { 20081129,   82, "acsis_00082_20081129T143134", 1 },
    { 20081129,   83, "acsis_00083_20081129T143421", 1 },
    { 20081129,   84, "acsis_00084_20081129T143724", 1 },
    { 20081129,   85, "acsis_00085_20081129T144044", 1 },
    { 20081129,   86, "acsis_00086_20081129T144346", 1 },
    { 20081129,   87, "acsis_00087_20081129T144637", 1 },
    { 20081129,   88, "acsis_00088_20081129T144940", 1 },
    { 20081129,   89, "acsis_00089_20081129T145244", 1 },
    { 20081129,   90, "acsis_00090_20081129T150140", 1 },
    { 20081129,   92, "acsis_00092_20081129T151236", 1 },
    { 20081129,   93, "acsis_00093_20081129T151526", 1 },
    { 20081129,   94, "acsis_00094_20081129T151812", 1 },
    { 20081129,   95, "acsis_00095_20081129T152105", 1 },
    { 20081129,   96, "acsis_00096_20081129T152408", 1 },
    { 20081129,   97, "acsis_00097_20081129T152712", 1 },
    { 20081129,   98, "acsis_00098_20081129T153014", 1 },
    { 20081129,   99, "acsis_00099_20081129T153304", 1 },
    { 20081129,  100, "acsis_00100_20081129T153610", 1 },
    { 20081129,  101, "acsis_00101_20081129T153917", 1 },
    { 20081130,   53, "acsis_00053_20081130T192229", 1 },
    { 20081130,   54, "acsis_00054_20081130T193413", 1 },
    { 20081130,   56, "acsis_00056_20081130T193902", 1 },
    { 20081130,   57, "acsis_00057_20081130T194154", 1 },
    { 20081130,   58, "acsis_00058_20081130T194444", 1 },
    { 20081130,   59, "acsis_00059_20081130T194731", 1 },
    { 20081130,   60, "acsis_00060_20081130T195023", 1 },
    { 20081130,   61, "acsis_00061_20081130T195314", 1 },
    { 20081130,   62, "acsis_00062_20081130T195618", 1 },
    { 20081130,   63, "acsis_00063_20081130T195924", 1 },
    { 20081130,   64, "acsis_00064_20081130T200227", 1 },
    { 20081130,   65, "acsis_00065_20081130T200519", 1 },
    { 20081130,   66, "acsis_00066_20081130T201728", 1 },
    { 20081130,   67, "acsis_00067_20081130T202145", 1 },
    { 20081130,   70, "acsis_00070_20081130T203902", 1 },
    { 20081130,   71, "acsis_00071_20081130T204149", 1 },
    { 20081130,   72, "acsis_00072_20081130T204516", 1 },
    { 20081130,   73, "acsis_00073_20081130T204819", 1 },
    { 20081130,   74, "acsis_00074_20081130T205121", 1 },
    { 20081130,   75, "acsis_00075_20081130T205411", 1 },
    { 20081130,   76, "acsis_00076_20081130T205715", 1 },
    { 20081130,   77, "acsis_00077_20081130T210007", 1 },
    { 20081130,   78, "acsis_00078_20081130T210309", 1 },
    { 20081130,   79, "acsis_00079_20081130T210600", 1 },
    { 20081130,   80, "acsis_00080_20081130T210854", 1 },
    { 20081130,   81, "acsis_00081_20081130T211157", 1 },
    { 20081130,   82, "acsis_00082_20081130T211504", 1 },
    { 20081130,   83, "acsis_00083_20081130T211757", 1 },
    { 20081130,   84, "acsis_00084_20081130T212059", 1 },
    { 20081130,   85, "acsis_00085_20081130T212405", 1 },
    { 20081130,   86, "acsis_00086_20081130T212704", 1 },
    { 20081130,   87, "acsis_00087_20081130T212952", 1 },
    { 20081130,   88, "acsis_00088_20081130T213253", 1 },
    { 20081130,   89, "acsis_00089_20081130T213555", 1 },
    { 20081130,   90, "acsis_00090_20081130T213923", 1 },
    { 20081130,   91, "acsis_00091_20081130T214251", 1 },
    { 20081130,   93, "acsis_00093_20081130T215544", 1 },
    { 20081130,   94, "acsis_00094_20081130T215845", 1 },
    { 20081130,   95, "acsis_00095_20081130T220146", 1 },
    { 20081130,   96, "acsis_00096_20081130T220450", 1 },
    { 20081130,   97, "acsis_00097_20081130T220753", 1 },
    { 20081130,   98, "acsis_00098_20081130T221044", 1 },
    { 20081130,   99, "acsis_00099_20081130T221406", 1 },
    { 20081130,  100, "acsis_00100_20081130T221655", 1 },
    { 20081130,  101, "acsis_00101_20081130T221956", 1 },
    { 20081130,  102, "acsis_00102_20081130T222242", 1 },
    { 20081216,   15, "acsis_00015_20081216T062623", 1 },
    { 20081216,   16, "acsis_00016_20081216T064910", 1 },
    { 20081216,   17, "acsis_00017_20081216T065138", 1 },
    { 20081216,   18, "acsis_00018_20081216T065618", 1 },
    { 20081216,   19, "acsis_00019_20081216T070106", 1 },
    { 20081216,   21, "acsis_00021_20081216T070749", 1 },
    { 20081216,   22, "acsis_00022_20081216T071132", 1 },
    { 20081216,   25, "acsis_00025_20081216T073152", 1 },
    { 20081216,   26, "acsis_00026_20081216T074509", 1 },
    { 20081216,   27, "acsis_00027_20081216T074923", 1 },
    { 20081216,   29, "acsis_00029_20081216T080853", 1 },
    { 20081216,   31, "acsis_00031_20081216T081949", 1 },
    { 20081216,   32, "acsis_00032_20081216T082927", 1 },
    { 20081216,   33, "acsis_00033_20081216T084009", 1 },
    { 20081216,   34, "acsis_00034_20081216T084227", 1 },
    { 20081216,   35, "acsis_00035_20081216T085005", 1 },
    { 20081216,   36, "acsis_00036_20081216T085247", 1 },
    { 20081217,   18, "acsis_00018_20081217T073842", 1 },
    { 20081217,   19, "acsis_00019_20081217T075010", 1 },
    { 20081217,   20, "acsis_00020_20081217T075310", 1 },
    { 20081217,   27, "acsis_00027_20081217T082940", 1 },
    { 20081217,   28, "acsis_00028_20081217T083313", 1 },
    { 20081217,   29, "acsis_00029_20081217T085358", 1 },
    { 20081217,   30, "acsis_00030_20081217T085717", 1 },
    { 20081217,   31, "acsis_00031_20081217T090148", 1 },
    { 20081217,   32, "acsis_00032_20081217T090913", 1 },
    { 20081217,   33, "acsis_00033_20081217T092017", 1 },
    { 20081217,   35, "acsis_00035_20081217T093140", 1 },
    { 20081217,   36, "acsis_00036_20081217T093406", 1 },
    { 20081217,   37, "acsis_00037_20081217T093903", 1 },
    { 20081217,   38, "acsis_00038_20081217T094541", 1 },
    { 20081217,   39, "acsis_00039_20081217T095216", 1 },
    { 20081217,   40, "acsis_00040_20081217T100800", 1 },
    { 20081217,   41, "acsis_00041_20081217T101416", 1 },
    { 20081217,   42, "acsis_00042_20081217T102432", 1 },
    { 20081217,   43, "acsis_00043_20081217T113428", 1 },
    { 20081217,   44, "acsis_00044_20081217T113736", 1 },
    { 20081217,   45, "acsis_00045_20081217T124946", 1 },
    { 20081217,   46, "acsis_00046_20081217T125535", 1 },
    { 20081217,   47, "acsis_00047_20081217T130023", 1 },
    { 20081217,   48, "acsis_00048_20081217T130501", 1 },
    { 20081217,   49, "acsis_00049_20081217T143009", 1 },
    { 20081217,   50, "acsis_00050_20081217T143458", 1 },
    { 20081217,   51, "acsis_00051_20081217T155807", 1 },
    { 20081217,   52, "acsis_00052_20081217T160444", 1 },
    { 20081217,   53, "acsis_00053_20081217T161115", 1 },
    { 20081217,   55, "acsis_00055_20081217T162159", 1 },
    { 20081217,   56, "acsis_00056_20081217T162926", 1 },
    { 20081217,   57, "acsis_00057_20081217T163932", 1 },
    { 20081217,   58, "acsis_00058_20081217T164527", 1 },
    { 20081217,   59, "acsis_00059_20081217T164955", 1 },
    { 20081219,   39, "acsis_00039_20081219T150846", 1 },
    { 20081219,   40, "acsis_00040_20081219T151201", 1 },
    { 20081219,   41, "acsis_00041_20081219T151530", 1 },
    { 20081219,   42, "acsis_00042_20081219T151841", 1 },
    { 20081219,   43, "acsis_00043_20081219T152147", 1 },
    { 20081219,   44, "acsis_00044_20081219T152520", 1 },
    { 20081219,   45, "acsis_00045_20081219T152841", 1 },
    { 20081219,   46, "acsis_00046_20081219T153201", 1 },
    { 20081219,   47, "acsis_00047_20081219T153547", 1 },
    { 20081219,   48, "acsis_00048_20081219T153918", 1 },
    { 20081219,   49, "acsis_00049_20081219T154236", 1 },
    { 20081219,   50, "acsis_00050_20081219T154557", 1 },
    { 20081219,   51, "acsis_00051_20081219T154924", 1 },
    { 20081219,   52, "acsis_00052_20081219T155319", 1 },
    { 20081219,   53, "acsis_00053_20081219T155655", 1 },
    { 20081219,   54, "acsis_00054_20081219T160024", 1 },
    { 20081219,   55, "acsis_00055_20081219T160332", 1 },
    { 20081219,   56, "acsis_00056_20081219T160641", 1 },
    { 20081219,   57, "acsis_00057_20081219T160954", 1 },
    { 20081219,   58, "acsis_00058_20081219T161301", 1 },
    { 20081219,   59, "acsis_00059_20081219T161620", 1 },
    { 20081219,   60, "acsis_00060_20081219T162057", 1 },
    { 20081219,   61, "acsis_00061_20081219T162536", 1 },
    { 20081219,   62, "acsis_00062_20081219T163012", 1 },
    { 20081219,   63, "acsis_00063_20081219T163457", 1 },
    { 20090130,   47, "acsis_00047_20090130T133241", 1 },
    { 20090130,   48, "acsis_00048_20090130T133545", 1 },
    { 20090130,   49, "acsis_00049_20090130T133847", 1 },
    { 20090130,   50, "acsis_00050_20090130T134202", 1 },
    { 20090130,   51, "acsis_00051_20090130T134509", 1 },
    { 20090130,   52, "acsis_00052_20090130T134808", 1 },
    { 20090130,   53, "acsis_00053_20090130T135123", 1 },
    { 20090130,   54, "acsis_00054_20090130T140406", 1 },
    { 20090130,   55, "acsis_00055_20090130T140722", 1 },
    { 20090130,   56, "acsis_00056_20090130T141042", 1 },
    { 20090130,   57, "acsis_00057_20090130T141357", 1 },
    { 20090130,   58, "acsis_00058_20090130T141713", 1 },
    { 20090130,   59, "acsis_00059_20090130T142024", 1 },
    { 20090130,   60, "acsis_00060_20090130T142335", 1 },
    { 20090130,   61, "acsis_00061_20090130T142637", 1 },
    { 20090130,   62, "acsis_00062_20090130T142955", 1 },
    { 20090130,   63, "acsis_00063_20090130T143327", 1 },
    { 20090130,   64, "acsis_00064_20090130T143701", 1 },
    { 20090130,   65, "acsis_00065_20090130T144014", 1 },
    { 20090130,   66, "acsis_00066_20090130T144341", 1 },
    { 20090130,   67, "acsis_00067_20090130T144721", 1 },
    { 20090130,   68, "acsis_00068_20090130T145050", 1 },
    { 20090130,   69, "acsis_00069_20090130T145407", 1 },
    { 20090130,   70, "acsis_00070_20090130T145716", 1 },
    { 20090130,   71, "acsis_00071_20090130T150051", 1 },
    { 20090130,   72, "acsis_00072_20090130T150425", 1 },
    /* make sure this UT date is stored in "maxut" if
       and extra line is added */
    { 20090130,   73, "acsis_00073_20090130T150726", 1 },
    { VAL__BADI, VAL__BADI, "", VAL__BADI }
};
/* keep this value in sync with last line above */
const int maxut = 20090130;

/* looks through the LUT and finds the matchinf row. Returns null if nothing matches */

static const ObsIdLUT * smf__find_obsidlut( const struct FitsHeaderStruct * fitsvals,
                                            int *status ) {
  int i;

  if (*status != SAI__OK) return NULL;

  /* drop out immediately if the most recent entry is too old to
     be relevant. Do not bother to check the oldest since that
     will be checked immediately in the while loop */
  if (fitsvals->utdate > maxut ) return NULL;

  /* now look at each entry */
  i = 0;
  while ( obsidlut[i].utdate != VAL__BADD ) {
    if ( fitsvals->utdate < obsidlut[i].utdate ) {
      /* this UT date is older than the current value being tested
         so there is no match */
      return NULL;
    } else if (fitsvals->utdate > obsidlut[i].utdate ) {
      /* this UT date is newer so try again */
    } else {
      /* have a match now compare obsnum */
      if (fitsvals->obsnum == obsidlut[i].obsnum) {
        /* match obsnum so confirm with OBSID match */
        if ( strcmp( fitsvals->obsid, obsidlut[i].obsid ) == 0 ) {
          return &(obsidlut[i]);
        }
      }
    }
    i++;
  }
  return NULL; /* no match */
}


/* See if the supplied longitude values pass through zero and back to
   2*PI. If so, it sets bad any values that fall in the middle of the
   wrap-around. */

static int smf__fix_wrap( double *start, size_t stride, size_t nel,
                          int have_fixed, int *status ){

  size_t hi;                 /* Index of last value to remove */
  size_t hi0;                /* Index of first high azimuth value */
  size_t hi1;                /* Index of last high azimuth value */
  size_t i;                  /* Time slice index */
  size_t lo;                 /* Index of first value to remove */
  size_t lo0;                /* Index of first low azimuth value */
  size_t lo1;                /* Index of last low azimuth value */
  double *p;                 /* Pointer to next angle value */

  if( *status != SAI__OK ) return have_fixed;

  /* Convert the stride from bytes to doubles. */
  stride /= sizeof(double);

  /* Find the indices of the first and last longitude values that are
     close to zero (i.e. below 0.01 radians). Also find the indices of
     the first and last longitude values that are close to 2.PI (i.e.
     above 6.27 radians). */
  p = start;
  lo0 = lo1 = hi0 = hi1 = SMF__BADSZT;
  for( i = 0; i < nel; i++ ) {
    if( *p != VAL__BADD ) {
      if( *p < 0.01 ) {
        if( lo0 == SMF__BADSZT ) lo0 = i;
        lo1 = i;
      }
      if( *p > 6.27 ) {
         if( hi0 == SMF__BADSZT ) hi0 = i;
         hi1 = i;
      }
    }
    p += stride;
  }

  /* Check vaalues both below 0.01 and above 6.27 were found. */
  if( lo1 != SMF__BADSZT && hi1 != SMF__BADSZT ) {

    /* Get the lowest and highest indices to set bad. This depends on
      whether the longitude values are generally decreasing or generally
      increasing. */
    if( hi1 > lo1 ) {
       lo = lo1;
       hi = hi0;
    } else {
       lo= hi1;
       hi = lo0;
    }

    /* Back off by one sample to avoid removing good samples */
    lo++;
    hi--;

    /* Set the required range of longitude values to VAL__BADD. */
    p = start + lo*stride;
    for( i = lo; i <= hi; i++ ) {
      *p = VAL__BADD;
      p += stride;
    }

    have_fixed |= SMF__FIXED_JCMTSTATE;
  }

  return have_fixed;
}

