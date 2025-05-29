/*
 *+
 *  Name:
 *     sc2sim_simulate

 *  Purpose:
 *     Simulate a SCUBA-2 observation

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     SC2SIM subroutine

 *  Invocation:
 *     sc2sim_simulate ( struct sc2sim_obs_struct *inx,
 *                       struct sc2sim_sim_struct *sinx,
 *                       double coeffs[], double digcurrent, double digmean,
 *                       double digscale, char filter[], double *heater,
 *                       int maxwrite, obsMode mode, mapCoordframe coordframe,
 *                       int nbol, double *pzero, int rseed, double samptime,
 *                       double weights[], double *xbc, double *xbolo,
 *                       double *ybc, double *ybolo,
 *                       int hitsonly, int overwrite, int *status);

 *  Arguments:
 *     inx = sc2sim_obs_struct* (Given)
 *        Structure for values from XML
 *     sinx = sc2sim_sim_struct* (Given)
 *        Structure for sim values from XML
 *     coeffs = double[] (Given)
 *        Bolometer response coeffs
 *     digcurrent = double (Given)
 *        Digitisation mean current
 *     digmean = double (Given)
 *        Digitisation mean value
 *     digscale = double (Given)
 *        Digitisation scale factor
 *     filter = char[] (Given)
 *        String to hold filter name
 *     heater = double* (Given)
 *        Bolometer heater ratios
 *     maxwrite = int (Given)
 *        File close time
 *     mode = obsMode (Given)
 *        Observation mode
 *     coordframe = mapCoordframe (Given)
 *        Coordinate frame for the map
 *     nbol = int (Given)
 *        Total number of bolometers
 *     pzero = double* (Given)
 *        Bolometer power offsets
 *     rseed = int (Given)
 *        Seed for random number generator
 *     samptime = double (Given)
 *        Sample time in sec
 *     weights = double[] (Given)
 *        Impulse response
 *     xbc = double* (Given)
 *        Projected NAS X offsets of bolometers in arcsec
 *     xbolo = double* (Given)
 *        Native bolo x-offsets
 *     ybc = double* (Given)
 *        Projected NAS Y offsets of bolometers in arcsec
 *     ybolo = double* (Given)
 *        Native bolo y-offsets
 *     hitsonly = int (Given)
 *        Flag to indicate hits-only simulation
 *     overwrite = int (GIven)
 *        Flag to indicate whether to overwrite existing files
 *     simstats = int (Given)
 *        Flag to indicate whether to just report simulation statistics
 *        and exit
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     The is the main routine implementing the SIMULATE task.
 *
 *     This attempts to simulate the data taken by a SCUBA2 subarray
 *     (or subarrays) when observing an astronomical image plus atmospheric
 *     background while driving the JCMT.  The simulation includes photon
 *     and 1/f noise, and nonlinear response which varies for different
 *     bolometers.  It also includes SCUBA-2 field distortion.
 *
 *     Planet observations are now supported. In this case the WCS
 *     FrameSet is created on the fly. In principle this should be done
 *     at every time step, but in practice planets do not move
 *     significantly on the sky in 5 ms. Currently the geocentric
 *     apparent coordinates are calculated every 0.5 s (100 time
 *     steps).
 *
 *     smf_simulate combines the functionality of a number of executables
 *     built in earlier versions of the simulator : staresim, dreamsim,
 *     pongsim

 *  Notes:
 *     One of the fundamental properties of an astronomical simulator
 *     is time. In the SCUBA-2 simulator the user specifies a start
 *     time as a UTC modified Julian date. The internal time axis is
 *     established as a UT1 MJD. Planet positions are calculated using
 *     Terrestrial Time (TT). The JCMT state structure has two time
 *     entries: TCS_TAI is the TAI time of the current sample and
 *     RTS_END is the TAI at the end of the current sample. The sample
 *     length (steptime) is also taken to be in units of UTC seconds
 *     (which are the same as TAI but NOT the same as the UT/UT1
 *     second). See the documentation for SOFA for further
 *     information on astronomical time systems.

 *  Authors:
 *     Tim Jenness (JAC, Hawaii)
 *     Andy Gibb (UBC)
 *     Edward Chapin (UBC)
 *     David Berry (JAC, UCLan)
 *     B.D.Kelly (ROE)
 *     Jen Balfour (UBC)
 *     Christa VanLaerhoven (UBC)
 *     {enter_new_authors_here}

 *  History:
 *     2006-03-28 (EC):
 *        Original version
 *     2006-04-19 (EC):
 *        Added jiggle offsets, filename consistent with mjd
 *     2006-06-06 (AGG/EC/JB):
 *        Clone from smurf_makemap
 *     2006-06-09 (JB):
 *        Added heatrun task
 *     2006-07-26 (JB):
 *        Moved into sc2sim_simulate
 *     2006-07-28 (JB):
 *        Changed sc2head to JCMTState
 *     2006-08-07 (TIMJ):
 *        GRP__NOID is not a Fortran concept.
 *     2006-08-08 (JB)
 *        Replaced call to sc2sim_hor2eq with call to slaDh2e
 *     2006-08-17 (TIMJ):
 *        Don't rely on a loop variable outside of the loop
 *     2006-08-18 (EC)
 *        Improved status handling, constants from smurf_par
 *        Fixed large number of memory leaks
 *        Removed unnecessary fopen/ndfGtwcs calls
 *     2006-08-21 (EC)
 *        Annul sc2 frameset at each time slice after calling simframe
 *     2006-09-01 (JB)
 *        Removed dependence on sc2sim_telpos
 *     2006-09-05 (JB)
 *        Check for ast & atm files.
 *     2006-09-06 (EC)
 *        Modified ndfwrdata call to include INSTRUME keyword
 *     2006-09-07 (EC):
 *        Modified sc2ast_createwcs calls to use new interface.
 *     2006-09-08 (EC):
 *        Modified call to sc2sim_calctime to use new interface.
 *     2006-09-11 (EC):
 *        Fixed pointer problem with callc to smf_calc_telpos
 *     2006-09-13 (EC):
 *        Removed another instance of hard-wired telescope coordinates
 *     2006-09-14 (EC):
 *        Added the ability to define scans in AzEl and RaDec coord. frames
 *     2006-09-22 (JB):
 *        Replaced dxml_structs with sc2sim_structs
 *     2006-10-03 (JB):
 *        Use width & height instead of gridcount in PONG
 *     2006-10-10 (JB) :
 *        Fill tcs_tai component.
 *     2006-10-16 (EC):
 *        Fixed a sign error in the rotation of the map coordinate frame.
 *     2006-10-17 (JB):
 *        Check for pong_type
 *     2006-10-18 (AGG):
 *        Ensure jig_x/y coordinates are in the correct units (radians)
 *     2006-11-16 (JB):
 *        Pass accel to curve PONG
 *     2006-11-21 (JB):
 *        Add liss mode
 *     2006-11-22 (JB):
 *        Add multiple map cycle capabilites to liss/pong
 *     2006-12-01 (AGG):
 *        Add DATE-OBS calculation
 *     2006-12-07 (JB):
 *        Merged with sc2sim_simhits and streamlined memory usage.
 *     2006-12-14 (JB):
 *        Corrected check for missing heatrun files.
 *     2006-12-14 (TIMJ):
 *        Put AST effective position error check in correct place
 *     2006-12-14 (AGG):
 *        Corrections to coordinate/time processing to makes things
 *        consistent. RTS_END is now written as a TAI time.
 *     2006-12-15 (AGG):
 *        TAI-UTC obtained from slaDat, assume DUT1 is zero
 *     2006-12-18 (AGG):
 *        DUT1 now obtained from input struct
 *     2006-12-18 (JB):
 *        Replace pattern-specific parameters with general
 *        parameters.
 *     2006-12-21 (AGG):
 *        Set TAI to midpoint of sample, RTS to end. Use instap_x/y
 *        from inx struct
 *     2007-01-10 (AGG):
 *        - Add check that source is above 20 deg at start of observation
 *        - Fix off-by-one bug in digitizing signal
 *        - Set airmass to self-consistent value below 1 deg
 *     2007-01-10 (AGG):
 *        Add planet observations
 *     2007-01-11 (AGG):
 *        Set BASE position correctly for planets
 *     2007-01-16 (AGG):
 *        - Fill SMU_CHOP_PHASE, TCS_BEAM and TCS_SOURCE header entries
 *        - Fix time bug in calculating planet positions
 *     2007-01-26 (AGG):
 *        Add `overwrite' flag to allow simulations to create new files
 *        without overwriting old ones
 *     2007-02-01 (AGG):
 *        Fix `zero remainder' bug in calculating number of frames in
 *        last file
 *     2007-02-26 (AGG):
 *        Store planet RA, Dec in inx struct so they can be written to
 *        the FITS header
 *     2007-03-01 (AGG):
 *        Add simstats to API for reporting simulation statistics
 *     2007-03-20 (AGG):
 *        Update time passed to simframe to be the number of seconds
 *        since the simulation started, not the full MJD.
 *     2007-03-27 (AGG):
 *        Make this time-since-start a new variable for clarity
 *     2007-04-02 (AGG):
 *        Derive more FITS headers, add extra args to ndfwrdata
 *     2007-04-02 (EC):
 *        Moved telpos into sinx
 *     2007-05-29 (AGG):
 *        Minor change to error messages if atm/ast files could not be opened.
 *     2007-06-29 (EC):
 *        Try to re-set digitisation coefficients once the sky level is known,
 *        overriding values calculated in sc2sim_instrinit
 *     2007-07-03 (EC):
 *        Made obsMode and mapCoordframe enumerated types more readable.
 *     2007-07-06 (AGG):
 *        Initialize FITS header strings
 *     2007-08-15 (CV):
 *        Added microstepping ability for STARE observations
 *     2007-08-27 (CV):
 *        Changed from microstepping by changing the telescope boresight
 *        pointing to using instrument aperature offsets
 *     2007-10-05 (AGG):
 *        Add subscanno and obsend variables to allow sc2sim_ndfwrdata
 *        to write the correct OBSEND keyword.
 *     2007-10-26 (EC):
 *        Pass curframe instead of frame to sc2sim_simframe to fix step
 *        in sky-signal bug.
 *     2007-10-29 (EC):
 *        Modified interface to smf_open_file.
 *     2007-10-31 (TIMJ):
 *        use dim_t for some variables following sc2store mods
 *     2007-12-18 (AGG):
 *        Update to use new smf_free behaviour
 *     2008-01-30 (AGG):
 *        - Fix posptr bug when calling sc2sim_ndfwrdata
 *        - Factor out calculation of frame offset into time series for
 *          current output file
 *     2008-02-08 (CV):
 *        Set obscounter to 1 at the beginning of the simulation and
 *        increment as necessary rather than resetting to 1 (then
 *        incrementing as necessary) for each file.
 *     2008-04-02 (TIMJ):
 *        Fix strncpy usage.
 *     2008-04-18 (AGG):
 *        Use transmission for current wavelength to get atmospheric power
 *     2008-04-25 (AGG):
 *        Add loop over focus positions
 *     2008-05-14 (AGG):
 *        Take focus step into account for setting obsend flag
 *     2008-05-23 (AGG):
 *        Pass focposn to sc2sim_ndfwrdata
 *     2008-07-17 (TIMJ):
 *        - initialise scancrd
 *        - use one_strlcpy
 *     2008-08-25 (AGG):
 *        Force sc2store to think it's initialized to avoid EMS stack warnings
 *     2008-10-10 (AGG):
 *        Add NOISE observations
 *     2009-11-13 (TIMJ):
 *        Use AST to determine pixel scale of astronomical image.
 *     2009-11-24 (DSB):
 *        Assign values to state.tcs_az_bc1/2 before calling sc2ast_createwcs.
 *     2012-03-06 (TIMJ):
 *        Use PAL+SOFA instead of SLA for all SLA routines except slaRdplan.
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2007-2009, 2012 Science and Technology Facilities Council.
 *     Copyright (C) 2006-2007 Particle Physics and Astronomy Research
 *     Council.
 *     Copyright (C) 2006-2008 University of British Columbia. All
 *     Rights Reserved.

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

/* Standard includes */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/one.h"
#include "star/pal.h"
#include "erfa.h"
#include "erfam.h"

/* JCMT includes */
#include "jcmt/state.h"
#include "wvm/wvmCal.h" /* Water Vapor Monitor routines */

/* SC2DA includes */
#include "sc2da/Dits_Err.h"
#include "sc2da/Ers.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2ast.h"

/* Simulator includes */
#include "sc2sim.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmurf/smurflib.h"
#include "libsmf/smf.h"

#define FUNC_NAME "sc2sim_simulate"
#define LEN__METHOD 20

#define INSTRUMENT "SCUBA-2"

void sc2sim_simulate ( struct sc2sim_obs_struct *inx,
                       struct sc2sim_sim_struct *sinx,
                       double coeffs[], double digcurrent, double digmean,
                       double digscale, char filter[], double *heater,
                       int maxwrite, obsMode mode, mapCoordframe coordframe,
                       int nbol, double *pzero, int rseed, double samptime,
                       double weights[], double *xbc, double *xbolo,
                       double *ybc, double *ybolo,
                       int hitsonly, int overwrite, int simstats,
                       int *status ) {

  double accel[2];                /* telescope accelerations (arcsec) */
  double aeff[3];                 /* output of wvmOpt */
  double *airmass=NULL;           /* mean airmass of observation */
  double amprms[21];              /* AMPRMS parameters for SLALIB routines */
  smfData *astdata=NULL;          /* pointer to SCUBA2 data struct */
  smfHead *asthdr=NULL;           /* pointer to header in data */
  int astnaxes[2];                /* dimensions of simulated image */
  double astscale;                /* pixel size in simulated image */
  smfData *atmdata=NULL;          /* pointer to SCUBA2 data struct */
  smfHead *atmhdr=NULL;           /* pointer to header in data */
  int atmnaxes[2];                /* dimensions of simulated atm background */
  double atmscale;                /* pixel size in simulated atm background */
  double *base_az=NULL;           /* Az of telescope base */
  double *base_el=NULL;           /* El of BASE telescope position */
  double *base_p=NULL;            /* Parall. ang. of BASE at time step */
  int bol;                        /* counter for indexing bolometers */
  double *bor_az=NULL;            /* Az of telescope in spherical coord. */
  double *bor_dec=NULL;           /* telescope dec. spherical coordinates */
  double *bor_el=NULL;            /* El of telescope in spherical coord. */
  double *bor_ra=NULL;            /* telescope r.a. spherical coordinates */
  double bor_y_cel=0;             /* boresight y-celestial tanplane offset */
  double bor_y_hor=0;             /* boresight y-horizontal tanplane offset */
  double bor_y_nas=0;             /* boresight y-nasmyt0h tanplane offset */
  double bor_x_cel=0;             /* boresight y-celestial tanplane offset */
  double bor_x_hor=0;             /* boresight x-horizontal tanplane offset */
  double bor_x_nas=0;             /* boresight x-nasmyth tanplane offset */
  int chunks;                     /* number of chunks of size maxwrite
                                     needed to complete the simulation */
  dim_t colsize;                  /* column size for flatfield */
  double corner;                  /* corner frequency in Hz */
  int count;                      /* number of samples in full pattern */
  int curchunk;                   /* current chunk of simulation */
  int curframe;                   /* current frame in context of entire
                                     simulation (not just this chunk) */
  int curms;                      /* current microstep (loop counter) */
  char dateobs[SZFITSTR] = "\0";  /* DATE-OBS string for observation */
  int date_da;                    /* day corresponding to MJD */
  double date_df;                 /* day fraction corresponding to MJD */
  int date_mo;                    /* month corresponding to MJD */
  int date_yr;                    /* year corresponding to MJD */
  int date_hr;                    /* Hour corresponding to MJD */
  int date_mn;                    /* minute corresponding to MJD */
  int date_status;                /* status of mjd->calendar date conversion*/
  double *dbuf=NULL;              /* simulated data buffer */
  double decapp;                  /* Apparent Dec */
  double decapp1;                 /* Recalculated apparent Dec */
  double diam;                    /* Angular diameter of planet */
  int *digits=NULL;               /* output data buffer */
  int *dksquid=NULL;              /* dark squid values */
  int dodigcalc=1;                /* If set, calculate digitization pars */
  double drytau183;               /* Broadband 183 GHz zenith optical depth */
  double dtt = 0;                 /* Time difference between UTC and TT (TT - UTC s)*/
  double exptime = 0.0;           /* Subimage exposure time */
  AstFitsChan *fc=NULL;           /* FITS channels for tanplane projection */
  int fileexists = 1;             /* Flag to denote whether the named
                                     output file already exists */
  char filename[SC2SIM__FLEN];    /* name of output file */
  AstFitsChan *fitschan=NULL;     /* FITS channels for tanplane projection */
  AstFrameSet *fitswcs=NULL;      /* Frameset for input image WCS */
  double *flatcal[8];             /* flatfield calibrations for all
                                     subarrays */
  char flatname[8][SC2STORE_FLATLEN];/* flatfield algorithm names for
                                        all subarrays */
  double *flatpar[8];             /* flatfield parameters for all subarrays */
  dim_t focidx;                   /* Focus position counter */
  double focposn;                 /* SMU focus position */
  int frame;                      /* frame counter */
  int frameoffset;                /* Frame offset into time stream for current file */
  int frmperms;                   /* number of frames per microstep */
  AstFrameSet *fs=NULL;           /* frameset for tanplane projection */
  int obscounter=1;               /* Counter for observation number
                                     portion of output filename */
  JCMTState *head = NULL;         /* per-frame headers */
  char heatname[SC2SIM__FLEN];    /* name of flatfield cal file */
  double hourangle;               /* Current hour angle */
  int i;                          /* loop counter */
  int ihmsf[4];                   /* H, M, S and fractional seconds */
  double instap[2];               /* Focal plane instrument offsets */
  int j;                          /* loop counter */
  double jigpat[SC2SIM__MXSIM][2];/* pointing: nas jiggle offsets from cen.
                                      in ARCSEC */
  dim_t jigsamples=1;             /* number of samples in jiggle pattern */
  double *jig_y_hor=NULL;         /* jiggle y-horizontal tanplane offset (radians) */
  double *jig_x_hor=NULL;         /* jiggle x-horizontal tanplane offset (radians) */
  int k;                          /* loop counter */
  int lastframe;                  /* number of frames in the last chunk */
  char loclcrd[SZFITSTR] = "\0";  /* Coordinate frame */
  double *lst=NULL;               /* local appar. sidereal time at time step */
  char lstend[SZFITSTR] = "\0";   /* LST at end of sub-scan */
  char lststart[SZFITSTR] = "\0"; /* LST at start of sub-scan */
  double meanatm;                 /* Atmos. emission at start airmass */
  double *mjuldate=NULL;          /* Modified Julian date each sample - UT1 */
  dim_t nflat[8];                 /* number of flat coeffs per bol */
  int nimage = 0;                 /* Number of subimages within subscan */
  static double noisecoeffs[SC2SIM__MXBOL*3*60]; /* noise coefficients */
  int noutfiles = 1;              /* Total number of output files per subarray */
  int nterms=0;                   /* number of 1/f noise frequencies */
  int obsend = 0;                 /* Flag to indicate whether current file is last in
                                     observation */
  char obsid[SZFITSTR];           /* OBSID for each observation */
  char obstype[SZFITSTR];         /* Observation type, e.g. SCIENCE */
  FILE *ofile = NULL;             /* File pointer to check for existing files*/
  double phi;                     /* latitude (radians) */
  int planet = 0;                 /* Flag to indicate planet observation */
  double pnoise=0;                /* photon noise due to atmosphere */
  double *posptr=NULL;            /* pointing: nasmyth offsets (arcsec) */
  double pwvlos;                  /* mm precip. wat. vapor. line of site */
  double pwvzen = 0;              /* zenith precipital water vapour (mm) */
  double raapp;                   /* Apparent RA */
  double raapp1;                  /* Recalculated apparent RA */
  double refres[8];               /* Reference resistance used for flatfield */
  dim_t rowsize;                  /* row size for flatfield */
  char scancrd[SZFITSTR];         /* SCAN coordinate frame */
  double sigma;                   /* instrumental white noise */
  char sign[2];                   /* Sign of angle (+/-) */
  Grp *skygrp = NULL;             /* Group of input files */
  AstMapping *sky2map=NULL;       /* Mapping celestial->map coordinates */
  double sky_el=0;                /* effective el on sky (bor+jig) */
  double sky_x_hor=0;             /* effective x hor. off. on sky (bor+jig) */
  double sky_y_hor=0;             /* effective y hor. off. on sky (bor+jig) */
  double skytrans;                /* sky transmission */
  JCMTState state;                /* Telescope state at one time slice */
  double start_time=0.0;          /* UTC time of start of current scan */
  dim_t steps_per_map = 0;        /* Number of steps in single pass of scan map */
  int subnum;                     /* Subarray number */
  int subscanno = 0;              /* Sub-scan number (last number in output filanem)*/
  double taiutc;                  /* Difference between TAI and UTC time (TAI-UTC s) */
  double tauCSO=0;                /* CSO zenith optical depth */
  double tbri[3];                 /* simulated wvm measurements */
  double teff[3];                 /* output of wvmOpt */
  double temp1;                   /* store temporary values */
  double temp2;                   /* store temporary values */
  double temp3;                   /* store temporary values */
  double timesincestart = 0.0;    /* Time since start of simulation */
  double totaltime;               /* Total integration time */
  double tt;                      /* Terrestrial Time (TT) for calculating planet position */
  double ttau[3];                 /* output of wvmOpt */
  double twater;                  /* water line temp. for WVM simulation */
  char utdate[SZFITSTR] = "\0";   /* UT date in YYYYMMDD form */
  double vmax[2];                 /* telescope maximum velocities (arcsec) */
  double zenatm;                  /* zenith atmospheric emission */

  if ( *status != SAI__OK) return;

  /* Main routine */
  ndfBegin ();

  /* Force sc2store to think it's initialized */
  sc2store_force_initialised( status );

  /* Setup instap (converted from arcsec to radians) and telpos */
  smf_calc_telpos( NULL, "JCMT", sinx->telpos, status );
  sc2sim_instap_calc( inx, 0, instap, status );

  if( *status == SAI__OK ) {
    /* Calculate year/month/day corresponding to MJD at start.
       Remember that inx->mjdaystart is a UTC date */
    date_status = eraJd2cal( ERFA_DJM0, inx->mjdaystart, &date_yr, &date_mo,
                             &date_da, &date_df );
    if( date_status ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Couldn't calculate calendar date from MJD", status);
    }
  }

  if ( !hitsonly && ( *status == SAI__OK ) ) {

    /* Get simulation of astronomical and atmospheric images. */
    msgOutif(MSG__VERB," ",
             "Get astronomical and atmospheric images", status);

    /* Create a group to store the sky images, and open them. */
    skygrp = grpNew ( "GRP", status );
    grpPut1 ( skygrp, sinx->astname, 1, status );
    grpPut1 ( skygrp, sinx->atmname, 2, status );

    smf_open_file( NULL, skygrp, 1, "READ", 0, &astdata, status);
    if ( *status != SAI__OK ) {
      msgSetc ( "FILENAME", sinx->astname );
      errRep( " ", "Unable to open astronomical file ^FILENAME", status );
      goto CLEANUP;
    }

    smf_open_file( NULL, skygrp, 2, "READ", 0, &atmdata, status);
    if ( *status != SAI__OK ) {
      msgSetc ( "FILENAME", sinx->atmname );
      errRep( " ", "Unable to open atmospheric file ^FILENAME", status);
      goto CLEANUP;
    }

    if( *status == SAI__OK ) {
      atmhdr = atmdata->hdr;
      smf_fits_getD ( atmhdr, "PIXSIZE", &atmscale, status );
    }

    /* Retrieve the WCS info from the astronomical image. For planet
       observations this is not meaningful so WCS is created later */
    if( *status == SAI__OK ) {
      fitswcs = astdata->hdr->wcs;
    }

    /* Check the dimensions of the ast and atm data. */
    if( *status == SAI__OK ) {
      if ( ( astdata->ndims ) != 2 ) {
        msgSetc ( "FILENAME", sinx->astname );
        errRep(FUNC_NAME,
               "^FILENAME should have 2 dimensions, but it does not.",
               status);
        *status = DITS__APP_ERROR;
        goto CLEANUP;
      }

      if ( ( atmdata->ndims ) != 2 ) {
        msgSetc ( "FILENAME", sinx->atmname );
        errRep(FUNC_NAME,
               "^FILENAME should have 2 dimensions, but it does not.",
               status);
        *status = DITS__APP_ERROR;
        goto CLEANUP;
      }
    }

    /* Set the PLANET flag */
    if ( inx->planetnum != -1 ) {
      planet = 1;
      dtt = palDtt( start_time ); /* start_time is UTC */
    } else {
      planet = 0;
    }

    if( *status == SAI__OK ) {
      /* Retrieve the dimensions of the ast & atm images */
      astnaxes[0] = (astdata->dims)[0];
      astnaxes[1] = (astdata->dims)[1];
      atmnaxes[0] = (atmdata->dims)[0];
      atmnaxes[1] = (atmdata->dims)[1];

      /* Extract the Sky->map pixel mapping for the astronomical
         image. As noted above, this will have no meaning for planet
         observations since the WCS FrameSet is noted created until
         later. */
      if ( planet ) {
        astSetC( fitswcs, "SYSTEM", "GAPPT" );
      } else {
        astSetC( fitswcs, "SYSTEM", "ICRS" );
      }
      sky2map = astGetMapping( fitswcs, AST__CURRENT, AST__BASE );

      if( !astOK ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "AST error extracting sky->image pixel mapping",
               status);
      }
    }

    /* Retrieve the astscale and atmscale from the FITS headers. */
    if( *status == SAI__OK ) {
      double gridc[2];
      double pixsc[2];

      gridc[0] = astnaxes[0] / 2.0;
      gridc[1] = astnaxes[1] / 2.0;

      kpgPixsc( fitswcs, gridc, pixsc, NULL, NULL, 0, status );
      if (*status == SAI__OK) {
        astscale = (pixsc[0] + pixsc[1]) / 2.0;
        astscale *= ERFA_DR2AS;
      } else {
        errFlush(status);
        errAnnul( status );
        asthdr = astdata->hdr;
        smf_fits_getD ( asthdr, "PIXSIZE", &astscale, status );
        /* If PIXSIZE couldn't be found, then look for CDELT2 */
        if ( *status != SAI__OK ) {
          errAnnul(status);
          smf_fits_getD ( asthdr, "CDELT2", &astscale, status );
          astscale *= 3600.0; /* Convert from deg to arcsec */
        }
      }
      msgOutiff( MSG__VERB, "","Input image has pixel scale of %.1f arcsec",
                status, astscale );
    }


  }/* if not hits-only */

  /* Set some static FITS headers */
  one_strlcpy( obstype, "SCIENCE", sizeof(obstype), status );
  one_strlcpy( scancrd, "TRACKING", sizeof(scancrd), status );

  /* KLUDGE !!! */
  exptime = 200.0 * inx->steptime;

  if( *status == SAI__OK ) {
    /*  Re-initialise random number generator to give a different sequence
        each time by using the given seed. */
    srand ( rseed );

    /* Initialize SMU nasmyth jiggle offsets to 0 */
    for( i=0; i<SC2SIM__MXSIM; i++ ) {
      for( j=0; j<2; j++ ) {
        jigpat[i][j] = 0;
      }
    }

    /* Get the relevant pointing solution for the telescope based on the
       observation type */
    msgOutif(MSG__VERB," ", "Get pointing solution", status );

    /* The three primary observing modes are STARE, DREAM, and SCAN.
       In STARE, the telescope points in one direction.  In DREAM, the
       SMU is jiggled while the primary mirror remains stationary.  In
       the SCAN patterns, the telescope slews across the sky to create
       larger maps.  The simulator can recreate the following scanning
       patterns :

       singlescan : Single straight line segment.

       bous : Simple Boustrophedon (raster) pattern.

       liss : Lissajous pattern.

       pong : StraightPong fills in a rectangular region with
       crosslinking straight line segments at angles of
       45 degrees relative to the sides of the box.
       CurvePong approximates the StraightPong pattern
       by Fourier-expanding the Lissajous pattern with
       five terms, resulting in approximately straight
       sweeps across the central region of the map, with
       smooth curved turnarounds at the edges of the map. */

    /* Retrieve the map grid coordinates for each step in the
       pattern, and determine the number of frames required to
       complete the observation */
    switch( mode ) {

    case MODE__STARE:
      /* Stare just points at a nasmyth offset of 0 from the map centre */
      msgOutif(MSG__VERB, " ", "Do a STARE observation", status );
      count = inx->numsamples;
      posptr = astCalloc( count*2, sizeof(*posptr) );
      if( *status == SAI__OK ) {
        memset( posptr, 0, count*2*sizeof(double) );
      }

      break;

    case MODE__NOISE:
      /* A noise observation is just a short Stare */
      msgOutif(MSG__VERB, " ", "Do a NOISE observation", status );
      /* Always only lasts 10 s */
      count = 10. / inx->steptime;
      posptr = astCalloc( count*2, sizeof(*posptr) );
      if( *status == SAI__OK ) {
        memset( posptr, 0, count*2*sizeof(double) );
      }

      break;

    case MODE__DREAM:
      /* Call sc2sim_getpat to get the dream pointing solution */
      msgOutif(MSG__VERB, " ", "Do a DREAM observation", status );

      /*  Get jiggle pattern.
          jigpat[*][0] - X-coordinate in arcsec/time of the Jiggle position.
          jigpat[*][1] - Y-coordinate in arcsec/time of the Jiggle position.
          The number of values is returned in count, and should be equal
          to the number of samples per cycle. */

      sc2sim_getpat ( inx->nvert, inx->smu_samples, inx->steptime,
                      inx->smu_offset+sinx->smu_terr, inx->conv_shape,
                      inx->conv_sig, inx->smu_move, inx->jig_step_x,
                      inx->jig_step_y, inx->jig_vert, &jigsamples, jigpat,
                      status );

      count = jigsamples*sinx->ncycle;

      /* dream uses the SMU to do the jiggle pattern so the posptr
         is just set to 0 */
      posptr = astCalloc( count*2, sizeof(*posptr) );

      if( *status == SAI__OK ) {
        memset( posptr, 0, count*2*sizeof(double) );
      }

      break;

    case MODE__SINGLESCAN:
      /* Call sc2sim_getsinglescan to get scan pointing solution */
      msgOutif(MSG__VERB, " ", "Do a SINGLESCAN observation", status );
      accel[0] = 432.0;           /* Source? */
      accel[1] = 540.0;
      vmax[0] = inx->vmax;        /* Max is 600.0 arcsec/s */
      vmax[1] = inx->vmax;        /* Max is 600.0 arcsec/s */

      sc2sim_getsinglescan ( inx->scan_angle, inx->width, accel, vmax,
                             samptime, &count, &posptr, status );

      /* indicate that we only have one pass */
      inx->nmaps = 1;
      steps_per_map = count;

      break;

    case MODE__BOUS:
      /* Call sc2sim_getbous to get boustrophedon pointing solution */
      msgOutif(MSG__VERB, " ", "Do a BOUS observation", status );
      accel[0] = 432.0;
      accel[1] = 540.0;
      vmax[0] = inx->vmax;        /* Max is 600.0 arcsec/s */
      vmax[1] = inx->vmax;        /* Max is 600.0 arcsec/s */

      sc2sim_getbous ( inx->bous_angle, inx->width, inx->height,
                       inx->spacing, accel, vmax, samptime, &count,
                       &posptr, status );

      /* indicate that we only have one pass */
      inx->nmaps = 1;
      steps_per_map = count;

      break;

    case MODE__LISS:
      /* Call sc2sim_getliss to get lissjous pointing solution */
      msgOutif(MSG__VERB, " ", "Do a LISSAJOUS observation", status );

      accel[0] = 0.0;
      accel[1] = 0.0;

      vmax[0] = inx->vmax;        /* Max is 600.0 arcsec/s */
      vmax[1] = inx->vmax;        /* Max is 600.0 arcsec/s */

      sc2sim_getliss ( inx->liss_angle, inx->width, inx->height,
                       inx->spacing, accel, vmax, samptime, inx->nmaps,
                       &count, &posptr, status );
      steps_per_map = count / inx->nmaps;

      break;


    case MODE__PONG:
      /* Get pong pointing solution */
      vmax[0] = inx->vmax;        /* Max is 600.0 arcsec/s */
      vmax[1] = inx->vmax;        /* Max is 600.0 arcsec/s */

      accel[0] = 0.0;
      accel[1] = 0.0;

      if ( strncmp ( inx->pong_type, "STRAIGHT", 8 ) == 0 ) {

        msgOutif(MSG__VERB, " ", "Do a STRAIGHT PONG observation", status );

        sc2sim_getstraightpong ( inx->pong_angle, inx->width, inx->height,
                                 inx->spacing, accel, vmax, samptime,
                                 inx->nmaps, &count, &posptr, status );

      } else if ( strncmp ( inx->pong_type, "CURVE", 5 ) == 0 ) {

        msgOutif(MSG__VERB, " ", "Do a CURVE PONG observation", status );

        sc2sim_getcurvepong ( inx->pong_angle, inx->width, inx->height,
                              inx->spacing, accel, vmax, samptime,
                              inx->nmaps, &count, &posptr, status );
      } else {

        *status = SAI__ERROR;
        msgSetc( "P", inx->pong_type );
        errRep( " ", "^P is not a valid PONG type", status );

      }

      steps_per_map = count / inx->nmaps;

      break;

    case MODE__EXTERNAL:
      /* Call sc2sim_getexternal to get scan pointing solution */
      msgOutif(MSG__VERB, " ", "Do an EXTERNAL observation", status );
      sc2sim_getexternal(inx->externobs, &count, &posptr, status );

      /* indicate that we only have one pass */
      inx->nmaps = 1;
      steps_per_map = count;

      break;

    default: /* should never be reached...*/
      msgSetc( "MODE", inx->obsmode );
      errRep("", "^MODE is not a supported observation mode", status);
      break;

    }/* switch */

    msgSeti( "COUNT", count );
    msgOutif(MSG__VERB, " ", "Count = ^COUNT", status );

  }/* if status OK */

  /* Set maxwrite to the maximum amount of frames to be written
     (either count, or the users-specified maxwrite value, whichever
     is least) */
  if ( count < maxwrite ) {
    maxwrite = count;
  }

  /* Report simulation properties if requested */
  if ( simstats ) {
    sc2sim_simstats( count, inx->steptime, maxwrite, nbol, sinx->nsubarrays, inx->rowsize,
                     status );
    goto CLEANUP;
  }

  /* Allocated buffers for quantities that are calculated at each
     time-slice */

  /* All four subarrays need to have their data stored simultaneously */
  dbuf = astCalloc( maxwrite*nbol*(sinx->nsubarrays), sizeof(*dbuf) );
  digits = astCalloc( maxwrite*nbol, sizeof(*digits) );
  dksquid = astCalloc( maxwrite*inx->rowsize, sizeof(*dksquid) );

  /* Frames will be "chunked" into blocks of size 'maxwrite' */
  mjuldate = astCalloc( maxwrite, sizeof(*mjuldate) );
  lst = astCalloc( maxwrite, sizeof(*lst) );
  base_az = astCalloc( maxwrite, sizeof(*base_az) );
  base_el = astCalloc( maxwrite, sizeof(*base_el) );
  base_p = astCalloc( maxwrite, sizeof(*base_p) );
  bor_az = astCalloc( maxwrite, sizeof(*bor_az) );
  bor_el = astCalloc( maxwrite, sizeof(*bor_el) );
  bor_ra = astCalloc( maxwrite, sizeof(*bor_ra) );
  bor_dec = astCalloc( maxwrite, sizeof(*bor_dec) );
  jig_x_hor = astCalloc( maxwrite, sizeof(*jig_x_hor) );
  jig_y_hor = astCalloc( maxwrite, sizeof(*jig_y_hor) );
  airmass = astCalloc( maxwrite, sizeof(*airmass) );
  head = astCalloc( maxwrite, sizeof( *head ) );

  /* Create an instrumental 1/f noise sequence for each bolometer by
     generating random amplitudes for the sine and cosine
     components of the lowest few frequencies, suitably scaled. */
  if( !hitsonly && ( *status == SAI__OK ) ) {

    sigma = 1.0e-9;
    corner = 0.01;
    nterms = 20;

    if ( sinx->add_fnoise == 1 ) {

      msgOutif(MSG__VERB," ",
               "Create 1/f coefficients", status );

      for ( bol=0; bol<nbol; bol++ ) {

        msgSeti( "BOL", bol );
        msgOutif(MSG__VERB," ",
                 "1/f for bolometer number ^BOL", status);

        sc2sim_getinvf ( sigma, corner, samptime, nterms,
                         &(noisecoeffs[bol*3*nterms]), status );

        msgOutif(MSG__VERB," ",
                 "1/f noise array made", status);
      }/* for all bolometers */

    }/* if add fnoise */

  }/* if not hits-only */

  msgOutif(MSG__VERB," ",
           "Get flatfield calibrations", status );

  /* Retrieve the flatfield calibrations for each subarray */
  for ( k = 0; k < sinx->nsubarrays; k++ ) {

    /* Preset all the flatfield calibrations and parameters
       to NULL */
    flatcal[k] = NULL;
    flatpar[k] = NULL;

    if( *status == SAI__OK ) {

      sprintf ( heatname, "%sheat%04i%02i%02i_00001",
                (sinx->subname)[k], date_yr, date_mo, date_da );

      sc2store_rdflatcal ( heatname, SC2STORE_FLATLEN, &colsize,
                           &rowsize, &(nflat[k]), &(refres[k]), flatname[k], &(flatcal[k]),
                           &(flatpar[k]), status );

    }

  }/* for all subarrays */

  msgSetd( "DSTART", inx->mjdaystart );
  msgSeti( "YR", date_yr );
  msgSeti( "MO", date_mo );
  msgSeti( "DAY", date_da );
  msgOutif(MSG__VERB," ",
           "Start observing at MJD ^DSTART, ^YR-^MO-^DAY", status);

  /* Calculate the apparent-to-mean coordinate conversion
     parameters. The time parameter should be a TDB but UTC should be
     good enough, according to SUN/67. These vary very slowly so doing
     the calculation once per simulation should be fine */
  palMappa( 2000.0, inx->mjdaystart, amprms );

  /* Telescope latitude */
  phi = ERFA_DD2R*(sinx->telpos)[1];
  /* determine values of variables used for looping: nmicstp,
     frmperms, chunks, maxwrite */

  /* determine number of frames per microstep */
  frmperms = count / inx->nmicstep; /* NOTE: will round down number of
                                       frames simulated if count not
                                       divisable by nmicstep */

  /* adjust maxwrite */
  if ( maxwrite > frmperms ) { maxwrite = frmperms; }

  /* Determine how many `chunks' of size maxwrite are required to
     complete the pattern, and how many frames are in the last
     chunk */
  chunks = ceil ( (double)frmperms / maxwrite );

  /* Number of output files per subarray */
  noutfiles = inx->nmicstep * chunks * inx->nfocstep;

  /* Loop over number of SMU (focus) positions */
  for ( focidx = 0; focidx < inx->nfocstep; focidx++ ) {
    focposn = inx->focstart + (double)focidx * inx->focstep;

    /* loop over microsteps */
    for ( curms = 0; curms < inx->nmicstep; curms++ ) {
      sc2sim_instap_calc( inx, curms, instap, status );

      /* For each chunk, determine the data for the corresponding
         frames.  At the last frame, write the data for each
         subarray to a file */
      for (curchunk = 0; curchunk < chunks && (*status == SAI__OK); curchunk++) {

        /* Adjust the lastframe value depending on whether this is the
           last chunk */
        lastframe = maxwrite;

        /* Sub-scan number to write into filename */
        subscanno++;
        if ( subscanno == noutfiles ) {
          obsend = 1;
        }

        /* If we have more than 1 chunk, and we are on the final chunk
           then the number of frames in the last file is the remainder of
           count divided by maxwrite. This FAILS if maxwrite is a factor
           of count (ie no remainder), so check if we have zero
           remainder. */
        if ( ( chunks != 1 ) && ( curchunk == ( chunks - 1 ) ) ) {
          lastframe = frmperms % maxwrite;
          if ( lastframe == 0 ) {
            lastframe = maxwrite;
          }
        }

        /* Increment mjdaystart (UTC) to the beginning of this chunk, then
           calculate the UT1/LAST at each timestep */
        totaltime = ((double)(curms * frmperms + curchunk * maxwrite))
          * samptime;
        start_time = inx->mjdaystart + (totaltime / SPD);
        taiutc = palDat( start_time );
        sc2sim_calctime( (sinx->telpos[0])*ERFA_DD2R, start_time, inx->dut1, samptime,
                         lastframe, mjuldate, lst, status );

        /* If we're not simulating a planet observation then we only need
           to calculate the apparent RA, Dec once */
        if ( !planet ) {
          /* Convert BASE RA, Dec to apparent RA, Dec for current epoch.
             Use quick conversion - should be more than good enough */
          palMapqkz( inx->ra, inx->dec, amprms, &raapp, &decapp );
        }

        /* Retrieve the values for this chunk */
        frameoffset = ( curms * frmperms ) + ( curchunk * maxwrite );
        for ( frame = 0; frame < lastframe && (*status == SAI__OK); frame++ ) {

          curframe = frame + frameoffset;

          /* If we are simulating a planet observation, calculate apparent
             RA, Dec at each time step - actually don't need to do this if
             the simulation is short, say 1 min or less but in the first
             instance let's do it the correct way. Update it only every
             100 frames or 0.5 sec, else use the previous one */
          if ( planet ) {
            if ( frame%100 == 0 ) {
              /* Calculate the TT from UT1 mjuldate, DUT1 and TT-UTC from palDtt */
              tt = mjuldate[frame] + ((dtt - inx->dut1) / SPD);

              palRdplan( tt, inx->planetnum, -ERFA_DD2R*(sinx->telpos)[0],
                         phi, &raapp, &decapp, &diam );

              /* Store RA Dec in inx struct so they can be written as FITS
                 headers. Note these are APPARENT RA, Dec which are more
                 relevant for planets */
              inx->ra = raapp;
              inx->dec = decapp;
              /* Create frameset to allow sky2map mapping to be determined */
              fitschan = astFitsChan ( NULL, NULL, " " );
              sc2ast_makefitschan( astnaxes[0]/2.0, astnaxes[1]/2.0,
                                   (-astscale*DAS2D), (astscale*DAS2D),
                                   raapp*DR2D, decapp*DR2D,
                                   "RA---TAN", "DEC--TAN", fitschan, status );
              astClear( fitschan, "Card" );
              fitswcs = astRead( fitschan );
              /* Extract the Sky->REF_PIXEL mapping. */
              astSetC( fitswcs, "SYSTEM", "GAPPT" );
              sky2map = astGetMapping( fitswcs, AST__CURRENT, AST__BASE );
            }
          }

          /* Calculate hour angle */
          hourangle = lst[frame] - raapp;

          /* Calculate the az/el corresponding to the map centre (base) */
          palDe2h ( hourangle, decapp, phi, &temp1, &temp2 );

          /* Issue an error if the source is below 20 degrees */
          if ( temp2 < 0.349066 ) {
            if ( *status == SAI__OK ) {
              *status = SAI__ERROR;
              msgSetd("E",temp2*57.29);
              errRep(" ", "Source is below 20 deg elevation (^E)", status);
              goto CLEANUP;
            }
          }

          /* Parallactic angle */
          temp3 = palPa ( hourangle, decapp, phi );

          if( *status == SAI__OK ) {
            base_az[frame] = temp1;
            base_el[frame] = temp2;
            base_p[frame] = temp3;

            /* The scan pattern (posptr) is defined as a series of offsets
               in ARCSEC in the map coordinate frame. Depending on the
               frame chosen, project the pattern into AzEl and RADec so
               that it can be written to the JCMTState structure */
            switch( coordframe ) {

            case FRAME__NASMYTH:
              /* Get boresight tanplate offsets in Nasmyth coordinates (radians) */
              bor_x_nas = (posptr[curframe*2] - instap[0])*ERFA_DAS2R;
              bor_y_nas = (posptr[curframe*2+1] - instap[1])*ERFA_DAS2R;

              /* Calculate boresight offsets in horizontal coord. */
              bor_x_hor =  bor_x_nas*cos(base_el[frame]) -
                bor_y_nas*sin(base_el[frame]);
              bor_y_hor = bor_x_nas*sin(base_el[frame]) +
                bor_y_nas*cos(base_el[frame]);

              /* Calculate jiggle offsets in horizontal coord. */
              /* jigpat is in ARCSEC: jig_x/y_hor must be in RADIANS */
              jig_x_hor[frame] = ERFA_DAS2R*(jigpat[curframe%jigsamples][0]*
                                        cos(base_el[frame]) -
                                        jigpat[curframe%jigsamples][1]*
                                        sin(base_el[frame]));

              jig_y_hor[frame] = ERFA_DAS2R*(jigpat[curframe%jigsamples][0]*
                                        sin(base_el[frame]) +
                                        jigpat[curframe%jigsamples][1]*
                                        cos(base_el[frame]));

              break;

            case FRAME__AZEL:
              /* posptr and jigpat already give the azel tanplane offsets */
              bor_x_hor = (posptr[curframe*2])*ERFA_DAS2R;
              bor_y_hor = (posptr[curframe*2+1])*ERFA_DAS2R;

              /* jigpat is in ARCSEC: jig_x/y_hor must be in RADIANS */
              jig_x_hor[frame] = ERFA_DAS2R*jigpat[curframe%jigsamples][0];
              jig_y_hor[frame] = ERFA_DAS2R*jigpat[curframe%jigsamples][1];
              break;

            case FRAME__RADEC:
              /* posptr and jigpat give the RADec tanplane offsets */
              bor_x_cel = (posptr[curframe*2])*ERFA_DAS2R;
              bor_y_cel = (posptr[curframe*2+1])*ERFA_DAS2R;

              /* Rotate by the parallactic angle to get offsets in AzEl */
              bor_x_hor =  bor_x_cel*cos(-base_p[frame]) -
                bor_y_cel*sin(-base_p[frame]);

              bor_y_hor = bor_x_cel*sin(-base_p[frame]) +
                bor_y_cel*cos(-base_p[frame]);

              /* jigpat is in ARCSEC: jig_x/y_hor must be in RADIANS */
              jig_x_hor[frame] = ERFA_DAS2R*(jigpat[curframe%jigsamples][0]*
                                        cos(base_p[frame]) -
                                        jigpat[curframe%jigsamples][1]*
                                        sin(base_p[frame]));

              jig_y_hor[frame] = ERFA_DAS2R*(jigpat[curframe%jigsamples][0]*
                                        sin(base_p[frame]) +
                                        jigpat[curframe%jigsamples][1]*
                                        cos(base_p[frame]));
              break;

            default:
              *status = SAI__ERROR;
              errRep(FUNC_NAME, "Un-recognised map coordinate frame", status);
              break;
            }/* switch */

          }/* if status OK */

          /* Calculate boresight spherical horizontal coordinates */

          fc = astFitsChan ( NULL, NULL, " " );

          if( *status == SAI__OK ) {
            sc2ast_makefitschan( 0.0, 0.0, AST__DR2D, AST__DR2D,
                                 base_az[frame]*AST__DR2D,
                                 base_el[frame]*AST__DR2D,
                                 "CLON-TAN", "CLAT-TAN", fc, status );

            astClear( fc, "Card" );
            fs = astRead( fc );
          }

          if( *status == SAI__OK ) {
            astTran2( fs, 1, &bor_x_hor, &bor_y_hor, 1, &temp1, &temp2 );
            if( !astOK ) {
              *status = SAI__ERROR;
              errRep(FUNC_NAME, "AST error calculating telescope position",
                     status);
            }
          }

          if( *status == SAI__OK ) {
            bor_az[frame] = fmod(temp1+2.*AST__DPI,2.*AST__DPI);
            bor_el[frame] = fmod(temp2+2.*AST__DPI,2.*AST__DPI);

            /* Calculate sky (effective) horiz. coordinates (boresight+jiggle) */
            sky_x_hor = bor_x_hor + jig_x_hor[frame];
            sky_y_hor = bor_y_hor + jig_y_hor[frame];

            astTran2( fs, 1, &sky_x_hor, &sky_y_hor, 1, &temp1, &temp2 );

            sky_el = fmod(temp2+2.*AST__DPI,2.*AST__DPI);

            if( !astOK ) {
              *status = SAI__ERROR;
              errRep(FUNC_NAME, "AST error calculating effective position",
                     status);
            }      if( !astOK ) {
              *status = SAI__ERROR;
              errRep(FUNC_NAME, "AST error calculating effective position",
                     status);
            }

          }

          /* Free AST resources required for boresite pointing calculation */
          if( fs ) fs = astAnnul(fs);
          if( fc ) fc = astAnnul(fc);

          if( *status == SAI__OK ) {
            /* Calculate the airmass - note this assumes a PLANE-PARALLEL
               atmosphere with no refraction. Set constant below an
               elevation of 1 deg. */
            if( sky_el >= 1.0 * AST__DPI/180. ) {
              airmass[frame] = 1.0/sin(sky_el);
            } else {
              airmass[frame] = 3283.0;
            }
            /* Calculate equatorial from horizontal */
            palDh2e( bor_az[frame], bor_el[frame], phi, &raapp1, &decapp1 );
            raapp1 = fmod(lst[frame] - raapp1 + ERFA_D2PI, ERFA_D2PI );
            /* Convert apparent RA Dec to Mean RA, Dec for current epoch */
            palAmpqk( raapp1, decapp1, amprms, &temp1, &temp2 );
            /* Store the mean RA, Dec */
            bor_ra[frame] = temp1;
            bor_dec[frame] = temp2;
            if ( planet ) {
              inx->ra = temp1;
              inx->dec = temp2;
            }
          }

          if ( !hitsonly ) {

            /* Create an sc2 frameset for this time slice and extract
               bolo->sky mapping */
            state.tcs_az_ac2 = bor_el[frame];
            state.tcs_az_ac1 = bor_az[frame];
            state.tcs_tr_dc1 = bor_ra[frame];
            state.tcs_tr_dc2 = bor_dec[frame];
            state.tcs_tr_ac1 = bor_ra[frame];
            state.tcs_tr_ac2 = bor_dec[frame];
            /* Set BASE position */
            state.tcs_az_bc1 = base_az[ frame ];
            state.tcs_az_bc2 = base_el[ frame ];
            if (planet) {
              state.tcs_tr_bc1 = raapp;
              state.tcs_tr_bc2 = decapp;
            } else {
              state.tcs_tr_bc1 = inx->ra;
              state.tcs_tr_bc2 = inx->dec;
            }

            state.smu_az_jig_x = jig_x_hor[frame];
            state.smu_az_jig_y = jig_y_hor[frame];
            state.smu_az_chop_x = 0;
            state.smu_az_chop_y = 0;
            state.tcs_tai = mjuldate[frame] + (taiutc - inx->dut1 + 0.5*samptime)/SPD;
            state.rts_end = state.tcs_tai + 0.5*samptime/SPD;

            /* For each subarray, retrieve the wcs frameset, then generate
               the frame of data */
            timesincestart = ( mjuldate[frame] - inx->mjdaystart ) * SPD;
            for ( k = 0; k < sinx->nsubarrays; k++ ) {

              /* Get the numerical subarray number from the name */
              sc2ast_name2num( (sinx->subname)[k], &subnum, status );

              if( *status == SAI__OK ) {
                sc2ast_createwcs(subnum, &state, instap, sinx->telpos, NO_FTS, &fs,
                                 status);
              }

              /* simulate one frame of data */
              if( *status == SAI__OK ) {
                sc2sim_simframe ( *inx, *sinx, astnaxes, astscale,
                                  astdata->pntr[0], atmnaxes, atmscale,
                                  atmdata->pntr[0], coeffs, fs, heater, nbol,
                                  focposn, curframe, nterms, noisecoeffs, pzero, samptime,
                                  timesincestart, weights,
                                  sky2map, xbolo, ybolo, xbc, ybc,
                                  &(posptr[curframe*2]),
                                  &(dbuf[(k*nbol*maxwrite) + (nbol*frame)]),
                                  status );
              }

              /* Annul sc2 frameset for this time slice */
              if( fs ) fs = astAnnul( fs );
              if( fitschan ) fitschan = astAnnul( fitschan );

            }/* for each subarray */

          }/* if not hits-only */

          /* If this is the last frame, generate the headers for every
             frame and write the data to a file for all the subarrays */

          if ( ( frame == ( lastframe - 1 ) ) && ( *status == SAI__OK ) ) {

            for ( j = 0; j < lastframe; j++ ) {

              /* RTS -------------------------------------------------------*/
              /* Sequence number */
              head[j].rts_num = frameoffset + j;
              /* Calculate TAI corresponding to MID-TIME of sample and store */
              head[j].tcs_tai = mjuldate[j] + (taiutc - inx->dut1 + 0.5*samptime)/SPD;

              /* RTS_END is a TAI time, corresponding to END time of sample */
              head[j].rts_end = head[j].tcs_tai +0.5*samptime/SPD;

              /* TCS - Telescope tracking structure ----------------------- */
              /* Coord. system  */
              if ( planet ) {
                /* Note: the JCMT writes out APP not GAPPT */
                snprintf(head[j].tcs_tr_sys,6,"APP");
              } else {
                snprintf(head[j].tcs_tr_sys,6,"J2000");
              }

              /* Percentage complete */
              switch( mode ) {
                dim_t percent;

              case MODE__STARE:
              case MODE__NOISE:
              case MODE__DREAM:
                /* offsets and micro steps - simulator does not seem to do offsetting */
                percent = 100 * (1 + (head[j].rts_num / frmperms)) / inx->nmicstep;
                head[j].tcs_percent_cmp = percent;
                break;
              case MODE__SINGLESCAN:
              case MODE__BOUS:
              case MODE__LISS:
              case MODE__EXTERNAL:
              case MODE__PONG:
                /* Assumes steps are spread equally over nmaps */
                percent = (100 * (head[j].rts_num % steps_per_map)) / steps_per_map;
                head[j].tcs_percent_cmp = percent;
                break;
              default: /* should not get here */
                if ( *status == SAI__OK ) {
                  *status = SAI__ERROR;
                  errRep("", "Error - observing mode not set", status);
                }
              } /* switch */

              /* Angle between "up" in Nasmyth coordinates, and "up"
                 in tracking coordinates at the base telescope
                 positions */
              head[j].tcs_tr_ang = base_el[j] + base_p[j];

              /* Demand coordinates */
              head[j].tcs_tr_dc1 = bor_ra[j];
              head[j].tcs_tr_dc2 = bor_dec[j];

              /* Actual coordinates */
              head[j].tcs_tr_ac1 = bor_ra[j];
              head[j].tcs_tr_ac2 = bor_dec[j];

              /* Base coordinates (e.g. tangent point for nominal map) */
              if (planet) {
                head[j].tcs_tr_bc1 = raapp;
                head[j].tcs_tr_bc2 = decapp;
              } else {
                head[j].tcs_tr_bc1 = inx->ra;
                head[j].tcs_tr_bc2 = inx->dec;
              }

              /* TCS - Telescope tracking in horizontal coordinates ------- */

              /* Angle between "up" in Nasmyth coordinates, and "up" in
                 horizontal coordinates at the base telescope positions */
              head[j].tcs_az_ang = base_el[j];

              /* Base coordinates */
              head[j].tcs_az_bc1 = base_az[j];
              head[j].tcs_az_bc2 = base_el[j];

              /* Demand coordinates */
              head[j].tcs_az_dc1 = bor_az[j];
              head[j].tcs_az_dc2 = bor_el[j];

              /* Actual coordinates */
              head[j].tcs_az_ac1 = bor_az[j];
              head[j].tcs_az_ac2 = bor_el[j];

              /* Write airmass into header */
              head[j].tcs_airmass = airmass[j];

              /* SMU - Secondary mirror structure ------------------------- */
              /* Jiggle horizontal offsets */
              head[j].smu_az_jig_x = jig_x_hor[j];
              head[j].smu_az_jig_y = jig_y_hor[j];

              /* Other headers - more to be added as necessary */
              one_strlcpy( head[j].smu_chop_phase, "M",
                           sizeof(head[j].smu_chop_phase), status );
              one_strlcpy( head[j].tcs_beam, "M",
                           sizeof(head[j].tcs_beam), status );
              one_strlcpy( head[j].tcs_source, "SCIENCE",
                           sizeof(head[j].tcs_source), status);

              if ( !hitsonly ) {

                /* WVM - Water vapour monitor ------------------------------- */

                /* Simulate WVM measurements consistent with airmass and
                   pwv using a subroutine from the WVM library used to
                   calculate opacities from the real monitor. drytau183 is
                   the excess broadband opacity, aka the dry component,
                   which is why it's small and independent of the PWV.

                   Using a fixed value of drytau183 and choosing twater to be
                   10 degrees away from the ambient temperature seems to
                   generate WVM observations that can then be "fit" to get
                   the correct (input) values for CSO tau and the PWV using
                   wvmOpt. */

                /* Determine pwv from tauzen */
                pwvzen = tau2pwv (sinx->tauzen);

                /* Line of site pwv      */
                pwvlos = airmass[j]*pwvzen;

                /* Effective water temp. */
                twater = sinx->atstart + 273.15 - 10;

                /* Not physically unreasonable number... Note it's negative
                   because of a missed -ve sign in the wvmEst */
                drytau183 = -0.03;

                /* Only update once every 240 samples */
                if( j % 240 == 0 ) {
                  wvmEst( airmass[j], pwvlos, /* model temp. */
                          twater, drytau183, tbri, ttau, teff, aeff );
                }

                head[j].wvm_t12 = tbri[0];
                head[j].wvm_t42 = tbri[1];
                head[j].wvm_t78 = tbri[2];

              }/* if not hits-only */

            }/* for each frame in this chunk */

            if ( !hitsonly ) {

              /* For now just set to the last airmass calculated */
              sinx->airmass = airmass[frame-1];

              /* Calculate `mean' tau CSO from the pwv */
              tauCSO = pwv2tau(pwvzen);

            }/* if not hits-only */

            dateobs[0] = '\0'; /* Initialize the dateobs string to NULL */
            sc2sim_dateobs( start_time, dateobs, status );

            /* For each subarray, digitise the data and write it to
               a file */
            for ( k = 0; k < sinx->nsubarrays; k++ ) {

              /* If we haven't calculate parameters for the digitization yet,
                 do it now before the first digitise call. This will override
                 the guess parameters provided by sc2sim_instrinit */

              if( dodigcalc ) {

                /* Get photon noise level */
                sc2sim_calctrans( inx->lambda, &skytrans, sinx->tauzen, status );
                sc2sim_atmsky( inx->lambda, skytrans, &zenatm, status );
                meanatm = zenatm * ( 1.0 - pow(0.01*skytrans,airmass[0]) ) /
                  ( 1.0 - (0.01*skytrans) );
                sc2sim_getsigma( sinx->refload, sinx->refnoise,
                                 sinx->telemission + meanatm, &pnoise, status );

                /* Calculate scaling coefficients */

                sc2sim_getscaling ( SC2SIM__NCOEFFS, coeffs, inx->targetpow,
                                    pnoise, &digmean, &digscale, &digcurrent,
                                    status );

                dodigcalc = 0;
              }

              /* Digitise the numbers */
              if( !hitsonly && ( *status == SAI__OK ) ) {
                sc2sim_digitise ( nbol*(frame+1), &dbuf[k*maxwrite*nbol],
                                  digmean, digscale,
                                  digcurrent, digits, status );
              }

              /* Compress and store as NDF */
              if( *status == SAI__OK ) {
                sprintf( filename, "%s%04i%02i%02i_%05d_%04d", (sinx->subname)[k],
                         date_yr, date_mo, date_da, obscounter, subscanno );
                /*If we are not overwriting existing files see if the file exists*/
                if ( !overwrite ) {
                  fileexists = 1;
                  while ( fileexists ) {
                    one_strlcat( filename, ".sdf", sizeof(filename), status );
                    ofile = fopen( filename, "r" );
                    /* First see if we can open the file */
                    if ( ofile ) {
                      /* If yes, close it again, increment counter and set
                         new file name */
                      fileexists = 1;
                      fclose(ofile);
                      obscounter++;
                      sprintf( filename, "%s%04i%02i%02i_%05d_%04d", (sinx->subname)[k],
                               date_yr, date_mo, date_da, obscounter, subscanno );
                    } else {
                      /* If not, unset fileexists flag, increment counter
                         and set new file name */
                      fileexists = 0;
                      sprintf( filename, "%s%04i%02i%02i_%05d_%04d", (sinx->subname)[k],
                               date_yr, date_mo, date_da, obscounter, subscanno );
                    }
                  }
                }


                msgSetc( "FILENAME", filename );
                msgOutif(MSG__NORM, "", "  Writing ^FILENAME", status );

                /* Calculate OBSID */
                date_hr = (int)(date_df * 24);
                date_mn = 60 * ((date_df * 24.0) - (float)date_hr);
                sprintf(obsid, "%s_%d_%04d%02d%02dT%02d%02d%02d",
                        INSTRUMENT, obscounter, date_yr, date_mo, date_da,
                        date_hr,date_mn,0);

                sprintf( utdate, "%04d%02d%02d", date_yr, date_mo, date_da);

                /* Number of .In images KLUDGE */
                if ( mode == MODE__DREAM || mode == MODE__STARE ) {
                  nimage = maxwrite / 200;
                }
                /* LST start/end */
                eraA2tf(4, lst[0], sign, ihmsf);
                sprintf( lststart, "%02d:%02d:%02d.%04d",
                         ihmsf[0], ihmsf[1], ihmsf[2], ihmsf[3]);
                eraA2tf(4, lst[lastframe-1], sign, ihmsf);
                sprintf( lstend, "%02d:%02d:%02d.%04d",
                         ihmsf[0], ihmsf[1], ihmsf[2], ihmsf[3]);
                /* HST start/end */

                /* Write the data out to a file */
                sc2sim_ndfwrdata( inx, sinx, k, tauCSO, filename, lastframe, nflat[k],
                                  refres[k], flatname[k], head, digits, dksquid, flatcal[k],
                                  flatpar[k], INSTRUMENT, filter, dateobs, obsid,
                                  &(posptr[frameoffset*2]), jigsamples,
                                  jigpat, obscounter, focposn,
                                  subscanno, obsend, utdate,
                                  head[0].tcs_az_bc1,
                                  head[lastframe-1].tcs_az_bc1,
                                  head[0].tcs_az_bc2,
                                  head[lastframe-1].tcs_az_bc2,
                                  lststart, lstend, loclcrd, scancrd,
                                  totaltime, exptime, nimage,
                                  sinx->tauzen, sinx->tauzen,
                                  status);

              }/* if status OK */

            }/* for each subarray */

          }/* if lastframe */

          /* exit loop over time slice if bad status */
          if( *status != SAI__OK ) {
            frame = lastframe;
            curchunk = chunks;
            curms = inx->nmicstep;
          }

        }/* For all frames in this chunk */

      } /* For each chunk */
    } /* For each microstep */
  } /* For each focus position */

  /* Release memory */
  /* Free buffers that get allocated for each subarray */
//  for ( k = 0; k < sinx->nsubarrays; k++ ) {
//
//   if( flatcal[k] ) {
//      free( flatcal[k] );
//      flatcal[k] = NULL;
//    }
//
//    if( flatpar[k] ) {
//      free( flatpar[k] );
//      flatpar[k] = NULL;
//    }
//
//  }/* for all subarrays */

 CLEANUP:

  head = astFree( head );
  posptr = astFree( posptr );
  dbuf = astFree( dbuf );
  digits = astFree( digits );
  dksquid = astFree( dksquid );
  mjuldate = astFree( mjuldate );
  lst = astFree( lst );
  base_az = astFree( base_az );
  base_el = astFree( base_el );
  base_p = astFree( base_p );
  bor_az = astFree( bor_az );
  bor_el = astFree( bor_el );
  bor_ra = astFree( bor_ra );
  bor_dec = astFree( bor_dec );
  jig_x_hor = astFree( jig_x_hor );
  jig_y_hor = astFree( jig_y_hor );
  airmass = astFree( airmass );

  if ( !hitsonly ) {

    smf_close_file( NULL, &astdata, status);
    smf_close_file( NULL, &atmdata, status);

    if ( sky2map ) sky2map = astAnnul( sky2map );

    grpDelet( &skygrp, status);

  }/* if hits-only */

  ndfEnd ( status );

  msgOutif(MSG__VERB," ", "Simulation successful.", status );

}
