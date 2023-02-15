/*
*+
*  Name:
*     SC2SIM

*  Purpose:
*     SCUBA-2 Simulator

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_sc2sim( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This command attempts to simulate the data taken by a SCUBA-2
*     subarray when observing an astronomical image plus atmospheric
*     background while driving the JCMT. The simulation includes
*     photon and 1/f noise from the atmosphere, and nonlinear response
*     which varies for different bolometers. It also includes SCUBA-2
*     field distortion. Sc2sim combines the functionality of a number
*     of executables built in earlier versions of the simulator:
*     staresim, dreamsim, pongsim. The output file from a `heat'
*     observation is named after the subarray, date, and filenumber,
*     for example: s8a20060301_00001_0001.sdf.  For each subarray used
*     in a simulation, a corresponding `heat' (flatfield) output file
*     must be present in the working directory.
*
*     Sc2sim also performs the heatrun task when supplied an input
*     file with 'heat' observation mode specified.  This generates a
*     heater flat-field measurement from simulated data for each of
*     range of heater settings.  The output file from a `heat'
*     observation is named after the subarray, date, and filenumber,
*     for example: s8aheat20060301_00001.sdf.

*  ADAM Parameters:
*     MAXWRITE = INTEGER (Read)
*          Number of samples to write in output file.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OBSPAR = GROUP (Read)
*          Specifies values for the observation parameters used by
*          the simulation.
*
*          The supplied value should be either a comma-separated list
*          of strings or the name of a text file preceded by an
*          up-arrow character "^", containing one or more
*          comma-separated (or line-break separated) lists of
*          strings. Each string is either a "keyword=value" setting,
*          or the name of a text file preceded by an up-arrow
*          character "^". Such text files should contain further
*          comma-separated lists which will be read and interpreted in
*          the same manner (any blank lines or lines beginning with
*          "#" are ignored). Within a text file, newlines can be used
*          as delimiters, as well as commas. Settings are applied in
*          the order in which they occur within the list, with later
*          settings over-riding any earlier settings given for the
*          same keyword.
*
*          Each individual setting should be of the form:
*
*             <keyword>=<value>
*
*          The parameter names and their default values are listed
*          below.  The default values will be used for any unspecified
*          parameters.  Unrecognized parameters are ignored (i.e. no
*          error is reported).
*     OVERWRITE = LOGICAL (Read)
*          Flag to specify whether existing files are
*          overwritten. Setting this to FALSE increments the `group'
*          counter in the output file names. Default is TRUE.
*     SEED = INTEGER (Read)
*          Seed for random number generator.  If a seed
*          is not specified, the clock time in milliseconds
*          is used.
*     SIMPAR = GROUP (Read)
*          Specifies values for the simulation parameters.  See
*          the description for OBSPAR for the file format.
*
*          The parameter names and their default values are listed
*          below.  The default values will be used for any unspecified
*          parameters.  Unrecognized parameters are ignored (i.e. no
*          error is reported).
*     SIMSTATS = LOGICAL (Read)
*          Flag to specify whether to report the properties of the
*          current simulation given the parameters specified in
*          SIMPAR and OBSPAR. The simulation is not carried out.
*     SIMTYPE = CHAR (Read)
*          Simulation type : In a 'full' simulation the flux for each
*          bolometer at each time slice is calculated and stored in
*          the output files, along with the pointing information.  In
*          a 'weights' simulation, the flux is set to zero for each
*          bolometer, but the pointing information is written to the
*          output files.  These 'weights' files can be generated in
*          less time than for a 'full' simulation and may be used to
*          predict the rate of sampling across a mapped area for a
*          given observation.

*  Observation Parameters:
*     bol_distx (DOUBLE)
*          Average bolometer distance in the x-direction in
*          arcseconds. [6.28]
*     bol_disty (DOUBLE)
*          Average bolometer distance in the y-direction in arcseconds
*          [6.28].
*     bous_angle (DOUBLE)
*       For the BOUS obsmode, this parameter specifies
*       the angle of the pattern relative to the telescope
*       axes in radians anticlockwise. [0.0]
*     bous_height (DOUBLE)
*       For the BOUS obsmode, this parameter specifies
*       the height of the boustrophedon pattern in arcseconds.  This
*       height is the height of the stack of back-and-forth sweeps
*       across the sky, and is measured from the centre of the
*       array. [2000.0]
*     bous_spacing (DOUBLE)
*       For the BOUS obsmode, this parameter specifies
*       the spacing of the boustrophedon pattern in arcseconds.  This
*       spacing is the distance between the parallel horizontal
*       sweeps across the sky, and is measured from the centre of
*       the array. [240.0]
*     bous_vmax (DOUBLE)
*       For the BOUS obsmode, this parameter specifies
*       the maximum telescope velocity in arcseconds/second. [200.0]
*     bous_width (DOUBLE)
*       For the BOUS obsmode, this parameter specifies
*       the width of the boustrophedon pattern in arcseconds.  This width
*       is the width of each of back-and-forth sweeps across
*       the sky, and is measured from the centre of the array. [2000.0]
*     colsize (INTEGER)
*       This is the number of bolometers in a "column",
*       and this is the total number of "rows". [40]
*     conv_shape (INTEGER)
*       Flag for the possible convolution functions where
*         - 0 - Gaussian
*         - 1 - sinc(dx).sinc(dy)
*         - 2 - sinc(dx).sinc(dy) tapered
*         - 3 - sinc(dx).sinc(dy) after first 1.0
*         - 4 - bessel tapered
*
*         [1]
*     conv_sig (DOUBLE)
*       Convolution function parameter. [1.0]
*     coordframe (CHAR)
*       Map coordinate frame, ether NASMYTH, AZEL, or RADEC. [RADEC]
*     dec (CHAR)
*       Sexagesimal string representation of the Declination
*       of the observation. [00:00:00.0]
*     distfac (DOUBLE)
*       Distortion factor, where 0 = no distortion. [0.0]
*     flatname (CHAR)
*       Name of flatfield correction technique, either TABLE or POLYNOMIAL.
*     grid_max_x (INTEGER)
*       DREAM reconstruction grid max x. [1]
*     grid_max_y (INTEGER)
*       DREAM reconstruction grid max y. [1]
*     grid_min_x (INTEGER)
*       DREAM reconstruction grid min x. [1]
*     grid_min_y (INTEGER)
*       DREAM reconstruction grid min y. [1]
*     grid_step_x (DOUBLE)
*       DREAM grid step in X direction in arcseconds. [6.28]
*     grid_step_y (DOUBLE)
*       DREAM grid step in Y direction in arcseconds. [6.28]
*     heatnum (INTEGER)
*       Number of heater settings. [1]
*     heatstart (DOUBLE)
*       Initial heater setting in pW. [0.0]
*     heatstep (DOUBLE)
*       Increment of heater setting in pW. [0.0]
*     jig_step_x (DOUBLE) : 6.28 (arcseconds)
*       The DREAM step size in the x-direction between
*       jiggle positions.
*     jig_step_y (DOUBLE) : 6.28 (arcseconds)
*       The DREAM step size in the y-direction between
*       jiggle positions.
*     jig_pos.x (CHAR)
*       Array with relative DREAM vertex coordinates
*       for the x-direction, in units of pixel
*       distance.  This parameter value should
*       be a comma-separated list of integer
*       values surrounded by parentheses.
*       The number of values must be the
*       same as the number of values in the
*       jig_pos.y array. [(0,-1,1,-1,0,1,-1,1)]
*     jig_pos.y (CHAR)
*       Array with relative vertex coordinates
*       for the y-direction, in units of pixel
*       distance.  This parameter value should
*       be a comma-separated list of integer
*       values surrounded by parentheses.
*       The number of values must be the
*       same as the number of values in the
*       jig_pos.x array. [(1,-1,0,1,-1,1,0,-1)]
*     lambda (DOUBLE)
*       Wavelength of observation in m. [0.85e-3]
*     liss_angle (DOUBLE)
*       For the LISS obsmode, this parameter specifies
*       the angle of the pattern relative to the telescope
*       axes in radians anticlockwise. [0.0]
*     liss_height (DOUBLE)
*       For the LISS obsmode, this parameter specifies
*       the height of the Lissajous pattern in arcseconds. [2000.0]
*     liss_nmaps (INTEGER)
*       The number of times the Lissajous pattern should
*       repeat. [1]
*     liss_spacing (DOUBLE)
*       For the LISS obsmode, this parameter specifies
*       the spacing of the Lissajous pattern in arcseconds.
*       This spacing is the distance between the parallel
*       sweeps across the sky, and is measured from the
*       centre of the array. [240.0]
*     liss_vmax (DOUBLE)
*       For the LISS obsmode, this parameter specifies
*       the maximum telescope velocity in arcseconds/second. [200.0]
*     liss_width (DOUBLE)
*       For the LISS obsmode, this parameter specifies
*       the width of the Lissajous pattern in arcseconds. [2000.0]
*     mjdaystart (DOUBLE)
*       Modified Julian date at start of observation. [53795.0]
*     mstap_x (DOUBLE)
*       Array of microstep X-offsets in the focal plane.
*       Units are arcseconds. Multiple values can be
*       supplied as comma-separated list of offsets
*       surrounded by parentheses, e.g "(10,20)". [0.0]
*     mstap_y (DOUBLE)
*       Array of microstep Y-offsets in the focal plane.
*       Units are arcseconds. Multiple values can be
*       supplied as comma-separated list of offsets
*       surrounded by parentheses, e.g "(10,20)". [0.0]
*     numsamples (INTEGER)
*       For the STARE obsmode, this is the number of
*       samples. [128]
*     nvert (INTEGER)
*       The number of vertices in the DREAM jiggle pattern. [8]
*     obsmode (CHAR)
*       The observation mode, which can be any of
*       the following :
*       -   STARE : Simulates a simple point-and-shoot
*                  observation mode in which the camera
*                  stares at at a specified area of sky
*                  for a period of time.
*       -   DREAM : (Dutch REal-time Acquisition Mode)
*                  Simulates an observition in which the
*                  Secondary Mirror unit moves rapidly
*                  in a star-like pattern such that each
*                  bolometer observes multiple points
*                  on the sky.
*       -   SINGLESCAN : Simulates moving the array in
*                       a straight path across the sky.
*       -   BOUS : Simulates mapping a rectangular area of
*                 sky using a simple Boustrophedon or
*                 raster pattern.
*       -   PONG : Simulates mapping a rectangular area of
*                 sky by filling the box with a orthogonally
*                 cross-linked pattern by "bouncing" off the
*                 sides of the box.  There are two
*                 subcategories of PONG, either Straight
*                 pong (straight lines between vertices) or
*                 Curve pong (slightly wiggly lines between
*                 vertices - pattern is a Fourier-expanded
*                 Lissajous).
*       -   LISS : Simulates mapping a rectangular area of
*                 sky by filling the box with a Lissajous
*                 pattern.
*       -   EXTERN : Recreates the scanning pattern of a real
*                   SCUBA2 observation.
*
*       [PONG]
*     platenum (INTEGER)
*       The number of waveplate rotations. [1]
*     platerev (DOUBLE)
*       The waveplate rotation in revolutions/second. [2.0]
*     pong_angle (DOUBLE)
*       For the PONG obsmode, this parameter specifies
*       the angle of the pattern relative to the telescope
*       axes in radians anticlockwise. [0.0]
*     pong_height (DOUBLE)
*       For the PONG obsmode, this parameter specifies
*       the height of the pong pattern in arcseconds. [2000.0]
*     pong_nmaps (INTEGER)
*       The number of times the Pong pattern should
*       repeat. [1]
*     pong_spacing (DOUBLE)
*       For the PONG obsmode, this parameter specifies
*       the spacing of the Pong pattern in arcseconds.
*       This spacing is the distance between the parallel
*       sweeps across the sky, and is measured from the
*       centre of the array. [240.0]
*     pong_type (CHAR)
*       Specifies the type of pong simulation (STRAIGHT or
*       CURVE). [STRAIGHT]
*     pong_vmax (DOUBLE)
*       For the PONG obsmode, this parameter specifies
*       the maximum telescope velocity in arcseconds/second. [200.0]
*     pong_width (DOUBLE)
*       For the PONG obsmode, this parameter specifies
*       the width of the Pong pattern in arcseconds. [2000.0]
*     ra (CHAR)
*       Sexagesimal string representation of the Right
*       Ascension of the observation. [00:00:00.0]
*     rowsize (INTEGER)
*       This is the number of bolometers in a "row",
*       and this is the total number of "columns". [32]
*     scan_angle (DOUBLE)
*       For the SINGLESCAN obsmode, this parameter specifies
*       the angle of the pattern relative to the telescope
*       axes in radians anticlockwise. [0.0]
*     scan_pathlength (DOUBLE)
*       For the SINGLESCAN obsmode, this parameter specifies
*       the width of the scan path in arcseconds. [2000.0]
*     scan_vmax (DOUBLE)
*       For the SINGLESCAN obsmode, this parameter specifies
*       the maximum telescope velocity in arcseconds/second.
*     smu_move (INTEGER)
*       Code for the SMU move algorithm.  The possible codes
*       are :
*       -   0 : Block wave.
*       -   1 : 2 term not damped.
*       -   2 : 3 term not damped.
*       -   3 : 4 term not damped.
*       -   4 : 2 term flat end.
*       -   5 : 3 term flat end.
*       -   6 : 4 term flat end.
*       -   7 : ScubaWave -  After 1 ms 0.098. After 8 ms
*              0.913. After 9 ms 1.000. popepi points is
*              equivalent with 64 ms.
*       -   8 : This is an experimental wave form, which may
*              change often.  Now it is a cosine waveform
*              from 0 to 1 in the full time.
*
*       [8]
*     smu_offset (DOUBLE)
*       SMU phase shift. [0.0]
*     smu_samples (INTEGER)
*       Number of samples per jiggle vertex. [1]
*     steptime (DOUBLE)
*       Sample interval time in seconds. [0.005]
*     subsysnr (INTEGER)
*       Subsystem number. [1]
*     targetpow (DOUBLE)
*       Target bolometer power input in pW. [25.0]

*  Simulation Parameters:
*     add_atm (INTEGER)
*       Flag for adding atmospheric emission. [0]
*     add_fnoise (INTEGER)
*       Flag for adding 1/f noise. [0]
*     add_hnoise (INTEGER)
*       Flag for adding heater noise. [0]
*     add_pns (INTEGER)
*       Flag for adding photon noise. [0]
*     airmass (DOUBLE)
*       Airmass of simulated observation. [1.2]
*     anang (DOUBLE)
*       Polarisation angle of analyser in degrees. [0.0]
*     anpol (DOUBLE)
*       Polarisation of analyser in percent. [100.0]
*     antrans (DOUBLE)
*       Transmission of analyser in percent. [100.0]
*     aomega (DOUBLE)
*       Coupling factor (0.179 for 850 microns,
*       0.721 for 450 microns). [0.179]
*     astname (CHAR)
*       Name of the input file containing astronomical
*       sky image.
*     astpol (DOUBLE)
*       Polarisation of source in percent. [10.0]
*     atend (DOUBLE)
*       Ambient temperature at end in degrees celsius. [5.0]
*     atmname (CHAR)
*       Name of the input file containing atmospheric
*       sky image.
*     atmxvel (DOUBLE)
*       Atmospheric background velocity in arcseconds/second in the X
*       direction. [5000.0]
*     atmyvel (DOUBLE)
*       Atmospheric background velocity in arcseconds/second in the Y
*       direction. [0.0]
*     atmzerox (DOUBLE)
*       Atmospheric background offset in arcseconds in the X
*       direction. [5000.0]
*     atmzeroy (DOUBLE)
*       Atmospheric background offset in arcseconds in the Y
*       direction. [50000.0]
*     atstart (DOUBLE)
*       Ambient temperature at start in degrees celsius. [5.0]
*     bandGHz (DOUBLE)
*       Bandwidth in GHz. [35.0]
*     blindang (DOUBLE)
*       Polarisation angle of JCMT windblind in degrees. [10.0]
*     blindpol (DOUBLE)
*       Polarisation of blind in percent. [1.0]
*     blindtrans (DOUBLE)
*       Transmission of JCMT windblind in percent. [93.0]
*     cassang (DOUBLE)
*       Polarisation angle of Cassegrain optics in degrees. [135.0]
*     casspol (DOUBLE)
*       Polarisation of Cassegrain optics in percent. [1.0]
*     casstrans (DOUBLE)
*       Transmission of Cassegrain optics in percent. [98.0]
*     flux2cur (INTEGER)
*       Flag to indicate conversion of power
*       from flux to current. [1]
*     interp (CHAR)
*       Name of the interpolation scheme to use when sampling the sky
*       image. See docs for astResample in SUN/210. [NEAREST]
*     meanatm (DOUBLE)
*       Mean expeected atmospheric signal in pW. [7.0]
*     nasang (DOUBLE)
*       Polarisation angle of Nasmyth optics in degrees. [90.0]
*     naspol (DOUBLE)
*       Polarisation of Nasmyth optics in percent. [1.0]
*     nastrans (DOUBLE)
*       Transmission of Nasmyth optics in percent. [98.0]
*     ncycle (INTEGER)
*       Number of cycles through the DREAM
*       pattern. [1]
*     param1 (DOUBLE)
*       Name of the first parameter for the sky interpolation scheme
*       specified by "interp". See docs for astResample in
*       SUN/210. [2.0]
*     param2 (DOUBLE)
*       Name of the second parameter for the sky interpolation scheme
*       specified by "interp". See docs for astResample in
*       SUN/210. [2.0]
*     smu_terr (DOUBLE)
*       SMU timing error in seconds. [0.0]
*     subname (CHAR)
*       Subarray names for the simulation. Any number of subarrays
*       can be selected in any order. A single subarray can be named
*       as a simple string (e.g. "s8a"), multiple subarrays can be
*       given using commas and parentheses (e.g. "(s8a,s4a)". [s8a]
*     tauzen (DOUBLE)
*       Optical depth at 225 GHz at the zenith. [0.052583]
*     telemission (DOUBLE)
*       Telescope background per pixel in pW. [4.0]
*     xpoint (DOUBLE)
*       X pointing offset on the sky in arcseconds. [20.0]
*     ypoint (DOUBLE)
*       Y pointing offset on the sky in arcseconds. [20.0]

*  Related Applications:
*     SMURF: SKYNOISE

*  Authors:
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     B.D.Kelly (ROE)
*     Jen Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2005-02-16 (BDK):
*        Original.
*     2005-05-18 (BDK):
*        Get xbc, ybc from instrinit.
*     2005-05-20 (BDK):
*        Add flatcal.
*     2005-06-17 (BDK):
*        Allocate workspace dynamically.
*     2005-08-19 (BDK):
*        Do calibration fit, remove flux2cur flag check.
*     2005-10-04 (BDK):
*        Change to new data interface.
*     2006-01-13 (EC):
*        Write subarray name.
*     2006-01-24 (EC):
*        Write filter/atstart/atend.
*     2006-04-19 (EC):
*        Added jiggle offsets, filename consistent with MJD.
*     2006-06-06 (AGG/EC/JB):
*        Convert to SMURF task: clone from smurf_makemap.
*     2006-06-09 (JB):
*        Added heatrun task.
*     2006-06-27 (TIMJ):
*        switch from sc2head to a generic JCMTState definition that
*        can be used for ACSIS and SCUBA-2. Also involves moving the
*        STATE information into a separate part of the data file.
*     2006-07-31 (JB):
*        Split into subroutines and added simhits capability.
*     2006-08-18 (EC):
*        Fixed memory leak.
*     2006-08-21 (JB):
*        Removed unused variables.
*     2006-08-21 (EC):
*        Free resources allocated in sc2sim_instrinit.
*     2006-09-14 (JB):
*        Seed optional.
*     2006-09-14 (EC):
*        Ability to scan in AzEl and RADec coordinates.
*     2006-09-22 (JB):
*        Convert to using AstKeyMaps for input parameters.
*     2006-10-03 (JB):
*        Remove unused variables.
*     2006-11-21 (JB):
*         Expanded comments section for use with
*         smurf_help task and added Lissajous mode.
*     2006-11-22 (JB):
*         Added multiple map cycle capabilities to liss/pong
*     2006-12-08 (JB):
*         Removed sc2sim_simhits and replaced with hits-only flag
*     2007-01-26 (AGG):
*         Added OVERWRITE parameter.
*     2007-03-01 (AGG):
*         Added SIMSTATS parameter.
*     2007-07-03 (EC):
*         Made obsMode enumerated type more readable.
*     2007-10-05 (AGG):
*         Changed OBSFILE and SIMFILE to OBSPAR and SIMPAR.
*     2007-12-18 (AGG):
*         Update to use new smf_free behaviour.
*     2008-04-24 (AGG):
*         Rationalize code layout.
*     2008-04-24 (TIMJ):
*         History now uses standard SST format.
*     2008-04-28 (AGG):
*         Check for simstats parameter before writing simulation info to stdout
*     2008-05-29 (TIMJ):
*         Free bolos array.
*     2008-10-10 (AGG):
*         Add NOISE observing mode
*     2009-11-20 (DSB):
*         Document "interp", "param1" and "param2".
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2006-2008 University of British Columbia.
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif
/* System includes */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/time.h>
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
#include "star/kaplibs.h"
#include "star/ard.h"

/* SC2DA includes */
#include "sc2da/Dits_Err.h"
#include "sc2da/Ers.h"
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2ast.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsc2sim/sc2sim.h"

#define FUNC_NAME "smurf_sc2sim"
#define TASK_NAME "SC2SIM"
#define LEN__METHOD 20

void smurf_sc2sim( int *status ) {

  /* Local variables */
  struct sc2sim_obs_struct inx;   /* structure for values from XML */
  struct sc2sim_sim_struct sinx;  /* structure for sim values from XML */
  mapCoordframe coordframe;       /* Coordinate frame for simulated map */
  double coeffs[SC2SIM__NCOEFFS]; /* bolometer response coeffs */
  double digcurrent;              /* digitisation mean current */
  double digmean;                 /* digitisation mean value */
  double digscale;                /* digitisation scale factore */
  double elevation;               /* telescope elevation (radians) */
  char filter[8];                 /* string to hold filter name */
  double *heater = NULL;          /* bolometer heater ratios */
  int heatrun = 0;                /* Flag for running a HEATRUN observation */
  int hitsonly = 0;               /* Flag to indicate hits-only simulation */
  int maxwrite;                   /* file close time */
  obsMode mode;                   /* what type of observation are we doing? */
  dim_t nbol;                     /* total number of bolometers */
  Grp *obsGrp = NULL;             /* Group containing obs parameter file */
  AstKeyMap *obskeymap = NULL;    /* AstKeyMap for obs parameters */
  size_t osize = 0;               /* Size of obsGrp */
  int overwrite = 0;              /* Flag to specify whether existing
                                     files are overwritten */
  double *pzero = NULL;           /* bolometer power offsets */
  int rseed;                      /* seed for random number generator */
  int scan = 0;                   /* Flag for running a SCAN observation */
  Grp *simGrp = NULL;             /* Group containing sim parameter file */
  AstKeyMap *simkeymap = NULL;    /* AstKeyMap for sim parameters */
  int simstats = 0;               /* Flag to denote whether just to
                                     list simulation statistics */
  char simtype[LEN__METHOD];      /* String for simulation type */
  size_t ssize = 0;               /* Size of simGrp */
  struct timeval time;            /* Structure for system time */
  static double weights[SC2SIM__MXIRF]; /* impulse response */
  double *xbc = NULL;             /* projected NAS X offsets of bolometers
                                     in arcsec */
  double *xbolo = NULL;           /* Native bolo x-offsets */
  double *ybc = NULL;             /* projected NAS Y offsets of bolometers
                                     in arcsec */
  double *ybolo = NULL;           /* Native bolo y-offsets */
  /* ARD parameters */
  char ard[LEN__METHOD];          /* Name of ARD description */
  int ardFlag=0;                  /* Flag for ARD description */
  Grp *ardGrp = NULL;             /* Group containing ARD description */
  int *bolos = NULL;              /* Array of all bolometers */
  dim_t lbnd[2];                  /* Lower pixel bounds for bad pixel mask */
  dim_t lbnde[2];                 /* Lower pixel bounds encompassing all
                                     external pixels */
  dim_t lbndi[2];                 /* Lower pixel bounds encompassing all
                                     internal pixels */
  int regval=0;                   /* First keyword in ARD description */
  float trcoeff;                  /* Coefficients for ARD mapping */
  dim_t ubnd[2];                  /* Upper pixel bounds for bad pixel mask */
  dim_t ubnde[2];                 /* Upper pixel bounds encompassing all
                                     external pixels */
  dim_t ubndi[2];                 /* Upper pixel bounds encompassing all
                                     internal pixels */

  /* Get input parameters */
  kpg1Gtgrp ( "OBSPAR", &obsGrp, &osize, status );
  kpg1Kymap ( obsGrp, &obskeymap, status );
  kpg1Gtgrp ( "SIMPAR", &simGrp, &ssize, status );
  kpg1Kymap ( simGrp, &simkeymap, status );

  /* Seed random number generator, either with the time in
     milliseconds, or from user-supplied seed */
  parGet0i( "SEED", &rseed, status );
  if ( *status == PAR__NULL ) {
    errAnnul ( status );
    gettimeofday ( &time, NULL );
    rseed = (int)( ( time.tv_sec * 1000 ) + ( time.tv_usec / 1000 ) );
    msgOutif(MSG__VERB," ",
             "Seeding random numbers with clock time", status);
  } else {
    msgSeti( "SEED", rseed );
    msgOutif(MSG__VERB," ","Seeding random numbers with ^SEED", status);
  }

  /* Initialise random number generator to give same sequence every time,
     leading to the same series of pzero and heater offsets */
  srand(53);

  /* Information for user */
  msgOutif(MSG__NORM, "", " ==== SCUBA-2 simulator ====", status);

  msgOutif(MSG__VERB, "", "  Initialise instrument", status);
  sc2sim_instrinit ( &inx, &sinx, obskeymap, simkeymap, coeffs, &digcurrent,
                     &digmean, &digscale, &elevation, weights, &heater,
                     &pzero, &xbc, &ybc, &xbolo, &ybolo, status );

  /* Re-initialise random number generator to give a different sequence
     each time by using the given seed. */
  srand ( rseed );

  nbol = inx.colsize * inx.rowsize;
  /* Bad bolometer mask */
  bolos = astCalloc( (dim_t)(nbol), sizeof(int) );
  lbnd[0] = 1;
  lbnd[1] = 1;
  ubnd[SC2STORE__COL_INDEX] = inx.rowsize;
  ubnd[SC2STORE__ROW_INDEX] = inx.colsize;
  parGet0c("BADBOL", ard, LEN__METHOD, status);
  if ( *status == PAR__NULL ) {
    errAnnul( status );
  } else {
    ardGrpex ( ard, NULL, &ardGrp, &ardFlag, status );
    trcoeff = VAL__BADR;
    ardWork8 ( ardGrp, 2, lbnd, ubnd, &trcoeff, 0, &regval, bolos,
               lbndi, ubndi, lbnde, ubnde, status );
  }

  /* String for the wavelength of the filter */
  sprintf( filter,"%i",(int) (inx.lambda*1e6) );

  /* Get observation mode and coordinate frame */
  mode = sc2sim_getobsmode( inx.obsmode, status );
  coordframe = sc2sim_getcoordframe( inx.coordframe, status );

  /* Let user know details of simulation */
  msgSetc("L", filter);
  switch ( mode ) {

  case MODE__HEATRUN:
    msgSetc("M","HEATRUN");
    heatrun = 1;
    break;
  case MODE__STARE:
    msgSetc("M","STARE");
    break;
  case MODE__NOISE:
    msgSetc("M","NOISE");
    break;
  case MODE__DREAM:
    msgSetc("M","DREAM");
    break;
  case MODE__PONG:
    if ( strncmp(inx.pong_type, "CURV", 4) == 0 ) {
      msgSetc("M","CURVY PONG");
    } else if ( strncmp(inx.pong_type, "STRA", 4) == 0 ) {
      msgSetc("M","STRAIGHT PONG");
    }
    scan = 1;
    break;
  case MODE__BOUS:
    msgSetc("M","BOUSTROPHEDON");
    scan = 1;
    break;
  case MODE__SINGLESCAN:
    msgSetc("M","SINGLESCAN");
    scan = 1;
    break;
  case MODE__LISS:
    msgSetc("M","LISSAJOUS");
    scan = 1;
    break;
  case MODE__EXTERNAL:
    msgSetc("M","EXTERNAL");
    scan = 1;
    break;
  default:
    /* Check status - if things are OK then there is a problem with
       the given observation mode */
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSetc( "M", inx.obsmode );
      errRep("", "^M is not a supported observation mode", status);
    }
  }

  /* Has the user requested a listing of the stats only? */
  parGet0l( "SIMSTATS", &simstats, status );

  if ( !simstats ) {
    int i;
    msgSetc("T",inx.obstype);
    msgOutif(MSG__NORM, "", "  Simulating a ^T observation at ^L um in ^M mode",
             status);
    for (i=0; i<sinx.nsubarrays; i++) {
      msgSetc("SUB", (sinx.subname)[i]);
      if (i != sinx.nsubarrays-1) msgSetc("SUB", ",");
    }
    msgOutif(MSG__NORM, "", "  Simulating subarrays: ^SUB", status);
  }

  /* Carry out the appropriate simulation based on the observation type */
  if ( heatrun ) {
    /* Do a heatrun/flatfield simulation */
    sc2sim_heatrun ( &inx, &sinx, coeffs, digcurrent, digmean, digscale, filter,
                     heater, (int) nbol, pzero, inx.steptime, status );

  } else {

    if ( scan ) {
      /* Check if this is a full of weights-only simulation */
      parChoic( "SIMTYPE", "FULL", "FULL, WEIGHTS", 1,
                simtype, LEN__METHOD, status);
      /* Set the flag for hits-only */
      if( strncmp( simtype, "FULL", 4 ) == 0 ) {
        hitsonly = 0;
      } else if ( strncmp( simtype, "WEIGHTS", 7 ) == 0 ) {
        hitsonly = 1;
      } else {
        if ( *status == SAI__OK ) {
          *status = SAI__ERROR;
          msgSetc( "S", simtype );
          errRep("", "^S is not a supported simulation type", status);
        }
      }
    }

    /* Get the file close interval */
    parGet0i("MAXWRITE", &maxwrite, status);
    /* Will new output files overwrite old ones? */
    parGet0l("OVERWRITE", &overwrite, status);

    sc2sim_simulate ( &inx, &sinx, coeffs, digcurrent, digmean, digscale,
                      filter, heater, maxwrite, mode, coordframe, (int) nbol,
                      pzero, rseed, inx.steptime, weights, xbc, xbolo, ybc,
                      ybolo, hitsonly, overwrite, simstats, status);
  }

  /* Free resources */
  heater = astFree( heater );
  pzero = astFree( pzero );
  xbc = astFree( xbc );
  ybc = astFree( ybc );
  xbolo = astFree( xbolo );
  ybolo = astFree( ybolo );
  bolos = astFree( bolos );

  if ( ardGrp ) grpDelet ( &ardGrp, status );
  if ( simGrp ) grpDelet ( &simGrp, status );
  if ( obsGrp ) grpDelet ( &obsGrp, status );

  msgOutif(MSG__NORM, "", " Simulation complete", status);

}
