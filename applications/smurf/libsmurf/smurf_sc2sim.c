/*
*+
*  Name:
*     SC2SIM

*  Purpose:
*     Top-level SIMULATE implementation

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
*     This is the main routine implementing the SIMULATE task.  This
*     attempts to simulate the data taken by a SCUBA-2 subarray when
*     observing an astronomical image plus atmospheric background
*     while driving the JCMT. The simulation includes photon and 1/f
*     noise, and nonlinear response which varies for different
*     bolometers. It also includes SCUBA-2 field distortion. Sc2sim
*     combines the functionality of a number of executables built in
*     earlier versions of the simulator: staresim, dreamsim,
*     pongsim. The output file from a 'heat' observation is named
*     after the subarray, date, and filenumber, for example:
*     s8a20060301_00001_0001.sdf.  For each subarray used in a
*     simulation, a corresponding 'heat' output file must be present
*     in the working directory.

*     Sc2sim also performs the heatrun task when supplied an input
*     file with 'heat' observation mode specified.  This generates a
*     heater flat-field measurement from simulated data for each of
*     range of heater settings.  The output file from a 'heat'
*     observation is named after the subarray, date, and filenumber,
*     for example : s8aheat20060301_00001.sdf.

*  ADAM Parameters:
*     OBSPAR = GROUP (Read)
*          Specifies values for the observation parameters used by 
*          simulation.  
*
*          The supplied value should be either a 
*          comma-separated list of strings or the name of a text 
*          file preceded by an up-arrow character "^", containing 
*          one or more comma-separated (or line-break separated) 
*          list of strings. Each string is either a "keyword=value" 
*          setting, or the name of a text file preceded by an up-arrow 
*          character "^". Such text files should contain further 
*          comma-separated lists which will be read and interpreted 
*          in the same manner (any blank lines or lines beginning 
*          with "#" are ignored). Within a text file, newlines can 
*          be used as delimiters as well as commas. Settings are 
*          applied in the order in which they occur within the list, 
*          with later settings over-riding any earlier settings given 
*          for the same keyword.
*
*          Each individual setting should be of the form:
*
*             <keyword>=<value>
*
*          The parameter names and their default values are listed
*          below.  The default values will be used for any unspecified
*          parameters.  Unregnized parameters are ignored (i.e. no
*          error is reported).
*
*          bol_distx (double) : 6.28 (arcseconds)
*            Average bolometer distance in the x-direction.
*          bol_disty (double) : 6.28 (arcseconds)
*            Average bolometer distance in the y-direction.
*          bous_angle (double) : 0.0 (radians)
*            For the Boustrophedon obsmode, this parameter specifies 
*            the angle of the pattern relative to the telescope
*            axes in radians anticlockwise.
*          bous_width (double) : 2000.0 (arcseconds)
*            For the Boustrophedon obsmode, this parameter specifies
*            the width of the bous pattern in arcseconds.  This width
*            is the width of each of back-and-forth sweeps across
*            the sky, and is measured from the centre of the array.
*          bous_height (double) : 2000.0 (arcseconds)
*            For the Boustrophedon obsmode, this parameter specifies
*            the height of the bous pattern in arcseconds.  This 
*            height is the height of the stack of back-and-forth sweeps
*            across the sky, and is measured from the centre of the 
*            array.
*          bous_spacing (double) : 240.0 (arcseconds)
*            For the Boustrophedon obsmode, this parameter specifies
*            the spacing of the bous pattern in arcseconds.  This
*            spacing is the distance between the parallel horizontal
*            sweeps across the sky, and is measured from the centre of
*            the array.
*          bous_vmax (double) : 200.0 (arcseconds/second)
*            For the Boustrophedon obsmode, this parameter specifies
*            the maximum telescope velocity.
*          conv_shape (int) : 1
*            Flag for the possible convolution functions where
*               0 - Gaussian
*               1 - sinc(dx).sinc(dy)
*               2 - sinc(dx).sinc(dy) tapered
*               3 - sinc(dx).sinc(dy) after first 1.0
*               4 - bessel tapered
*          conv_sig (double) : 1.0
*            Convolution function parameter.
*          coordframe (char[]) : RADEC
*            Map coordinate frame, ether NASMYTH, AZEL, or RADEC.
*          dec (char[]) : 0:0:0.0 O (degrees:minutes:seconds)
*            Sexagesimal string representation of the Declination
*            of the observation.
*          distfac (double) : 0.0
*            Distortion factor, where 0 = no distortion.
*          flatname (char[]) : ""
*            Name of flatfield correction technique.
*          grid_max_x (integer) : 1
*            Reconstruction grid max x.
*          grid_max_x (integer) : 1
*            Reconstruction grid max y.
*          grid_min_x (integer) : 1
*            Reconstruction grid min x.
*          grid_min_x (integer) : 1
*            Reconstruction grid min y.
*          grid_step_x (double) : 6.28 (arcseconds)
*            Grid step in X direction.
*          grid_step_y (double) : 6.28 (arcseconds)
*            Grid step in Y direction.
*          heatnum (int) : 1
*            Number of heater settings.
*          heatstart (double) : 0.0 (pW)
*            Initial heater setting.
*          heatstep (double) : 0.0 (pW)
*            Increment of heater setting.
*          jig_step_x (double) : 6.28 (arcseconds)
*            The step size in the x-direction between
*            jiggle positions. 
*          jig_step_y (double) : 6.28 (arcseconds)
*            The step size in the y-direction between
*            jiggle positions.
*          jig_pos.x (char[]) : "0;-1;1;-1;0;1;-1;1"
*            Array with relative vertex coordinates
*            for the x-direction, in units of pixel
*            distance.  This parameter value should
*            be a semicolon-separated list of integer
*            values.  The number of values must be the
*            same as the number of values in the 
*            jig_pos.y array. 
*          jig_pos.y (char[]) : "1;-1;0;1;-1;1;0;-1"
*            Array with relative vertex coordinates
*            for the y-direction, in units of pixel
*            distance.  This parameter value should
*            be a semicolon-separated list of integer
*            values.  The number of values must be the
*            same as the number of values in the 
*            jig_pos.x array.
*          lambda (double) : 0.85e-3 (metres)
*            Wavelength of observation.
*          liss_angle (double) : 0.0 (radians)
*            For the LISS obsmode, this parameter specifies 
*            the angle of the pattern relative to the telescope
*            axes in radians anticlockwise.
*          liss_width (double) : 2000.0 (arcseconds)
*            For the LISS obsmode, this parameter specifies
*            the width of the Lissajous pattern in arcseconds.  
*          liss_height (double) : 2000.0 (arcseconds)
*            For the LISS obsmode, this parameter specifies
*            the height of the Lissajous pattern in arcseconds.  
*          liss_spacing (double) : 240.0 (arcseconds)
*            For the LISS obsmode, this parameter specifies
*            the spacing of the Lissajous pattern in arcseconds.  
*            This spacing is the distance between the parallel 
*            sweeps across the sky, and is measured from the 
*            centre of the array.
*          liss_vmax (double) : 200.0 (arcseconds/second)
*            For the LISS obsmode, this parameter specifies
*            the maximum telescope velocity.
*          liss_nmaps (integer) : 1 
*            The number of times the Lissajous pattern should
*            repeat.
*          mjdaystart (double) : 53795.0
*            Modified julian date at start of observation.
*          nbolx (integer) : 40
*            Number of bolometers in x direction.  This
*            is the number of bolometers in a "row", 
*            and this is the total number of "columns".
*          nboly (integer) : 32
*            Number of bolometers in x direction.  This
*            is the number of bolometers in a "column", 
*            and this is the total number of "rows".
*          numsamples (integer) : 128
*            For the Stare obsmode, this is the number of
*            samples.
*          nvert (integer) : 8
*            The number of vertices in the Jiggle pattern.
*          obsmode (char[]) : PONG
*            The observation mode, which can be any of 
*            the following :
*               STARE : Simulates a simple point-and-shoot 
*                       observation mode in which the camera 
*                       stares at at a specified area of sky 
*                       for a period of time.
*               DREAM : (Dutch REal-time Acquisition Mode)
*                       Simulates an observition in which the
*                       Secondary Mirror unit moves rapidly 
*                       in a star-like pattern such that each
*                       bolometer observes multiple points
*                       on the sky.
*               SINGLESCAN : Simulates moving the array in 
*                            a straight path across the sky.
*               BOUS : Simulates mapping a rectangular area of
*                      sky using a simple Boustrophedon or
*                      raster pattern.
*               PONG : Simulates mapping a rectangular area of
*                      sky by filling the box with a orthogonally
*                      cross-linked pattern by "bouncing" off the
*                      sides of the box.  There are two 
*                      subcategories of PONG, either Straight
*                      Pong (straight lines between vertices) or
*                      Curve Pong (slightly wiggly lines between
*                      vertices - pattern is a Fourier expanded
*                      Lissajous).
*               LISS : Simulates mapping a rectangular area of
*                      sky by filling the box with a Lissajous
*                      pattern.
*          platenum (integer) : 1
*            The number of waveplate rotations.
*          platerev (double) : 2.0
*            The waveplate rotation in revolutions/second.
*          pong_angle (double) : 0.0 (radians)
*            For the PONG obsmode, this parameter specifies 
*            the angle of the pattern relative to the telescope
*            axes in radians anticlockwise.
*          pong_width (double) : 2000.0 (arcseconds)
*            For the PONG obsmode, this parameter specifies
*            the width of the Pong pattern in arcseconds.  
*          pong_height (double) : 2000.0 (arcseconds)
*            For the PONG obsmode, this parameter specifies
*            the height of the Pong pattern in arcseconds.  
*          pong_spacing (double) : 240.0 (arcseconds)
*            For the PONG obsmode, this parameter specifies
*            the spacing of the Pong pattern in arcseconds.  
*            This spacing is the distance between the parallel 
*            sweeps across the sky, and is measured from the 
*            centre of the array.
*          pong_type (char[]) : STRAIGHT
*            Specifies the type of Pong simulation (straight or
*            curve.
*          pong_vmax (double) : 200.0 (arcseconds/second)
*            For the PONG obsmode, this parameter specifies
*            the maximum telescope velocity.
*          pong_nmaps (integer) : 1 
*            The number of times the Pong pattern should
*            repeat.
*          ra (char[]) : 0:0:0.0 O (hours:minutes:seconds)
*            Sexagesimal string representation of the Right 
*            ascension of the observation.
*          steptime (double) : 0.005 (sec)
*            Sample interval time.
*          scan_angle (double) : 0.0 (radians)
*            For the SINGLESCAN obsmode, this parameter specifies 
*            the angle of the pattern relative to the telescope
*            axes in radians anticlockwise.
*          scan_pa8thlength (double) : 2000.0 (arcseconds)
*            For the SINGLESCAN obsmode, this parameter specifies
*            the width of the scan path in arcseconds.  
*          scan_vmax (double) : 200.0 (arcseconds/second)
*            For the SINGLESCAN obsmode, this parameter specifies
*            the maximum telescope velocity.
*          smu_move (integer) : 8 
*            Code for the SMU move algorithm.  The possible codes 
*            are :
*               0 : Block wave.
*               1 : 2 term not damped.
*               2 : 3 term not damped.
*               3 : 4 term not damped.
*               4 : 2 term flat end.
*               5 : 3 term flat end.
*               6 : 4 term flat end.
*               7 : ScubaWave -  After 1 Ms 0.098. After 8 Ms 
*                   0.913. After 9 Ms 1.000. popepi points is 
*                   equivalent with 64 Ms.
*               8 : This is an experimental wave form, which may 
*                   change often.  Now it is a cosine waveform 
*                   from 0 to 1 in the full time.
*          smu_offset (double) : 0.0
*            SMU phase shift.
*          smu_samples (integer) : 1
*            Number of samples per jiggle vertex .
*          subsysnr (integer) : 1
*            Subsystem number.
*          targetpow (double) : 25.0 (pW)
*            Target bolometer power input.
*
*     SIMPAR = GROUP (Read)
*          Specifies values for the simulation parameters.  See 
*          the description for OBSPAR for the file format.
*
*          The parameter names and their default values are listed
*          below.  The default values will be used for any unspecified
*          parameters.  Unregnized parameters are ignored (i.e. no
*          error is reported).
*
*          add_atm (integer) : 0
*            Flag for adding atmospheric emission.
*          add_fnoise (integer) : 0
*            Flag for adding 1/f noise.
*          add_hnoise (integer) : 0
*            Flag for adding heater noise.
*          add_pns (integer) : 0
*            Flag for adding photon noise.
*          airmass (double) : 1.2
*            Airmass of simulated observation.
*          anang (double) : 0.0 (degrees)
*            Polarisation angle of analyser.
*          anpol (double) : 100.0 (percent)
*            Polarisation of analyser.
*          antrans (double) : 100.0 (percent)
*            Transmission of analyser.
*          aomega (double) : 0,179
*            Coupling factor (0.179 for 850 microns,
*            0.721 for 450 microns).
*          astname (char[]) : ""
*            Name 8of the file containing astronomical
*            sky image.
*          astpol (double) : 10.0 (percent)
*            Polarisation of source.
*          atmname (char[]) : ""
*            Name of the file containing atmospheric
*            sky image.
*          atend (double) : 5.0 (Degrees Celsius)
*            Ambient temperature at end.
*          atmxvel (double) : 5000.0 (arcsec/sec)
*            Atm background velocity in X.
*          atmyvel (double) : 0.0 (arcsec/sec)
*            Atm background velocity in Y.
*          atmzerox (double) : 5000.0 (arcsec)
*            Atm background offset in X.
*          atmzeroy (double) : 50000.0 (arcsec)
*            Atm background offset in Y.
*          atstart (double) : 5.0 (Degrees Celsius)
*            Ambient temperature at start.
*          bandGHz (double) : 35.0 (GHz)
*            Bandwidth in GHz.
*          blindang (double) : 10.0 (Degrees)
*            Polarisation angle of blind.
*          blindpol (double) : 1.0 (Percent)
*            Polarisation of blind.
*          blindtrans (double) : 93.0 (Percent)
*            Transmission of blind.
*          cassang (double) : 135.0 (Degrees)
*            Polarisatio nangle of Cass optics.
*          casspol (double) : 1.0 (Percent)
*            Polarisation of Cass optics.
*          casstrans (double) : 98.0 (Percent)
*            Transmission of Cass optics.
*          flux2cur (integer) : 1 
*            Flag to indicate to convert power 
*            from flux to current.
*          meanatm (double) : 7.0 (pW)
*            Mean expeected atmospheric signal.
*          nasang (double) : 90.0 (Degrees)
*            Polarisation angle of Nasmyth optics.
*          naspol (double) : 1.0 (Percent)
*            Polarisation of Nasmyth optics.
*          nastrans (double) : 98.0 (Percent)
*            Transmissio nof Nasmyth optics.
*          ncycle (integer) : 1
*            Number of cycles through the DREAM
*            pattern.
*          smu_terr (double) : 0.0
*            SMU timing error.
*          subname (char[]) : s8a
*            Semi-colon-separated list of subarray
*            names for the simulation.  Any number
*            of subarrays can be selected in any
*            order, and should be separated by
*            semi-colons (with no spaces) e.g.
*            s8a;s8b;s8d
*          telemission (double) : 4.0 (pW)
*            Telescope background pW per pixel.
*          tauzen (double) : 0.052583
*            Optical depth at 225 GHz at the zenith.
*          xpoint (double) : 20.0 (arcsec)
*            X pointing offset on sky.
*          ypoint (double) : 20.0 (arcsec)
*            Y pointing offset on sky.
*
*     SEED = INTEGER (Read)
*          Seed for random number generator.  If a seed
*          is not specified, the clock time in milliseconds
*          is used.
*     MAXWRITE = INTEGER (Read)
*          Number of samples to write in output file.
*     OVERWRITE = LOGICAL (Read)
*          Flag to specify whether existing files are
*          overwritten. Setting this to FALSE increments the `group'
*          counter in the output file names. Default is TRUE.
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
*        switch from sc2head to a generic JCMTState definition that can be used for 
*        ACSIS and SCUBA-2. Also involves moving the STATE information into a separate
*        part of the data file.
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
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

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
#include "star/slalib.h"
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

#include "wvm/wvmCal.h" /* Water Vapor Monitor routines */
#include "f77.h"

/* prototype for slalib routine that calculates mjd -> calendar date */
/*void slaDjcl(double djm, int *iy, int *im, int *id, double *fd, int *j);*/


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
  int hitsonly = 0;               /* Flag to indicate hits-only simulation */
  int maxwrite;                   /* file close time */
  obsMode mode;                   /* what type of observation are we doing? */
  int nbol;                       /* total number of bolometers */
  Grp *obsGrp = NULL;             /* Group containing obs parameter file */
  AstKeyMap *obskeymap = NULL;    /* AstKeyMap for obs parameters */
  size_t osize = 0;               /* Size of obsGrp */
  int overwrite = 0;              /* Flag to specify whether existing
				     files are overwritten */
  double *pzero = NULL;           /* bolometer power offsets */
  int rseed;                      /* seed for random number generator */
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
  
  char ard[LEN__METHOD];         /* Name of ARD description */
  int ardFlag=0;                 /* Flag for ARD description */
  Grp *ardGrp = NULL;            /* Group containing ARD description */
  int *bolos = NULL;             /* Array of all bolometers */
  int lbnd[2];                   /* Lower pixel bounds for bad pixel mask */
  int lbnde[2];                  /* Lower pixel bounds encompassing all
				    external pixels */
  int lbndi[2];                  /* Lower pixel bounds encompassing all
				    internal pixels */
  int regval=0;                  /* First keyword in ARD description */
  float trcoeff;                 /* Coefficients for ARD mapping */
  int ubnd[2];                   /* Upper pixel bounds for bad pixel mask */
  int ubnde[2];                  /* Upper pixel bounds encompassing all
				    external pixels */
  int ubndi[2];                  /* Upper pixel bounds encompassing all
				    internal pixels */
  int heatrun = 0;
  int dreamstare = 0;
  int scan = 0;

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
    rseed = ( time.tv_sec * 1000 ) + ( time.tv_usec / 1000 );
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

  nbol = inx.nbolx * inx.nboly;
  /* Bad bolometer mask */
  bolos = smf_malloc( (size_t)(nbol), sizeof(int), 1, status );
  lbnd[0] = 1;
  lbnd[1] = 1;
  ubnd[0] = inx.nbolx;
  ubnd[1] = inx.nboly;
  parGet0c("BADBOL", ard, LEN__METHOD, status);
  if ( *status == PAR__NULL ) {
    errAnnul( status );
  } else {
    ardGrpex ( ard, NULL, &ardGrp, &ardFlag, status );
    trcoeff = VAL__BADR;
    ardWork ( ardGrp, 2, lbnd, ubnd, &trcoeff, 0, &regval, bolos,
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
    dreamstare = 1;
    break;
  case MODE__DREAM:
    msgSetc("M","DREAM");
    dreamstare = 1;
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
  default:
    *status = SAI__ERROR;
    msgSetc( "M", inx.obsmode );
    errRep("", "^M is not a supported observation mode", status);
  }

  /* Has the user requested a listing of the stats only? */
  parGet0l( "SIMSTATS", &simstats, status );

  if ( !simstats ) {
    msgSetc("T",inx.obstype);
    msgOutif(MSG__NORM, "", "  Simulating a ^T observation at ^L um in ^M mode", 
	     status);
    msgSetc("SUB", sinx.subname);
    msgOutif(MSG__NORM, "", "  Simulating subarrays: ^SUB", status);
  }

  /* Carry out the appropriate simulation based on the observation type */
  if ( heatrun ) {
    /* Do a heatrun/flatfield simulation */
    sc2sim_heatrun ( &inx, &sinx, coeffs, digcurrent, digmean, digscale, filter,
		     heater, nbol, pzero, inx.steptime, status );

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
    } else if ( dreamstare ) {
      hitsonly = 0;
    }

    /* Get the file close interval */
    parGet0i("MAXWRITE", &maxwrite, status);
    /* Will new output files overwrite old ones? */
    parGet0l("OVERWRITE", &overwrite, status);

    sc2sim_simulate ( &inx, &sinx, coeffs, digcurrent, digmean, digscale, 
		      filter, heater, maxwrite, mode, coordframe, nbol, 
		      pzero, rseed, inx.steptime, weights, xbc, xbolo, ybc, 
		      ybolo, hitsonly, overwrite, simstats, status);
  }
 
  /* Free resources */
  heater = smf_free( heater, status );
  pzero = smf_free( pzero, status );
  xbc = smf_free( xbc, status );
  ybc = smf_free( ybc, status );
  xbolo = smf_free( xbolo, status );
  ybolo = smf_free( ybolo, status );
  bolos = smf_free( bolos, status );

  if ( ardGrp ) grpDelet ( &ardGrp, status ); 
  if ( simGrp ) grpDelet ( &simGrp, status ); 
  if ( obsGrp ) grpDelet ( &obsGrp, status ); 

  msgOutif(MSG__NORM, "", " Simulation complete", status);

}
