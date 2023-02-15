/*
 *+
 *  Name:
 *     sc2sim_struct.h

 *  Purpose:
 *     structure definitions for simulator

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     C include file

 *  Authors:
 *     B.D.Kelly (bdk@roe.ac.uk)
 *     A.G. Gibb (agg@astro.ubc.ca)
 *     E.L. Chapin (echapin@phas.ubc.ca)
 *     J. Balfour (jbalfour@phas.ubc.ca)
 *     C. VanLaerhoven (clvl@phas.ubc.ca)
 *     T. Jenness (t.jenness@jach.hawaii.edu)

 *  History:
 *     2003-02-14 (BDK):
 *        Original.
 *     2003-06-26 (BDK):
 *        Add separate struct for simulator.
 *     2004-01-23 (BDK):
 *        Add components for Earth's atmospheric emission.
 *     2004-02-02 (BDK):
 *        Add atmrefval and atmrefnu.
 *     2005-01-26 (AGG):
 *        Add xpoint, ypoint
 *     2005-02-04 (BDK):
 *        Add telemission.
 *     2005-02-08 (BDK):
 *        Add polarimeter parameters.
 *     2005-02-17 (BDK):
 *        Add heatstart, heatstep, heatnum.
 *     2005-05-12 (BDK):
 *        Add ra, dec, subname.
 *     2005-06-17 (BDK):
 *        Add numsamples.
 *     2005-10-06 (BDK):
 *        Add flatname.
 *     2006-01-24 (EC):
 *        Add pwvzen / atstart / atend.
 *     2006-02-23 (EC):
 *        Add mjdaystart.
 *     2006-03-03 (EC):
 *        Add pong*.
 *     2006-06-14 (JB):
 *        Replace pwvzen with tauzen.
 *     2006-06-29 (JB):
 *        Removed dataname.
 *     2006-08-04 (JB):
 *        Added bous & singlescan parameters.
 *     2006-08-21 (EC):
 *        Put in ifndef to avoid including multiple times.
 *     2006-09-22 (JB):
 *        Merge with dxml_struct.h.
 *     2006-10-05 (JB):
 *        Replace pong_gridheight with pong_height and pong_width.
 *     2006-10-12 (AGG):
 *        Delete wt0_name and wt1_name as they are deprecated.
 *     2006-10-16 (JB):
 *        Add pong_type.
 *     2006-11-21 (JB):
 *        Add liss parameters and remove bolfile (deprecated)
 *     2006-11-22 (JB):
 *        Add pong_nmaps and liss_nmaps.
 *     2006-12-18 (AGG):
 *        Add DUT1 parameter.
 *     2006-12-18 (JB):
 *        Replace pattern-specific parameters with general parameters.
 *     2006-12-21 (AGG)
 *        Add instap & instap_x/y.
 *     2006-12-22 (AGG):
 *        Add planetnum.
 *     2007-03-26 (AGG):
 *        Change units of steptime (formerly sample_t) to seconds, not ms
 *     2007-05-23 (EC):
 *        Added telpos to sc2sim_sim_struct.
 *     2007-06-29 (EC):
 *        Simplified bolometer noise, changed sc2sim_sim_struct:
 *           -removed guessatm, aomega, bandGHz
 *           -added jy2pw, refload, refnoise
 *     2007-07-03 (EC):
 *        Made obsMode/mapCoordframe enumerated types more readable.
 *     2007-07-04 (EC):
 *        Added spike_t0/p0/p1/alpha parameters to sc2sim_sim_struct.
 *     2007-08-15 (CLV):
 *        Added microstepping parameters to sc2sim_obs_struct.
 *     2007-09-06 (AGG):
 *        Redefine heatnum as an int.
 *     2007-10-31 (TIMJ):
 *        Use dim_t following sc2store changes.
 *     2008-03-19 (AGG):
 *        Add obstype.
 *     2008-04-24 (AGG):
 *        Add nfocstep, focstep and focstart
 *     2008-10-10 (AGG):
 *        Add MODE__NOISE to obsMode
 *     2009-10-15 (TIMJ):
 *        Store subarray names in char array.
 *     2009-11-20 (DSB):
 *        Added interp and params.

*  Copyright:
*     Copyright (C) 2007,2009 Science and Technology Facilities Council.
*     Copyright (C) 2003-2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2005-2008 University of British Columbia.
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

 *-
*/
#include "libsc2sim/sc2sim_par.h"

#ifndef SC2SIM_STRUCT_DEFINED
#define SC2SIM_STRUCT_DEFINED

#include "smurf_par.h"
#include "smurf_typ.h"

struct bolpix             /* pixel location of bolometer */
       {
            int quad;     /* array quadrant, 0-3 */
            int x;        /* X-index in quadrant 0-39 */
            int y;        /* Y-index in quadrant 0-39 */
       };

/* Enumerated type for observing modes */
typedef enum {MODE__STARE, MODE__DSTARE, MODE__DREAM, MODE__PONG,
	      MODE__POLSPIN, MODE__HEATRUN, MODE__NOISE, MODE__EXTERNAL,
              MODE__BOUS, MODE__SINGLESCAN, MODE__LISS, MODE__NONE} obsMode;

/* Enumerated type for map coordinate frame */
typedef enum {FRAME__NASMYTH, FRAME__AZEL, FRAME__RADEC, FRAME__NOCOORD}
  mapCoordframe;

struct sc2sim_obs_struct      /* parameters read from obs input file */
{
  double bol_distx;           /* average bolometer distance */
  double bol_disty;           /* average bolometer distance */
  double bous_angle;          /* angle of pattern relative to telescope
				 axes in radians anticlockwise  */
  int conv_shape;             /* Possible convolution functions are
				 0 - Gaussian
				 1 - sinc(dx).sinc(dy)
				 2 - sinc(dx).sinc(dy) tapered
				 3 - sinc(dx).sinc(dy) after first 1.0
				 4 - bessel tapered */
  double conv_sig;            /* convolution function parameter */
  char coordframe[SZFITSTR];  /* Map coord. frame (nas/azel/radec) */
  double dec;                 /* declination in radians */
  double distfac;             /* distortion factor (0=no distortion) */
  double dut1;                /* Value of UT1 - UTC for current date */
  char flatname[SC2SIM__FLEN]; /* name of flatfield algorithm */
  double focstart;            /* Starting SMU position (mm) for focus observation */
  double focstep;             /* Interval (in mm) between SMU positions for focus */
  double grid_step_x;         /* Grid step in X direction */
  double grid_step_y;         /* Grid step in Y direction */
  int gridpts[SC2SIM__MXGRID][2];  /* relative grid coordinates */
  int heatnum;                /* number of heater settings */
  double heatstart;           /* initial heater setting (pW) */
  double heatstep;            /* increment of heater setting (pW) */
  double height;              /* min height of pattern (arcsec) */
  char instap[SC2SIM__FLEN];  /* Name of instrument aperture */
  double instap_x;            /* X Focal plane offset (arcsec) */
  double instap_y;            /* Y Focal plane offset (arcsec) */
  double jig_step_x;          /* The step size in -X- direction between
				 Jiggle positions in arcsec */
  double jig_step_y;          /* The step size in -Y- direction between
				 Jiggle positions in arcsec */
  int jig_vert[SC2SIM__MXVERT][2];/* Array with relative vertex coordinates
				        in units of pixel distance */
  double lambda;              /* wavelength in metres */
  double liss_angle;          /* angle of pattern relative to telescope
				 axes in radians anticlockwise */
  double mjdaystart;          /* Modified julian date at start (UTC) */
  double mspat_x[SC2SIM__MXMSTP]; /* microstep pattern offsets in x
				    (units: number of detectors) */
  double mspat_y[SC2SIM__MXMSTP]; /* microstep pattern offsets in y
				    (units: number of decectors) */
  dim_t colsize;             /* number of bolometers in column */
  dim_t rowsize;             /* number of bolometers in row */
  int nfocstep;               /* Number of focus positions */
  int ngrid;                  /* Nr of reconstruction points for single
				 bolometer */
  double nmaps;               /* Number of times to repeat pattern */
  int nmicstep;               /* number of microsteps */
  int numsamples;             /* number of samples in STARE */
  dim_t nvert;               /* Nr of vertices in the Jiggle pattern */
  char obsmode[SZFITSTR];     /* Observation mode (DREAM, STARE, PONG etc) */
  char externobs[SC2SIM__FLEN];/* filename of an external SCUBA2 observation used to generate a scan pattern */
  char obstype[SZFITSTR];     /* Observation type (POINT, FOCUS or SCIENCE) */
  int planetnum;              /* Number corresponding to a planet */
  int platenum;               /* number of waveplate rotations */
  double platerev;            /* waveplate rotation rev/sec */
  double pong_angle;          /* angle of pattern relative to telescope
				 axes in radians anticlockwise */
  char pong_type[SZFITSTR];   /* Type of PONG scan (straight or curve) */
  double pong_vmax;           /* Telescope max velocities (arcsec/sec) */
  double ra;                  /* right ascension in radians */
  double scan_angle;          /* angle of pattern relative to telescope
				 axes in radians anticlockwise  */
  int smu_move;               /* Code for the SMU move algorithm */
  double smu_offset;          /* SMU phase shift */
  int smu_samples;            /* Nr of samples per jiggle vertex */
  double spacing;             /* grid spacing in arcsec */
  double steptime;            /* Time interval between samples in sec */
  int subsysnr;               /* subsystem number */
  double targetpow;           /* target bolometer power input pW */
  double vmax;                /* Telescope max velocities (arcsec/sec) */
  double width;               /* min width of pattern (arcsec) */
};

struct sc2sim_sim_struct      /* parameters read from sim input file */
{
  int add_atm;                /* Add atmospheric emission 1=yes, 0=no */
  int add_fnoise;             /* Add 1/f noise 1=yes, 0=no */
  int add_hnoise;             /* Add heater noise 1=yes, 0=no */
  int add_pns;                /* Add photon noise 1=yes, 0=no */
  double airmass;             /* Airmass of simulated observation */
  double anang;               /* polarisation angle of analyser (deg) */
  double anpol;               /* polarisation of analyser (%) */
  double antrans;             /* transmission of analyser (%) */
  char astname[SC2SIM__FLEN];  /* name of file for ast simulation */
  double astpol;              /* polarisation of source (%) */
  char atmname[SC2SIM__FLEN];  /* name of file for atm simulation */
  double atmrefnu;            /* atm reference corner frequency (Hz) */
  double atmrefvel;           /* atm reference velocity (m/sec) */
  double atmxvel;             /* atm background velocity in X (arcsec/sec) */
  double atmyvel;             /* atm background velocity in Y (arcsec/sec) */
  double atmzerox;            /* atm background offset in X (arcsec) */
  double atmzeroy;            /* atm background offset in Y (arcsec) */
  double atend;               /* Ambient temperature at end (Celcius) */
  double atstart;             /* Ambient temperature at start (Celcius) */
  double blindang;            /* polarisation angle of blind (deg) */
  double blindpol;            /* polarisation of blind (%) */
  double blindtrans;          /* transmission of blind (%) */
  double cassang;             /* polarisation angle of Cass optics (deg) */
  double casspol;             /* polarisation of Cass optics (%) */
  double casstrans;           /* transmission of Cass optics (%) */
  int flux2cur;               /* Convert power to current 1=yes, 0=no */
  int interp;                 /* Interpolation method */
  double jy2pw;               /* Jy to pW conversion modulo atm transmission */
  double nasang;              /* polarisation angle of Nasmyth optics (deg) */
  double naspol;              /* polarisation of Nasmyth optics (%) */
  double nastrans;            /* transmission of Nasmyth optics (%) */
  int ncycle;                 /* Number of cycles through the pattern */
  double params[2];           /* Interpolation parameters */
  double refload;             /* Reference load in pW */
  double refnoise;            /* Reference NEP in pW/sqrt(Hz) @ refload */
  double smu_terr;            /* SMU timing error */
  double spike_alpha;         /* Index of spike p-law distribution */
  double spike_p0;            /* Minimum spike power (Jy) */
  double spike_p1;            /* Peak spike power (Jy) */
  double spike_t0;            /* Mean time between bolometer spikes (s) */
  char subname[SC2SIM__MAXSUBS][SC2SIM__SUBLEN]; /* names of subarrays */
  int  nsubarrays;            /* number of subarrays */
  double telemission;         /* telescope background pW per pixel */
  double telpos[3];           /* telescope W. lon/lat/altitude (deg/deg/m) */
  double tauzen;              /* optical depth at 225GHz at the zenith */
  double xpoint;              /* X pointing offset on sky (arcsec) */
  double ypoint;              /* Y pointing offset on sky (arcsec) */
};

#endif /* SC2SIM_STRUCT_DEFINED */
