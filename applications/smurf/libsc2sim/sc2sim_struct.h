/*  sc2sim_struct.h - structure definitions for simulator 

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)
    A.G. Gibb (agg@astro.ubc.ca)
    E.L. Chapin (echapin@phas.ubc.ca)
    J. Balfour (jbalfour@phas.ubc.ca)

   History :
    14Feb2003 : original (bdk)
    26Jun2003 : add separate struct for simulator (bdk)
    23Jan2004 : add components for Earth's atmospheric emission (bdk)
    02Feb2004 : add atmrefval and atmrefnu (bdk)
    26Jan2005 : add xpoint, ypoint (agg)
    04Feb2005 : add telemission (bdk)
    08Feb2005 : add polarimeter parameters (bdk)
    17Feb2005 : add heatstart, heatstep, heatnum (bdk)
    12May2005 : add ra, dec, subname (bdk)
    17Jun2005 : add numsamples (bdk)
    06Oct2005 : add flatname (bdk)
    24Jan2006 : add pwvzen / atstart / atend (ec)
    23Feb2006 : add mjdaystart (ec)
    03Mar2006 : add pong* (ec)
    15Jun2006 : replace pwvzen with tauzen (jb)
    29Jun2006 : removed dataname (jb)
    04Aug2006 : added bous & singlescan parameters (jb)
    21Aug2006 : Put in ifndef to avoid including multiple times (ec)
    22Sep2006 : Merge with dxml_struct.h (jb)
    05Oct2006 : Replace pong_gridcount with pong_height and pong_width (jb)
    12Oct2006 : Delete wt0_name and wt1_name as they are deprecated (agg)
    16Oct2006 : Add pong_type (jb)
    21Nov2006 : Add liss parameters and remove bolfile (deprecated)(jb)
    22Nov2006 : Add pong_nmaps and liss_nmaps (jb)
    18Dec2006 : Add DUT1 parameter (agg)
    18Dec2006 : Replace pattern-specific parameters with general 
                parameters (jb)
*/
#include "libsc2sim/sc2sim_par.h"

#ifndef SC2SIM_STRUCT_DEFINED
#define SC2SIM_STRUCT_DEFINED

struct bolpix             /* pixel location of bolometer */
       {
            int quad;     /* array quadrant, 0-3 */
            int x;        /* X-index in quadrant 0-39 */
            int y;        /* Y-index in quadrant 0-39 */
       };

/* Enumerated type for observing modes */
typedef enum {stare, dstare, dream, pong, polspin, heatrun, 
              bous, singlescan, liss, none} obsMode;

/* Enumerated type for map coordinate frame */
typedef enum {nasmyth, azel, radec, nocoord} mapCoordframe;

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
  char coordframe[80];        /* Map coord. frame (nas/azel/radec) */
  double dec;                 /* declination in radians */
  double distfac;             /* distortion factor (0=no distortion) */
  double dut1;                /* Value of UT1 - UTC for current date */
  char flatname[SC2SIM__FLEN]; /* name of flatfield algorithm */
  double grid_step_x;         /* Grid step in X direction */
  double grid_step_y;         /* Grid step in Y direction */
  int gridpts[SC2SIM__MXGRID][2];  /* relative grid coordinates */
  double heatnum;             /* number of heater settings */
  double heatstart;           /* initial heater setting (pW) */
  double heatstep;            /* increment of heater setting (pW) */
  double height;              /* min height of pattern (arcsec) */
  double jig_step_x;          /* The step size in -X- direction between
				 Jiggle positions in arcsec */
  double jig_step_y;          /* The step size in -Y- direction between
				 Jiggle positions in arcsec */
  int jig_vert[SC2SIM__MXVERT][2];/* Array with relative vertex coordinates in
				    units of pixel distance */
  double lambda;              /* wavelength in metres */
  double liss_angle;          /* angle of pattern relative to telescope
				 axes in radians anticlockwise */
  double mjdaystart;          /* Modified julian date at start */      
  int nbolx;                  /* number of bolometers in X */
  int nboly;                  /* number of bolometers in Y */
  int ngrid;                  /* Nr of reconstruction points for single
				 bolometer */
  double nmaps;               /* Number of times to repeat pattern */
  int numsamples;             /* number of samples in STARE */
  int nvert;                  /* Nr of vertices in the Jiggle pattern */
  char obsmode[80];           /* Type of observation */
  int platenum;               /* number of waveplate rotations */
  double platerev;            /* waveplate rotation rev/sec */
  double pong_angle;          /* angle of pattern relative to telescope
				 axes in radians anticlockwise */
  char pong_type[80];         /* Type of PONG scan (straight or curve) */
  double pong_vmax;           /* Telescope max velocities (arcsec/sec) */
  double ra;                  /* right ascension in radians */
  double sample_t;            /* sample time in msec */
  double scan_angle;          /* angle of pattern relative to telescope
				 axes in radians anticlockwise  */
  int smu_move;               /* Code for the SMU move algorithm */
  double smu_offset;          /* SMU phase shift */
  int smu_samples;            /* Nr of samples per jiggle vertex */
  double spacing;             /* grid spacing in arcsec */
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
  double aomega;              /* coupling factor 0.179(850) 0.721(450) */
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
  double bandGHz;             /* bandwidth in GHz */
  double blindang;            /* polarisation angle of blind (deg) */
  double blindpol;            /* polarisation of blind (%) */
  double blindtrans;          /* transmission of blind (%) */
  double cassang;             /* polarisation angle of Cass optics (deg) */
  double casspol;             /* polarisation of Cass optics (%) */
  double casstrans;           /* transmission of Cass optics (%) */
  int flux2cur;               /* Convert power to current 1=yes, 0=no */
  double meanatm;             /* mean expected atmospheric signal pW */
  double nasang;              /* polarisation angle of Nasmyth optics (deg) */
  double naspol;              /* polarisation of Nasmyth optics (%) */
  double nastrans;            /* transmission of Nasmyth optics (%) */
  int ncycle;                 /* Number of cycles through the pattern */
  double smu_terr;            /* SMU timing error */
  char subname[80];           /* name of subarray */
  double telemission;         /* telescope background pW per pixel */
  double tauzen;              /* optical depth at 225GHz at the zenith */
  double xpoint;              /* X pointing offset on sky (arcsec) */
  double ypoint;              /* Y pointing offset on sky (arcsec) */
};
   
#endif /* SC2SIM_STRUCT_DEFINED */
