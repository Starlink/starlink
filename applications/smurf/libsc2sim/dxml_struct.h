/* dxml_struct - definition of structures for returning XML values 

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
    21Aug2006 : Put in ifndef to avoid including multiple times (EC)
*/

#include "libsc2sim/dream_par.h"

#ifndef DXML_STRUCT_DEFINED
#define DXML_STRUCT_DEFINED

struct dxml_struct {
  double bol_distx;           /* average bolometer distance */
  double bol_disty;           /* average bolometer distance */
  char bolfile[DREAM__FLEN];  /* name of file for bolometer details */
  double bous_angle;          /* angle of pattern relative to telescope
				 axes in radians anticlockwise  */
  double bous_pathlength;     /* length of path across sky (arcsec) */
  int bous_scancount;         /* number of scan lines  */
  double bous_spacing;        /* scan line spacing in arcsec  */
  double bous_vmax;           /* Telescope max velocities */
  int conv_shape;             /* Possible convolution functions are
				 0 - Gaussian
				 1 - sinc(dx).sinc(dy)
				 2 - sinc(dx).sinc(dy) tapered
				 3 - sinc(dx).sinc(dy) after first 1.0
				 4 - bessel tapered */
  double conv_sig;            /* convolution function parameter */
  double dec;                 /* declination in radians */
  double distfac;             /* distortion factor (0=no distortion) */
  char flatname[DREAM__FLEN]; /* name of flatfield algorithm */
  double heatnum;             /* number of heater settings */
  double heatstart;           /* initial heater setting (pW) */
  double heatstep;            /* increment of heater setting (pW) */
  double grid_step_x;         /* Grid step in X direction */
  double grid_step_y;         /* Grid step in Y direction */
  int gridpts[DREAM__MXGRID][2];  /* relative grid coordinates */
  double jig_step_x;          /* The step size in -X- direction between
				 Jiggle positions in arcsec */
  double jig_step_y;          /* The step size in -Y- direction between
				 Jiggle positions in arcsec */
  int jig_vert[DREAM__MXVERT][2];/* Array with relative vertex coordinates in
				    units of pixel distance */
  double lambda;              /* wavelength in metres */
  double mjdaystart;          /* Modified julian date at start */      
  int nbolx;                  /* number of bolometers in X */
  int nboly;                  /* number of bolometers in Y */
  int ngrid;                  /* Nr of reconstruction points for single
				 bolometer */
  int numsamples;             /* number of samples in STARE */
  int nvert;                  /* Nr of vertices in the Jiggle pattern */
  char obsmode[80];           /* Type of observation */
  double pong_angle;          /* angle of pattern relative to telescope
				 axes in radians anticlockwise */
  int pong_gridcount;         /* number of grid lines (odd) */
  double pong_spacing;        /* grid spacing in arcsec */
  double pong_vmax;           /* Telescope max velocities (arcsec/sec) */
  int platenum;               /* number of waveplate rotations */
  double platerev;            /* waveplate rotation rev/sec */
  double ra;                  /* right ascension in radians */
  double sample_t;            /* sample time in msec */
  double scan_angle;          /* angle of pattern relative to telescope
				 axes in radians anticlockwise  */
  double scan_pathlength;     /* length of path across sky (arcsec) */
  double scan_vmax;           /* Telescope max velocities */
  int smu_move;               /* Code for the SMU move algorithm */
  double smu_offset;          /* SMU phase shift */
  int smu_samples;            /* Nr of samples per jiggle vertex */
  int subsysnr;               /* subsystem number */
  double targetpow;           /* target bolometer power input pW */
  char wt0_name[DREAM__FLEN]; /* name of file for pixel weights */
  char wt1_name[DREAM__FLEN]; /* name of file for piston weights */
};

struct dxml_sim_struct
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
  double astang;              /* polarisation angle of source (deg) */
  char astname[DREAM__FLEN];  /* name of file for ast simulation */
  double astpol;              /* polarisation of source (%) */
  char atmname[DREAM__FLEN];  /* name of file for atm simulation */
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

#endif
