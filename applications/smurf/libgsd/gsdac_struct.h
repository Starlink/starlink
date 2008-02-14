/*
 *+
 *  Name:
 *     gsdac_struct.h

 *  Purpose:
 *     structure definitions for gsd2acsis 

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     C include file

 *  Description:
 *     Structures used by the gsd2acsis library.
 

 *  Authors:
 *     J. Balfour (j.balfour@jach.hawaii.edu)

 *  History:
 *     2008-01-28 (JB):
 *        Original.
 *     2008-02-14 (JB):
 *        Add gsdac_gsdVars_struct   

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

 *-
*/

#define MAXSTRING 17

#ifndef GSDAC_STRUCT_DEFINED
#define GSDAC_STRUCT_DEFINED

typedef struct gsdac_gsd_struct  /* GSD file access parameters */
{
  char *dataPtr;              /* GSD data */
  void *fileDsc;              /* GSD file descriptor */
  void *itemDsc;              /* GSD item descriptors */
} gsd;

typedef struct gsdac_gsdVars_struct /* GSD header and array data */
{
  float *alphas;              /* ?? */
  double antennaGain;         /* antenna gain */
  double apertureEff;         /* ratio total power observed/incident
                                 on the telescope */
  char backend[MAXSTRING];    /* name of the backend */
  float badVal;               /* bad value flag */
  float *bandwidths;          /* bandwidths (MHz) */
  double beamEff;             /* fraction of beam in diffraction
                                 limited main beam */
  int *BEChans;               /* number of BE chans for each section */
  int BEConfig;               /* backend configuration */
  int *BEConnChans;           /* number of IF output channels connected 
                                 to each BE input channel */
  int *BEInputChans;          /* number of BE input chans for each section */
  double *BEInputFreqs;       /* BE input frequencies (GHz) */
  int *BESubsys;              /* subsystem numbers for each BE section */
  char BEType[MAXSTRING];     /* type of the backend */
  int *bitModes;              /* correlation bit modes */
  char calInstrument[MAXSTRING]; /* calibration instrument */
  char calMode[MAXSTRING];    /* way of calibrating */
  char calSource[MAXSTRING];  /* calibration source */
  float *calTemps;            /* calibration temperatures */
  char calType[MAXSTRING];    /* type of calibration */
  int cellCode;               /* code for cell coordinate frame */
  char cellCoords[MAXSTRING]; /* cell coordinate frame */
  char cellUnit[MAXSTRING];   /* units of cell and mapping */
  double cellV2X;             /* scanning angle from local vertical to 
                                 x axis measured CW (degrees) */
  double cellV2Y;             /* position angle of cell y axis 
                                 (degrees CCW) */
  double cellX;               /* width of cells (arcsec) */
  double cellX2Y;             /* angle between cell y and x axis 
                                 (degrees CCW) */
  double cellY;               /* height of cells (arcsec) */
  double centreAz;            /* Az of centre at obs date (degrees) */
  int centreCode;             /* code for centre coordinate frame */
  char centreCoords[MAXSTRING]; /* centre coordinate frame */
  double centreDec;           /* Dec of centre (degrees) */
  double centreDec1950;       /* 1950 Dec of centre (degrees) */
  double centreDec2000;       /* 2000 Dec of centre (degrees) */
  double centreEl;            /* El of centre at obs date (degrees) */
  double centreEpoch;         /* date of RA/Dec coordinates (year) */
  double *centreFreqs;        /* centre frequencies (GHz) */
  double centreGB;            /* galactic latitude of centre */
  double centreGL;            /* galactic longitude of centre */
  char centreMoving;          /* centre moving flag */
  double centreOffsetX;       /* commanded x centre position */
  double centreOffsetY;       /* commanded y centre position */
  double centreRA;            /* RA of centre (hours) */
  double centreRA1950;        /* 1950 RA of centre (hours) */
  double centreRA2000;        /* 2000 RA of centre (hours) */
  char chopCoords[MAXSTRING]; /* chopping coordinate frame */
  float chopFrequency;        /* chopping frequency (Hz) */
  float chopPA;               /* chop position angle (degrees) */
  char chopping;              /* chopping flag */
  float chopThrow;            /* chop throw (arcsec) */
  char chopWaveform[MAXSTRING]; /* SMU chopping waveform */
  int *corrModes;             /* correlation function modes. */
  int cycleTime;              /* duration of each cycle (seconds) */
  float *data;                /* complete data array */
  char dataOutput[MAXSTRING]; /* description of output */
  char dataUnits[MAXSTRING];  /* units of data */
  char epochType[MAXSTRING];  /* epoch type */
  double errEl;               /* El offset at start (arcsec) */
  double errAz;               /* Az offset at start (arcsec) */
  double etafss;              /* forward spillover and scattering
                                 efficiency */
  double etal;                /* rear spillover and scattering 
                                 efficiency */
  float *FESkyTrans;          /* FE-derived sky transmission */
  float *FETSkyIm;            /* FE-derived tsky, image sideband */
  float *FETSysIm;            /* FE-derived tsys, image sideband */
  char FEType[MAXSTRING];     /* type of the frontend */
  float *freqRes;             /* frequency resolutions (MHz) */
  char frontend[MAXSTRING];   /* name of the frontend */
  float *gains;               /* gains */
  double hamb;                /* mean atmospheric relative humidity (%) */
  int IFPerSection;           /* number of IF inputs per section */
  int *intTimes;              /* integration times. */
  double *LOFreqs;            /* frontend LO frequencies */
  char mapPosX;               /* in first row x increases? */
  char mapPosY;               /* in first row y increases? */
  float mapStartX;            /* X coordinate of first map point */
  float mapStartY;            /* Y coordinate of first map point */
  float *mapTable;            /* xy offsets for each scan */
  int *mixNums;               /* ?? */
  int nBEChansIn;             /* number of BE input channels */
  int nBEChansOut;            /* number of BE output channels */
  int nBESections;            /* number of BE sections */
  int nCycle;                 /* number of cycles done */
  int nCyclePts;              /* number of sky points complete in
                                 observation */
  int nFEChans;               /* number of FE output channels */
  int nMapDims;               /* number of dimensions of map table */
  int nMapPts;                /* number of map points */
  int nMapPtsX;               /* number of map points in X direction */
  int nMapPtsY;               /* number of map points in Y direction */
  int noCyclePts;             /* total number of xy positions 
                                 observed during a cycle */
  int noCycles;               /* Maximum number of cycles in scan */
  double nObs;                /* observation number */
  int noScans;                /* number of scans */
  int nPhases;                /* number of phases per cycle */
  int nPhaseVars;             /* number of phase table variables */
  unsigned int nRecep;        /* number of receptors participating in 
                                 this observation */
  int nScan;                  /* number of scans done */
  int nScanPts;               /* maximum number of map points done in 
                                 a phase */
  int nScanVars1;             /* number of scan table 1 variables */
  int nScanVars2;             /* number of scan table 2 variables */
  unsigned int nSteps;        /* number of time steps in the observation */
  unsigned int nSubsys;       /* number of subsystems */
  unsigned int nTotChans;     /* total number of channels in observation */
  int nVRad;                  /* number of elements in vradial array */
  char object1[MAXSTRING];    /* object of interest */
  char object2[MAXSTRING];    /* object of interest (second half of name) */
  char obsCalibration;        /* calibration observation flag */
  char obsCentre;             /* centre moves between scans flag */
  char obsContinuous;         /* data taken on-the-fly flag */
  char obsDirection[MAXSTRING]; /* direction of scan */
  char obsFocus;              /* focus observation flag */
  double obsLST;              /* LST at start of observation (hour) */
  char obsMap;                /* map observation flag */
  char obsType[MAXSTRING];    /* observation type */
  double obsUT1C;             /* UT1-UTC correction (days) */
  double obsUT1d;             /* UT1 date of observation (YYYY.MMDD) */
  double obsUT1h;             /* UT1 hour of observation (hours) */
  float *opacities;           /* water opacities (NEPER) */
  double pamb;                /* mean atmospheric pressure (mmHg) */
  float *phaseTable;          /* phase table */
  char *phaseVars;            /* phase table column names */
  char polarity[MAXSTRING];   /* polarity ? */
  char project[MAXSTRING];    /* name of the project */
  char projectObs1[MAXSTRING];/* name of the primary observer */
  char projectObs2[MAXSTRING];/* name of the support scientist */
  char projectObs3[MAXSTRING];/* name of the telescope operator */
  float *recTemps;            /* receiver temperatures (K) */
  double referenceX;          /* reference X position (arcsec) */
  double referenceY;          /* reference Y position (arcsec) */
  double *restFreqs;          /* rest frequencies (GHz) */
  float *sbGainNorms;         /* normalizes signal sideband gain */
  char sbMode[MAXSTRING];     /* sideband mode */
  float *sbOverlaps;          /* sideband overlaps */
  float *sbRatios;            /* ratio of signal sideband to image
                                 sideband sky transmission */
  int *sbSigns;               /* sb signs for each section */
  char scanRev;               /* scan reversal flag */
  int scanTime;               /* total time of scan (seconds) */
  float *scanTable1;          /* array of data from scan table 1 */
  float *scanTable2;          /* array of data from scan table 2 */  
  char *scanVars1;            /* array of column names for table 1 */
  char *scanVars2;            /* array of column names for table 2 */
  float seeing;               /* seeing at JCMT */
  char seeTime[MAXSTRING];    /* time of seeing (YYMMDDHHMM) */
  float shiftFrac;            /* ?? */
  float *skyTemps;            /* sky temperatures (K) */
  float *skyTrans;            /* sky transmissions */
  float smuDX;                /* SMU X displacement at start (mm) */
  float smuDY;                /* SMU Y displacement at start (mm) */
  float smuDZ;                /* SMU Z displacement at start (mm) */
  int smuEWEnc;               /* SMU EW encoder value (encoder) */
  float smuEWScale;           /* SMU EW chop scale (arcsec/enc) */
  int smuNSEnc;               /* SMU NS encoder value (encoder) */
  float smuNSScale;           /* SMU NS chop scale (arcsec/enc) */
  float smuOffsEW;            /* SMU offset parallel to lower axis 
                                 (arcsec) */
  float smuOffsNS;            /* SMU offset parallel to upper axis
                                 (arcsec) */
  float smuX;                 /* SMU absolute X position at start
                                 (mm) */
  float smuY;                 /* SMU absolute Y position at start
                                 (mm) */
  float smuZ;                 /* SMU absolute Z position at start
                                 (mm) */
  float *sourceSysTemps;      /* source sky temperatures (K) */
  char swMode[MAXSTRING];     /* switch mode */
  double tamb;                /* ambient temperature (degC) */
  float tau225;               /* CSO Tau 225 GHz */
  float tauRMS;               /* CSO Tau rms */
  char tauTime[MAXSTRING];    /* Time of CSO Tau 225 (YYMMDDHHMM) */
  float tCold;                /* cold load temperature (K) */
  char telCoords[MAXSTRING];  /* telescope mount coordinates */
  double telHeight;           /* height ot telescope above sea level (km) */
  double telLatitude;         /* geographical latitude of telescope
                                 (degrees North +ve) */
  double telLongitude;        /* geographical longitude of telescope
                                 (degrees West +ve) */
  char telName[MAXSTRING];    /* telescope name */
  float *telTemps;            /* telescope temperatures (K) */
  float *telTrans;            /* telescope transmissions */
  float tHot;                 /* ambient load temperature (K) */
  double *totIFs;             /* total IF for each section */
  double userAz;              /* user Az correction (arcsec) */
  double userEl;              /* user El correction (arcsec) */
  char velDefn[MAXSTRING];    /* velocity definition */
  char velRef[MAXSTRING];     /* velocity reference frame */
  double velocity;            /* radial velocity of the source (km/sec) */
  double *vRadial;            /* array of radial velocities */
} gsdVars;

typedef struct gsdac_wcs_struct  /* pointing and time */
{  
  double *airmass;            /* airmass */
  double *acAz;               /* actual telescope Az */
  double *acEl;               /* actual telescope El */
  double *acTr1;              /* actual telescope in Tracking */
  double *acTr2;              /* actual telescope in Tracking */
  double *azAng;              /* angle between focal plane and AZEL */
  double *baseAz;             /* base Az */
  double *baseEl;             /* base El */
  double *baseTr1;            /* base in Tracking */
  double *baseTr2;            /* base in Tracking */
  double *el;                 /* actual telescope El */
  double *index;              /* index into observing area */
  double *tai;                /* TAI time */
  double *trAng;              /* angle between focal and tracking planes */
} wcs;
   
#endif /* GSDAC_STRUCT_DEFINED */
