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
 *     Tim Jenness (JAC, Hawaii)

 *  History:
 *     2008-01-28 (JB):
 *        Original.
 *     2008-02-14 (JB):
 *        Add gsdac_gsdVars_struct  .
 *     2008-02-19 (JB):
 *        Add gsdWCS struct.
 *     2008-02-28 (JB):
 *        Add dateVars and mapVars structs.
 *     2008-03-27 (JB):
 *        Add spectral line lookup table.
 *     2008-03-28 (JB):
 *        Add standard source lookup table.
 *     2008-04-02 (JB):
 *        Move lookup tables to smf_moltrans.h and gsdac_standard_sources.h.
 *     2008-04-21 (JB):
 *        Add gsdDType.
 *     2008-07-04 (TIMJ):
 *        GSD library now uses real types.

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

 *-
*/

#include "gsd.h"
#include "smurf_par.h"

#define MAXSTRING 17
#define TRANSITIONS_SIZE 1054

#ifndef GSDAC_STRUCT_DEFINED
#define GSDAC_STRUCT_DEFINED

/* Enumerated type for DAS file type */
typedef enum {DAS_NONE, DAS_TP, DAS_CONT_CAL, DAS_CROSS_CORR} dasFlag;

/* Enumerated type for DAS file type */
typedef enum {COORD_AZ = 1, COORD_EQ = 3, COORD_RD = 4,
              COORD_RB = 6, COORD_RJ = 7, COORD_GA = 8} gsdCoordType;

/* Enumerated type for GSD headers and arrays (data types). */
typedef enum {GSD_INT = 1, GSD_DOUBLE = 2, GSD_FLOAT = 3, GSD_CHAR = 4, GSD_CHARPTR = 5} gsdDType;

typedef struct gsdac_gsd_struct  /* GSD file access parameters */
{
  char *dataPtr;              /* GSD data */
  GSDFileDesc *fileDsc;       /* GSD file descriptor */
  GSDItemDesc *itemDsc;       /* GSD item descriptors */
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
  gsdCoordType cellCode;      /* code for cell coordinate frame */
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
  gsdCoordType centreCode;    /* code for centre coordinate frame */
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
  char CSOAbsorb[MAXSTRING];  /* position of absorber IN or OUT */
  float CSODAz;               /* position offset in Az (arcsec) */
  double CSODec;              /* Dec */
  float CSODEl;               /* position offset in elevation (arcsec) */
  double CSOEpoch;            /* epoch of CSO RA and Dec */
  char CSOFocus[MAXSTRING];   /* focus mode of CSO */
  float CSOFocusX;            /* CSO X position of focus */
  float CSOFocusY;            /* CSO Y position of focus */
  float CSOFocusZ;            /* CSO Z position of focus */
  double CSOFreqOffset;       /* Frequency offset (GHz) */
  double CSOIFFreq;           /* IF frequency (GHz) */
  double CSOLOFreq;           /* LO frequency (GHz) */
  int CSOMultHarm;            /* Multiplier Harmonic number */
  float CSOPAz;               /* pointing Az offset (arcsec) */
  float CSOPEl;               /* pointing El offset (arcsec) */
  char CSOPhaseLock[MAXSTRING]; /* phase lock status L or U */
  double CSORA;               /* RA */
  float CSORadVel;            /* radial velocity (km/x) */
  double CSORestFreq;         /* Rest frequency of line (GHz) */
  char CSOSideband[MAXSTRING]; /* Sideband U or L */
  int CSOStatus;              /* overall status 0 = bad 1 = good */
  float CSOTau;               /* CSO TAU value */
  char CSOTrack[MAXSTRING];   /* CSO Track mode of telescope Y or N */
  float CSOVelocity;          /* velocity of source (km/s) */
  float CSOVelOffset;         /* velocity offset (km/s) */
  int cycleTime;              /* duration of each cycle (seconds) */
  float *data;                /* complete data array */
  char dataOutput[MAXSTRING]; /* description of output */
  char dataUnits[MAXSTRING];  /* units of data */
  char epochType[MAXSTRING];  /* epoch type */
  double errAz;               /* Az offset at start (arcsec) */
  double errEl;               /* El offset at start (arcsec) */
  double etafss;              /* forward spillover and scattering
                                 efficiency */
  double etal;                /* rear spillover and scattering
                                 efficiency */
  double *FEFreqs;            /* observing frequencies */
  double *FELOFreqs;          /* FE LO frequencies */
  int *FESBSigns;             /* FE sideband signs */
  float *FESkyTrans;          /* FE-derived sky transmission */
  float *FETSkyIm;            /* FE-derived tsky, image sideband */
  float *FETSysIm;            /* FE-derived tsys, image sideband */
  char FEType[MAXSTRING];     /* type of the frontend */
  float *freqRes;             /* frequency resolutions (MHz) */
  char frontend[MAXSTRING];   /* name of the frontend */
  float *gains;               /* gains */
  double hamb;                /* mean atmospheric relative humidity (%) */
  float *hotPower;            /* total power measurement on hot load */
  int IFONCycle;              /* ?? */
  int IFONIntCycle;           /* ?? */
  int IFONPhase;              /* number of phases for interferometry
                                 observing */
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
  int nCorrCycle;             /* nunber of correlation cycles */
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
  int nScan;                  /* number of scans done */
  int nScanPts;               /* maximum number of map points done in
                                 a phase */
  int nScanVars1;             /* number of scan table 1 variables */
  int nScanVars2;             /* number of scan table 2 variables */
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
  int procBits;               /* data processing done */
  char procLoc[MAXSTRING];    /* desription of where processing is done */
  char project[MAXSTRING];    /* name of the project */
  char projectObs1[MAXSTRING];/* name of the primary observer */
  char projectObs2[MAXSTRING];/* name of the support scientist */
  char projectObs3[MAXSTRING];/* name of the telescope operator */
  float *recTemps;            /* receiver temperatures (K) */
  double referenceX;          /* reference X position (arcsec) */
  double referenceY;          /* reference Y position (arcsec) */
  double *restFreqs;          /* rest frequencies (GHz) */
  double RXJLengthX;          /* X length of projected baseline (metres) */
  double RXJLengthY;          /* Y length of projected baseline (metres) */
  double RXJLengthZ;          /* Z length of projected baseline (metres) */
  double RXJConstant;         /* coefficient of constant term in expression
                                 for fringe rate */
  double RXJCos;              /* coefficient of cos term in expression
                                 for fringe rate */
  int RXJCSOSwitch;           /* delay setting of RXJ micro for CSO side */
  int RXJJCMTSwitch;          /* delay setting of RXJ micro for JCMT side */
  int RXJNSecs;               /* number of the tick on which integration
                                 started */
  double RXJSin;              /* coefficient of sin term in expression
                                 for fringe rate */
  float *samples;             /* samples to store for cross_correlation mode */
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
  float *skyPower;            /* ?? */
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
  double telAz;               /* centre_az from tel sdis array */
  double telEl;               /* centre_el from tel sdis array */
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
  float *totPower;            /* total power measurement per subband
                                 per integration */
  double userAz;              /* user Az correction (arcsec) */
  double userEl;              /* user El correction (arcsec) */
  char velDefn[MAXSTRING];    /* velocity definition */
  char velRef[MAXSTRING];     /* velocity reference frame */
  double velocity;            /* radial velocity of the source (km/sec) */
  double *vRadial;            /* array of radial velocities */
} gsdVars;

typedef struct gsdac_gsdWCS_struct  /* pointing and time */
{
  double airmass;            /* airmass */
  double acAz;               /* actual telescope Az */
  double acEl;               /* actual telescope El */
  double acTr1;              /* actual telescope in Tracking */
  double acTr2;              /* actual telescope in Tracking */
  double azAng;              /* angle between focal plane and AZEL */
  double baseAz;             /* base Az */
  double baseEl;             /* base El */
  double baseTr1;            /* base in Tracking */
  double baseTr2;            /* base in Tracking */
  double index;              /* index into observing area */
  double tai;                /* TAI time */
  double trAng;              /* angle between focal and tracking planes */
} gsdWCS;

typedef struct gsdac_mapVars_struct  /* map/chop/scan parameters */
{
  char chopCrd[SZFITSTR];     /* chopper coordinate system */
  char loclCrd[SZFITSTR];     /* local offset coordinate system for
                                 map_x / map_y */
  float mapHght;              /* requested height of rectangle to be mapped
                                 (arcsec) */
  float mapPA;                /* requested PA of map vertical, +ve towards
                                 +ve long */
  float mapWdth;              /* requested width of rectangle to be mapped
                                 (arcsec) */
  char scanCrd[SZFITSTR];     /* coordinate system of scan */
  float scanDy;               /* scan spacing perpendicular to scan
                                 (arcsec) */
  float scanPA;               /* Scan PA rel. to lat. line; 0=lat,
                                 90=long in scanCrd system */
  char scanPat[SZFITSTR];     /* name of scanning scheme */
  float scanVel;              /* scan velocity (arcsec/sec) */
  char skyRefX[SZFITSTR];     /* X co-ord of reference position (arcsec) */
  char skyRefY[SZFITSTR];     /* Y co-ord of reference position (arcsec) */
  char swMode[SZFITSTR];      /* switch mode */
} mapVars;

typedef struct gsdac_dateVars_struct  /* date and time data */
{
  char dateEnd[SZFITSTR];     /* UTC datetime of end of observation
                                 in format YYYY-MM-DDTHH:MM:SS */
  char dateObs[SZFITSTR];     /* UTC datetime of start of observation
                                 in format YYYY-MM-DDTHH:MM:SS */
  char HSTend[SZFITSTR];      /* HST at observation end in format
                                 YYYY-MM-DDTHH:MM:SS */
  char HSTstart[SZFITSTR];    /* HST at observation start in format
                                 YYYY-MM-DDTHH:MM:SS */
  char LSTstart[SZFITSTR];    /* LST at observation start in format
                                 YYYY-MM-DDTHH:MM:SS */
  char LSTend[SZFITSTR];      /* LST at observation end in format
                                 YYYY-MM-DDTHH:MM:SS */
  char obsID[SZFITSTR];       /* unique observation number in format
                                 INSTR_NNNNN_YYYYMMDDTHHMMSS */
} dateVars;

#endif /* GSDAC_STRUCT_DEFINED */
