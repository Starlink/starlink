/*
 *+
 *  Name:
 *     sc2sim_ndfwrdata

 *  Purpose:
 *     Generic digitise/compress and store SCUBA-2 data as NDF

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_ndfwrdata ( const struct sc2sim_obs_struct *inx,
 *                        const struct sc2sim_sim_struct *sinx,
 *                        double meanwvm, const char file_name[],
 *                        dim_t numsamples, dim_t nflat, double refres, const char flatname[],
 *                        const JCMTState *head, const int *dbuf, const int *dksquid,
 *                        const double *fcal, const double *fpar,
 *                        const char instrume[], const char filter[],
 *                        const char dateobs[], const char obsid[],
 *                        const double *posptr, dim_t jigsamples,
 *                        const double jigptr[][2], const int obsnum,
 *                        const int nsubscan,
 *                        const char utdate[], const double azstart,
 *                        const double azend, const double elstart,
 *                        const double elend, const char lststart[],
 *                        const char lstend[], const char loclcrd[],
 *                        const char scancrd[], const double totaltime,
 *                        const double exptime, const int nimage,
 *                        const double wvmstart, const double wvmend,
 *                        int *status )

 *  Arguments:
 *     inx = const sc2sim_obs_struct* (Given)
 *        Pointer to struct with observation parameters
 *     sinx = const sc2sim_sim_struct* (Given)
 *        Pointer to struct with simulation parameters
 *     meanwvm = double (Given)
 *        225 GHz tau
 *     file_name = const char[] (Given)
 *        Output file name
 *     numsamples = dim_t (Given)
 *        Number of samples
 *     nflat = dim_t (Given)
 *        Number of flat coeffs per bol
 *     refres = double (Given)
 *        Resistance used to calculate flatfield
 *     flatname = const char[] (Given)
 *        Name of flatfield algorithm
 *     head = const JCMTState* (Given)
 *        Header data for each frame
 *     dbuf = const int* (Given)
 *        Simulated data
 *     dksquid = const int* (Given)
 *        Dark SQUID time stream data
 *     fcal = const double* (Given)
 *        Flatfield calibration
 *     fpar = double (Given)
 *        Flat-field parameters
 *     instrume = const char[] (Given)
 *        Instrument name (usually SCUBA-2)
 *     filter = const char[] (Given)
 *        String representing filter (e.g. "850")
 *     dateobs = const char[] (Given)
 *        DATE-OBS FITS string
 *     obsid = const char[] (Given)
 *        Observation ID string
 *     posptr = const double* (Given)
 *        Pointing offsets from map centre
 *     jigsamples = dim_t (Given)
 *        Number of jiggle samples in DREAM pattern
 *     jigptr[][2] = double (Given)
 *        Array of jiggle X and Y positions
 *     obsnum = const int (Given)
 *        Observation number
 *     nsubscan = const int (Given)
 *        Sub-scan number
 *     utdate[] = const char (Given)
 *        UT date in YYYYMMDD form
 *     azstart = const double (Given)
 *        Azimuth at start of sub-scan
 *     azend = const double (Given)
 *        Azimuth at end of sub-scan
 *     elstart = const double (Given)
 *        Elevation at start of sub-scan
 *     elend = const double (Given)
 *        Elevation at end of sub-scan
 *     lststart[] = const char (Given)
 *        LST at start of sub-scan
 *     lstend[] = const char (Given)
 *        LST at end of sub-scan
 *     loclcrd[] = const char (Given)
 *        Coordinate frame
 *     scancrd[] = const char (Given)
 *        SCAN coordinate frame
 *     totaltime = const double (Given)
 *        Total integration time
 *     exptime = const double (Given)
 *        Subimage exposure time
 *     nimage = const int (Given)
 *        Number of subimages within subscan
 *     wvmstart = const double (Given)
 *        225-GHz tau at beginning of subscan
 *     wvmend = const double (Given)
 *        225-GHz tau at end of subscan
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Create and map a SCUBA-2 NDF file. Scale the data to integers
 *     one frame at a time and add a compressed version to the mapped
 *     file.  Store the per-frame header items and the FITS
 *     headers. Calculates images for DREAM and STARE modes, and
 *     scanfit polynomial fits (for 1/f drift) for all modes.

 *  Authors:
 *     E.Chapin (UBC)
 *     A.G. Gibb (UBC)
 *     J. Balfour (UBC)
 *     Tim Jenness (JAC, Hawaii)
 *     C. VanLaerhoven (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2006-03-29 (EC):
 *        dsim_ndfwrdata adapted from dsim_ndfwrpong
 *     2006-05-11 (AGG)
 *        Added obsmode
 *     2006-07-21 (JB):
 *        Split from dsim.c
 *     2006-07-28 (JB):
 *        Changed sc2head to JCMTState
 *     2006-08-08 (EC):
 *        Added INSTRUME FITS keyword
 *     2006-08-18 (AGG):
 *        Update API to take:
 *        - pointers to inx and sinx structs
 *        - DREAM jiggle position parameters
 *     2006-09-06 (EC):
 *        INSTRUME keyword now taken as argument (to accomodate AzTEC)
 *     2006-09-15 (AGG):
 *        Write out name of DREAM weights file into FITS header
 *     2006-09-22 (JB):
 *        Replace dxml_structs with sc2sim_structs
 *     2006-10-06 (AGG):
 *        Add WAVELEN FITS keyword
 *     2006-10-26 (JB):
 *        Convert to using AstFitsChans
 *     2006-12-01 (AGG):
 *        Now takes dateobs string, writes TIMESYS FITS header
 *     2006-12-15 (AGG):
 *        Write out DUT1 FITS header
 *     2006-12-19 (TIMJ):
 *        sc2store_wrtstream has additional subnum argument
 *     2006-12-21 (AGG):
 *        Add instap & instap_x/y FITS headers
 *     2007-03-20 (TIMJ):
 *        - Write header units in compliance with FITS standard
 *        - Use const arguments and add OBSID argument/header
 *     2007-04-02 (AGG):
 *        Add more FITS headers
 *     2007-04-10 (AGG):
 *        Calculate STARE images and polynomial fits, write to raw data
 *     2007-04-26 (AGG):
 *        - Shorten comment for SEQSTART/END to ensure correct formatting
 *        - Add some more (admittedly blank) FITS headers
 *     2007-05-04 (AGG):
 *        Add INSTRUME FITS keyword
 *     2007-05-17 (EC):
 *        Use astFitsSetCN instead of astFitsSetS for COMMENT lines
 *     2007-05-22 (EC):
 *        Hard-wire obsgeo keywords to Mauna Kea - not Socorro NM!
 *     2007-05-23 (EC):
 *        Derive obsgeo keywords from telpos using smf_terr
 *     2007-06-26 (EC):
 *        Derive OBSIDSS keyword from OBSID and inx->lambda
 *     2007-07-10 (AGG):
 *        - Use instrume argument for writing INSTRUME keyword
 *        - Initialize dreamweights file name to null string
 *     2007-08-13 (AGG):
 *        Write out consistent coordinate system for DREAM/STARE images
 *     2007-08-14 (AGG):
 *        Write out planet name for planet simulations
 *     2007-08-22 (CV):
 *        Fixed coordinate system string termination issue
 *     2007-08-27 (AGG):
 *        Set SkyRef and coordinate system correctly for DREAM/STARE images
 *     2007-08-29 (CV):
 *        Removed call to sc2sim_instap_calc as it was already called in
 *        sc2sim_simulate
 *     2007-10-05 (AGG):
 *        Add obsend flag
 *     2007-10-09 (AGG):
 *        Correct bug in calculating DATE-OBS for subimages
 *     2007-10-22 (TIMJ):
 *        Use new definition for fits header as required by sc2store.
 *     2007-10-31 (TIMJ):
 *        DA now uses dim_t to count things. A read-only dateobs was being
 *        written to.
 *     2007-11-15 (AGG):
 *        Write sub-image FITS headers in the correct order, check old
 *        headers exist before deleting them and make sure that
 *        SEQSTART/END headers are written for first image
 *     2007-12-18 (AGG):
 *        Update to use new smf_free behaviour
 *     2008-03-19 (AGG):
 *        Add calls to new routines to get ORAC-DR recipe and DRGROUP
 *     2008-04-17 (AGG):
 *        Back out call to derive DRGROUP
 *     2008-04-24 (AGG):
 *        - Write out FOCUS parameters
 *        - Remove obstype from API as it is in obs struct
 *     2008-05-07 (AGG):
 *        Set seqstart/end from beginning of observation, not just
 *        within current subscan
 *     2008-05-23 (AGG):
 *        Add focposn to API
 *     2008-05-28 (TIMJ):
 *        - Fix strncpy issues
 *        - Correct call to sc2sim_get_recipe
 *        - use one_strlcpy
 *     2008-07-23 (AGG):
 *        Write out BASEC1, BASEC2 and TRACKSYS
 *     2008-07-23 (EC):
 *        - Shutter is now a floating point
 *     2008-08-21 (AGG):
 *        - Set TCS_TAI not RTS_END when creating WCS for DREAM/STARE images
 *        - Set output coordinate system with sc2ast_set_output_system as this
 *          routine deals with moving sources too
 *     2009-07-08 (TIMJ):
 *        Use astSetFitsCM instead of astSetFitsCN
 *     2009-08-18 (TIMJ):
 *        Use new API for sc2store_putimage that supports provenance
 *     2010-01-29 (AGG):
 *        Write BASETEMP, SEQSTART and SEQEND to the FITS header for SCAN obs
 *     2010-03-16 (TIMJ):
 *        Use one_strlcat not strncat
 *     2017-04-06 (GSB):
 *        Set dtai=VAL__BADD in telpar

 *  Copyright:
 *     Copyright (C) 2007-2010 Science and Technology Facilities Council.
 *     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
 *     Council. Copyright (C) 2005-2010 University of British
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

/* Standard includes */
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "ndf.h"
#include "star/kaplibs.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "par_par.h"
#include "star/one.h"

/* SC2SIM includes */
#include "sc2sim.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2ast.h"
#include "sc2da/sc2math.h"
#include "sc2da/dream_par.h"
#include "jcmt/state.h"

void sc2sim_ndfwrdata
(
 const struct sc2sim_obs_struct *inx,  /* structure for values from XML (given) */
 const struct sc2sim_sim_struct *sinx, /* structure for sim values from XML (given)*/
 int subindex,            /* index into sinx->subname of subarray being written */
 double meanwvm,          /* Mean 225 GHz tau */
 const char file_name[],  /* output file name (given) */
 dim_t numsamples,        /* number of samples (given) */
 dim_t nflat,             /* number of flat coeffs per bol (given) */
 double refres,           /* Resistance used to calculate flatfield */
 const char flatname[],   /* name of flatfield algorithm (given) */
 const JCMTState *head,   /* header data for each frame (given) */
 const int *dbuf,         /* simulated data (given) */
 const int *dksquid,      /* dark SQUID time stream data (given) */
 const double *fcal,      /* flatfield calibration (given) */
 const double *fpar,      /* flat-field parameters (given) */
 const char instrume[],   /* String representing instrument (e.g. "SCUBA-2") (given) */
 const char filter[],     /* String representing filter (e.g. "850") (given) */
 const char dateobs[],    /* String representing UTC DATE-OBS (given) */
 const char obsid[],      /* unique obsid for this observation (given) */
 const double *posptr,    /* Pointing offsets from map centre (given) */
 dim_t jigsamples,        /* Number of jiggle samples (given) */
 double jigptr[][2],      /* Array of X, Y jiggle positions (given) */
 const int obsnum,        /* Observation number (given) */
 const double focposn,    /* Focus position */
 const int nsubscan,      /* Sub-scan number (given) */
 const int obsend,        /* Flag to indicate whether this is the last file */
 const char utdate[],     /* UT date in YYYYMMDD form (given) */
 const double azstart,    /* Azimuth at start of sub-scan (given) */
 const double azend,      /* Azimuth at end of sub-scan (given) */
 const double elstart,    /* Elevation at start of sub-scan (given) */
 const double elend,      /* Elevation at end of sub-scan (given) */
 const char lststart[],   /* LST at start of sub-scan (given) */
 const char lstend[],     /* LST at end of sub-scan (given) */
 const char loclcrd[],    /* Coordinate frame (given) */
 const char scancrd[],    /* SCAN coordinate frame (given) */
 const double totaltime,  /* Total integration time (given) */
 const double exptime,    /* Subimage exposure time (given) */
 const int nimage,        /* Number of subimages within subscan (given) */
 const double wvmstart,   /* 225-GHz tau at beginning of subscan (given) */
 const double wvmend,     /* 225-GHz tau at end of subscan (given) */
 int *status              /* Global status (given and returned) */
 )

{
  /* Local variables */
  double coadd[2*DREAM__MXBOL];   /* Coadded values in output image */
  char cosys[JCMT__SZTCS_TR_SYS+1]; /* Tracking coordinate system */
  int dims[2];                    /* Extent of output image */
  AstFitsChan *fitschan;          /* FITS headers */
  int fitsfind;
  char fitsrec[SC2STORE__MAXFITS*SZFITSCARD+1]; /* Store for FITS records */
  int framesize;                  /* Number of points in a single `frame' */
  dim_t i;                       /* Loop counter */
  double instap[2];               /* Instrument aperture */
  dim_t j;                       /* Loop counter */
  int jigvert[SC2SIM__MXVERT][2]; /* Temp array to jig_vert */
  dim_t k;                       /* Loop counter */
  int naver;                      /* Number of frames to average */
  int ndim;                       /* Dimensionality of output image */
  dim_t nrec;                    /* number of FITS header records */
  int nsubim;                     /* Number of DREAM/STARE images */
  double map_hght;                /* Map height in arcsec */
  double map_pa;                  /* Map PA in degrees  */
  double map_wdth;                /* Map width in arcsec  */
  double map_x = 0;               /* Map X offset in arcsec */
  double map_y = 0;               /* Map Y offset in arcsec */
  int ncoeff = 2;                 /* Number of coefficients in polynomial fit */
  char objectname[JCMT__SZTCS_SOURCE+1]; /* Name of object */
  double obsgeo[3];               /* Cartesian geodetic observatory coords. */
  char obsidss[84];               /* OBSID + wavelength */
  double *poly = NULL;            /* Pointer to polynomial fit solution */
  char prvname[2*PAR__SZNAM+1];   /* Name to use for provenance */
  double *rdata = NULL;           /* Pointer to flatfielded data */
  char recipe[30];                /* Name of default ORAC-DR recipe */
  SC2STORETelpar telpar;          /* Struct for telescope info */
  int seqend;                     /* RTS index of last frame in output image */
  int seqoffset;                  /* Sequence number offset from beginning of
                                     observation */
  int seqstart;                   /* RTS index of first frame in output image */
  JCMTState state;                /* Dummy JCMT state structure for creating WCS */
  sc2ast_subarray_t subnum;       /* sub array index */
  AstFrameSet *wcs;               /* WCS frameset for output image */
  char weightsname[SZFITSTR];     /* Name of weights file for DREAM
                                     reconstruction */
  double x_max = -1.0e38;         /* X extent of pointing centre offsets */
  double x_min =  1.0e38;         /* X extent of pointing centre offsets */
  double y_max = -1.0e38;         /* Y extent of pointing centre offsets */
  double y_min =  1.0e38;         /* Y extent of pointing centre offsets */
  double zero[2*DREAM__MXBOL];    /* Bolometer zero points */
  char imdateobs[30];             /* DATE-OBS for IMAGE header */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* This calc should go in a higher level routine */
  /* Determine extent of the map from posptr + known size of the arrays */
  for( i=0; i<numsamples; i++ ) {
    /* Initialize extrema */
    if( i == 0 ) {
      x_min = posptr[0];
      x_max = posptr[0];
      y_min = posptr[1];
      y_max = posptr[1];
    }
    if( posptr[i*2] < x_min ) x_min = posptr[i*2];
    if( posptr[i*2] > x_max ) x_max = posptr[i*2];
    if( posptr[i*2+1] < y_min ) y_min = posptr[i*2+1];
    if( posptr[i*2+1] > y_max ) y_max = posptr[i*2+1];
  }

  map_wdth = (x_max - x_min) + 650.0; /* 650 arcsec for array diagonal FOV */
  map_hght = (y_max - y_min) + 650.0; /* 650 arcsec for array diagonal FOV */
  map_pa = 0; /* kludge for now since it is not specified by the user */
  map_x = (x_max + x_min)/2.;
  map_y = (y_max + y_min)/2.;

  /* Define the FITS headers to add to the output file */
  fitschan = astFitsChan ( NULL, NULL, " " );

  /* Telescope */
  astSetFitsCM ( fitschan, "-- Telescope specific parameters --", 0 );
  astSetFitsS ( fitschan, "TELESCOP", "JCMT", "Name of telescope", 0 );
  astSetFitsS ( fitschan, "ORIGIN", "SMURF SCUBA-2 simulator",
                "Origin of file", 0 );

  smf_terr( (sinx->telpos)[1]*DD2R, (sinx->telpos)[2],
            -(sinx->telpos)[0]*DD2R, obsgeo );

  astSetFitsF ( fitschan, "OBSGEO-X", obsgeo[0], //-5464545.04,
                "x,y,z triplet for JCMT", 0 );
  astSetFitsF ( fitschan, "OBSGEO-Y", obsgeo[1], //-2492986.33,
                "relative to centre of the Earth", 0 );
  astSetFitsF ( fitschan, "OBSGEO-Z", obsgeo[2], //2150635.34,
                "[m]", 0 );

  astSetFitsF ( fitschan, "ALT-OBS", (sinx->telpos)[2], //4092,
                "[m] Height of observatory above sea level", 0 );
  astSetFitsF ( fitschan, "LAT-OBS", (sinx->telpos)[1], //19.8258323669,
                "[deg] Latitude of observatory", 0 );
  astSetFitsF ( fitschan, "LONG-OBS", -(sinx->telpos)[0], //204.520278931,
                "[deg] East longitude of observatory", 0 );

  astSetFitsF ( fitschan, "ETAL", 1.0, "Telescope efficiency", 0 );

  /* Observation, date & pointing */
  astSetFitsCM ( fitschan, "-- Observation & date parameters --", 0 );
  astSetFitsS ( fitschan, "OBSID", obsid, "Unique observation ID", 0 );

  sprintf( obsidss, "%s_%i", obsid, (int) (inx->lambda*1e6) );
  astSetFitsS ( fitschan, "OBSIDSS", obsidss,
                "Unique observation/wavelength ID", 0 );

  /* Set object name - use planet name if known */
  if ( inx->planetnum != -1 ) {
    if ( inx->planetnum == 2 ) {
      one_strlcpy( objectname, "VENUS", JCMT__SZTCS_SOURCE+1, status );
    } else if ( inx->planetnum == 3 ) {
      one_strlcpy( objectname, "MOON", JCMT__SZTCS_SOURCE+1, status );
    } else if ( inx->planetnum == 4 ) {
      one_strlcpy( objectname, "MARS", JCMT__SZTCS_SOURCE+1, status );
    } else if ( inx->planetnum == 5 ) {
      one_strlcpy( objectname, "JUPITER", JCMT__SZTCS_SOURCE+1, status );
    } else if ( inx->planetnum == 6 ) {
      one_strlcpy( objectname, "SATURN", JCMT__SZTCS_SOURCE+1, status );
    } else if ( inx->planetnum == 7 ) {
      one_strlcpy( objectname, "URANUS", JCMT__SZTCS_SOURCE+1, status );
    } else if ( inx->planetnum == 8 ) {
      one_strlcpy( objectname, "NEPTUNE", JCMT__SZTCS_SOURCE+1, status );
    }
  } else {
    /* Generic name... */
    one_strlcpy( objectname, "SMURF", JCMT__SZTCS_SOURCE+1, status );
  }
  astSetFitsS ( fitschan, "OBJECT", objectname, "Object Name", 0 );
  astSetFitsL ( fitschan, "STANDARD", 0, "True if source is a calibrator", 0 );
  astSetFitsI ( fitschan, "OBSNUM", obsnum, "Observation Number", 0 );
  astSetFitsI ( fitschan, "NSUBSCAN", nsubscan, "Sub-scan Number", 0 );
  astSetFitsI ( fitschan, "SEQCOUNT", obsnum, "Sequence Counter", 0 );
  astSetFitsL ( fitschan, "OBSEND", obsend,
                "True if frame is last in current observation", 0 );
  astSetFitsS ( fitschan, "UTDATE", utdate,
                "UT date as a string in yyyymmdd format", 0 );
  astSetFitsS ( fitschan, "DATE-OBS", dateobs,
                "Date and time (UTC) of start of sub-scan", 0 );
  /* Warning: KLUDGE alert!!! DATE-END needs to be calculated properly */
  astSetFitsS ( fitschan, "DATE-END", dateobs,
                "Date and time (UTC) of end of sub-scan", 0 );
  astSetFitsS ( fitschan, "WVMDATST", dateobs,
                "Fudged date and time to make simulator work with makemap (UTC)", 0 );
  astSetFitsS ( fitschan, "WVMDATEN", dateobs,
                "Fudged date and time to make simulator work with makemap (UTC)", 0 );
  astSetFitsS ( fitschan, "INBEAM", "",
                "Fudged to make simulator work with makemap", 0 );
  astSetFitsS ( fitschan, "SEQ_TYPE", "SCIENCE",
                "Fudged to make simulator work with makemap", 0 );

  astSetFitsF ( fitschan, "DUT1", inx->dut1, "[d] UT1 - UTC correction", 0 );
  astSetFitsS ( fitschan, "INSTAP", inx->instap, "Instrument aperture", 0 );
  astSetFitsF ( fitschan, "INSTAP_X", inx->instap_x,
                "[arcsec] X focal plane offset", 0 );
  astSetFitsF ( fitschan, "INSTAP_Y", inx->instap_y,
                "[arcsec] Y focal plane offset", 0 );
  astSetFitsF ( fitschan, "AMSTART", 1./cos(AST__DPIBY2-elstart),
                "Air mass at start", 0 );
  astSetFitsF ( fitschan, "AMEND", 1./cos(AST__DPIBY2-elend),
                "Air mass at end", 0 );
  astSetFitsF ( fitschan, "AZSTART", AST__DR2D*azstart,
                "[deg] Azimuth at sub-scan start", 0 );
  astSetFitsF ( fitschan, "AZEND", AST__DR2D*azend,
                "[deg] Azimuth at sub-scan end", 0 );
  astSetFitsF ( fitschan, "ELSTART", AST__DR2D*elstart,
                "[deg] Elevation at sub-scan start", 0 );
  astSetFitsF ( fitschan, "ELEND", AST__DR2D*elend,
                "[deg] Elevation at sub-scan end", 0 );
  astSetFitsS ( fitschan, "LSTSTART", lststart, "LST at start of sub-scan", 0 );
  astSetFitsS ( fitschan, "LSTEND", lstend, "LST at end of sub-scan", 0 );

  /* Environment */
  astSetFitsCM ( fitschan, "-- Environment parameters --", 0 );
  astSetFitsF ( fitschan, "ATSTART", sinx->atstart,
                "[deg C] Ambient temperature at start", 0 );
  astSetFitsF ( fitschan, "ATEND", sinx->atend,
                "[deg C] Ambient temperature at end", 0 );
  astSetFitsF ( fitschan, "WVMTAUST", wvmstart, "WVM tau at start", 0 );
  astSetFitsF ( fitschan, "WVMTAUEN", wvmend, "WVM tau at end", 0 );
  astSetFitsF ( fitschan, "SEEINGST", 1.0, "Seeing at start", 0 );
  astSetFitsF ( fitschan, "SEEINGEN", 1.0, "Seeing at end", 0 );

  /* OMP & ORAC-DR */
  astSetFitsCM ( fitschan, "-- OMP & ORAC-DR parameters --", 0 );
  astSetFitsS ( fitschan, "PROJECT", "M08AC00",
                "The proposal ID for the PROJECT", 0 );
  sc2sim_get_recipe( inx, recipe, sizeof(recipe), status );
  astSetFitsS ( fitschan, "RECIPE", recipe, "The ORAC-DR recipe", 0 );
  astSetFitsS ( fitschan, "DRGROUP", "",
                "Name of group to combine current observation with", 0 );
  astSetFitsS ( fitschan, "MSBID", "", "ID of min schedulable block", 0 );
  astSetFitsS ( fitschan, "MSBTID", "", "Translation ID of MSB", 0 );
  astSetFitsS ( fitschan, "SURVEY", "", "Survey Name", 0 );

  /* SCUBA-2 */
  astSetFitsCM ( fitschan, "-- SCUBA-2 specific parameters --", 0 );
  astSetFitsS ( fitschan, "INSTRUME", instrume, "Instrument name - SCUBA-2", 0 );
  astSetFitsS ( fitschan, "SUBARRAY", (sinx->subname)[subindex], "subarray name", 0 );
  astSetFitsF ( fitschan, "SHUTTER", 1, "Shutter for darks: 0=closed, 1=open",
                0 );
  astSetFitsS ( fitschan, "FILTER", filter, "filter used", 0 );
  astSetFitsF ( fitschan, "WAVELEN", inx->lambda, "[m] Wavelength", 0 );

  /* Switching and mapping */
  astSetFitsCM ( fitschan, "-- Mapping parameters --", 0 );
  if ( strncmp( inx->obsmode, "DREAM", 5) == 0 ||
       strncmp( inx->obsmode, "STARE", 5) == 0 ) {
    astSetFitsS ( fitschan, "SAM_MODE", inx->obsmode,
                  "Sample mode: STARE, DREAM or SCAN", 0 );
  } else {
    astSetFitsS ( fitschan, "SAM_MODE", "SCAN",
                  "Sample mode: STARE, DREAM or SCAN", 0 );
  }
  astSetFitsS ( fitschan, "SW_MODE", "NONE",
                "Switch mode: CHOP, PSSW, FREQ, or NONE", 0 );
  astSetFitsS ( fitschan, "OBS_TYPE", inx->obstype,
                "Observation type -  Science, Pointing or Focus", 0 );

  if ( strncmp( inx->obsmode, "DREAM", 5) == 0 ) {
    astSetFitsI ( fitschan, "JIGL_CNT", inx->nvert,
                  "Number of points in DREAM pattern", 0 );
    astSetFitsS ( fitschan, "JIGL_NAM", "",
                  "Name containing DREAM jiggle offsets", 0 );
    astSetFitsF ( fitschan, "JIGL_PA", 0,
                  "Number of points in DREAM pattern", 0 );
    astSetFitsS ( fitschan, "JIGL_CRD", "FPLANE",
                  "Coord frame of jiggle pattern", 0 );
    astSetFitsF ( fitschan, "JIG_SCAL", inx->jig_step_x,
                  "[arcsec] SMU jiggle pattern scale factor", 0 );
    /* Construct weights name from subarray */
    one_strlcpy( weightsname, "dreamweights_", sizeof(weightsname), status );
    one_strlcat( weightsname, (sinx->subname)[subindex], sizeof(weightsname), status );
    one_strlcat( weightsname, ".sdf", sizeof(weightsname), status );
    astSetFitsS ( fitschan, "DRMWGHTS", weightsname,
                  "Name of DREAM weights file", 0 );
  } else {
    astSetFitsI ( fitschan, "JIGL_CNT", 0,
                  "Number of points in DREAM pattern", 0 );
    astSetFitsS ( fitschan, "JIGL_NAM", "",
                  "Name containing DREAM jiggle offsets", 0 );
    astSetFitsF ( fitschan, "JIGL_PA", 0,
                  "Number of points in DREAM pattern", 0 );
    astSetFitsS ( fitschan, "JIGL_CRD", "",
                  "Coord frame of jiggle pattern", 0 );
    astSetFitsF ( fitschan, "JIG_SCAL", 0.0,
                  "[arcsec] SMU jiggle pattern scale factor", 0 );
    astSetFitsS ( fitschan, "DRMWGHTS", "",
                  "Name of DREAM weights file", 0 );
  }
  astSetFitsF ( fitschan, "MAP_HGHT", map_hght, "[arcsec] Map height", 0 );
  astSetFitsF ( fitschan, "MAP_PA", map_pa, "[deg] Map PA", 0 );
  astSetFitsF ( fitschan, "MAP_WDTH", map_wdth, "[arcsec] Map width", 0 );
  astSetFitsS ( fitschan, "LOCL_CRD", loclcrd,
                "Local offset coordinate system", 0 );
  astSetFitsF ( fitschan, "MAP_X", map_x, "[arcsec] Map X offset", 0 );
  astSetFitsF ( fitschan, "MAP_Y", map_y, "[arcsec] Map Y offset", 0 );
  /* Fill in for obsmode = PONG only */
  if ( strncmp( inx->obsmode, "PONG", 4) == 0 ) {
    astSetFitsS ( fitschan, "SCAN_CRD", scancrd, "Scan coordinate system", 0 );
    astSetFitsF ( fitschan, "SCAN_VEL", inx->vmax,
                  "[arcsec/s] Requested scanning rate", 0 );
    astSetFitsF ( fitschan, "SCAN_DY", inx->spacing,
                  "[arcsec] Sample spacing perpendicular to scan", 0 );
    astSetFitsF ( fitschan, "SCAN_PA", inx->scan_angle,
                  "[deg] Scan PA relative to N in SCAN_CRD system", 0 );
    astSetFitsS ( fitschan, "SCAN_PAT", "PONG", "Scanning pattern", 0 );
  } else {
    astSetFitsS ( fitschan, "SCAN_CRD", "", "Scan coordinate system", 0 );
    astSetFitsF ( fitschan, "SCAN_VEL", 0,
                  "[arcsec/s] Requested scanning rate", 0 );
    astSetFitsF ( fitschan, "SCAN_DY", 0,
                  "[arcsec] Sample spacing perpendicular to scan", 0 );
    astSetFitsF ( fitschan, "SCAN_PA", 0,
                  "[deg] Scan PA relative to N in SCAN_CRD system", 0 );
    astSetFitsS ( fitschan, "SCAN_PAT", "", "Scanning pattern", 0 );
  }

  /* Write out BASE position and tracking coordinate system */
  astSetFitsS ( fitschan, "TRACKSYS", head[0].tcs_tr_sys,
                "TCS Tracking coordinate system", 0 );
  astSetFitsF ( fitschan, "BASEC1", (head[0].tcs_tr_bc1)*AST__DR2D,
                "[deg] TCS BASE position (longitude) in TRACKSYS", 0 );
  astSetFitsF ( fitschan, "BASEC2", (head[0].tcs_tr_bc2)*AST__DR2D,
                "[deg] TCS BASE position (latitude) in TRACKSYS", 0 );

  /* JOS parameters */
  astSetFitsCM ( fitschan, "-- JOS parameters --", 0 );
  astSetFitsF ( fitschan, "STEPTIME", inx->steptime,
                "[s] Time interval between samples", 0 );

  /* Integration time */
  astSetFitsCM ( fitschan,
                 "-- Integration time-related parameters --", 0 );
  astSetFitsF ( fitschan, "INT_TIME", totaltime,
                "[s] Time spent integrating on source", 0 );
  /* Only write exp_time for DREAM and STARE*/
  if ( strncmp( inx->obsmode, "DREAM", 5) == 0 ||
       strncmp( inx->obsmode, "STARE", 5) == 0) {
    astSetFitsF ( fitschan, "EXP_TIME", exptime,
                  "[s] Mean integration time per output pixel", 0 );
    astSetFitsI ( fitschan, "N_SUB", nimage,
                  "Number of sub-scans written to file", 0 );
  } else {
    astSetFitsI ( fitschan, "N_SUB", 0,
                  "Number of sub-scans written to file", 0 );
    astSetFitsI ( fitschan, "SEQSTART", (head[0].rts_num),
		  "RTS index number of first frame in image", 0 );
    astSetFitsI ( fitschan, "SEQEND", (head[numsamples - 1].rts_num ),
		  "RTS index number of last frame in image", 0 );
  }

  /* Write out details for FOCUS observation */
  if ( strcmp(inx->obstype, "FOCUS") == 0 ) {
    astSetFitsS ( fitschan, "FOCAXIS", "Z",
                  "Focus axis to move (X, Y, Z)", 0 );

    astSetFitsI ( fitschan, "NFOCSTEP", inx->nfocstep,
                  "Number of focal position steps", 0 );

    astSetFitsF ( fitschan, "FOCPOSN", focposn,
                  "[mm] Absolute position of the SMU", 0 );

    astSetFitsF ( fitschan, "FOCSTEP", inx->focstep,
                  "[mm] Distance between focus steps", 0 );
  }

  /* SMU specific */
  astSetFitsCM ( fitschan, "-- SMU-specific parameters --", 0 );
  astSetFitsF ( fitschan, "ALIGN_DX", 0.0, "SMU tables X axis alignment offset", 0 );
  astSetFitsF ( fitschan, "ALIGN_DY", 0.0, "SMU tables Y axis alignment offset", 0 );
  astSetFitsF ( fitschan, "FOCUS_DZ", 0.0, "SMU tables Z axis focus offset", 0 );
  astSetFitsF ( fitschan, "DAZ", 0.0, "SMU azimuth pointing offset", 0 );
  astSetFitsF ( fitschan, "DEL", 0.0, "SMU elevation pointing offset", 0 );
  astSetFitsF ( fitschan, "UAZ", 0.0, "User azimuth pointing offset", 0 );
  astSetFitsF ( fitschan, "UEL", 0.0, "User elevation pointing offset", 0 );

  /* Misc */
  astSetFitsCM ( fitschan, "-- Miscellaneous --", 0 );
  astSetFitsS ( fitschan, "OCSCFG", "config.xml",
                "Name of OCS Configuration XML file defining the observation", 0 );
  astSetFitsL ( fitschan, "SIMULATE", 1, "True if data produced by simulator", 0 );
  astSetFitsL ( fitschan, "SIM_SMU", 1, "True if SMU data are simulated", 0 );
  astSetFitsL ( fitschan, "SIM_RTS", 1, "True if RTS data are simulated", 0 );
  astSetFitsL ( fitschan, "SIM_TCS", 1, "True if TCS data are simulated", 0 );
  astSetFitsS ( fitschan, "STATUS", "NORMAL",
                "Status at obs. end - NORMAL or ABORT", 0 );

  /* Others... */
  astSetFitsL ( fitschan, "POL_CONN", 0, "True if polarimeter is in the beam", 0 );
  astSetFitsL ( fitschan, "FTS_CONN", 0, "True if FTS is used", 0 );

  astSetFitsF ( fitschan, "MEANWVM", meanwvm,
                "Mean zenith tau at 225 GHz from WVM", 0 );

  astSetFitsF ( fitschan, "BASETEMP", 0.1,
                "[K] Base temperature", 0 );

  astSetFitsS( fitschan, "ARRAYID", "SIMULATED",
               "Unique array identifier", 0 );

  /* Convert the AstFitsChan data to a char array */
  smf_fits_export2DA ( fitschan, &nrec, fitsrec, status );

  /* Calculate the sub array index */
  sc2ast_name2num( (sinx->subname)[subindex], &subnum, status );

  /* There are "issues" handling const for arrays in call to wrtstream
     partly caused by the input struct being const and the jig_vert
     member therefore also being const. Rather than try to work out how
     to fix it properly we just copy the data from the struct to a local
     variable before calling wrtstream */
  for (i=0; i < inx->nvert; i++) {
    for (j=0; j < 2; j++) {
      jigvert[i][j] = inx->jig_vert[i][j];
    }
  }

  /* Telescope parameters */
  telpar.latdeg = (sinx->telpos)[1];
  telpar.longdeg = -(sinx->telpos)[0];
  telpar.dut1 = SPD * inx->dut1;
  telpar.dtai = VAL__BADD;

  /* Store the timestream data */
  sc2store_wrtstream ( file_name, subnum, nrec, fitsrec, inx->colsize,
                       inx->rowsize, numsamples, nflat, refres, 0, flatname, head,
                       &telpar, dbuf, dksquid, fcal, fpar, inx->obsmode,
                       NULL, NULL, jigvert, inx->nvert, jigptr, jigsamples,
                       NULL, status );

  /* Create SCU2RED extension for storing polynomial fits and
     reconstructed images */
  sc2store_creimages ( status );

  /* Number of points in 1 frame - placeholder to remind us that it
     may be different for DREAM */
  if ( strncmp( inx->obsmode, "DREAM", 5) == 0 ) {
    framesize = inx->colsize * inx->rowsize;
  } else {
    framesize = inx->colsize * inx->rowsize;
  }

  /* Now we need to play with flat-fielded data */
  rdata = astCalloc( framesize*numsamples, sizeof(*rdata) );
  if (*status != SAI__OK) goto CLEANUP;

  /* Apply flatfield to timestream */
  for ( j=0; j<framesize*numsamples; j++ ) {
    rdata[j] = (double)dbuf[j];
  }
  sc2math_flatten ( framesize, numsamples, flatname, nflat, fcal, fpar,
                    rdata, status );

  /* For DREAM/STARE data, calculate .In images and write to the
     output file. The default is to average every second. KLUDGE:
     ONLY STARE CURRENTLY SUPPORTED */
  if ( strncmp( inx->obsmode, "STARE", 5) == 0 ) {
    /* Calculate number of samples to average */
    naver = (int)(1./inx->steptime);
    /* And then the number of images to create */
    nsubim = numsamples / naver;

    /* Set jig/chop entries to zero for non-DREAM data */
    state.smu_az_jig_x = 0.0;
    state.smu_az_jig_y = 0.0;
    state.smu_az_chop_x = 0.0;
    state.smu_az_chop_y = 0.0;

    /*smf_calc_telpos( NULL, "JCMT", telpos, status );*/
    instap[0] = DAS2R * inx->instap_x;
    instap[1] = DAS2R * inx->instap_y;

    /* Set coordinate system */
    one_strlcpy( cosys, head[0].tcs_tr_sys, JCMT__SZTCS_TR_SYS+1, status );
    if ( strncmp( cosys, "APP", 3 ) == 0 ) {
      one_strlcpy( cosys, "GAPPT", JCMT__SZTCS_TR_SYS+1, status );
    } else if (strncmp( cosys, "J2000", 5 ) == 0) {
      one_strlcpy( cosys, "ICRS", JCMT__SZTCS_TR_SYS+1, status);
    }

    /* Loop over number of images */
    seqoffset = (nsubscan-1)*nimage/(inx->steptime) + 1;
    for ( k=0; k<nsubim; k++ ) {
      /* Initialize sums to zero */
      for ( i=0; i<framesize; i++ ) {
        coadd[i] = 0.0;
        zero[i] = 0.0;
      }
      /* Begin and end sequence number indices. Note the FITS headers
         SEQSTART/SEQEND are incremented by 1 from these values. */
      seqstart = k*naver;
      seqend = seqstart + naver - 1;

      /* Create average image */
      for ( j=seqstart; j<=seqend; j++ ) {
        /* coadd frame */
        for ( i=0; i<framesize; i++ ){
          coadd[i] += rdata[framesize*j+i];
        }
      }
      /* Average the coadd frame - set bad bolometers to BAD */
      for ( i=0; i<framesize; i++ ) {
        coadd[i] /= (double)naver;
      }

      state.tcs_az_ac1 = head[seqstart].tcs_az_ac1;
      state.tcs_az_ac2 = head[seqstart].tcs_az_ac2;
      state.tcs_tai = head[seqstart].tcs_tai;

      /* Set DATE-OBS string for this image - UTC */
      sc2sim_dateobs( inx->mjdaystart +
                      ((((nsubscan-1)*numsamples) + k*naver)*inx->steptime/SPD),
                      imdateobs, status );

      /* Set dimensions of output image */
      ndim = 2;
      dims[SC2STORE__ROW_INDEX] = inx->colsize;
      dims[SC2STORE__COL_INDEX] = inx->rowsize;

      /* Construct WCS FrameSet */
      sc2ast_createwcs( subnum, &state, instap, sinx->telpos, NO_FTS, &wcs, status );

      /* Set the SkyRef to the current AzEl position and then the
         coordinate system. Allow AST to work out the SkyRef in the
         new system rather than worrying about it ourselves. */
      astSetD( wcs, "SkyRef(1)", head[seqstart].tcs_az_ac1 );
      astSetD( wcs, "SkyRef(2)", head[seqstart].tcs_az_ac2 );

      /* Set the output coordinate system */
      sc2ast_set_output_system( head[seqstart].tcs_tr_sys, wcs, status );

      /* Set seqstart/end for FITS header */
      seqstart += seqoffset;
      seqend += seqoffset;

      /* First time round the loop the SEQSTART/END headers won't
         exist so just write them. Plus there is no need to change
         the DATE-OBS/END for the first sub-image */
      if ( k == 0 ) {
        astSetFitsI( fitschan, "SEQSTART", seqstart,
                     "RTS index number of first frame in image", 0);
        astSetFitsI( fitschan, "SEQEND", seqend,
                     "RTS index number of last frame in image", 0);
      } else {
        /* On subsequent passes find and delete old entries before
           adding new ones - check that fitsfind is not 0 before
           proceeding. First `rewind' to beginning of FitsChan. */
        astClear( fitschan, "Card");
        fitsfind = astFindFits( fitschan, "DATE-OBS", NULL, 0);
        if ( fitsfind ) {
          astDelFits( fitschan );
          astSetFitsS ( fitschan, "DATE-OBS", imdateobs,
                        "Date and time (UTC) of start of sub-scan", 0 );
        }
        fitsfind = astFindFits( fitschan, "DATE-END", NULL, 0);
        if ( fitsfind ) {
          astDelFits( fitschan );
          astSetFitsS ( fitschan, "DATE-END", imdateobs,
                        "Date and time (UTC) of end of sub-scan", 0 );
        }
        fitsfind = astFindFits( fitschan, "SEQSTART", NULL, 0);
        if ( fitsfind ) {
          astDelFits( fitschan );
          astSetFitsI( fitschan, "SEQSTART", seqstart,
                       "RTS index number of first frame in image", 0);
        }
        fitsfind = astFindFits( fitschan, "SEQEND", NULL, 0);
        if ( fitsfind ) {
          astDelFits( fitschan );
          astSetFitsI( fitschan, "SEQEND", seqend,
                       "RTS index number of last frame in image", 0);
        }
      }

      /* Convert the AstFitsChan data to a char array */
      smf_fits_export2DA ( fitschan, &nrec, fitsrec, status );

      /* Store image */
      smf_get_taskname( NULL, prvname, status );
      sc2store_putimage ( k, wcs, ndim, dims, inx->colsize,
                          inx->rowsize, coadd, zero, obsidss, prvname, fitsrec, nrec, status );
    }
  }

  /* Calculate polynomial fits and write out SCANFIT extension */
  poly = astMalloc( (framesize*ncoeff)*sizeof( *poly ) );
  sc2math_fitsky ( 0, framesize, numsamples, ncoeff, rdata, poly, status );
  sc2store_putscanfit ( inx->colsize, inx->rowsize, ncoeff, poly, status );

 CLEANUP:
  /* Free memory allocated for pointers */
  if (poly) poly = astFree( poly );
  if (rdata) rdata = astFree( rdata );

  /* Close the file */
  sc2store_free ( status );
}
