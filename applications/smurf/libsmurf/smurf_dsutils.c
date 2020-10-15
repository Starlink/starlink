/*
*+
*  Name:
*     DSUTILS

*  Purpose:
*     A collection of utilities for estimating focal plane distortion.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_dsutils( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine provides various functions needed by the scripts that
*     determine a 2D polynomial describing the focal plane disortion
*     produced by SCUBA-2 optics.

*  ADAM Parameters:
*     BMAP = FILENAME (Read)
*          Only accessed if a time series cube is specified via parameter
*          "IN". It gives the name of a text file in which to place an
*          AST dump of a Mapping from a modified PIXEL Frame to the SKY
*          offset frame for a time slice close to the middle of the data set
*          The PIXEL frame is modified in the sense that its origin (i.e.
*          pixel coords (0,0,0.0) ) is shifted so that it co-incides with
*          the sky reference point. No file is created if a null (!)
*          value is supplied. [!]
*     BORDER = _INTEGER (Read)
*          Only accessed if a time series cube is specified via parameter
*          "IN", and a null(!) value sispecified for parameter BMAP. It
*          gives the border width in pixels. Time slices with peak positions
*          closer to any edge than this amount will not be included in the
*          output catalogue. [4]
*     COLNAME = LITERAL (Read)
*          Only accessed if a value is supplied for "INCAT". If supplied,
*          COLNAME should be the name of a column in the INCAT catalogue.
*          An output NDF holding these values will be created (see
*          parameter COLNDF). [!]
*     COLNDF = NDF (Write)
*          Only accessed if a value is supplied for "COLNAME". If supplied,
*          an NDF is created holding the values from the catalogue column
*          specified by COLNAME. The column value from each row in the INCAT
*          catalgue is pasted into the output NDF at a position specified
*          by the BD1/BF2 columns.
*     FORWARD = _LOGICAL (Read)
*          Only accessed if input NDFs are specified for parameter INFITX
*          and INFITY, and if a non-null (!) value is supplied for parameter
*          OUTCODE. It indices whether the code written to the OUTCODE
*          file should describe the forward or inverse PolyMap transformation.
*     FPIXSIZE = _DOUBLE (Read)
*          Only acccessed if a non-null value is supplied for parameter
*          OUTMAG. It gives the pixel size (in mm), of the NDFs created
*          via parameters OUTANG and OUTMAG. [1.0]
*     FXHI = _DOUBLE (Read)
*          Only acccessed if a non-null value is supplied for parameter
*          OUTMAG. It gives the upper bound (in mm) on the focal plane X
*          axis, of the NDFs created via parameters OUTANG and OUTMAG. [50]
*     FXLO = _DOUBLE (Read)
*          Only acccessed if a non-null value is supplied for parameter
*          OUTMAG. It gives the lower bound (in mm) on the focal plane X
*          axis, of the NDFs created via parameters OUTANG and OUTMAG. [-50]
*     FXOFF = _DOUBLE (Read)
*          Only acccessed if a non-null value is supplied for parameter
*          OUTMAG. It gives the focal plane X coord for a point that
*          defines a global offset to be removed from OUTMAG and OUTANG.
*          The point specified by FXOFF and FYOFF will have value zero
*          in OUTANG. No offset is removed if a null (!) value is supplied. [!]
*     FYHI = _DOUBLE (Read)
*          Only acccessed if a non-null value is supplied for parameter
*          OUTMAG. It gives the upper bound (in mm) on the focal plane Y
*          axis, of the NDFs created via parameters OUTANG and OUTMAG. [50]
*     FYLO = _DOUBLE (Read)
*          Only acccessed if a non-null value is supplied for parameter
*          OUTMAG. It gives the lower bound (in mm) on the focal plane Y
*          axis, of the NDFs created via parameters OUTANG and OUTMAG. [-50]
*     FYOFF = _DOUBLE (Read)
*          Only acccessed if a non-null value is supplied for parameter
*          OUTMAG. It gives the focal plane Y coord for a point that
*          defines a global offset to be removed from OUTMAG and OUTANG.
*          The point specified by FXOFF and FYOFF will have value zero
*          in OUTANG. No offset is removed if a null (!) value is supplied. [!]
*     IN = NDF (Read)
*          Only accessed if null (!) values are supplied for parameter INFITX,
*          INFITY, and INCAT. It should be a time series cube. If a non-null
*          value is supplied, then the BMAP parameter can be used to get the
*          WCS Mapping for a typical time slice, or the OUTCAT parameter can be
*          used to create a catalogue holding the expected source position (in
*          GRID coords) in every time slice. A single time slice from this cube
*          can also be written to an output NDF (see parameter OUTSLICE). A
*          list of time slices in which the reference point is close to a
*          specified bolometer can also be produced (see parameter XBOL).
*     INFITX = NDF (Read)
*          A 2D NDF holding fitted focal plane X offsets (in mm) at every
*          bolometer, or null (!). This NDF shoudl have been created by
*          KAPPA:FITSURFACE, and should hold the coefficients of the fit
*          in the SURFACEFIT extension. If NDFs are supplied for both INFITX
*          and INFITY, then the OUTCODE parameter can be used to create C
*          source code describing the coefficients in a form usable by
*          the AST PolyMap constructor. In addition, the OUTDX and OUTDY
*          parameters can be used to create output NDFs containing the
*          values for the inverse quantities.
*     INFITY = NDF (Read)
*          A 2D NDF holding fitted focal plane Y offsets (in mm) at every
*          bolometer, or null (!). See INFITX.
*     INCAT = FILENAME (Read)
*          Only access if a null (!) value is supplied for INFITX or INFITY.
*          It is a text file holding a catalogue of corrected and uncorrected
*          pixel positions for every bolometer in the subarray specified by
*          SUBARRAY. These are used to create output NDFs holding the X and
*          Y focal plane offset (in mm) at every bolometer in the subarray
*          (see OUTDX and OUTDY). An output catalogue can also be
*          produced holding extra columns, and from which abberant rows
*          have been rejected (see parameter OUTCAT).
*     ITIME = _INTEGER (Read)
*          The integer index of a time slice to be dumped to an NDF (see
*          OUTSLICE). If supplied, the application terminates without
*          further action once the NDF has been created. [!]
*     LOWFACTOR = _REAL (Read)
*          Only accessed if a value is supplied for parameter IN. It gives
*          the lowest time slice data sum (as a fraction of the largest
*          time slice data sum in the supplied timne series cube) for
*          usable time slices. Any time slices that have total data sums
*          less than this value are skipped.
*     NITER = _INTEGER (Read)
*          Only accessed if a value is supplied for parameter INCAT. It gives
*          the number of sigma-clipping iterations to be performed whilst
*          creating the output NDFs. [3]
*     OUTCAT = FILENAME (Write)
*          If a value was supplied for INCAT, then OUTCAT is the name of
*          an output catalogue to create, containing a copy of the input
*          catalogue form which abberant rows have been removed, and
*          contaiing some extra informative columns (e.g. offsets in
*          focal plane and pixel coordinates). No catalogue is created if
*          a null (!) value is supplied. If a value is supplied for IN,
*          then OUTCAT will hold the expected source position in each time
*          slice.
*     OUTCODE = FILENAME (Write)
*          If a value was supplied for INFITX and INFITY, then OUTCODE is
*          the name of an output text file in which to store the C code
*          describing the coefficients of the forward or inverse distortion
*          polynomial.
*     OUTDX = NDF (Write)
*          If a value was supplied for INFITX and INFITY, then OUTDX
*          gives the name of the NDF in which to store the inverse X axis
*          corrections at each bolometer in the subarray (in mm). If a
*          value was supplied for INCAT, then OUTDX is the name of an NDF
*          to recieve the forward X axis correctiosn at every bolometer
*          in the subarray (in mm).
*     OUTDY = NDF (Write)
*          If a value was supplied for INFITX and INFITY, then OUTDX
*          gives the name of the NDF in which to store the inverse Y axis
*          corrections at each bolometer in the subarray (in mm). If a
*          value was supplied for INCAT, then OUTDX is the name of an NDF
*          to recieve the forward Y axis correction at every bolometer
*          in the subarray (in mm).
*     OUTANG = NDF (Write)
*          Only acccessed if a non-null value is supplied for parameter
*          OUTMAG. OUTANG specifies the output NDF to receive the
*          orientation of the distortion (in degrees anti-clockwise from
*          the positive Y axis) at each point in the focal plane.
*     OUTFX = NDF (Write)
*          The name of an NDF to recieve the focal plane X value (in
*          arc-sec) at each bolometer in the subarray specified by SUBARRAY.
*          Only produced if a non-null value is also supplied for OUTFY. [!]
*     OUTFY = NDF (Write)
*          The name of an NDF to recieve the focal plane Y value (in
*          arc-sec) at each bolometer in the subarray specified by SUBARRAY.
*          Only produced if a non-null value is also supplied for OUTFX. [!]
*     OUTMAG = NDF (Write)
*          An output NDF to receive the magnitude of the distortion (in mm)
*          at each point in the focal plane. If a null (!) value is supplied,
*          no NDF will be created. The NDFs specified by OUTMAG and OUTANG
*          can be displayed as a vector plot using KAPPA:VECPLOT. In addition,
*          the outline of any sub-array can be over-plotted by changing
*          the current coordinate Frame and then using KAPPA:ARDPLOT (for
*          instance "wcsframe outmag s8a" followed by "ardplot s8a").
*          Note, for some distortions (e.g. NEW4) the distortion at 450
*          and 850 are different. The waveband to use is determined by
*          the value supplied for the SUBARRAY parameter. [!]
*     OUTSLICE = NDF (Write)
*          If a value was supplied for IN and ITIME, then OUTSLICE gives the
*          name of  the NDF in which to store the bolometer data for the
*          given time slice, including celestial WCS.
*     SUBARRAY = LITERAL (Write)
*          The name of the subarray being processed: one of "s8a", "s8b",
*          "s8b", "s8d", "s4a", "s4b", "s4b", "s4d". If OUTMAG is not
*          null, then the value supplied for SUBARRAY determines the waveband
*          for which the distortion is returned.
*     XBOL = _INTEGER (Read)
*          The index of a test bolometer on the first GRID axis within
*          the sub-array containing the bolometer. If values are supplied
*          for all of IN, XBOL, YBOL and RADIUS, then a list of time slice
*          indices are displayed. These are the indices of the time
*          slice in which the reference point is close to the bolometer
*          specified by (XBOL,YBOL). The RADIUS parameter spcified the
*          distance limit. [!]
*     YBOL = _INTEGER (Read)
*          The index of a test bolometer on the second GRID axis within
*          the sub-array containing the bolometer. See XBOL. [!]
*     RADIUS = _REAL (Given)
*          The radius of a test circle, in bolometers. See XBOL. [!]

*  Related Applications:
*     SMURF: DISTORTION, SHOWDISTORTION

*  Authors:
*     David Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     2-DEC-2009 (DSB):
*        Initial version.
*     3-DEC-2009 (DSB):
*        Modified to ensure that the average of the X and Y linear
*        coefficients in the final polymap have a mean value of 1.0.
*        This requires MM2RAD to be changed in sc2ast.c - include the
*        required scaling factor in the forward transformation code
*        written to paremeter OUTCODE.
*     2010-01-18 (TIMJ):
*        Prologue tweaks for hawaiki release
*     18-FEB-2010 (DSB):
*        Added parameters ITIME and OUTSLICE.
*     5-APR-2011 (DSB):
*        Use SUBARRAY value to determine waveband of distortion to return in
*        OUTMAG.
*     20-SEP-2011 (DSB):
*        OUTFX/FY NDFs now have pixel index bounds (0:31,0:39) rather than
*        (1:32,1:40).

*  Copyright:
*     Copyright (C) 2009-2011 Science and Technology Facilities Council.
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

#include <stdio.h>
#include <stdint.h>
#include <ctype.h>

/* STARLINK includes */
#include "par.h"
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/grp.h"
#include "star/hds.h"
#include "star/atl.h"
#include "star/kaplibs.h"
#include "prm_par.h"
#include "star/one.h"

/* SMURF includes */
#include "smurf_typ.h"
#include "smurflib.h"
#include "libsmf/smf.h"


/* Local constants */
#define FUNC_NAME "smurf_dsutils"
#define EXPAND 50
#define MARGIN 500
#define MAXPATH 255
#define MAXCOLNAME 15
#define BUFSIZE 255
#define HEADER "# SCUBA-2 focal plane distortion data"
#define BC1 0
#define BC2 1
#define DBF1 2
#define DBF2 3
#define F1 4
#define F2 5
#define AMP 6
#define DAMP 7
#define RMS 8
#define EXTRA 9
#define IZ 10

#define PIX2MM 1.135                   /* Focal plane pixel interval in mm */
#define PIBY2 1.57079632679
#define NSIGMA 3.0
#define NITER 3
#define DBF_MAX 3.0

/* True if the data array is configured (COL, ROW) [the new way]
   Undefined or false if (ROW,COL) order.
   Should match the definition of ROW and COL index from sc2store.c
   Remove COLROW once we have finished testing.
*/
#define COLROW 1

/* Type definitions */
typedef struct SliceInfo {
   double sum;    /* Total data sum in time slice */
   dim_t ipx;     /* Pixel X index at peak data value */
   dim_t ipy;     /* Pixel Y index at peak data value */
   double bcx;    /* Pixel X coord at telescope base position */
   double bcy;    /* Pixel Y coord at telescope base position */
} SliceInfo;

static AstFrameSet *GetFPFrameSet( const char *name, int *status );
static void PasteNDF( const char *subarray, dim_t *lb, dim_t *ub, int nvals,
                      double *xvals,
                      double *yvals, double *datvals, double *varvals,
                      AstMapping *map, AstFrameSet *wcs, int niter,
                      int genvar, double wlim, const char *param, int *status );
static void Filter( dim_t n, double *flags, double *vals, double nsigma,
                    const char *title, int * status );
static void Filter2( dim_t n, double *flags, double *vals, double nsigma,
                     const char *title, int * status );
static int SaveBoloMapping( const char *param, smfData *data, int *status );
static int SaveTimeSlice( const char *param1, const char *param2, smfData *data, int *status );
static AstKeyMap *GetHeader( FILE *fp, int *status );
static int GetColIndex( AstKeyMap *header, const char *colname, int *status );
static void AddColName( AstKeyMap *header, const char *colname, const char *desc,
                        int *status );
static void PutHeader( FILE *fp, AstKeyMap *header, int *status );
static AstPolyMap *FindPolyMap( AstMapping *map, int *status );
static double *SmoothZ( dim_t zbox, dim_t nrow, double *in_vals, double *iz_vals, int *status );
static double Mode( dim_t n, double *vals, double *dev, int * status );
static int FindSlices( const char *pxbol, const char *pybol, const char *pradius, smfData *data, int *status );
static int ShowSlices( const char *param, smfData *data, int *status );


#define __USE_GNU 1
#include <fenv.h>
#include <math.h>

void smurf_dsutils( int *status ) {

/* Local Variables */
   AstBox *box;
   AstFrame *fp_frm;
   AstFrame *frm;
   AstFrameSet *fp_fset;     /* GRID->focal plane FrameSet */
   AstFrameSet *swcsin;      /* WCS FrameSet for current time slice */
   AstFrameSet *wcs;         /* WCS FrameSet for output NDF */
   AstKeyMap *header;        /* Holds header info from input catalogue */
   AstMapping *fp_map;       /* GRID->focal plane (in millimetres - no PolyMap) */
   AstMapping *map = NULL;   /* Mapping that moves the centroid to the origin */
   AstMapping *map2 = NULL;  /* Mapping that moves the centroid to the origin */
   AstPolyMap *polymap;      /* Polynomial distortion correction mapping */
   AstWinMap *winmap;        /* Expanded GRID to full size GRID mapping */
   FILE *fp1 = NULL;         /* File pointer for input catalogue */
   FILE *fp2 = NULL;         /* File pointer for output catalogue */
   Grp *igrp = NULL;         /* Group of input files */
   HDSLoc *floc[ 2 ] = { NULL, NULL };
   HDSLoc *xloc1 = NULL;
   HDSLoc *xloc2 = NULL;
   SliceInfo *slice = NULL;  /* Point to structure describing current time slice */
   SliceInfo *slices = NULL; /* Array of structures describing each time slice */
   char **words;             /* Pointer to array of word pointers */
   char buf[ BUFSIZE ];      /* Line read from input catalogue */
   char colname[ MAXCOLNAME ];
   char incat[ MAXPATH ];    /* Name of input catalogue */
   char outcat[ MAXPATH ];   /* Name of output catalogue */
   char outcode[ MAXPATH ];  /* Name of output file to hold C source code */
   char subarray[ 9 ];       /* Name of subarray */
   const char *skyrefis;     /* Original value of SkyRefIs in sky frame */
   dim_t border;             /* Width of excluded border, in pixels */
   dim_t gxoff;
   dim_t gyoff;
   dim_t irow;               /* Row index */
   dim_t itime;              /* Grid index on the third pixel axis */
   dim_t ix;                 /* Grid index on the first pixel axis */
   dim_t iy;                 /* Grid index on the second pixel axis */
   dim_t iz;
   dim_t lbnd[ 2 ];          /* Pixel index lower bounds in input array */
   dim_t lbnd_out[ 2 ];      /* Pixel index lower bounds in output NDF */
   dim_t nrow;               /* Number of usable rows */
   dim_t ntime;              /* Length of the third pixel axis */
   dim_t nx;                 /* Length of the first pixel axis */
   dim_t ny;                 /* Length of the second pixel axis */
   dim_t peakx;              /* Index on the first pixel axis at peak */
   dim_t peaky;              /* Index on the second pixel axis at peak */
   dim_t ubnd[ 2 ];          /* Pixel index upper bounds in input NDF */
   dim_t ubnd_out[ 2 ];      /* Pixel index upper bounds in output NDF */
   dim_t zbox;
   double *amp_vals = NULL;
   double *bc1_vals = NULL;  /* Pointer to BC1 column values */
   double *bc2_vals = NULL;  /* Pointer to BC2 column values */
   double *cofs;
   double *damp_vals = NULL;
   double *data1;
   double *data2;
   double *dbf1_vals = NULL; /* Pointer to DBF1 column values */
   double *dbf2_vals = NULL; /* Pointer to DBF2 column values */
   double *dfx_vals = NULL;  /* Pointer to focal plane X offset values */
   double *dfy_vals = NULL;  /* Pointer to focal plane Y offset values */
   double *dpx_vals = NULL;  /* Pointer to pixel X offset values */
   double *dpy_vals = NULL;  /* Pointer to pixel Y offset values */
   double *extra_vals = NULL;
   double *f1_vals = NULL;   /* Pointer to F1 column values */
   double *f2_vals = NULL;   /* Pointer to F2 column values */
   double *fx_vals = NULL;   /* Pointer to focal plane X values */
   double *fy_vals = NULL;   /* Pointer to focal plane Y values */
   double *iz_vals = NULL;
   double *p1x;
   double *p1y;
   double *p2x;
   double *p2y;
   double *p3;
   double *p4;
   double *p;                /* Pointer to array in which to put column value */
   double *pd0;              /* Ptr to 1st data value in current i/p time slice */
   double *pd;               /* Ptr to data array */
   double *pv0;              /* Ptr to 1st var value in current i/p time slice */
   double *rms_vals = NULL;
   double *work = NULL;      /* Pointer to astRebinSeq work array */
   double *worka;            /* Work array */
   double *workb;            /* Work array */
   double *xlin = NULL;
   double *ylin = NULL;
   double alpha;
   double bcx;               /* Pixel X coord at base position */
   double bcy;               /* Pixel Y coord at base position */
   double dx0;
   double dx;
   double dxhi;
   double dxlo;
   double dy0;
   double dy;
   double dyhi;
   double dylo;
   double f1_hi;             /* High bound on feature pixel X coord */
   double f1_lo;             /* Low bound on feature pixel X coord */
   double f2_hi;             /* High bound on feature pixel Y coord */
   double f2_lo;             /* Low bound on feature pixel Y coord */
   double fpixsize;
   double fxhi;
   double fxlo;
   double fxoff;
   double fyhi;
   double fylo;
   double fyoff;
   double gx;                /* Value for 1st axis in GRID frame */
   double gy;                /* Value for 2ns axis in GRID frame */
   double ina[ 2 ];
   double inb[ 2 ];
   double lowsum;            /* Lwest data sum in any usable time slice */
   double maxsum;            /* Largest data sum in any time slice */
   double outa[ 2 ];
   double outb[ 2 ];
   double ox;                /* Value for 1st axis in offset sky frame */
   double oy;                /* Value for 2nd axis in offset sky frame */
   double peakv;             /* Largest pixel value found so far */
   double pnt1[ 2 ];
   double pnt2[ 2 ];
   double shift[ 2 ];        /* Axis shifts */
   double sum;               /* Sum of all good data values in time slice */
   double sx2;
   double sx;
   double sy2;
   double sy;
   double xin[ 2 ];          /* Pixel coord X positions */
   double xout[ 2 ];         /* Focal plane X positions (mm) */
   double yin[ 2 ];          /* Pixel coord Y positions */
   double yout[ 2 ];         /* Focal plane Y positions (mm) */
   float lowfactor;          /* Factor giving lowest usable total data sum */
   float val;                /* Column value */
   int col[ 20 ];            /* Indices of required input catalogue columns */
   int forward;
   int genvar;
   int i;
   int icurr;                /* Index of current Frame in FrameSet */
   int indf1;                /* Identifier for NDF */
   int indf2;                /* Identifier for NDF */
   int ival;                 /* Temporary integer value storage */
   int l;                    /* Input line number */
   int ncoeff_f, nin;
   int niter;                /* Number of sigma-clips to perform */
   int ns;
   int nword;                /* Number of words in line of text */
   int ok;                   /* Was the row good */
   sc2ast_subarray_t subnum;
   size_t j;                 /* Loop count */
   size_t nel;
   size_t nxy;               /* Number of elements in a time slice */
   size_t size;              /* Number of files in input group */
   smfData *data = NULL;     /* Pointer to data struct for input file */
   smfFile *file = NULL;     /* Pointer to file struct for input file */
   smfHead *hdr = NULL;      /* Pointer to data header for time slice */

   const char *sa_name[] = { "s8a", "s8b", "s8c", "s8d",
                            "s4a", "s4b", "s4c", "s4d" };

/* Uncomment to stop the program on any of the following exceptions. */
/*   feenableexcept(FE_DIVBYZERO | FE_INVALID | FE_OVERFLOW ); */

/* Check inherited status */
   if (*status != SAI__OK) return;

/* Start AST and NDF contexts. */
   astBegin;
   ndfBegin();

/* First, see if output NDFs holding the vector shifts implied by the polymap
   currently selected via environment variable SMURF_DISTORTION should be
   created. This is the case if a value is supplied for OUTMAG.
   --------------------------------------------------------------------- */

/* Begin a new AST context. */
   astBegin;

/* First get the focal plane bounds of the required output NDF (in mm). */
   parGet0d( "FXLO", &fxlo, status );
   parGet0d( "FXHI", &fxhi, status );
   parGet0d( "FYLO", &fylo, status );
   parGet0d( "FYHI", &fyhi, status );

/* Get the pixel size, in mm. NDFs. */
   parGet0d( "FPIXSIZE", &fpixsize, status );

/* Determine the bounds of the output NDFs, placing pixel coords
   (0.0,0.0) at the origin of the focal plane coord system. */
   lbnd[ 0 ] = floor( fxlo/fpixsize );
   ubnd[ 0 ] = ceil( fxhi/fpixsize );
   lbnd[ 1 ] = floor( fylo/fpixsize );
   ubnd[ 1 ] = ceil( fyhi/fpixsize );

/* Create the output vector magnitude NDF. */
   ndfCreat( "OUTMAG", "_DOUBLE", 2, lbnd, ubnd, &indf1, status );

/* If a null value was supplied, annul the error and skip. */
   if( *status == PAR__NULL ) {
      errAnnul( status );

/* Otherwise, create the output vector angle NDF. */
   } else {
      ndfCreat( "OUTANG", "_DOUBLE", 2, lbnd, ubnd, &indf2, status );

/* Get a Mapping from GRID coords to focal plane coords, in mm. */
      ina[ 0 ] = 0.0;
      ina[ 1 ] = 0.0;
      inb[ 0 ] = 1.0;
      inb[ 1 ] = 1.0;
      outa[ 0 ] = fpixsize*( lbnd[ 0 ] - 1.5 );
      outa[ 1 ] = fpixsize*( lbnd[ 1 ] - 1.5 );
      outb[ 0 ] = outa[ 0 ] + fpixsize;
      outb[ 1 ] = outa[ 1 ] + fpixsize;
      fp_map = (AstMapping *) astWinMap( 2, ina, inb, outa, outb, " " );

/* Create a WCS FrameSet. The current Frame (index 2 in the FrameSet) is
   focal plane (X,Y) in mm. The origin of focal plane (X,Y) coincides with
   the origin of pixel coordinates. */
      wcs = astFrameSet( astFrame( 2, "Domain=GRID" ), " " );
      fp_frm = astFrame( 2, "Domain=FPLANE,label(1)=FplaneX,"
                         "label(2)=FplaneY,Unit(1)=mm,Unit(2)=mm" );
      astAddFrame( wcs, AST__BASE, fp_map, fp_frm );

/* We now need the PolyMap  specified by the current setting of
   environment variable SMURF_DISTORTION. Get the whole GRID-> FP (in
   rads) Mapping for the first sub array, and extract the PolyMap from it. */
      parGet0c( "SUBARRAY", subarray, 8, status );
      sc2ast_name2num( subarray, &subnum, status );
      sc2ast_createwcs( subnum, NULL, NULL, NULL, NO_FTS, &fp_fset, status );
      polymap = FindPolyMap( astGetMapping( fp_fset, AST__BASE, AST__CURRENT ),
                             status );

/* If no PolyMap was found, use a UnitMap instead. */
      if( !polymap ) polymap = (AstPolyMap *) astUnitMap( 2, " " );

/* Get the focal plane FrameSet for subarray s8a, and extract the FP->GRID
   Mapping. */
      fp_fset = GetFPFrameSet( "s8a", status );
      map = astGetMapping( fp_fset, AST__CURRENT, AST__BASE );

/* Create a Region defining the bounds of the s8a aray in grid coords. */
      frm = astFrame( 2, "Unit(1)=pixel,Unit(2)=pixel" );
      astSet( frm, "Title=Grid coords in sub-array s8a,Domain=S8A" );
      pnt1[ 0 ] = 0.5;
      pnt1[ 1 ] = 0.5;
      pnt2[ 0 ] = 32.5;
      pnt2[ 1 ] = 40.5;
      box = astBox( frm, 1, pnt1, pnt2, NULL, " " );

/* Add it into the WCS FrameSet. */
      astAddFrame( wcs, 2, map, box );

/* Do the same for each of the other subarrays. */
      for( i = 1; i < 8; i++ ) {
         fp_fset = GetFPFrameSet( sa_name[ i ], status );
         map = astGetMapping( fp_fset, AST__CURRENT, AST__BASE );
         astSet( frm, "Title=Grid coords in sub-array %s,Domain=%s",
                 sa_name[ i ], sa_name[ i ] );
         box = astBox( frm, 1, pnt1, pnt2, NULL, " " );
         astAddFrame( wcs, 2, map, box );
      }

/* Re-instate the original current frame, and store in the NDFs. */
      astSetI( wcs, "Current", 2 );
      ndfPtwcs( wcs, indf1, status );
      ndfPtwcs( wcs, indf2, status );

/* Map the DATA component of the output NDFs. */
      ndfMap( indf1, "Data", "_DOUBLE", "Write", (void *) &data1,
              &nel, status );
      ndfMap( indf2, "Data", "_DOUBLE", "Write", (void *) &data2,
              &nel, status );

/* Create a pair of work arrays. */
      worka = astMalloc( 2*sizeof( double )*nel );
      workb = astMalloc( 2*sizeof( double )*nel );

/* Fill the first work array with the focal plane (X,Y) values at the centre
   of every pixel. The X values are stored first in the work array, followed
   by the Y values. */
      lbnd_out[ 0 ] = 1;
      lbnd_out[ 1 ] = 1;
      ubnd_out[ 0 ] = ( ubnd[ 0 ] - lbnd[ 0 ] + 1 );
      ubnd_out[ 1 ] = ( ubnd[ 1 ] - lbnd[ 1 ] + 1 );
      astTranGrid8( fp_map, 2, lbnd_out, ubnd_out, 0.0, 0, 1, 2,
                    nel, worka );

/* Transform these focal plane (X,Y) values into corrected focal plane
   (X,Y) values using the polymap, storeing the results in the second work
   array. */
      astTran28( polymap, nel, worka, worka + nel, 1, workb, workb + nel );

/* Get the focal plane coords for the point that defines the global offset to be
   removed (if any). */
      dx0 = 0.0;
      dy0 = 0.0;
      if( *status == SAI__OK ) {
         parGet0d( "FXOFF", &fxoff, status );
         parGet0d( "FYOFF", &fyoff, status );

         if( *status == PAR__NULL ) {
            errAnnul( status );

         } else {

/* Convert to grid coords. */
            astTran28( wcs, 1, &fxoff, &fyoff, 0, &fxoff, &fyoff );

/* Convert to GRID indices. */
            if( fxoff > 0.0 ) {
               gxoff = (int) ( fxoff + 0.5 );
            } else {
               gxoff = (int) ( fxoff - 0.5 );
            }
            if( fyoff > 0.0 ) {
               gyoff = (int) ( fyoff + 0.5 );
            } else {
               gyoff = (int) ( fyoff - 0.5 );
            }

/* Convert to a vector offset. */
            iz = ( gxoff - 1 ) + ( gyoff - 1 )*( ubnd[ 0 ] - lbnd[ 0 ] + 1 );

/* Store the x and y offsets to remove. */
            dx0 = workb[ iz ] - worka[ iz ];
            dy0 = workb[ iz + nel ] - worka[ iz + nel ];

            msgSetr( "DX", dx0 );
            msgSetr( "DY", dy0 );
            msgOut( " ", "Removing vector offset (^DX,^DY) [mm]", status );

         }
      }

/* For each point, get the offset from uncorrected to corrected focal
   plane (X,Y), convert into a vector magnitude and angle and store in
   the two output NDFs. */
      if( *status == SAI__OK ) {
         p1x = worka;
         p1y = p1x + nel;
         p2x = workb;
         p2y = p2x + nel;
         p3 = data1;
         p4 = data2;
         for( j = 0; j < nel; j++ ) {
            dx = *(p2x++) - *(p1x++) - dx0;
            dy = *(p2y++) - *(p1y++) - dy0;
            *(p3++) = sqrt( dx*dx + dy*dy );
            *(p4++) = atan2( dy, dx )*AST__DR2D - 90.0;
         }
      }

/* Store units and labels in the output NDFs. */
      ndfCput( "Correction magnitude", indf1, "Label", status );
      ndfCput( "mm", indf1, "Unit", status );
      ndfCput( "Correction position angle", indf2, "Label", status );
      ndfCput( "Degrees", indf2, "Unit", status );

/* Free resources. */
      worka = astFree( worka );
      workb = astFree( workb );

/* Close the NDFs. */
      ndfAnnul( &indf1, status );
      ndfAnnul( &indf2, status );
   }

/* Get the NDFs to hold the focal plane X and Y values at every bolometer. */
   lbnd_out[ 0 ] = 0;
   lbnd_out[ 1 ] = 0;
   ubnd_out[ 0 ] = 31;
   ubnd_out[ 1 ] = 39;
   ndfCreat( "OUTFX", "_DOUBLE", 2, lbnd_out, ubnd_out, &indf1, status );
   ndfCreat( "OUTFY", "_DOUBLE", 2, lbnd_out, ubnd_out, &indf2, status );

   if( *status == PAR__NULL ) {
      errAnnul( status );

   } else {

/* Create the GRID->FP FrameSet for the required subarray. */
      parGet0c( "SUBARRAY", subarray, 8, status );
      sc2ast_name2num( subarray, &subnum, status );
      sc2ast_createwcs( subnum, NULL, NULL, NULL, NO_FTS, &fp_fset, status );

/* Calculate the focal plane positions at every bolometer. */
      nxy = ( ubnd_out[ 0 ] - lbnd_out[ 0 ] + 1 );
      nxy *= ( ubnd_out[ 1 ] - lbnd_out[ 1 ] + 1 );
      worka = astMalloc( 2*nxy*sizeof( double ) ) ;
      map = astGetMapping( fp_fset, AST__BASE, AST__CURRENT );
      astTranGrid8( map, 2, lbnd_out, ubnd_out, 0.0, 0, 1, 2, nxy, worka );

/* Store the subarray. */
      ndfXnew( indf1, "DSUTILS", "DSUTILS", 0, NULL, &xloc1, status );
      ndfXpt0c( subarray, indf1, "DSUTILS", "SUBARRAY", status );
      datAnnul( &xloc1, status );

      ndfXnew( indf2, "DSUTILS", "DSUTILS", 0, NULL, &xloc2, status );
      ndfXpt0c( subarray, indf2, "DSUTILS", "SUBARRAY", status );
      datAnnul( &xloc2, status );

/* Map the NDF DATA arrays. */
      ndfMap( indf1, "Data", "_DOUBLE", "Write", (void *) &data1,
              &nel, status );
      ndfMap( indf2, "Data", "_DOUBLE", "Write", (void *) &data2,
              &nel, status );

/* Copy the axis values into the NDF data arrays. */
      if( *status == SAI__OK ) {
         double *p1 = worka;
         double *p2 = worka + nxy;
         for( j = 0; j < nxy; j++ ) {
            *(data1++) = *(p1++);
            *(data2++) = *(p2++);
         }
      }

/* NDF character components. */
      ndfCput( "Focal plane X value", indf1, "Label", status );
      ndfCput( "arc-sec", indf1, "Unit", status );
      ndfCput( "Focal plane Y value", indf2, "Label", status );
      ndfCput( "arc-sec", indf2, "Unit", status );

/* WCS FrameSets. Current Frame (FPLANE) is fplane in mm with no PolyMap,
   but also include FPLANE2 in arc-sec with PolyMap. */
      wcs = GetFPFrameSet( subarray, status );
      icurr = astGetI( wcs, "Current" );
      astSetC( fp_fset, "Domain", "FPLANE2" );
      astSetI( fp_fset, "Current", astGetI( fp_fset, "Base" ) );
      astAddFrame( wcs, AST__BASE, astUnitMap( 2, " " ), fp_fset );
      astSetI( wcs, "Current", icurr );

      ndfPtwcs( wcs, indf1, status );
      ndfPtwcs( wcs, indf2, status );

/* Close the NDFs. */
      ndfAnnul( &indf1, status );
      ndfAnnul( &indf2, status );
   }

/* End the AST context. */
   astEnd;



/* If input NDFs are supplied for parameter INFITX and INFITY, we can do
   three things: 1) create C source code defining the coefficients
   needed for a single transformation (forward or inverse) of a 2D PolyMap
   corresponding to the Starlink POLYNOMIAL structure in the SURFACEFIT
   extension in both NDFs, 2) actually create the PolyMap and then use
   it to create two output NDFs holding the sampled PolyMap outputs, 3)
   create a pair of output NDFs holding the magnitude and orientation of
   the vectors representing the shift implied by the PolyMap at each point
   in the focal plane.
   ---------------------------------------------------------------- */
   ndfAssoc( "INFITX", "Read", &indf1, status );
   ndfAssoc( "INFITY", "Read", &indf2, status );

   if( *status == PAR__NULL ) {
      errAnnul( status );

/* If both NDFs were obtained, get locators to the NDF extensions
   holding the chebyshev polynomial coefficients. */
   } else {
      ndfXloc( indf1, "SURFACEFIT", "Read", &xloc1, status );
      datFind( xloc1, "FIT", floc, status );

      ndfXloc( indf2, "SURFACEFIT", "Read", &xloc2, status );
      datFind( xloc2, "FIT", floc + 1, status );

/* Convert them to coefficients of a standard polynomial. This excludes
   zero-valued coefficients, except for the linear coefficients, which are
   included even if they are zero valued. */
      cofs = kpg1Chcof( 2, floc, &ncoeff_f, &nin, status );

/* Get the name of the output file to hold the generated C code and attempt to open it. */
      if( *status == SAI__OK ) {
         parGet0c( "OUTCODE", outcode, MAXPATH, status );

/* If a name was supplied, attempt to open the file for writing. */
         if( *status == SAI__OK ) {
            fp2 = fopen( outcode, "w" );
            if( !fp2 && *status == SAI__OK ) {
               *status = SAI__ERROR;
               msgSetc ( "C", outcode );
               errRep( FUNC_NAME, "Output code file '^C' could not be opened "
                       "for writing.", status );

/* If an output file was specified, see if it is to receive the coefficients
   of the forward or inverse transformation. */
            } else {
               parGet0l( "FORWARD", &forward, status );
            }
         }
      }

/* Abort if an error has occurred. */
      if( *status != SAI__OK ) goto L999;

/* Get pointers to the linear X and Y coefficient values. */
      p = cofs;
      for( i = 0; i < ncoeff_f; i++ ) {
         if( p[ 1 ] == 1.0 && p[ 2 ] == 1 && p[ 3 ] == 0  ) {
            xlin = p;
         } else if( p[ 1 ] == 2.0 && p[ 2 ] == 0.0 && p[ 3 ] == 1.0  ) {
            ylin = p;
         }
         p += 4;
      }

/* We want a PolyMap that converts (x,y) into corrected (x,y) in one
   step. Therefore we need add 1.0 to the linear X and Y terms. */
      *(xlin) += 1.0;
      *(ylin) += 1.0;

/* We further modify the coefficients of the forward transformation to
   ensure that the mean of the two linear coefficients is unity. The
   scaling factor required to do this should be applied to the MM2RAD
   value used to create the supplied  distortion data. */
      if( forward ) {
         alpha = 0.5*( *xlin + *ylin );
         p = cofs;
         for( i = 0; i < ncoeff_f; i++ ) {
            p[ 0 ] /= alpha;
            p += 4;
         }
      } else {
         alpha = 1.0;
      }

/* Write out the code if required. */
      if( fp2 ) {

         fprintf( fp2, "\n" );
         if( forward ) {
            fprintf( fp2, "\n");
            fprintf( fp2, "/* Change the plate scale used to create the distortion data. */\n");
            fprintf( fp2, "#define MM2RAD MM2RAD*%g\n", alpha );
            fprintf( fp2, "\n");
            fprintf( fp2, "\n");
            fprintf( fp2, "/* SCUBA-2 PolyMap cooefficients. Forward coefficients are from \n");
            fprintf( fp2, "   FRAME850 to Nasmyth */ \n");
            fprintf( fp2, "\n");
            fprintf( fp2, "/* Forward transformation coefficients... */\n");
            fprintf( fp2, "   int ncoeff_f = %d;\n", ncoeff_f );
            fprintf( fp2, "   const double coeff_f[] = {\n");
         } else {
            fprintf( fp2, "/* Inverse transformation coefficients... */\n");
            fprintf( fp2, "   int ncoeff_i = %d;\n", ncoeff_f );
            fprintf( fp2, "   const double coeff_i[] = {\n");
         }
         fprintf( fp2, "\n");

/* The supplied polynomial gives a correction to add on to (x,y), as a function
   of (x,y). */
         fprintf( fp2, "/* X-coordinate */ \n");
         p = cofs;
         for( i = 0; i < ncoeff_f; i++ ) {
            if( p[ 1 ] == 1.0 ) {
               fprintf( fp2, "               %-12.5g,     %3.1f, %3.1f, %3.1f,\n",
                        p[ 0 ], p[ 1 ], p[ 2 ], p[ 3 ] );
            }
            p += 4;
         }

         fprintf( fp2, "\n/* Y-coordinate */ \n");
         p = cofs;
         for( i = 0; i < ncoeff_f; i++ ) {
            if( p[ 1 ] == 2.0 ) {
               fprintf( fp2, "               %-12.5g,     %3.1f, %3.1f, %3.1f,\n",
                        p[ 0 ], p[ 1 ], p[ 2 ], p[ 3 ] );
            }
            p += 4;
         }

         fprintf( fp2, "            };\n" );
         fprintf( fp2, "\n\n\n" );

         fclose( fp2 );
      }

/* If we are producing the forward transformation, create the PolyMap. It maps
   real (Fx,Fy) in mm, to corrected (Fx,Fy). */
      if( forward ) {
         polymap = astPolyMap( 2, 2, ncoeff_f, cofs, 0, NULL, " " );

/* Create a WinMap that transforms a sub-dividied GRID coordinate system
   (in which each bolometer is dividied into 10 along each axis) into the
   full-sized bolometer GRID coordinate system. */
         if( EXPAND > 1 ) {
            ina[ 0 ] = 0.5;
            ina[ 1 ] = 0.5;
            outa[ 0 ] = 0.5;
            outa[ 1 ] = 0.5;
            inb[ 0 ] = 0.5 + EXPAND/2;
            inb[ 1 ] = 0.5 + EXPAND/2;
            outb[ 0 ] = 1.0;
            outb[ 1 ] = 1.0;
            winmap = astWinMap( 2, ina, inb, outa, outb, " " );
         } else {
            winmap = (AstWinMap *) astUnitMap( 2, " " );
         }

/* Get the (full-sized) GRID->real (Fx,Fy) Mapping. */
         parGet0c( "SUBARRAY", subarray, 8, status );
         fp_fset = GetFPFrameSet( subarray, status );
         fp_map = astGetMapping( fp_fset, AST__BASE, AST__CURRENT );

/* Combine them to get the (expanded) GRID->real (Fx,Fy) Mapping. */
         map2 = (AstMapping *) astCmpMap( winmap, fp_map, 1, " " );

/* Remap the base (GRID) Frame in the FrameSet to represent expanded
   grid coords. */
         astInvert( winmap );
         astRemapFrame( fp_fset, AST__BASE, winmap );
         astInvert( winmap );

/* Create arrays holding the real focal plane coords for every tenth of a
   bolometer. */
         lbnd_out[ 0 ] = 1 - MARGIN;
         lbnd_out[ 1 ] = 1 - MARGIN;
         ubnd_out[ 0 ] = 32*EXPAND + MARGIN;
         ubnd_out[ 1 ] = 40*EXPAND + MARGIN;
         nxy = ( ubnd_out[ 0 ] - lbnd_out[ 0 ] + 1 );
         nxy *= ( ubnd_out[ 1 ] - lbnd_out[ 1 ] + 1 );
         worka = astMalloc( 2*nxy*sizeof( double ) ) ;
         astTranGrid8( map2, 2, lbnd_out, ubnd_out, 0.0, 0, 1, 2, nxy, worka );

/* Combine the two Mappings to get the expanded GRID->corrected (Fx,Fy) Mapping. */
         map = (AstMapping *) astCmpMap( map2, polymap, 1, " " );

/* Create arrays holding the corrected focal plane coords for every tenth
   of a bolometer. */
         workb = astMalloc( 2*nxy*sizeof( double ) ) ;
         astTranGrid8( map, 2, lbnd_out, ubnd_out, 0.0, 0, 1, 2, nxy, workb );

/* Subtract the corrected focal plane X coords from the real focal plane
   X coords. Correct the Y coords in the same way. This gives us the
   deltas needed to to convert corrected focal plane coords to real focal
   plane coords. */
         for( j = 0; j < nxy; j++ ) {
            worka[ j ] -= workb[ j ];
            worka[ j + nxy ] -= workb[ j + nxy ];
         }

/* Create an NDF by pasting the X deltas into an image with axes which are
   a linear function of corrected focal plane axes. To get a linear axis
   mapping, we use "fp_map" rather than "map". */
         lbnd_out[ 0 ] = -EXPAND + 1;
         ubnd_out[ 0 ] = lbnd_out[ 0 ] + 32*EXPAND - 1;
         lbnd_out[ 1 ] = -EXPAND + 1;
         ubnd_out[ 1 ] = lbnd_out[ 1 ] + 40*EXPAND - 1;
         astInvert( map2 );
         PasteNDF( subarray, lbnd_out, ubnd_out, (int) nxy, workb, workb + nxy, worka, NULL,
                   map2, fp_fset, 0, 0, 0.0, "OUTDX", status );

/* Create another NDF by pasting the Y deltas into an image with axes which
   are a linear function of corrected focal plane axes. */
         PasteNDF( subarray, lbnd_out, ubnd_out, (int) nxy, workb, workb + nxy, worka + nxy,
                   NULL, map2, fp_fset, 0, 0, 0.0, "OUTDY", status );
         astInvert( map2 );

/* Free resources. */
         worka = astFree( worka );
         workb = astFree( workb );

      }

/* Free remaining resources. */
      cofs = astFree( cofs );
      datAnnul( floc, status );
      datAnnul( floc + 1, status );
      datAnnul( &xloc1, status );
      datAnnul( &xloc2, status );

/* Nothing else to do, so jump to the end. */
      goto L999;
   }

/* If an input catalogue is supplied, we create output NDFs holding the
   binned focal plane X and Y corrections for each bolometer. We can also
   produce an output catalogue holding a copy of the input catalogue, but
   with aberant rows removed, and extra columns added.
   -------------------------------------------------------------------- */
   if( *status == SAI__OK ) {
      parGet0c( "INCAT", incat, MAXPATH, status );

/* If no name was supplied, annul the error and store a null file pointer. */
      if( *status == PAR__NULL ) {
         errAnnul( status );
         fp1 = NULL;

/* If a name was supplied, attempt to open the file for reading. */
      } else if( *status == SAI__OK ) {
         fp1 = fopen( incat, "r" );
         if( !fp1 && *status == SAI__OK ) {
            *status = SAI__ERROR;
            msgSetc ( "CAT", incat );
            errRep( FUNC_NAME, "Input catalogue '^CAT' could not be opened "
                    "for reading.", status );
         }
      }
   }

/* If we got an input catalogue... */
   if( fp1 && *status == SAI__OK ) {

/* See if we are pasting a single column of the catalogue into an output
   NDF. If not, annul the error and pass on. */
      parGet0c( "COLNAME", colname, MAXCOLNAME, status );
      if(  *status == PAR__NULL ) {
         errAnnul( status );
         colname[ 0 ] = 0;
      }

/* Get the name of any output catalogue and attempt to open it. */
      if( *status == SAI__OK ) {
         parGet0c( "OUTCAT", outcat, MAXPATH, status );

/* If no name was supplied, annul the error and store a null file pointer. */
         if( *status == PAR__NULL ) {
            errAnnul( status );
            fp2 = NULL;

/* If a name was supplied, attempt to open the file for writing. */
         } else if( *status == SAI__OK ) {
            fp2 = fopen( outcat, "w" );
            if( !fp2 && *status == SAI__OK ) {
               *status = SAI__ERROR;
               msgSetc ( "CAT", outcat );
               errRep( FUNC_NAME, "Output catalogue '^CAT' could not be opened "
                       "for writing.", status );
            }
         }
      }

/* The lower pixel index bounds on the spatial axes of a raw time series
   cube is always (0,0). */
      lbnd[ 0 ] = 0;
      lbnd[ 1 ] = 0;

/* Get the name of the scuba-2 subarray, and get the GRID - > focal
   plane (in millimeters) Mapping. This does not include any polynomial
   corrections, but assumes the original focal plane reference point for
   each subarray. . */
      parGet0c( "SUBARRAY", subarray, 8, status );
      fp_fset = GetFPFrameSet( subarray, status );
      fp_map = astGetMapping( fp_fset, AST__BASE, AST__CURRENT );

/* Get a KeyMap in which each entry has a key equal to a column name, and
   a value equal to the corresponding zero-based column index. */
      header = GetHeader( fp1, status );

/* Store the zero-based column indices of the required columns. */
      col[ BC1 ] = GetColIndex( header, "BC1", status );
      col[ BC2 ] = GetColIndex( header, "BC2", status );
      col[ DBF1 ] = GetColIndex( header, "DBF1", status );
      col[ DBF2 ] = GetColIndex( header, "DBF2", status );
      col[ AMP ] = GetColIndex( header, "AMP", status );
      col[ DAMP ] = GetColIndex( header, "DAMP", status );
      col[ RMS ] = GetColIndex( header, "RMS", status );
      col[ F1 ] = GetColIndex( header, "BF1", status );
      col[ F2 ] = GetColIndex( header, "BF2", status );

      if( colname[ 0 ] ) {
         col[ EXTRA ] = GetColIndex( header, colname, status );
      } else {
         col[ EXTRA ] = -1;
      }

/* See if we are to smooth featires positions across time slices. If so
   get the index of the time slice index column. */
      col[ IZ ] = -1;
      if( *status == SAI__OK ) {
         parGet0k( "ZBOX", &zbox, status );
         if( *status == PAR__NULL ) {
            errAnnul( status );
         } else {
            if( zbox < 1 ) zbox = 1;
            col[ IZ ] = GetColIndex( header, "IZ", status );
         }
      }

/* Initialiuse the number of rows read so far, and the upper and lower
   bounds on each axis of the selected feature position. */
      nrow = 0;
      f1_lo = VAL__MAXD;
      f2_lo = VAL__MAXD;
      f1_hi = VAL__MIND;
      f2_hi = VAL__MIND;

/* Read over all comment lines at the start of the file. */
      l = 0;
      while( fgets( buf, BUFSIZE, fp1 ) && buf[ 0 ] == '#' ) l++;

/* Initialise the number of rows read so far, and loop round all
   remaining lines in the input file. */
      genvar = 0;
      do {
         ok = 1;

/* Split the line into space separated words. */
         words = astChrSplit( buf, &nword );

/* Loop round all the words (i.e. columns) in this line. */
         for( i = 0; i < nword; i++ ) {
            p = NULL;

/* Attempt to read a float from the column. If the conversion fails report an error. */
            if( sscanf( words[ i ], "%g", &val ) == 0 ) {
               *status = SAI__ERROR;
               msgSetc( "B", buf );
               msgSeti( "I", i + 1 );
               msgSeti( "L", l );
               errRep( " ", "Failed to read column ^I of line ^L:", status );
               errRep( " ", buf, status );
               break;

/* If we read a floating point value succesfully, select the array in
   which to store it and expand the array to make room for the new value. */
            } else if( i == col[ BC1 ] ) {
               p = bc1_vals = astGrow( bc1_vals, nrow + 1, sizeof( *bc1_vals ) );

            } else if( i == col[ BC2 ] ) {
               p = bc2_vals = astGrow( bc2_vals, nrow + 1, sizeof( *bc2_vals ) );

            } else if( i == col[ DBF1 ] ) {
               p = dbf1_vals = astGrow( dbf1_vals, nrow + 1, sizeof( *dbf1_vals ) );

            } else if( i == col[ DBF2 ] ) {
               p = dbf2_vals = astGrow( dbf2_vals, nrow + 1, sizeof( *dbf2_vals ) );

            } else if( i == col[ AMP ] ) {
               p = amp_vals = astGrow( amp_vals, nrow + 1, sizeof( *amp_vals ) );

            } else if( i == col[ DAMP ] ) {
               p = damp_vals = astGrow( damp_vals, nrow + 1, sizeof( *damp_vals ) );

            } else if( i == col[ RMS ] ) {
               p = rms_vals = astGrow( rms_vals, nrow + 1, sizeof( *rms_vals ) );

            } else if( i == col[ IZ ] ) {
               p = iz_vals = astGrow( iz_vals, nrow + 1, sizeof( *iz_vals ) );

            }

/* If the word has a silly value, indicate the line cannot be used. */
            if( !isfinite( val ) || fabs( val ) > 1.0E10 ) {
               ok = 0;
               val = 0.0;
            }

/* Ensure the error on the source position is no less than 0.001 pixels.
   Also, indicate the row is unusable if the error on the source position is
   more than 0.1 pixels. */
            if( i == col[ DBF1 ] || i == col[ DBF2 ] ) {
               if( val != VAL__BADD && val < 0.001 ) {
                  val = 0.001;
               } else if( val != VAL__BADD && val > DBF_MAX ) {
                  val = 0;
                  ok = 0;
               }
            }

/* If we read a required value, store it in the expanded array. */
            if( p ) p[ nrow ] = val;
            p = NULL;

/* Now look for feature positions... */
            if( i == col[ F1 ] ) {
               p = f1_vals = astGrow( f1_vals, nrow + 1, sizeof( *f1_vals ) );

/* If the data was created from bolomaps (as shown by all true positions
   being a multiple of half a pixel), we cannot use AST__GENVAR flag with
   astRebinSeq. */
               if( val != VAL__BADD && !genvar && ( fabs( fmod( val, 0.5 ) ) > 0.001 ) ) genvar = 1;

            } else if( i == col[ F2 ] ) {
               p = f2_vals = astGrow( f2_vals, nrow + 1, sizeof( *f2_vals ) );
            }

/* If we read a feature position, store it in the expanded array. */
            if( p ) p[ nrow ] = val;

/* Now look for and store any extra column. */
            if( i == col[ EXTRA ] ) {
               p = extra_vals = astGrow( extra_vals, nrow + 1, sizeof( *extra_vals ) );
            }
            if( p ) p[ nrow ] = val;

         }

/* Free resources. */
         for( i = 0; i < nword; i++ ) words[ i ] = astFree( words[ i ] );
         words = astFree( words );

/* Convert the base telescope position and feature position from PIXEL to
   GRID, and then use astTran2 to convert GRID to focal plane (in mm). */
         if( *status == SAI__OK && ok ) {
            ok = 0;

            xin[ 0 ] = bc1_vals[ nrow ] - lbnd[ 0 ] + 1.5;
            yin[ 0 ] = bc2_vals[ nrow ] - lbnd[ 1 ] + 1.5;
            xin[ 1 ] = f1_vals[ nrow ] - lbnd[ 0 ] + 1.5;
            yin[ 1 ] = f2_vals[ nrow ] - lbnd[ 1 ] + 1.5;
            astTran28( fp_map, 2, xin, yin, 1, xout, yout ) ;

/* Store the offsets in focal plane coords at the current focal plane
   feature position. */
            bc1_vals[ nrow ] = xout[ 0 ] - xout[ 1 ];
            bc2_vals[ nrow ] = yout[ 0 ] - yout[ 1 ];

/* Expand memory to hold new columns in output catalogue. */
            fx_vals = astGrow( fx_vals, nrow + 1, sizeof( *fx_vals ) );
            fy_vals = astGrow( fy_vals, nrow + 1, sizeof( *fy_vals ) );
            dpx_vals = astGrow( dpx_vals, nrow + 1, sizeof( *dpx_vals ) );
            dpy_vals = astGrow( dpy_vals, nrow + 1, sizeof( *dpy_vals ) );
            dfx_vals = astGrow( dfx_vals, nrow + 1, sizeof( *dfx_vals ) );
            dfy_vals = astGrow( dfy_vals, nrow + 1, sizeof( *dfy_vals ) );
            if( *status == SAI__OK ) {

/* Store focal plane position */
               fx_vals[ nrow ] = xout[ 1 ];
               fy_vals[ nrow ] = yout[ 1 ];

/* Store pixel offsets */
               dpx_vals[ nrow ] = xin[ 0 ] - xin[ 1 ];
               dpy_vals[ nrow ] = yin[ 0 ] - yin[ 1 ];

/* Store focal plane offsets */
               dfx_vals[ nrow ] = xout[ 0 ] - xout[ 1 ];
               dfy_vals[ nrow ] = yout[ 0 ] - yout[ 1 ];

            }

/* Skip lines for which the offset on either axis is more than 6 mm, or for
   which the error on the beamfit position is more than 0.1 pixel */
            if( fabs( bc1_vals[ nrow ] ) < 6.0 &&
                fabs( bc2_vals[ nrow ] ) < 6.0 ) {

               if( col[F1] == 2 || ( dbf1_vals[ nrow ] < DBF_MAX &&
                                     dbf2_vals[ nrow ] < DBF_MAX ) ) {

/* Calculate the mean error on the feature position, in pixel coords, and
   convert to mm. This is used as a weight for the focal plane offsets
   calculated above. */
                  dbf1_vals[ nrow ] += dbf2_vals[ nrow ];
                  dbf1_vals[ nrow ] *= 0.5*PIX2MM;

/* Convert the beamfit errors into variances. */
                  dbf1_vals[ nrow ] *= dbf1_vals[ nrow ];
                  dbf2_vals[ nrow ] = dbf1_vals[ nrow ];

/* Update the bounds of the usable feature positions (in pixel coords). */
                  if( f1_vals[ nrow ] < f1_lo ) f1_lo = f1_vals[ nrow ];
                  if( f1_vals[ nrow ] > f1_hi ) f1_hi = f1_vals[ nrow ];
                  if( f2_vals[ nrow ] < f2_lo ) f2_lo = f2_vals[ nrow ];
                  if( f2_vals[ nrow ] > f2_hi ) f2_hi = f2_vals[ nrow ];

                  ok = 1;
               }
            }
         }

/* Flag bad rows by storing a bad bc1 value if the row was not usable. */
         if( !ok ) bc1_vals[ nrow ] = VAL__BADD;

/* Increment the number of usable rows. */
         nrow++;

/* Read the next line from the input catalogue. Leave the loop if we have
   reached the end. */
         l++;
      } while( *status == SAI__OK && fgets( buf, BUFSIZE, fp1 ) );

      if( *status != SAI__OK ) goto L999;

      printf( "%" DIM_T_FMT " bad rows\n", l - nrow );

/* Flag unusual rows by storing a bad value for each in "bc1_vals". */
      Filter2( nrow, bc1_vals, amp_vals, 3.0, "AMP", status );
      Filter( nrow, bc1_vals, damp_vals, 3.0, "DAMP", status );
      Filter( nrow, bc1_vals, rms_vals, 3.0, "RMS", status );

/* Transfer flags into bc2_vals and extra_vals (if used). */
      for( irow = 0; irow < nrow; irow++ ) {
         if( bc1_vals[ irow ] == VAL__BADD ) {
            bc2_vals[ irow ] = VAL__BADD;
            if( extra_vals ) extra_vals[ irow ] = VAL__BADD;
         }
      }

/* If required, smooth the offsets along the time axis. */
      if( col[ IZ ] != -1 ) {
         bc1_vals = SmoothZ( zbox, nrow, bc1_vals, iz_vals, status );
         bc2_vals = SmoothZ( zbox, nrow, bc2_vals, iz_vals, status );
      }

/* Create any required output catalogue */
      if( fp2 ) {

/* Add the new column nmaes to the header. */
         AddColName( header, "FX", "Focal plane X position in mm", status );
         AddColName( header, "FY", "Focal plane Y position in mm", status );
         AddColName( header, "DPX", "Focal plane X offsets in pixels", status );
         AddColName( header, "DPY", "Focal plane Y offsets in pixels", status );
         AddColName( header, "DFX", "Unsmoothed Focal plane X offsets in mm", status );
         AddColName( header, "DFY", "Unsmoothed Focal plane Y offsets in mm", status );
         AddColName( header, "SFX", "Smoothed Focal plane X offsets in mm", status );
         AddColName( header, "SFY", "Smoothed Focal plane Y offsets in mm", status );

/* Write out the catalogue header */
         PutHeader( fp2, header, status );

/* Re-read the input catalogue, skipping over comment lines. */
         rewind( fp1 );
         while( fgets( buf, BUFSIZE, fp1 ) && buf[ 0 ] == '#' );

/* Write each line out to the output catalogue, appending values for the new columns. */
         irow = 0;
         do {
            if( buf[ strlen( buf ) - 1 ] == '\n' ) buf[ strlen( buf ) - 1 ] = 0;
            if( bc1_vals[ irow ] != VAL__BADD ) fprintf( fp2, "%s %g %g %g %g %g %g %g %g\n", buf,
                                                         fx_vals[irow],  fy_vals[irow],
                                                         dpx_vals[irow], dpy_vals[irow],
                                                         dfx_vals[irow], dfy_vals[irow],
                                                         bc1_vals[irow], bc2_vals[irow] );
            irow++;

/* Read the next line from the input catalogue. Leave the loop if we have
   reached the end. */
         } while( *status == SAI__OK && fgets( buf, BUFSIZE, fp1 ) );
         fclose( fp2 );
      }

/* Close the input catalogue. */
      fclose( fp1 );

/* See how many sigma-clipping iterations should be performed whilst
   creating the output NDFs. */
      parGet0i( "NITER", &niter, status );

/* Create a Mapping from pixel coords in a bolometer slice (i.e. pixel
   origin (0,0) ) to GRID coords. */
      shift[ 0 ] = 1.5;
      shift[ 1 ] = 1.5;
      map = (AstMapping *) astShiftMap( 2, shift, " " );

/* Paste the X offset values into an output NDF. */
      PasteNDF( subarray, NULL, NULL, (int)nrow, f1_vals, f2_vals, bc1_vals, dbf1_vals, map,
                fp_fset, niter, genvar, 0.5, "OUTDX", status );

/* Create the Y offset NDF in the same way. */
      PasteNDF( subarray, NULL, NULL, (int)nrow, f1_vals, f2_vals, bc2_vals, dbf2_vals, map,
                fp_fset, niter, genvar, 0.5, "OUTDY", status );

/* Create an NDF holding any extra required column in the same way. */
      if( extra_vals ) {
         PasteNDF( subarray, NULL, NULL, (int)nrow, f1_vals, f2_vals, extra_vals,
                   dbf1_vals, map, fp_fset, niter, genvar, 0.5, "COLNDF", status );
      }

/* Free resources. */
      bc1_vals = astFree( bc1_vals );
      bc2_vals = astFree( bc2_vals );
      dbf1_vals = astFree( dbf1_vals );
      dbf2_vals = astFree( dbf2_vals );
      f1_vals = astFree( f1_vals );
      f2_vals = astFree( f2_vals );
      amp_vals = astFree( amp_vals );
      damp_vals = astFree( damp_vals );
      rms_vals = astFree( rms_vals );
      fx_vals = astFree( fx_vals );
      fy_vals = astFree( fy_vals );
      dpx_vals = astFree( dpx_vals );
      dpy_vals = astFree( dpy_vals );
      dfx_vals = astFree( dfx_vals );
      dfy_vals = astFree( dfy_vals );
      extra_vals = astFree( extra_vals );

/* Nothing else to do, so jump to the end. */
      goto L999;
   }

/* If a input time series NDF is supplied, we can either create a Mapping
   describing a typical time slice in it, or create an output catalogue
   holding the telescope base positions, peak positions, and data sums
   at each time slice, or extract a specified time slice into an output
   NDF.
   -------------------------------------------------------------------- */

/* Attempt to get the name of the input NDF. */
   igrp = NULL;
   if( *status == SAI__OK ) {
      kpg1Rgndf( "IN", 1, 1, "", &igrp, &size, status );
      if( *status == PAR__NULL ) errAnnul( status );
   }

/* If an input NDF name was obtained... */
   if( igrp && * status == SAI__OK ) {

/* Obtain information about the input NDF. */
      smf_open_file( NULL, igrp, 1, "READ", 0, &data, status );

/* Display the indices of time slices for which the reference position
   is close a to a specified bolometer. */
      if( ShowSlices( "OUTCAT", data, status ) ) goto L998;

/* Display the indices of time slices for which the reference position
   is close a to a specified bolometer. */
      if( FindSlices( "XBOL", "YBOL", "RADIUS", data, status ) ) goto L998;

/* Write the Mapping from a modified PIXEL Frame to the SKY offset frame.
   The PIXEL frame is modified in the sense that its origin (i.e. pixel
   coords (0,0,0.0) ) is shifted so that it co-incides with the sky
   reference point ). If succesful, do nothing more. */
      if( SaveBoloMapping( "BMAP", data, status ) ) goto L998;

/* Write out a specified time slice to a 2D NDF. */
      if( SaveTimeSlice( "OUTSLICE", "ITIME", data, status ) ) goto L998;

/* Convenience pointers */
      file = data->file;
      hdr = data->hdr;

/* Check the data type is _DOUBLE. */
      if( data->dtype != SMF__DOUBLE && *status == SAI__OK ){
         msgSetc( "FILE", file->name );
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "^FILE has an unspected data type, should be _DOUBLE.", status );
      }

/* Check that there are 3 pixel axes. */
      if( data->ndims != 3 ) {
         msgSetc( "FILE", file->name );
         msgSeti( "NDIMS", data->ndims );
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "^FILE has ^NDIMS pixel axes, should be 3.", status );
      }

/* Check it holds SCUBA-2 data. */
      if( data->ndims != 3 && *status == SAI__OK ) {
         msgSetc( "FILE", file->name );
         msgSeti( "NDIMS", data->ndims );
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "^FILE has ^NDIMS pixel axes, should be 3.", status );
      }

/* Note the array dimensions, and the number of elements in a single slice. */
      nx = data->dims[ 0 ];
      ny = data->dims[ 1 ];
      ntime = data->dims[ 2 ];
      nxy = nx*ny;

/* Store the upper pixel index bounds of each time slice. */
      ubnd[ 0 ] = data->lbnd[ 0 ] + nx - 1;
      ubnd[ 1 ] = data->lbnd[ 1 ] + ny - 1;

/* Get the border width in pixels. Time slices with peak positions closer
   to any edge than this amount will not be included in the output catalogue. */
      parGet0i( "BORDER", &ival, status );
      border = ival;

/* Get the name of the output catalogue and attempt to open it. */
      parGet0c( "OUTCAT", outcat, MAXPATH, status );

/* If a name was supplied, attempt to open the file for writing. */
      if( *status == SAI__OK ) {
         fp2 = fopen( outcat, "w" );
         if( !fp2 && *status == SAI__OK ) {
            *status = SAI__ERROR;
            msgSetc ( "CAT", outcat );
            errRep( FUNC_NAME, "Output catalogue '^CAT' could not be opened "
                    "for writing.", status );
         }
      }

/* Initialise a pointer to the next element in the data array */
      pd = data->pntr[ 0 ];

/* Initialise the largest total data sum in any one time slice. */
      maxsum = VAL__MIND;

/* Allocate memory to hold info about all time slices. */
      slices = astMalloc( sizeof( *slices )*ntime );

/* Loop round each time slice. */
      for( itime = 0; itime < ntime && *status == SAI__OK; itime++ ) {

/* Initialise bad values. */
         bcx = AST__BAD;
         bcy = AST__BAD;
         sum = VAL__MIND;
         peakv = VAL__MIND;
         peakx = VAL__BADI;
         peaky = VAL__BADI;

/* Get a FrameSet describing the spatial coordinate systems associated with
   the current time slice. The base frame in the FrameSet will be a 2D GRID
   Frame, the current Frame will be a SkyFrame (absolute or offset). NOTE,
   THE SMURF_NOPOLY environment variable should have been assigned a
   non-zero value before running this command in order to supress the
   usage of a polynomial distortion within sc2ast. */
         smf_tslice_ast( data, itime, 1, NO_FTS, status );
         swcsin = hdr->wcs;

/* If the current Frame (the SkyFrame) currrently represents absolute
   coords, temporarily set it to represent offset coords. */
         skyrefis = astGetC( swcsin, "SkyRefIs" );
         if( skyrefis && strcmp( skyrefis, "Origin" ) ) {
            astSet( swcsin, "SkyRefIs=Origin" );
         }

/* Map the reference point (i.e. (0,0) in the offset coords system) into
   GRID coords. */
         ox = 0.0;
         oy = 0.0;
         astTran28( swcsin, 1, &ox, &oy, 0, &gx, &gy );

/* Convert from GRID to PIXEL coords. */
         bcx = gx + data->lbnd[ 0 ] - 1.5;
         bcy = gy + data->lbnd[ 1 ] - 1.5;

/* If required, reset the SkyRefIs attribute back to its original value. */
         if( skyrefis && strcmp( skyrefis, "Origin" ) ) {
            astSetC( swcsin, "SkyRefIs", skyrefis );
         }

/* Save a pointer to the first data value in this time slice. */
         pd0 = pd;

/* Find the total data sum, and the grid coords of the peak pixel value,
   in the time slice data array. */
         sum = 0.0;
         for( iy = 1; iy <= (int) ny; iy++ ) {
            for( ix = 1; ix <= (int) nx; ix++, pd++ ) {
               if( *pd != VAL__BADD ) {
                  sum += *pd;
                  if( *pd > peakv ) {
                     peakv = *pd;
                     peakx = ix;
                     peaky = iy;
                  }
               }
            }
         }

/* If the peak is in the excluded border area, set its sum to zero. */
         if( peakx <= border || peakx > nx - border ||
             peaky <= border || peaky > ny - border ) {
            sum = 0.0;

/* Otherwise check there were some good values in the time slice. */
         } else if( peakv != VAL__MIND ) {

/* Update the maximum total data sum found in any time slice. */
            if( sum > maxsum ) maxsum = sum;

/* Convert GRID indices at the peak pixel to PIXEL indices. */
            peakx += data->lbnd[ 0 ] - 1;
            peaky += data->lbnd[ 1 ] - 1;
         }

/* Store values for this time slice. */
         slices[ itime ].sum = sum;
         slices[ itime ].ipx = peakx;
         slices[ itime ].ipy = peaky;
         slices[ itime ].bcx = bcx;
         slices[ itime ].bcy = bcy;
      }

/* Determine the lower total data sum limit for usable time slices. */
      parGet0r( "LOWFACTOR", &lowfactor, status );
      lowsum = maxsum*lowfactor;

/* Initialise statistics. */
      sx = 0;
      sx2 = 0;
      sy = 0;
      sy2 = 0;
      ns = 0;

/* Loop round all time slices again. Set the slice sum to zero if the original
   sum is less than the low sum limit, and calculate the dispalcement
   statistics for acceptable slices. */
      slice = slices;
      for( itime = 0; itime < ntime && *status == SAI__OK; itime++,slice++ ) {
         if( slice->sum >= lowsum ){
            dx = slices[ itime ].ipx - slices[ itime ].bcx;
            sx += dx;
            sx2 += dx*dx;
            dy = slices[ itime ].ipy - slices[ itime ].bcy;
            sy += dy;
            sy2 += dy*dy;
            ns++;
         } else {
            slice->sum = 0.0;
         }
      }

/* Form the mean and sigma for the X and Y displacements, and so
   calculate the bounds on acceptable X and Y displacement (mean plus or
   minus 3*sigma). */
      if( ns > 0 ) {
         dx = sx/ns;
         dxlo = dx - 3*sqrt( sx2/ns - dx*dx );
         dxhi = 2*dx - dxlo;
         dy = sy/ns;
         dylo = dy - 3*sqrt( sy2/ns - dy*dy );
         dyhi = 2*dy - dylo;
      } else {
         dxlo = 0;
         dxhi = 0;
         dylo = 0;
         dyhi = 0;
      }

/* Save pointers to the first data and variance value in the first input
   time slice. */
      pd0 = data->pntr[ 0 ];
      pv0 = data->pntr[ 1 ];

/* Print out the header to any output file. */
      if( fp2 ) {
         fprintf( fp2, "%s\n", HEADER );
         fprintf( fp2, "#\n" );
         fprintf( fp2, "# IZ Sum" );
         fprintf( fp2, " BC1 BC2" );
         fprintf( fp2, " PEAK1 PEAK2" );
         fprintf( fp2, "\n" );
      }

/* Loop round each time slice. */
      slice = slices;
      for( itime = 0; itime < ntime && *status == SAI__OK; itime++,slice++ ) {

/* Skip this time slice if its total data sum is not acceptable. Also
   skip it if the x or y displacement is more than 3*sigma from the mean. */
         dx = slices[ itime ].ipx - slices[ itime ].bcx;
         dy = slices[ itime ].ipy - slices[ itime ].bcy;
         if( slice->sum > 0.0 && dx > dxlo && dx < dxhi &&
             dy > dylo && dy < dyhi ) {

/* Write out the required information about this time slice to the output
   catalogue. Remember to convert the time slice index from a zero-based
   value to a one-based value. */
            if( fp2 ) {
               fprintf( fp2, "%d %g", (int) itime + 1, slices[ itime ].sum );
               fprintf( fp2, " %g %g", slices[ itime ].bcx,
                        slices[ itime ].bcy );
               fprintf( fp2, " %g %g", slices[ itime ].ipx - 0.5,
                        slices[ itime ].ipy - 0.5 );
               fprintf( fp2, "\n" );
            }
         }

/* Get a pointer to the start of the next input time slice. */
         pd0 += nxy;
         pv0 += nxy;

      }

      L998:

/* Free resources. */
      work = astFree( work );

/* Close the input data file. */
      smf_close_file( NULL, &data, status);

/* Free resources. */
      if( igrp != NULL ) grpDelet( &igrp, status);
      slices = astFree( slices );
   }

   L999:

/* End the AST and NDF contexts. */
   astEnd;
   ndfEnd( status );

/* If anything went wrong issue a context message. */
   if( *status != SAI__OK ) msgOutif( MSG__VERB, " ", "DSUTILS failed.",
                                      status );
}

/* Returns the Mapping from GRID to focal plane coords in millimetres,
   for a given scuba-2 sub-array without any polynomial correction.
   Copied from sc2ast_createwcs. Uses the original reference points for
   each subarray.
   --------------------------------------------------------------------- */

static AstFrameSet *GetFPFrameSet( const char *name, int *status ){

   /* Local Variables: */
   AstFrameSet *result;
   AstMapping *totmap = NULL;
   AstMatrixMap *rotmap;
   AstShiftMap *shiftmap;
   AstShiftMap *zshiftmap;
   AstZoomMap *zoommap;
   double r;
   double rot[ 4 ];
   double shift[ 2 ];
   double zshift[ 2 ];
   sc2ast_subarray_t subnum;

   const double rotangle[ 8 ] =
     { 0.0, PIBY2, 2*PIBY2, 3*PIBY2, 3*PIBY2, 2*PIBY2, PIBY2, 0.0 };

/* xoff and yoff are the distance in pixel units from the tracking centre
   to the [0][0] pixel in a subarray */

   const double xoff[8] =
   /*       s8a    s8b     s8c   s8d    s4a    s4b    s4c    s4d */
     { -41.5,   33.5, 41.5, -33.5, -41.5,  33.5,  41.5, -33.5 };
   const double yoff[8] =
     { -33.5,  -41.5, 33.5,  41.5,  33.5,  41.5, -33.5, -41.5 };

#if COLROW
   AstPermMap *permmap;
   int perm[ 2 ];
#endif

/* Check inherited status */
   if( *status != SAI__OK ) return NULL;

/* Start AST context. */
   astBegin;

/* Get the sub-array number. */
   sc2ast_name2num( name, &subnum, status );

/* Start off with a PermMap that swaps the grid axes from the new
   axes ordering to the old axis ordering (AST uses 1-based axis
   numbering). This covers the recent (ROW,COL) -> (COL,ROW) change. */
#if COLROW
   perm[ 0 ] = 2;
   perm[ 1 ] = 1;
   permmap = astPermMap( 2, perm, 2, perm, NULL, " " );
   totmap = (AstMapping *) permmap;
#endif

/* The GRID domain locates the [0][0] pixel at coordinates (1,1). Shift
   these so that the [0][0] pixel is at the origin of a coordinate system */
   zshift[0] = -1.0;
   zshift[1] = -1.0;
   zshiftmap = astShiftMap ( 2, zshift, " " );
#if COLROW
   totmap = (AstMapping *) astCmpMap( totmap, zshiftmap, 1, " " );
#else
   totmap = (AstMapping *) zshiftmap;
#endif

/* The mm coords now have to be rotated through an angle approximating
   a multiple of 90 degrees */
   r = rotangle[ subnum ];

/* 850 arrays */
   if( subnum < S4A ) {
      rot[ 0 ] =  cos( r );
      rot[ 1 ] = -sin( r );
      rot[ 2 ] =  sin( r );
      rot[ 3 ] =  cos( r );

/* 450 arrays */
   } else {
      rot[ 0 ] = -sin( r );
      rot[ 1 ] =  cos( r );
      rot[ 2 ] =  cos( r );
      rot[ 3 ] =  sin( r );
   }
   rotmap = astMatrixMap ( 2, 2, 0, rot, " " );
   totmap = (AstMapping *) astCmpMap( totmap, rotmap, 1, " " );

/* For each 450/850 subarray, the next Mapping creates FRAME450/FRAME850
   coordinates, which are coordinates in millimetres with origin at the
   center of the focal plane. */
   shift[ 0 ] = xoff[ subnum ];
   shift[ 1 ] = yoff[ subnum ];
   shiftmap = astShiftMap( 2, shift, " " );
   totmap = (AstMapping *) astCmpMap( totmap, shiftmap, 1, " " );

/* The mapping from pixel numbers to millimetres is a simple scaling,
   because the pixel separation is the same in both coordinates and is
   accurately constant. A ZoomMap can be used for this. */
   zoommap = astZoomMap ( 2, PIX2MM, " " );
   totmap = (AstMapping *) astCmpMap( totmap, zoommap, 1, " " );

/* Construct the returned FrameSet. */
   result = astFrameSet( astFrame( 2, "Domain=GRID" ), " " );
   astAddFrame( result, AST__BASE, astSimplify( totmap ),
                astFrame( 2, "Domain=FPLANE,label(1)=FplaneX,"
                          "label(2)=FplaneY,Unit(1)=mm,Unit(2)=mm" ) );

/* Clear up. */
   astExport( result );
   astEnd;

/* Return the FrameSet. */
   return result;
}

/* Create an output NDF and paste a list of data values into its data array.
   --------------------------------------------------------------------- */

static void PasteNDF( const char *subarray, dim_t *lb, dim_t *ub, int nvals,
                      double *xvals, double *yvals, double *datvals,
                      double *varvals, AstMapping *map, AstFrameSet *wcs,
                      int niter, int genvar, double wlim, const char *param,
                      int *status ){

/* Local Variables: */
   AstLutMap *lut1;
   AstLutMap *lut2;
   AstMapping *tmap;
   AstPermMap *pmap;
   HDSLoc *xloc = NULL;
   dim_t lbnd[ 2 ];
   dim_t rlbnd[ 1 ];
   dim_t rubnd[ 1 ];
   dim_t ubnd[ 2 ];
   double *data;
   double *var;
   double *work2d;
   double *work2v;
   double *work;
   double dx;
   int flags;
   int indf;
   int inperm[ 1 ];
   int irow;
   int iter;
   int nrej;
   int outperm[ 2 ];
   int64_t nused;
   size_t nel;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Create the NDF with the required bounds (the default bounds match a
   complete time slice). */
   if( !lb ) {
      lbnd[ 0 ] = 0;
      lbnd[ 1 ] = 0;
   } else {
      lbnd[ 0 ] = lb[ 0 ];
      lbnd[ 1 ] = lb[ 1 ];
   }
   if( !ub ) {
      ubnd[ 0 ] = 31;
      ubnd[ 1 ] = 39;
   } else {
      ubnd[ 0 ] = ub[ 0 ];
      ubnd[ 1 ] = ub[ 1 ];
   }

   ndfCreat( param, "_DOUBLE", 2, lbnd, ubnd, &indf, status );
   if( *status == PAR__NULL ) {
      errAnnul( status );

   } else {

/* Store the subarray. */
      if( subarray ) {
         ndfXnew( indf, "DSUTILS", "DSUTILS", 0, NULL, &xloc, status );
         ndfXpt0c( subarray, indf, "DSUTILS", "SUBARRAY", status );
         datAnnul( &xloc, status );
      }

/* Put the supplied WCS FrameSet into the NDF. */
      ndfPtwcs( wcs, indf, status );

/* Map the NDF DATA array, filling it with bad values. */
      ndfMap( indf, "Data", "_DOUBLE", "Write/Bad", (void *) &data,
              &nel, status );

/* If required, map the NDF VARIANCE array, filling it with bad values. */
      if( genvar ) {
         ndfMap( indf, "Variance", "_DOUBLE", "Write/Bad", (void *) &var,
                 &nel, status );
      } else {
         var = NULL;
      }

/* Set up the bounds of the NDF in GRID coords. */
      ubnd[ 0 ] = ubnd[ 0 ] - lbnd[ 0 ] + 1;
      ubnd[ 1 ] = ubnd[ 1 ] - lbnd[ 1 ] + 1;
      lbnd[ 0 ] = 1;
      lbnd[ 1 ] = 1;

/* Create a Mapping that maps the index within the supplied vals arrays into
   GRID coords. */
      lut1 = astLutMap( nvals, xvals, 0.0, 1.0, "LutInterp=1" );
      lut2 = astLutMap( nvals, yvals, 0.0, 1.0, "LutInterp=1" );
      inperm[ 0 ] = 1;
      outperm[ 0 ] = 1;
      outperm[ 1 ] = 1;
      pmap = astPermMap( 1, inperm, 2, outperm, NULL, " " );
      tmap = (AstMapping *) astCmpMap( astCmpMap( pmap, astCmpMap( lut1, lut2,
                                                                   0, " " ),
                                                  1, " " ),
                                       map, 1, " " );

/* Allocate work arrays. */
      work = astMalloc( 2*nel*sizeof( double ) );
      work2d = astMalloc( sizeof( double )*nvals );
      work2v = astMalloc( sizeof( double )*nvals );

/* Store the bounds of row number. */
      rlbnd[ 0 ] = 0;
      rubnd[ 0 ] = nvals - 1;

/* Get basic flags for astRebinSeq. */
      flags = AST__USEBAD;
      if( varvals ) flags = flags | AST__VARWGT;
      if( genvar ) flags = flags | AST__GENVAR | AST__DISVAR;

/* We do some sigma-clipping */
      for( iter = 0; iter <= niter && *status == SAI__OK; iter++ ) {

/* Bin the values array into the output pixel grid, forming variances from
   the spread of values if required. We use astRebinSeq because astRebin does
   not have a GENVAR facility. */
         astRebinSeq8D( tmap, wlim, 1, rlbnd, rubnd, datvals, varvals, AST__LINEAR,
                        NULL, (AST__REBININIT | flags ), 0.0, 0, VAL__BADD, 2,
                        lbnd, ubnd, rlbnd, rubnd, data, var, work, &nused );
         astRebinSeq8D( tmap, wlim, 1, rlbnd, rubnd, NULL, NULL, AST__LINEAR,
                        NULL, (AST__REBINEND | flags), 0.0, 0, VAL__BADD, 2,
                        lbnd, ubnd, rlbnd, rubnd, data, var, work, &nused );

/* Quit if we have reached the end or if we have no variances. */
         if( iter == niter || !genvar ) break;

/* Resample the binned offset image back into the supplied array format. */
         astInvert( tmap );
         astResample8D( tmap, 2, lbnd, ubnd, data, var, AST__LINEAR, NULL, NULL,
                        AST__USEBAD, 0.0, 0, VAL__BADD, 1, rlbnd, rubnd,
                        rlbnd, rubnd, work2d, work2v );
         astInvert( tmap );

/* Reject any rows where the resampled row value is more than 3*sigma
   from the original row value. */
         nrej = 0;
         for( irow = 0; irow < nvals; irow++ ) {
            if( datvals[ irow ] != VAL__BADD &&
                work2d[ irow ] != VAL__BADD && work2v[ irow ] != VAL__BADD ) {
               dx = fabs( datvals[ irow ] - work2d[ irow ] );
               if( dx > NSIGMA*sqrt( work2v[ irow ] ) ){
                  datvals[ irow ] = VAL__BADD;
                  nrej++;
               }
            }
         }
         printf("%s: iteration %d: %d values removed\n", param, iter, nrej );
      }

/* Free resources */
      work = astFree( work );
      work2d = astFree( work2d );
      work2v = astFree( work2v );
      ndfAnnul( &indf, status );
   }

}

/* Perform sigma-clipping to identify outlying values in a list of values.
   --------------------------------------------------------------------- */

static void Filter( dim_t n, double *flags, double *vals, double nsigma,
                    const char *title, int * status ) {

/* Local Variables: */
   dim_t i;
   dim_t ninc;
   dim_t nrej;
   double mean;
   double sigma;
   double sum1;
   double sum2;
   double var;
   int done;

/* Check inherited status */
   if( *status != SAI__OK ) return;

   nrej = 0;
   while( 1 ) {

      sum1 = 0.0;
      sum2 = 0.0;
      ninc = 0;

      for( i = 0; i < n; i++ ) {
         if ( !isfinite( vals[ i ] ) ) {
            msgOut( " ", "Warning - Non-finite value encountered within function "
                    "Filter.", status );

         } else if( vals[ i ] != VAL__BADD ) {
            sum1 += vals[ i ];
            sum2 += vals[ i ]*vals[ i ];
            ninc++;
         }
      }

      if( ninc > 0 ) {
         mean = sum1/ninc;
         var = sum2/ninc - mean*mean;
         sigma = ( var > 0.0 ) ? sqrt( var ) : 0.0;
      } else {
         mean = 0.0;
         sigma = -1.0;
      }


      done = 1;
      for( i = 0; i < n; i++ ) {
         if( vals[ i ] != VAL__BADD ) {
            if( fabs( mean - vals[ i ] ) > nsigma*sigma ) {
               vals[ i ] = VAL__BADD;
               flags[ i ] = VAL__BADD;
               nrej++;
               done = 0;
            }
         } else {
            flags[ i ] = VAL__BADD;
         }
      }
      if( done ) break;

   }

   printf("Filtering %s removed %d rows\n", title, (int) nrej );

}

/* Write a timeslice Mapping from a modified PIXEL Frame to the SKY offset
   frame. The PIXEL frame is modified in the sense that its origin (i.e.
   pixel coords (0,0,0.0) ) is shifted so that it co-incides with the sky
   reference point ).
   --------------------------------------------------------------------- */

static int SaveBoloMapping( const char *param, smfData *data, int *status ){

   /* Local Variables: */
   AstFrameSet *swcsin = NULL;
   AstCmpMap *map;
   const char *skyrefis;
   double ox;
   double oy;
   double gx;
   double gy;
   double shifts[ 2 ];
   int result;
   dim_t itime;

   result = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

   astBegin;

/* Loop until we have found a usable time slice. Start looking at he
   middle time slice, and check all subsequent time slices until a good
   slice is found. */
   for( itime = data->dims[ 2 ]/2; itime <  data->dims[ 2 ]; itime++ ) {

/* Get the WCS FrameSet describing the time slice. Current frame is AZEL
   at the epoch of the time slice. */
      smf_tslice_ast( data, itime, 1, NO_FTS, status );
      swcsin = data->hdr->wcs;

/* If the skyref position is good, leave the loop early. */
      if( astGetD( swcsin, "SkyRef(1)" ) != AST__BAD &&
          astGetD( swcsin, "SkyRef(2)" ) != AST__BAD ) break;
   }

/* Report an error if no good skyref position was found. */
   if( astGetD( swcsin, "SkyRef(1)" ) == AST__BAD ||
       astGetD( swcsin, "SkyRef(2)" ) == AST__BAD ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( " " , "Could not find a time slice with a good reference "
                 "point", status );
      }
   }

/* If the current Frame (the AZEL SkyFrame) currrently represents absolute
   coords, temporarily set it to represent offset coords. */
   skyrefis = astGetC( swcsin, "SkyRefIs" );
   if( skyrefis && strcmp( skyrefis, "Origin" ) ) {
      astSet( swcsin, "SkyRefIs=Origin" );
   }

/* Map the sky reference point (i.e. (0,0) in the offset coords system) into
   GRID coords. */
   ox = 0.0;
   oy = 0.0;
   astTran28( swcsin, 1, &ox, &oy, 0, &gx, &gy );

/* We want to define a translated version of GRID coords (a form of PIXEL
   coords) in which the pixel origin corresponds to the sky reference
   point. Create the ShiftMap that maps these tranlated grid coords into
   original GRID coords. And combine this with the Mapping from GRID to
   SKY, to get the Mapping from translated grid coords to SKY. */
   shifts[ 0 ] = gx;
   shifts[ 1 ] = gy;
   map = astCmpMap( astShiftMap( 2, shifts, " " ),
                    astGetMapping( swcsin, AST__BASE, AST__CURRENT ), 1, " " );

/* If required, reset the SkyRefIs attribute back to its original value. */
   if( skyrefis && strcmp( skyrefis, "Origin" ) ) {
      astSetC( swcsin, "SkyRefIs", skyrefis );
   }

/* Write the Mapping to an output text file, if required. */
   atlCreat( param, (AstObject *) map, status );
   if( *status == PAR__NULL ) {
      errAnnul( status );
   } else if( *status == SAI__OK ) {
      result = 1;
   }

/* Export the swcsin pointer, since it is annulled explicitly when the
   input data file is closed. */
   astExport( swcsin );
   astEnd;

   return result;
}




/* Write the bolometer data form a given timeslice to a 2D NDF.
   ---------------------------------------------------------- */

static int SaveTimeSlice( const char *param1, const char *param2,
                          smfData *data, int *status ){

/* Local Variables: */
   dim_t bstride;
   dim_t itime;
   dim_t lbnd[ 3 ];
   dim_t ntslice;
   dim_t tstride;
   dim_t ubnd[ 3 ];
   double *d_data = NULL;
   double *ipd = NULL;
   int *i_data = NULL;
   int *ipi = NULL;
   int indf = NDF__NOID;
   int ival;
   int result;
   size_t el;
   size_t i;

/* Initialise */
   result = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Get the strides for the data */
   smf_get_dims( data,  NULL, NULL, NULL, &ntslice, NULL,
                 &bstride, &tstride, status );

/* Get the index of the required time slice. */
   parGdr0i( param2, 1, 1, (int) ntslice, 0, &ival, status );
   itime = ival;

/* If a null value supplied, annul the error and return. */
   if( *status == PAR__NULL ) {
      errAnnul( status );
      return result;
   }

/* Store the spatial pixel index bounds of the time slice. */
   if( bstride == 1 ) {
      lbnd[ 0 ] = data->lbnd[ 0 ];
      ubnd[ 0 ] = lbnd[ 0 ] + data->dims[ 0 ] - 1;
      lbnd[ 1 ] = data->lbnd[ 1 ];
      ubnd[ 1 ] = lbnd[ 1 ] + data->dims[ 1 ] - 1;
   } else {
      lbnd[ 0 ] = data->lbnd[ 1 ];
      ubnd[ 0 ] = lbnd[ 1 ] + data->dims[ 1 ] - 1;
      lbnd[ 1 ] = data->lbnd[ 2 ];
      ubnd[ 1 ] = lbnd[ 2 ] + data->dims[ 2 ] - 1;
   }

/* Branch on data type. First _DOUBLE. */
   if( data->dtype == SMF__DOUBLE ) {

/* Create and map the output NDF. */
      ndfCreat( param1, "_DOUBLE", 2, lbnd, ubnd, &indf, status );
      ndfMap( indf, "Data", "_DOUBLE", "Write", (void **) &ipd, &el,
              status );

/* Copy the bolometer values into the NDF Data array. */
      d_data = (double *)(data->pntr)[0] + itime*tstride;
      for( i = 0; i < el; i++ ) {
         *(ipd++) = *d_data;
         d_data += bstride;
      }

/* Now do the same for _INTEGER. */
   } else if( data->dtype == SMF__INTEGER ) {

/* Create and map the output NDF. */
      ndfCreat( param1, "_INTEGER", 2, lbnd, ubnd, &indf, status );
      ndfMap( indf, "Data", "_INTEGER", "Write", (void **) &ipi, &el,
              status );

/* Copy the bolometer values into the NDF Data array. */
      i_data = (int *)(data->pntr)[0] + itime*tstride;
      for( i = 0; i < el; i++ ) {
         *(ipi++) = *i_data;
         i_data += bstride;
      }
   }

/* Get the WCS FrameSet describing the time slice, and store it in the NDF */
   smf_tslice_ast( data, itime, 1, NO_FTS, status );
   ndfPtwcs( data->hdr->wcs, indf, status );

/* Annul the NDF. */
   ndfAnnul( &indf, status );

   return 1;
}


/* Return a KeyMap holding information about the columns in the catalogue
   read from the supplied open file pointer.
   --------------------------------------------------------------------- */

static AstKeyMap *GetHeader( FILE *fp, int *status ) {

/* Local Variables: */
   AstKeyMap *result;
   AstKeyMap *coldes;
   char buf[ BUFSIZE ];
   char tbuf[ BUFSIZE ];
   char **fields;
   char *p;
   int i;
   int nfield;

/* Check inherited status */
   if( *status != SAI__OK ) return NULL;

/* Create the returned KeyMap. */
   result = astKeyMap( " " );

/* Create a secondary KeyMap to hold column descriptions, and store it in
   the returned KeyMap. */
   coldes = astKeyMap( " " );
   astMapPut0A( result, "XXCOLDES", coldes, NULL );

/* Ensure the file is re-wound. */
   rewind( fp );

/* Read lines from the file, until one is found that does not start with
   a comment character. */
   while( fgets( buf, BUFSIZE, fp ) && buf[ 0 ] == '#' ) {

/* If this line is a column description, extract the column name (and
   convert to upper case) and its description and store them in the
   coldes keyMap. */
      fields = astChrSplitRE( buf, "^# +(\\w+) +- +(.+?)\\s*$", &nfield, NULL );
      if( fields && nfield == 2 ) {

         p = fields[ 0 ] - 1;
         while( *(++p) ) *p = toupper( *p );

         astMapPut0C( coldes, fields[0], fields[1], NULL );
         for( i = 0; i < nfield; i++ ) fields[ i ] = astFree( fields[ i ] );
         fields = astFree( fields );

/* Otherwise take a copy of the line, minus the leading #. */
      } else {
         one_strlcpy( tbuf, buf + 1, sizeof(tbuf), status );
      }
   }

/* Get the column names from the last comment line read, convert to upper
   case, and store the corresponding zero-based index in the returned KeyMap.
   Also store the name for each column index. */
   fields = astChrSplit( tbuf, &nfield );
   for( i = 0; i < nfield; i++ ) {

      p = fields[ i ] - 1;
      while( *(++p) ) *p = toupper( *p );

      astMapPut0I( result, fields[ i ], i, NULL );

      sprintf( tbuf, "XXCOL%d", i );
      astMapPut0C( result, tbuf, fields[ i ], NULL );

      fields[ i ] = astFree( fields[ i ] );
   }
   fields = astFree( fields );

/* Store the number of columns. */
   astMapPut0I( result, "XXNCOL", nfield, NULL );

/* Annul objects. */
   coldes = astAnnul( coldes );

/* Rewind the file. */
   rewind( fp );

/* Return the KeyMap. */
   return result;
}

/* Return the index of a column in the supplied catalogue
   --------------------------------------------------------- */

static int GetColIndex( AstKeyMap *header, const char *colname, int *status ){

/* Local Variables; */
   int result;
   int there;

/* Check inherited status */
   if( *status != SAI__OK ) return 0;

/* Attempt to get the column index from the keymap. */
   there = astMapGet0I( header, colname, &result );

/* If not there try some standard translations. */
   if( !there ) {
      if( !strcmp( colname, "DBF1" ) ) {
         there = astMapGet0I( header, "DBC1", &result );
      } else if( !strcmp( colname, "DBF2" ) ) {
         there = astMapGet0I( header, "DBC2", &result );
      }
   }

/* Report an error if not found. */
   if( !there ) {
      msgSetc( "C", colname );
      *status = SAI__ERROR;
      errRep( " ", "Supplied catalogue does not contain column ^C.",
              status );
      result = 0;
   }

/* Return the column index. */
   return result;
}

/* Add a new column name and description to a catalogue header
   ---------------------------------------------------------------- */

static void AddColName( AstKeyMap *header, const char *colname, const char *desc,
                        int *status ){

/* Local Variables; */
   int ncol;
   AstKeyMap *coldes;
   char tbuf[ 20 ];

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get the current number of columns in the catalogue. */
   astMapGet0I( header, "XXNCOL", &ncol );

/* Store the new column name and index. */
   astMapPut0I( header, colname, ncol, NULL );
   sprintf( tbuf, "XXCOL%d", ncol );
   astMapPut0C( header, tbuf, colname, NULL );

/* Store the new column count. */
   astMapPut0I( header, "XXNCOL", ncol + 1, NULL);

/* Get the KeyMap holding the column descriptions. */
   astMapGet0A( header, "XXCOLDES", &coldes );

/* Store the new column description. */
   astMapPut0C( coldes, colname, desc, NULL);

/* Free objects */
   coldes = astAnnul( coldes );

}

/* Write a catalogue header to an open file pointer
   --------------------------------------------------------------------- */

static void PutHeader( FILE *fp, AstKeyMap *header, int *status ) {

/* Local Variables; */
   AstKeyMap *coldes;
   const char *coldesc;
   const char *colname;
   char *line;
   char tbuf[ 20 ];
   int icol;
   int nc;
   int ncol;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Write out the standard header line. */
   fprintf( fp, "%s\n", HEADER );
   fprintf( fp, "#\n" );

/* Get the KeyMap holding the column descriptions. */
   astMapGet0A( header, "XXCOLDES", &coldes );

/* Get the current number of columns in the catalogue. */
   astMapGet0I( header, "XXNCOL", &ncol );

/* Initialise buffer for final line of header. */
   nc = 0;
   line = astAppendString( NULL, &nc, "#" );

/* Loop round them all */
   for( icol = 0; icol < ncol; icol++ ) {

/* Get the name of the current column. */
      sprintf( tbuf, "XXCOL%d", icol );
      astMapGet0C( header, tbuf, &colname );

/* Append the column name to the buffer for final line of header. */
      line = astAppendString( line, &nc, " " );
      line = astAppendString( line, &nc, colname );

/* Get the column description, and print it out. */
      if( astMapGet0C( coldes, colname, &coldesc ) ) {
         fprintf( fp, "# %s - %s\n", colname, coldesc );
      }
   }

/* Print out the final lines. */
   fprintf( fp, "#\n" );
   fprintf( fp, "%s\n", line );

/* Free resources */
   line = astFree( line );
   coldes = astAnnul( coldes );

}

/* Search a supplied Mapping for a PolyMap, returning a pointer to it.
   NULL is returned if no PolyMap is found.
   --------------------------------------------------------------------- */
static AstPolyMap *FindPolyMap( AstMapping *map, int *status ){

/* Local Variables; */
   AstPolyMap *result;
   AstMapping *map1;
   AstMapping *map2;
   int series;
   int invert1;
   int invert2;

/* Check inherited status */
   if( *status != SAI__OK ) return NULL;

/* If the supplied Mapping is a PolyMap, return it. */
   if( astIsAPolyMap( map ) ) {
      result = astClone( map );

/* Otherwise, if the supplied Mapping is a series CmpMap, search each of its
   component Mappings. */
   } else if( astIsACmpMap( map ) ) {
      astDecompose( map, &map1, &map2, &series, &invert1, &invert2 );
      if( series ) {
         result = FindPolyMap( map1, status );
         if( !result ) result = FindPolyMap( map2, status );
      } else {
         result = NULL;
      }

/* Otherwise, return NULL */
   } else {
      result = NULL;
   }

   return result;
}

/* Smooth the supplied 1D vals arrays with a box filter.
   ---------------------------------------------------------- */
static double *SmoothZ( dim_t zbox, dim_t nrow, double *in_vals, double *iz_vals,
                        int *status ){

/* Local Variables; */
   dim_t n1, irow, irow0;
   double *result;
   double z0, s1, s2, mean, sig;

/* Check inherited status */
   if( *status != SAI__OK ) return NULL;

   result = astMalloc( nrow*sizeof( double ) ) ;

   for( irow0 = 0; irow0 < nrow; irow0++ ) {
      z0 = iz_vals[ irow0 ];
      s1 = in_vals[ irow0 ];
      if( s1 != AST__BAD ) {
         s2 = s1*s1;
         n1 = 1;
      } else {
         s2 = s1 = 0.0;
         n1 = 0;
      }

      irow = irow0;
      while( ++irow < nrow ) {
         if( iz_vals[ irow ] - z0 <= zbox ) {
            if( in_vals[ irow ] != AST__BAD ) {
               s1 += in_vals[ irow ];
               s2 += in_vals[ irow ]*in_vals[ irow ];
               n1++;
            }
         } else {
            break;
         }
      }

      irow = irow0;
      while( --irow >= 0 ) {
         if( z0 - iz_vals[ irow ] <= zbox ) {
            if( in_vals[ irow ] != AST__BAD ) {
               s1 += in_vals[ irow ];
               s2 += in_vals[ irow ]*in_vals[ irow ];
               n1++;
            }
         } else {
            break;
         }
      }

      if( n1 < 2 ) {
         result[ irow0 ] = AST__BAD;
      } else {
         mean = s1/n1;
         sig = sqrt( s2/n1 - mean*mean );
         if( fabs( in_vals[ irow0 ] - mean ) > 3*sig ) {
            result[ irow0 ] = AST__BAD;
         } else {
            result[ irow0 ] = mean;
         }
      }
   }

   astFree( in_vals );
   return result;
}

static double Mode( dim_t n, double *vals, double *dev, int * status ) {

#define NBIN 10

/* Local Variables: */
   dim_t i;
   dim_t ns;
   dim_t hist[ NBIN ];
   dim_t ibin;
   double s1;
   double s2;
   double mean;
   double var;
   double sigma;
   double a,b,delta;
   double *p;
   double mode;

/* Check inherited status */
   *dev = 0.0;
   if( *status != SAI__OK ) return 0.0;

/* Find mean and sigma */
   s1 = 0.0;
   s2 = 0.0;
   ns = 0;
   p = vals;
   for( i = 0; i < n; i++,p++ ) {
      if( *p != VAL__BADD ) {
         s1 += *p;
         s2 += (*p)*(*p);
         ns++;
      }
   }

   mean = s1/ns;
   var = s2/ns - mean*mean;
   sigma = sqrt( var );

/* Find bin width - NBIN bins over the range mean +/- 2*sigma */
   delta = 4*sigma/NBIN;

/* Initialise histogram. */
   memset( hist, 0, sizeof( dim_t )*NBIN );

/* Form histogram. */
   a = 1.0/delta;
   b = (mean - 2*sigma)/delta;
   p = vals;
   for( i = 0; i < n; i++,p++ ) {
      if( *p != VAL__BADD ) {
         ibin = (*p)*a - b;
         if( ibin >= 0 && ibin < NBIN ) hist[ ibin ]++;
      }
   }

/* Find mode. */
   dim_t imax = 0;
   for( i = 0; i < NBIN; i++ ) {
      if( hist[ i ] > hist[ imax ] ) imax = i;
   }
   mode = (imax + 0.5 + b)/a;

/* Find deviation */
   s1 = 0.0;
   s2 = 0.0;
   ns = 0;
   p = vals;
   for( i = 0; i < n; i++,p++ ) {
      if( *p != VAL__BADD ) {
         s1 = *p - mode;
         s2 += s1*s1;
         ns++;
      }
   }
   *dev = sqrt( s2/ns );

   return mode;
#undef NBIN
}


/* Perform sigma-clipping to identify outlying values in a list of values.
   --------------------------------------------------------------------- */

static void Filter2( dim_t n, double *flags, double *vals, double nsigma,
                     const char *title, int * status ) {

/* Local Variables: */
   dim_t i;
   dim_t nrej;
   double *p;
   double dev;
   double mode;
   int iter;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Find the mode and the RMS deviation about the mode. */
   mode = Mode( n, vals, &dev, status );

/* Do three rejection iterations. */
   nrej = 0;
   for( iter = 0; iter < 3; iter++ ) {

/* Reject points more than "nsigma" deviations from the mode. */
      p = vals;
      for( i = 0; i < n; i++,p++ ) {
         if( *p != VAL__BADD ) {
            if( fabs( mode - *p ) > nsigma*dev ) {
               *p = VAL__BADD;
               flags[ i ] = VAL__BADD;
               nrej++;
            }
         } else {
            flags[ i ] = VAL__BADD;
         }
      }
   }

   printf("Mode filtering %s removed %d rows\n", title, (int) nrej );

}



/* Display details of times slices in which the reference position is close
   to a nomiated bolometer. */

static int FindSlices( const char *pxbol, const char *pybol, const char *pradius,
                       smfData *data, int *status ) {


/* Local Variables: */
   AstFrameSet *swcsin = NULL;
   const char *srefis;
   double ox;
   double oy;
   double gx;
   double gy;
   float radius, r2;
   dim_t itime;
   int ix, iy;

/* Check inherited status */
   if( *status != SAI__OK ) return 0;

/* Get the required parameter values. */
   parGet0i( pxbol, &ix, status );
   parGet0i( pybol, &iy, status );
   parGet0r( pradius, &radius, status );

/* If any parameters were not supplied, annul the error and return. */
   if( *status == PAR__NULL ) {
      errAnnul( status );
      return 0;
   }

   astBegin;

/* Store squared radius */
   r2 = radius*radius;

/* Loop over all time slices. */
   for( itime = 0; itime <  data->dims[ 2 ]; itime++ ) {

/* Get the WCS FrameSet describing the time slice. Current frame is AZEL
   at the epoch of the time slice. */
      smf_tslice_ast( data, itime, 1, NO_FTS, status );
      swcsin = data->hdr->wcs;

/* Ensure the current Frame is offsets from the reference point. */
      srefis = astGetC( swcsin, "SkyRefIs" );
      astSet( swcsin, "SkyRefIs=ORIGIN" );

/* Get the grid coords of the reference poition. */
      ox = 0.0;
      oy = 0.0;
      astTran28( swcsin, 1, &ox, &oy, 0, &gx, &gy );

      astSetC( swcsin, "SkyRefIs", srefis );

/* Check they are good. */
      if( gx != AST__BAD && gy != AST__BAD ) {

/* See if the reference point is close to the specified bolometer. */
         gx -= ix;
         gy -= iy;
         if( gx*gx + gy*gy < r2 ) {
            printf( "%d: %g %g\n", (int) itime, gx + ix, gy + iy );
         }
      }
   }

/* Export the swcsin pointer, since it is annulled explicitly when the
   input data file is closed. */
   astExport( swcsin );
   astEnd;

   return 1;
}




/* Create a ascii catalogue containing the grid coords of the reference
   point in every slice of an input time serices cube. */

static int ShowSlices( const char *param, smfData *data, int *status ) {


/* Local Variables: */
   AstFrameSet *swcsin = NULL;
   const char *srefis;
   double gx;
   double gy;
   double ox;
   double oy;
   dim_t itime;
   char outcat[ 255 ];
   FILE *fd = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return 0;

/* Get the name of the output catalogue and open it. */
   parGet0c( param, outcat, 254, status );
   if( *status == PAR__NULL ) {
      errAnnul( status );
   } else if( *status == SAI__OK ) {
      fd = fopen( outcat, "w" );
      if( !fd && *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSetc ( "CAT", outcat );
         errRep( " ", "Output catalogue '^CAT' could not be opened "
                 "for writing.", status );
      }
   }

/* Return if no file was opened. */
   if( ! fd ) return 0;

   fprintf( fd, "#itime gx gy\n" );

   astBegin;

/* Loop over all time slices. */
   for( itime = 0; itime <  data->dims[ 2 ]; itime++ ) {

/* Get the WCS FrameSet describing the time slice. Current frame is AZEL
   at the epoch of the time slice. */
      smf_tslice_ast( data, itime, 1, NO_FTS, status );
      swcsin = data->hdr->wcs;

/* Ensure the current Frame is offsets from the reference point. */
      srefis = astGetC( swcsin, "SkyRefIs" );
      astSet( swcsin, "SkyRefIs=ORIGIN" );

/* Get the grid coords of the reference poition. */
      ox = 0.0;
      oy = 0.0;
      astTran28( swcsin, 1, &ox, &oy, 0, &gx, &gy );

/* Reset SkyRefIs. */
      astSetC( swcsin, "SkyRefIs", srefis );

/* Check they are good. */
      if( gx != AST__BAD && gy != AST__BAD ) {
         fprintf( fd, "%d %g %g\n", (int) itime, gx, gy );
      } else {
         fprintf( fd, "%d null null\n", (int) itime );
      }
   }

/* Export the swcsin pointer, since it is annulled explicitly when the
   input data file is closed. */
   astExport( swcsin );
   astEnd;

   fclose( fd );

   return 1;
}


