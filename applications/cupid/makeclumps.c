#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/hds.h"
#include "star/kaplibs.h"
#include "ast.h"
#include "par.h"
#include "prm.h"
#include "star/pda.h"
#include "star/thr.h"
#include "cupid.h"
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#include <unistd.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

/* Local Constants: */
/* ================ */
#define DTOR  0.017453292519943295769237

/* Global Variables: */
/* ================= */
/* A structure holding the global parameters needed by the function
   cupidGCmodel. Is is declared in cupidGaussClumps. */
extern CupidGC cupidGC;

/* Local data types: */
/* ================= */
typedef struct MakeclumpsData {
   size_t p1;
   size_t p2;
   float *pin;
   float *pout;
   gsl_rng *r;
   float rms;
   unsigned long int seed;
} MakeclumpsData;

/* Prototypes for local functions */
/* ============================== */
static void cupidMakeclumpsPar( void *job_data_ptr, int *status );

void makeclumps( int *status ) {
/*
*+
*  Name:
*     MAKECLUMPS

*  Purpose:
*     Create simulated data containing clumps and noise.

*  Language:
*     C

*  Type of Module:
*     ADAM A-task

*  Description:
*     This application creates a new 1-, 2- or 3-dimensional NDF containing
*     a collection of clumps with background noise. It also creates a
*     catalogue containing the clump parameters.
*
*     The clumps profiles are Gaussian, with elliptical isophotes. The
*     values of each parameter defining the clump shape can be either
*     fixed at a constant value or selected from a given probability
*     distribution. The clump positions can be distributed randomly, or
*     may be on a regular grid (see parameter GRID).

*  Usage:
*     makeclumps out outcat

*  ADAM Parameters:
*     ANGLE( 2 ) = _REAL (Read)
*        Defines the distribution from which the spatial position angle of
*        the major axis of the elliptical clump is chosen. Values should
*        be supplied in units of degrees. See parameter  PARDIST for
*        additional information. Note, angles are always taken from a
*        uniform distribution, irrespective of the setting of PARDIST.
*        [current value]
*     BEAMFWHM = _REAL (Read)
*        The spatial FHWM (Full Width at Half Max) of the instrumental beam,
*        in pixels. The generated clumps are smoothed with a Gaussian beam
*        of this FWHM, before noise is added. No spatial smoothing is
*        performed if BEAMFWHM is zero. See also parameter PRECAT.
*        [current value]
*     DECONV = _LOGICAL (Read)
*        If TRUE, the clump properties stored in the output catalogue
*        will be modified to take account of the smoothing caused by the
*        instrumental beam width. Note, if parameter PRECAT is TRUE,
*        then this deconvolution has no effect since no smoothing has
*        been applied to the clumps at the time when the catalogue is
*        created. [TRUE]
*     FWHM1( 2 ) = _REAL (Read)
*        Defines the distribution from which the FWHM (Full Width at Half
*        Max) for pixel axis 1 of each clump is chosen. Values should be
*        supplied in units of pixel. See parameter PARDIST for additional
*        information. [current value]
*     FWHM2( 2 ) = _REAL (Read)
*        Defines the distribution from which the FWHM (Full Width at Half
*        Max) for pixel axis 2 of each clump is chosen. Values should be
*        supplied in units of pixel. See parameter PARDIST for additional
*        information. [current value]
*     FWHM3( 2 ) = _REAL (Read)
*        Defines the distribution from which the FWHM (Full Width at Half
*        Max) for pixel axis 3 of each clump is chosen. Values should be
*        supplied in units of pixel. See parameter PARDIST for additional
*        information. [current value]
*     GRID = _INTEGER (Read)
*        If null (!), the clump centres are distributed randomly over the
*        array. If not null, the clump centres are positioned on a regular
*        grid with dimensions specified by parameter NCLUMPS. The grid
*        occupies the entire output array, excluding a border of width
*        equal to the integer value supplied for parameter GRID (in
*        pixels). [!]
*     LBND() = _INTEGER (Read)
*        The lower pixel bounds of the output NDF. The number of values
*        supplied (1, 2 or 3) defines the number of pixel axes in the output
*        NDF (an error is reported if the number of values supplied for LBND
*        and UBND differ). If an NDF is supplied for parameter LIKE, the
*        suggested defaults for this parameter will be the lower pixel
*        bounds of the supplied NDF.
*     LIKE = NDF (Read)
*        An NDF from which to inherited WCS information. If a null (!)
*        value is supplied, the output catalogue will hold values in
*        pixel coordinates, and there will be no WCS in any of the output
*        NDFs. [!]
*     MODEL = NDF (Write)
*        The NDF to receive the simulated data, excluding noise. A CUPID
*        extension is added to this NDF, containing information about each
*        clump in the same format as produced by the FINDCLUMPS command. This
*        includes an NDF holding an of the individual clump.
*     NCLUMP() = _INTEGER (Read)
*        The number of clumps to create. If parameter GRID is null (!),
*        a single value should be supplied, and the specified number of
*        clumps are positioned randomly over the output array. If
*        parameter GRID is not null, the number of values supplied should
*        match the number of pixel axes in the output array, and each
*        value is then the number of clumps along the corresponding
*        pixel axis. Note, any clumps that touch an edge of the aray will
*        be excluded from the output array.
*     OUT = NDF (Write)
*        The NDF to receive the simulated data, including instrumental
*        blurring and noise.
*     OUTCAT = FILENAME (Write)
*        The output catalogue in which to store the clump parameters.
*        There will be one row per clump, with the following columns:
*
*        - Peak1: The position of the clump peak value on axis 1.
*        - Peak2: The position of the clump peak value on axis 2.
*        - Peak3: The position of the clump peak value on axis 3.
*        - Cen1: The position of the clump centroid on axis 1.
*        - Cen2: The position of the clump centroid on axis 2.
*        - Cen3: The position of the clump centroid on axis 3.
*        - Size1: The size of the clump along pixel axis 1.
*        - Size2: The size of the clump along pixel axis 2.
*        - Size3: The size of the clump along pixel axis 3.
*        - Sum: The total data sum in the clump.
*        - Peak: The peak value in the clump.
*        - Volume: The total number of pixels falling within the clump.
*
*        There is also an optional column called "Shape" containing an
*        STC-S description of the spatial coverage of each clump. See
*        parameter SHAPE.
*
*        The coordinate system used to describe the peak and centroid
*        positions is determined by the value supplied for parameter
*        LIKE. If LIKE is null (!), then positions are specified in the
*        pixel coordinate system. In addition, the clump sizes are specified
*        in units of pixels, and the clump volume is specified in units of
*        cubic pixels (square pixels for 2D data). If an NDF is supplied
*        for LIKE, then positions are specified in the current coordinate
*        system of the specified NDF. In addition, the clump sizes and
*        volumes are specified in WCS units. Note, the sizes are still
*        measured parallel to the pixel axes, but are recorded in WCS units
*        rather than pixel units. Celestial coordinate positions are units
*        of degrees, sizes are in units are arc-seconds, and areas in square
*        arc-seconds. Spectral coordinates are in the units displayed by the
*        KAPPA command "ndftrace".
*
*        If the data has less than 3 pixel axes, then the columns
*        describing the missing axes will not be present in the catalogue.
*
*        The catalogue inherits any WCS information from the NDF supplied
*        for parameter LIKE.
*
*        The "size" of the clump on an axis is the RMS deviation of each
*        pixel centre from the clump centroid, where each pixel is
*        weighted by the correspinding pixel data value. This excludes
*        the instrumental smoothing specified by BEAMFWHM and VELFWHM.
*
*        The KAPPA command "listshow" can be used to draw markers at the
*        central positions of the clumps described in a catalogue. For
*        instance, the command "listshow fred plot=mark" will draw
*        markers identifying the positions of the clumps described in
*        file fred.FIT, overlaying the markers on top of the currently
*        displayed image. Specifying "plot=STCS" instead of "plot=mark"
*        will cause the spatial outline of the clump to be drawn if it is
*        present in the catalogue (see parameter SHAPE).
*     PARDIST = LITERAL (Read)
*        The shape of the distribution from which clump parameter values are
*        chosen. Can be "Normal", "Uniform" or "Poisson". The distribution
*        for each clump parameter is specified by its own ADAM parameter
*        containing two values; the mean and the width of the distribution.
*        If PARDIST is "Normal", the width is the standard deviation. If
*        PARDIST is "Uniform", the width is half the range between the
*        maximum and minimum parameter values. In either of these two cases,
*        if a width of zero is supplied, the relevant parameter is given a
*        constant value equal to the specified mean.  If PARDIST is
*        "Poisson", the width is ignored. [current value]
*     PEAK( 2 ) = _REAL (Read)
*        Defines the distribution from which the peak value (above the
*        local background) of each clump is chosen. See parameter PARDIST
*        for additional information. [current value]
*     PRECAT = _LOGICAL (Read)
*        If FALSE, the output catalogue is created from the clumps after
*        the instrumental smoothing specified by parameters BEAMFWHM and
*        VELFWHM has been applied. If TRUE, the catalogue is created from
*        the data before the instrumental smoothing is applied (in which
*        case parameter DECONV has no effect). [FALSE]
*     RMS = _REAL (Read)
*        The RMS (Gaussian) noise to be added to the output data. [current value]
*     SHAPE = LITERAL (Read)
*        Specifies the shape that should be used to describe the spatial
*        coverage of each clump in the output catalogue. It can be set to
*        any of the strings described below. If it is set to "None", the
*        spatial shape of each clump is not recorded in the output
*        catalogue. Otherwise, the catalogue will have an extra column
*        named "Shape" holding an STC-S description of the spatial coverage
*        of each clump. "STC-S" is a textual format developed by the IVOA
*        for describing regions within a WCS - see
*        http://www.ivoa.net/Documents/latest/STC-S.html for details.
*        These STC-S desriptions can be displayed by the KAPPA:LISTSHOW
*        command, or using GAIA. Since STC-S cannot describe regions within
*        a pixel array, it is necessary to provide an NDF to define the
*        WCS (using parameter LIKE) if using this option. An error will be
*        reported if the WCS in the NDF does not contain a pair of celestial
*        sky axes.
*
*        - Polygon: Each polygon will have, at most, 15 vertices. If the data
*        is 2-dimensional, the polygon is a fit to the clump's outer boundary
*        (the region containing all godo data values). If the data is
*        3-dimensional, the spatial footprint of each clump is determined
*        by rejecting the least significant 10% of spatial pixels, where
*        "significance" is measured by the number of spectral channels that
*        contribute to the spatial pixel. The polygon is then a fit to
*        the outer boundary of the remaining spatial pixels.
*
*        - Ellipse: All data values in the clump are projected onto the
*        spatial plane and "size" of the collapsed clump at four different
*        position angles - all separated by 45 degrees - is found (see the
*        OUTCAT parameter for a description of clump "size"). The ellipse
*        that generates the closest sizes at the four position angles is then
*        found and used as the clump shape.
*
*        - Ellipse2: The above method for determining ellipses works well
*        for clumps that are in fact elliptical, but can generate extremely
*        long thin ellipses for clumps are far from being ellitical. The
*        "Ellipse2" option uses a different method for determining the best
*        ellipse based on finding many marginal profiles at one degree
*        intervals of azimuth, and using the longest marginal profile as
*        the major axis. The ellipse is centred at the clump centroid.
*
*        - Ellipse3: The same as "Ellipse2" except that the ellipse is
*        centred at the clump peak, rather than the clump centroid, and
*        the pixel data values are used as weights when forming the mean
*        radial distance at each azimuth angle.
*
*        In general, ellipses will outline the brighter, inner regions
*        of each clump, and polygons will include the fainter outer
*        regions. [None]
*     TRUNC = _REAL (Read)
*        The level (above the local background) at which clumps should be
*        truncated to zero, given as a fraction of the noise level specified
*        by RMS. [current value]
*     UBND() = _INTEGER (Read)
*        The upper pixel bounds of the output NDF. The number of values
*        supplied (1, 2 or 3) defines the number of pixel axes in the output
*        NDF (an error is reported if the number of values supplied for LBND
*        and UBND differ). If an NDF is supplied for parameter LIKE, the
*        suggested defaults for this parameter will be the upper pixel
*        bounds of the supplied NDF.
*     VELFWHM = _REAL (Read)
*        The FWHM of the Gaussian velocity resolution of the instrument, in
*        pixels. The generated clumps are smoothed on the velocity axis with
*        a Gaussian beam of this FWHM, before noise is added. No velocity
*        smoothing is performed if VELFWHM is zero. See also parameter PRECAT.
*        [current value]
*     VGRAD1( 2 ) = _REAL (Read)
*        Defines the distribution from which the projection of the internal
*        velocity gradient vector onto pixel axis 1 of each clump is chosen.
*        Values should be supplied in dimensionless units of "velocity
*        pixels per spatial pixel". See parameter PARDIST for additional
*        information. [current value]
*     VGRAD2( 2 ) = _REAL (Read)
*        Defines the distribution from which the projection of the internal
*        velocity gradient vector onto pixel axis 2 of each clump is chosen.
*        Values should be supplied in dimensionless units of "velocity
*        pixels per spatial pixel". See parameter PARDIST for additional
*        information. [current value]

*  Notes:
*     - If 3D data is created, pixel axes 1 and 2 are the spatial axes,
*     and pixel axis 3 is the velocity axis.
*     - The positions of the clumps are chosen from a uniform
*     distribution on each axis.

*  Synopsis:
*     void makeclumps( int *status );

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     31-OCT-2005 (DSB):
*        Original version.
*     11-DEC-2006 (DSB):
*        Added parameter DECONV.
*     6-NOV-2007 (DSB):
*        "int" and "size_t" have different sizes on 64 bit machines, so
*         do not cast the address of an int to the address of a size_t.
*     15-JAN-2009 (DSB):
*        Remove ILEVEL arguments.
*     22-OCT-2009 (DSB):
*        - Add poisson distribution option.
*        - Add parameters LIKE and SHAPE.
*     19-NOV-2013 (DSB):
*        Ignore degenerate pixel axes when creating clumps.
*     23-APR-2014 (DSB):
*        Added parameter PRECAT.
*     28-MAY-2014 (DSB):
*        Added parameter GRID.
*     29-OCT-2019 (DSB):
*        - Integerise the Z-axis GRID position of each clump, as is
*        already done for the X and Y axes.
*        - Report each clump position to the user if MSG_FILTER = VERBOSE
*     21-NOV-2019 (DSB):
*        Multi-thread.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   AstFrameSet *iwcs;            /* WCS FrameSet */
   HDSLoc *obj;                  /* HDS array of NDF structures */
   HDSLoc *obj_precat;           /* HDS array of NDF structures without smoothing */
   HDSLoc *xloc;                 /* HDS locator for CUPID extension */
   MakeclumpsData *job_data;     /* Memory for job descriptions */
   MakeclumpsData *pdata;        /* Pointer to a single job description */
   ThrWorkForce *wf = NULL;      /* Pointer to a pool of worker threads */
   char attr[ 11 ];              /* AST attribute name */
   char shape[ 10 ];             /* Shape for spatial STC-S regions */
   char text[ 8 ];               /* Value of PARDIST parameter */
   const char *dom;              /* Pointer to AST Domain value */
   const char *envvar;           /* Pointer to environment variable text */
   const gsl_rng_type *type;     /* GSL random number generator type */
   double beamcorr[ 3 ];         /* Beam widths */
   double par[ 11 ];             /* Clump parameters */
   double sum;                   /* Integrated intensity */
   float *ipd2;                  /* Pointer to data array */
   float *ipd;                   /* Pointer to data array */
   float angle[ 2 ];             /* Values for ANGLE parameter */
   float beamfwhm;               /* Value of BEAMFWHM parameter */
   float fwhm1[ 2 ];             /* Values for FWHM1 parameter */
   float fwhm2[ 2 ];             /* Values for FWHM2 parameter */
   float fwhm3[ 2 ];             /* Values for FWHM3 parameter */
   float maxpeak;                /* Largest normalised peak value used */
   float peak[ 2 ];              /* Values for PEAK parameter */
   float pos1[ 2 ];              /* Distribution values for axis 1 position */
   float pos2[ 2 ];              /* Distribution values for axis 2 position */
   float pos3[ 2 ];              /* Distribution values for axis 3 position */
   float rms;                    /* Standard deviation of Gaussian noise */
   float trunc;                  /* Min usable clump value */
   float velfwhm;                /* Value of VELFWHM parameter */
   float vgrad1[ 2 ];            /* Values for VGRAD1 parameter */
   float vgrad2[ 2 ];            /* Values for VGRAD2 parameter */
   hdsdim dims[ 3 ];             /* Dimensions before axis permutation */
   hdsdim grid_delta1;           /* Clump spacing on pixel axis 1 */
   hdsdim grid_delta2;           /* Clump spacing on pixel axis 2 */
   hdsdim grid_delta3;           /* Clump spacing on pixel axis 3 */
   hdsdim ix;                    /* Clump centre on pixel axis 1 */
   hdsdim iy;                    /* Clump centre on pixel axis 2 */
   hdsdim iz;                    /* Clump centre on pixel axis 3 */
   hdsdim lbnd[ 3 ];             /* Lower pixel bounds */
   hdsdim slbnd[ 3 ];            /* Lower bounds of significant pixel axes */
   hdsdim subnd[ 3 ];            /* Upper bounds of significant pixel axes */
   hdsdim ubnd[ 3 ];             /* Upper pixel bounds */
   int addnoise;                 /* Add Gaussian noise to output array? */
   int deconv;                   /* Store deconvolved clump properties */
   int dist;                     /* Clump parameters distribution */
   int grid;                     /* Border for regular grid (-ve if random) */
   int grid_dims[ 3 ];           /* No. of clumps along each pixel axis */
   int i;                        /* Loop count */
   int indf2;                    /* Identifier for output NDF without noise */
   int indf3;                    /* Identifier for input WCS NDF */
   int indf;                     /* Identifier for output NDF with noise */
   int ishape;                   /* STC-S shape for spatial coverage */
   int ivals[ NDF__MXDIM ];      /* 4-byte interger bounds */
   int iw;                       /* Index of worker thread */
   int nc;                       /* Number of clumps created */
   int nclump;                   /* Number of clumps to create */
   int nclumps;                  /* Number of stored clumps */
   int ncold;                    /* Previous value of "nc" */
   int ndim;                     /* Number of pixel axes */
   int nskyax;                   /* Number of sky axes in the current WCS frame */
   int nspecax;                  /* Number of spectral axes in the current WCS frame */
   int nval;                     /* Number of values supplied */
   int nw;                       /* No. of worker threads in thread pool */
   int precat;                   /* Create catalogue before beam smoothing? */
   int sdims;                    /* Number of significant pixel axes */
   size_t area;                  /* Clump area */
   size_t ierr;                  /* Index of first conversion error */
   size_t nel;                   /* Number of elements in array */
   size_t nerr;                  /* Count of conversion errors */
   size_t st;                    /* A size_t that can be passed as an argument */
   size_t step;                  /* Number of pixels per thread */
   unsigned long int seed;       /* Seed for random number generator */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Start an AST context */
   astBegin;

/* Start an NDF context */
   ndfBegin();

/* INitialise pointers. */
   xloc = NULL;

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = thrGetWorkforce( thrGetNThread( "CUPID_THREADS", status ), status );

/* See if WCS is to be inherited from another NDF. If so, get the pixel
   bounds of the NDF and get its WCS FrameSet. Use the bounds to set the
   defaults for parameter LBND and UBND. */
   iwcs = NULL;
   if( *status == SAI__OK ) {
      ndfAssoc( "LIKE", "Read", &indf3, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
      } else {
         ndfBound( indf3, 3, lbnd, ubnd, &ndim, status );

         vecKtoI( 0, ndim, lbnd, ivals, &ierr, &nerr, status );
         if( nerr > 0 && *status == SAI__OK ) {
            errRep( " ", "Lower bounds of input NDF are too large",
                    status );
         }
         parDef1i( "LBND", ndim, ivals, status );
         vecItoK( 0, ndim, ivals, lbnd, &ierr, &nerr, status );

         vecKtoI( 0, ndim, ubnd, ivals, &ierr, &nerr, status );
         if( nerr > 0 && *status == SAI__OK ) {
            errRep( " ", "Upper bounds of input NDF are too large",
                    status );
         }
         parDef1i( "UBND", ndim, ivals, status );
         vecItoK( 0, ndim, ivals, ubnd, &ierr, &nerr, status );

         kpg1Gtwcs( indf3, &iwcs, status );
         ndfAnnul( &indf3, status );
      }
   }

/* Get the required axis bounds. */
   if( iwcs ) {
      parExaci( "LBND", ndim, ivals, status );
   } else {
      parGet1i( "LBND", 3, ivals, &ndim, status );
   }
   vecItoK( 0, ndim, ivals, lbnd, &ierr, &nerr, status );
   parExaci( "UBND", ndim, ivals, status );
   vecItoK( 0, ndim, ivals, ubnd, &ierr, &nerr, status );

/* Get the indices and bounds of the significant pixel axes. */
   sdims = 0;
   for( i = 0; i < ndim; i++ ) {
      if( ubnd[ i ] > lbnd[ i ] ) {
         slbnd[ sdims ] = lbnd[ i ];
         subnd[ sdims ] = ubnd[ i ];
         sdims++;
      }
   }

/* Create the output NDFs. */
   ndfCreat( "OUT", "_REAL", ndim, lbnd, ubnd, &indf, status );
   ndfCreat( "MODEL", "_REAL", ndim, lbnd, ubnd, &indf2, status );

/* If we have a WCS FrameSet, store it in the output NDFs and count the
   number of sky axes in the current Frame. */
   nskyax = 0;
   nspecax = 0;
   if( iwcs ) {
      ndfPtwcs( iwcs, indf, status );
      ndfPtwcs( iwcs, indf2, status );

      for( i = 0; i < ndim; i++ ) {
         sprintf( attr, "Domain(%d)", i + 1 );
         dom = astGetC( iwcs, attr );
         if( dom ) {
            if( !strcmp( dom, "SKY" ) ) {
               nskyax++;
            } else if( !strcmp( dom, "SPECTRUM" ) ||
                       !strcmp( dom, "DSBSPECTRUM" ) ) {
               nspecax++;
            }
         }
      }

/* The "LIKE" NDF may have a different origin to the output NDFs. This
   means that the GRID to PIXEL in the "iwcs" FrameSet will probably not
   describe the pixel origin requested via the LBND parameter.  To correct
   this, annul the existing "iwcs" FrameSet and retrieve it from the
   output NDF (whch has the correct pixel origin). The NDF library
   automatically corrects any FrameSet storing using ndfPtwcs so that the
   GRID->PIXEL Mapping is in agreement with the NDF pixel origin. */
      iwcs = astAnnul( iwcs );
      ndfGtwcs( indf, &iwcs, status );
   }

/* See what STC-S shape should be used to describe each spatial clump. */
   ishape = 0;
   parChoic( "SHAPE", "None", "Ellipse,Ellipse2,Ellipse3,Polygon,None", 1,
             shape, 10, status );
   if( *status == SAI__OK ) {
      if( !strcmp( shape, "POLYGON" ) ) {
         ishape = 2;
      } else if( !strcmp( shape, "ELLIPSE" ) ) {
         ishape = 1;
      } else if( !strcmp( shape, "ELLIPSE2" ) ) {
         ishape = 3;
      } else if( !strcmp( shape, "ELLIPSE3" ) ) {
         ishape = 4;
      }
   }

/* Report an error if an attempt is made to produce STC-S descriptions of
   the spatial coverage of each clump using pixel coords. */
   if( ishape && *status == SAI__OK ) {
      if( !iwcs ) {
         msgSetc( "S", shape );
         msgSetc( "S", "s" );
         *status = SAI__ERROR;
         errRep( " ", "Cannot produce STC-S ^S: no NDF has been supplied "
                 "using parameter LIKE.", status );

      } else if( nskyax < 2 ) {
         msgSetc( "S", shape );
         msgSetc( "S", "s" );
         *status = SAI__ERROR;
         errRep( " ", "Cannot produce STC-S ^S: the current WCS frame in "
                 "the input does not contain a pair of celestial sky axes.",
                 status );

      }
   }

/* See if the output catalogue is to be created from clumps that have not
   been smoothed using the instrumental beam. */
   parGet0l( "PRECAT", &precat, status );

/* Map the DATA component of the output NDFs. */
   ndfMap( indf, "DATA", "_REAL", "WRITE", (void *) &ipd, &nel, status );
   ndfMap( indf2, "DATA", "_REAL", "WRITE", (void *) &ipd2, &nel, status );

/* See if clumps are to be placed on a regular grid. If so, "grid" holds
   the width of the border to place round the outer edge of the grid, in
   pixels. If not, "grid" is set negative. */
   if( *status == SAI__OK ) {
      parGet0i( "GRID", &grid, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         grid = -1;
      }
   }

/* Get the number of clumps to create. */
   if( grid >= 0 ) {
      parExaci( "NCLUMP", ndim, grid_dims, status );
      nclump = grid_dims[0];
      if( ndim > 1 ) nclump *= grid_dims[1];
      if( ndim > 2 ) nclump *= grid_dims[2];
   } else {
      parGet0i( "NCLUMP", &nclump, status );
      grid_dims[0] = 1;
      grid_dims[1] = 1;
      grid_dims[2] = 1;
   }

/* Get the other needed parameters. Which are needed depends on whether
   we are producing 1, 2 or 3 dimensional data. */

   parChoic( "PARDIST", "NORMAL", "NORMAL,UNIFORM,POISSON", 1, text, 8,
             status );
   if( !strcmp( text, "UNIFORM" ) ) {
      dist = 0;
   } else if( !strcmp( text, "NORMAL" ) ) {
      dist = 1;
   } else if( !strcmp( text, "POISSON" ) ) {
      dist = 2;
   }

   parGet0r( "TRUNC", &trunc, status );
   parGet0r( "RMS", &rms, status );

   parGet1r( "FWHM1", 2, fwhm1, &nval, status );
   if( nval == 1 ) fwhm1[ 1 ] = 0.0;

   parGet1r( "PEAK", 2, peak, &nval, status );
   if( nval == 1 ) peak[ 1 ] = 0.0;

   if( sdims == 1 ) {
      parGet0r( "VELFWHM", &velfwhm, status );
      cupidGC.velres_sq = velfwhm*velfwhm;

   } else {

      parGet1r( "ANGLE", 2, angle, &nval, status );
      if( nval == 1 ) angle[ 1 ] = 0.0;
      angle[ 0 ] *= DTOR;
      angle[ 1 ] *= DTOR;

      parGet1r( "FWHM2", 2, fwhm2, &nval, status );
      if( nval == 1 ) fwhm2[ 1 ] = 0.0;
      parGet0r( "BEAMFWHM", &beamfwhm, status );
      cupidGC.beam_sq = beamfwhm*beamfwhm;

      if( sdims == 3 ) {

         parGet1r( "FWHM3", 2, fwhm3, &nval, status );
         if( nval == 1 ) fwhm3[ 1 ] = 0.0;

         parGet0r( "VELFWHM", &velfwhm, status );
         cupidGC.velres_sq = velfwhm*velfwhm;

         parGet1r( "VGRAD1", 2, vgrad1, &nval, status );
         if( nval == 1 ) vgrad1[ 1 ] = 0.0;

         parGet1r( "VGRAD2", 2, vgrad2, &nval, status );
         if( nval == 1 ) vgrad2[ 1 ] = 0.0;

      }
   }

/* There is no axis permutation (i.e. axis 3 will be the velocity axis). */
   cupidGC.dax[ 0 ] = 0;
   cupidGC.dax[ 1 ] = 1;
   cupidGC.dax[ 2 ] = 2;

/* Normalise peak value to the RMS noise level. */
   addnoise = ( rms != 0.0 );
   if( !addnoise ) rms = 1.0;
   peak[ 0 ] /= rms;
   peak[ 1 ] /= rms;

/* If clump positions are random, they are chosen from a uniform distribution.
   Set up the arrays describing the mean value and range on each axis. */
   dims[ 0 ] = subnd[ 0 ] - slbnd[ 0 ] + 1;
   dims[ 1 ] = 1;
   dims[ 2 ] = 1;
   pos1[ 0 ] = 0.5*( dims[ 0 ]  + 1 );
   pos1[ 1 ] = 0.5*dims[ 0 ];
   grid_delta1 = (hdsdim)( 0.5 + ( (float) dims[ 0 ] - 2*grid )/grid_dims[ 0 ] );

   if( sdims > 1 ) {

      dims[ 1 ] = subnd[ 1 ] - slbnd[ 1 ] + 1;
      pos2[ 0 ] = 0.5*( dims[ 1 ]  + 1 );
      pos2[ 1 ] = 0.5*dims[ 1 ];
      grid_delta2 = (hdsdim)( 0.5 + ( (float) dims[ 1 ] - 2*grid )/grid_dims[ 1 ] );

      if( sdims > 2 ) {
         dims[ 2 ] = subnd[ 2 ] - slbnd[ 2 ] + 1;
         pos3[ 0 ] = 0.5*( dims[ 2 ]  + 1 );
         pos3[ 1 ] = 0.5*dims[ 2 ];
         grid_delta3 = (hdsdim)( 0.5 + ( (float) dims[ 2 ] - 2*grid )/grid_dims[ 2 ] );
      }
   }

/* Set the PDA random number seed to a non-repeatable value */
   kpg1Pseed( status );

/* Loop round creating clumps. Clumps that touch the edge of the output
   array will be ignored by cupidGCUpdateArrays, so we keep on looping
   until we have created the required number of clumps (but we do not do
   this if the clumps are on a regular grid). */
   obj = NULL;
   obj_precat = NULL;
   maxpeak = 1.0;
   sum = 0.0;
   nc = 0;
   ncold = 0;
   i = 0;
   ix = grid + grid_delta1/2;
   iy = grid + grid_delta2/2;
   iz = grid + grid_delta3/2;

   while( (grid >= 0 ? i : nc ) < nclump && *status == SAI__OK) {
      if( i++ == 100*nclump ) {
         if( nc == 0 ) {
            *status = SAI__ERROR;
            errRep( "", "Cannot create any clumps - they all touch an "
                    "edge of the output array", status );

         } else if( nc == 1 ) {
            msgOut( "", "Can only create one clump - all others touch an "
                    "edge of the output array.", status );

         } else {
            msgSeti( "N", nc );
            msgOut( "", "Can only create ^N clumps - all others touch an "
                    "edge of the output array.", status );
         }
         break;
      }

/* Determine the parameter values to use for the clump. */
      par[ 0 ] = cupidRanVal( dist, peak, status );
      par[ 1 ] = 0.0;

      if( grid >= 0 ) {
         par[ 2 ] = ix;
      } else {
         par[ 2 ] = (hdsdim) cupidRanVal( 0, pos1, status );
      }

      par[ 3 ] = cupidRanVal( dist, fwhm1, status );

      if( sdims > 1 ) {
         if( grid >= 0 ) {
            par[ 4 ] = iy;
         } else {
            par[ 4 ] = (hdsdim) cupidRanVal( 0, pos2, status );
         }
         par[ 5 ] = cupidRanVal( dist, fwhm2, status );
         par[ 6 ] = cupidRanVal( 0, angle, status );

         if( sdims > 2 ) {
            if( grid >= 0 ) {
               par[ 7 ] = iz;
            } else {
               par[ 7 ] = (hdsdim) cupidRanVal( 0, pos3, status );
            }
            par[ 8 ] = cupidRanVal( dist, fwhm3, status );
            par[ 9 ] = cupidRanVal( dist, vgrad1, status );
            par[ 10 ] = cupidRanVal( dist, vgrad2, status );
         }
      }

      msgOutiff( MSG__VERB, " ", "Creating clump %d at (%g,%g,%g)", status,
                 nc, par[2], par[4], par[7] );

/* Add the clump into the output array. This also creates a "Clump" NDF
   containing the clump data values, appended to the end of the array of
   NDF structures in the HDS object located by "obj". */
      cupidGCUpdateArraysF( NULL, NULL, nel, sdims, dims, par, rms, trunc, 0,
                            0.0, slbnd, &obj, i, 0, 0.0, 0, &area, &sum, status );

/* Update the largest peak value. */
      if( par[ 0 ] > maxpeak ) maxpeak = par[ 0 ];

/* See how many clumps we now have (no NDF will have been created for
   this clump if it touches an edge of the output array). */
      if( obj ) {
         datSize( obj, &st, status );
         nc = (int) st;

/* If an NDF was created, and if PRECAT is true, we create the clump a
   second time, but this time without any beam smoothing. These secondary
   clumps are the ones that go into the output catalogue. */
         if( precat && nc == ncold + 1 ) {
            cupidGC.velres_sq = 0.0;
            cupidGC.beam_sq = 0.0;

/* Create the clump, appending it to the end of the array of NDF structures
   in the HDS object located by "obj_precat". */
            cupidGCUpdateArraysF( NULL, NULL, nel, sdims, dims, par, rms,
                                  trunc, 0, 0.0, slbnd, &obj_precat, i, 0,
                                  0.0, 0, &area, &sum, status );

/* Check we have the same number of NDFs as in the main HDS array. */
            datSize( obj_precat, &st, status );
            if( (int) st != nc && *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRepf( "", "Wrong number of precat NDFs (%d, should be "
                        "%d) - programming error.", status, (int) st, nc );
               break;
            }

/* Re-instate the original beam smoothing parameters. */
            if( sdims == 1 || sdims == 3 ) cupidGC.velres_sq = velfwhm*velfwhm;
            if( sdims == 2 || sdims == 3 ) cupidGC.beam_sq = beamfwhm*beamfwhm;
         }

/* If no clump was created, tell the user. */
      } else {
         msgOutiff( MSG__VERB, " ", "  (clump removed since it touches "
                    "the array edge)", status );
      }

      ncold = nc;

/* Move onto the next point in the regular grid. */
      ix += grid_delta1;
      if( ix > dims[ 0 ] - grid ) {
         ix = grid + grid_delta1/2;
         iy += grid_delta2;
         if( iy > dims[ 1 ]  - grid ) {
            iy = grid + grid_delta2/2;
            iz += grid_delta3;
         }
      }

   }

/* Create the output data array by summing the contents of the NDFs
   describing the beam-smoothed clumps. */
   msgOutiff( MSG__VERB, " ", "Creating noise-free output array", status );
   cupidSumClumps( CUPID__FLOAT, wf, NULL, sdims, slbnd, subnd, nel, obj,
                   NULL, ipd2, "GAUSSCLUMPS", status );

/* Add Gaussian noise to the beam-smoothed data. This operation is
   multi-threaded for speed. */
   msgOutiff( MSG__VERB, " ", "Adding noise to output array", status );

/* First store the number of worker threads in the work force. */
   nw = wf ? wf->nworker : 1;

/* Allocate job data for threads */
   job_data = astMalloc( nw*sizeof(*job_data) );
   if( *status == SAI__OK ) {

/* Get the default GSL random number generator type, and the seed (read
   the seed from environment variable STAR_SEED, or use a random seed formed
   from the time and the process ID). */
      if( addnoise ) {
         type = gsl_rng_default;
         envvar = getenv( "STAR_SEED" );
         if( envvar ) {
            seed = strtol( envvar, NULL, 10 );
         } else {
            seed = (int) time( NULL ) + (int) getpid();
         }
      } else {
         type = NULL;
         seed = 0;
      }

/* Get the number of pixels to process in each thread. */
      step = nel/nw;
      if( step == 0 ) step = 1;

/* Allocate job data for threads, and store the range of pixels to be
   processed by each one. Ensure that the last thread picks up any left-over
   pixels. Store the rest of the job description, and then submit the job
   to the workforce. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->p1 = iw*step;
         if( iw < nw - 1 ) {
            pdata->p2 = pdata->p1 + step;
         } else {
            pdata->p2 = nel - 1;
         }

         pdata->pin = ipd2;
         pdata->pout = ipd;
         pdata->r = type ? gsl_rng_alloc( type ) : NULL;
         pdata->seed = seed + 1234*iw;
         pdata->rms = rms;

         thrAddJob( wf, 0, pdata, cupidMakeclumpsPar, 0, NULL, status );
      }

/* Wait for all the jobs to complete. */
      thrWait( wf, status );
   }

/* Create a CUPID extension in the output model NDF.*/
   ndfXnew( indf2, "CUPID", "CUPID_EXT", 0, NULL, &xloc, status );

/* See if deconvolved values are to be stored in the output catalogue. */
   parGet0l( "DECONV", &deconv, status );

/* Store the clump properties in the output catalogue. */
   msgOutiff( MSG__VERB, " ", "Storing clump properties in catalogue", status );
   if( precat ) {
      beamcorr[ 0 ] = 0.0;
      beamcorr[ 1 ] = 0.0;
      beamcorr[ 2 ] = 0.0;
      cupidStoreClumps( "OUTCAT", NULL, NDF__NOID, xloc, obj_precat, sdims, 0,
                        1, ishape, 2, beamcorr, "Output from CUPID:MAKECLUMPS",
                        1, iwcs, "", NULL, NULL, &nclumps, status );
   } else {
      beamcorr[ 0 ] = beamfwhm;
      beamcorr[ 1 ] = beamfwhm;
      beamcorr[ 2 ] = velfwhm;
      cupidStoreClumps( "OUTCAT", NULL, NDF__NOID, xloc, obj, sdims, deconv, 1,
                        ishape, 2, beamcorr, "Output from CUPID:MAKECLUMPS", 1,
                        iwcs, "", NULL, NULL, &nclumps, status );
   }

/* Release the extension locator.*/
   datAnnul( &xloc, status );

/* Release the HDS object containing the list of NDFs describing the
   clumps. */
   if( obj ) datAnnul( &obj, status );
   if( obj_precat ) datAnnul( &obj_precat, status );

/* Free thread resources. */
   if( job_data ) {
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         if( pdata->r ) gsl_rng_free( pdata->r );
      }
      job_data = astFree( job_data );
   }

/* End the NDF context */
   msgOutiff( MSG__VERB, " ", "Closing NDFs", status );
   ndfEnd( status );

/* End the AST context */
   astEnd;

/* If an error has occurred, issue another error report identifying the
   program which has failed (i.e. this one). */
   if( *status != SAI__OK ) {
      errRep( "MAKECLUMPS_ERR", "MAKECLUMPS: Failed to create artificial "
              "clump data.", status );
   }
}


static void cupidMakeclumpsPar( void *job_data_ptr, int *status ) {
/*
*  Name:
*     cupidMakeclumpsPar

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     makeclumps.

*  Invocation:
*      cupidMakeclumpsPar( void *job_data_ptr, int *status );

*  Arguments:
*     job_data_ptr = MakeclumpsData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   MakeclumpsData *pdata;
   float *pin;
   float *pout;
   float rms;
   gsl_rng *r;
   size_t iel;
   size_t p1;
   size_t p2;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (MakeclumpsData *) job_data_ptr;
   p1 = pdata->p1;
   p2 = pdata->p2;
   r = pdata->r;
   rms = pdata->rms;

/* Get a pointer to the first input and output pixel to be processed by
   this thread. */
   pin = pdata->pin + p1;
   pout = pdata->pout + p1;

/* If no noise is to be added, just copy input to output. */
   if( !r ) {
      memcpy( pout, pin, sizeof( *pin )*( pout - pin + 1 ) );

/* If noise is to be added, set the seed for the random number generator
   and then add a Gaussian value - mean 0.0, sigma=rms - onto each pixle
   to be processed by this thread. */
   } else {
      gsl_rng_set( r, pdata->seed );

      for( iel = p1; iel <= p2; iel++ ) {
         *(pout++) = *(pin++) + gsl_ran_gaussian( r, rms );
      }
   }

}



