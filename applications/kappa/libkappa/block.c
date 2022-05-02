#include "sae_par.h"
#include "par_par.h"
#include "prm_par.h"
#include "star/lpg.h"
#include "chr.h"
#include "star/thr.h"
#include "par.h"
#include "ndf.h"
#include "star/kaplibs.h"
#include "par_err.h"
#include "msg_par.h"
#include <math.h>
#include "star/util.h"
#include "mers.h"
#include <string.h>

F77_SUBROUTINE(block)( INTEGER(status) ){
/*
*+
*  Name:
*     BLOCK

*  Purpose:
*     Smooths an NDF using an n-dimensional rectangular box filter.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL BLOCK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application smooths an n-dimensional NDF using a rectangular
*     box filter, whose dimensionality is the same as that of the NDF
*     being smoothed.   Each output pixel is either the mean or the
*     median of the input pixels within the filter box.  The mean
*     estimator provides one of the fastest methods of smoothing an
*     image and is often useful as a general-purpose smoothing
*     algorithm when the exact form of the smoothing point-spread
*     function is not important.
*
*     It is possible to smooth in selected dimensions by setting
*     the boxsize to 1 for the dimensions not requiring smoothing.
*     For example you can apply two-dimensional smoothing to the planes
*     of a three-dimensional NDF (see Parameter BOX).  If it has three
*     dimensions, then the filter is applied in turn to each plane in
*     the cube and the result written to the corresponding plane in the
*     output cube.

*  Usage:
*     block in out box [estimator]

*  ADAM Parameters:
*     BOX() = _INTEGER (Read)
*        The sizes (in pixels) of the rectangular box to be applied to
*        smooth the data.  These should be given in axis order.  A value
*        set to 1 indicates no smoothing along that axis.  Thus, for
*        example, BOX=[3,3,1] for a three-dimensional NDF would apply a
*        3x3-pixel filter to all its planes independently.
*
*        If fewer values are supplied than the number of dimensions of
*        the NDF, then the final value will be duplicated for the
*        missing dimensions.
*
*        The values given will be rounded up to positive odd integers, if
*        necessary, to retain symmetry.
*     ESTIMATOR = LITERAL (Read)
*        The method to use for estimating the output pixel values.  It
*        can be either "Mean" or "Median". ["Mean"]
*     IN = NDF (Read)
*        The input NDF to which box smoothing is to be applied.
*     OUT = NDF (Write)
*        The output NDF which is to contain the smoothed data.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF. A null value will cause
*        the title of the input NDF to be used. [!]
*     WLIM = _REAL (Read)
*        If the input image contains bad pixels, then this parameter
*        may be used to determine the number of good pixels which must
*        be present within the smoothing box before a valid output
*        pixel is generated.  It can be used, for example, to prevent
*        output pixels from being generated in regions where there are
*        relatively few good pixels to contribute to the smoothed
*        result.
*
*        By default, a null (!) value is used for WLIM, which causes
*        the pattern of bad pixels to be propagated from the input
*        image to the output image unchanged. In this case, smoothed
*        output values are only calculated for those pixels which are
*        not bad in the input image.
*
*        If a numerical value is given for WLIM, then it specifies the
*        minimum fraction of good pixels which must be present in the
*        smoothing box in order to generate a good output pixel.  If
*        this specified minimum fraction of good input pixels is not
*        present, then a bad output pixel will result, otherwise a
*        smoothed output value will be calculated.  The value of this
*        parameter should lie between 0.0 and 1.0 (the actual number
*        used will be rounded up if necessary to correspond to at least
*        1 pixel). [!]

*  Examples:
*     block aa bb 9
*        Smooths the two-dimensional image held in the NDF structure aa,
*        writing the result into the structure bb.  The smoothing box is
*        9 pixels square.  If any pixels in the input image are bad,
*        then the corresponding pixels in the output image will also be
*        bad.  Each output pixel is the mean of the corresponding input
*        pixels.
*     block spectrum spectrums 5 median title="Smoothed spectrum"
*        Smooths the one-dimensional data in the NDF called spectrum
*        using a box size of 5 pixels, and stores the result in the NDF
*        structure spectrums.  Each output pixel is the median of the
*        corresponding input pixels.  If any pixels in the input image
*        are bad, then the corresponding pixels in the output image
*        will also be bad.  The output NDF has the title "Smoothed
*        spectrum".
*     block ccdin(123,) ccdcol [1,9]
*        Smooths the 123rd column in the two-dimensional NDF called
*        ccdin using a box size of 9 pixels, and stores the result in
*        the NDF structure ccdcol.  The first value of the smoothing box
*        is ignored as the first dimension has only one element.  Each
*        output pixel is the mean of the corresponding input pixels.
*     block in=image1 out=image2 box=[5,7] estimator=median
*        Smooths the two-dimensional image held in the NDF structure
*        image1 using a rectangular box of size 5x7 pixels.  The
*        smoothed image is written to the structure image2.  Each
*        output pixel is the median of the corresponding input pixels.
*     block etacar etacars box=[7,1] wlim=0.6
*        Smooths the specified image data using a rectangular box 7x1
*        pixels in size.  Smoothed output values are generated only if
*        at least 60% of the pixels in the smoothing box are good,
*        otherwise the affected output pixel is bad.
*     block in=cubein out=cubeout box=[3,3,7]
*        Smooths the three-dimensional NDF called cubein using a box
*        that has three elements along the first two axes and seven
*        along the third.  The smoothed cube is written to NDF cubeout.
*     block in=cubein out=cubeout box=[3,1,7]
*        As the previous example, except that planes comprising the
*        first and third axes are smoothed independently for all
*        lines.

*  Related Applications:
*     KAPPA: CONVOLVE, FFCLEAN, GAUSMOOTH, MEDIAN; Figaro: ICONV3,
*     ISMOOTH, IXSMOOTH, MEDFILT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, WCS and HISTORY components of the input NDF
*     and propagates all extensions.  In addition, if the mean estimator
*     is used, the VARIANCE component is also processed.  If the median
*     estimator is used, then the output NDF will have no VARIANCE
*     component, even if there is a VARIANCE component in the input
*     NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.  The bad-pixel flag is also written for the data and
*     variance arrays.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single-precision floating point, or double
*     precision, if appropriate.

*  Timing:
*     When using the mean estimator, the execution time is
*     approximately proportional to the number of pixels in the image
*     to be smoothed and is largely independent of the smoothing box
*     size. This makes the routine particularly suitable for applying
*     heavy smoothing to an image.  Execution time will be
*     approximately doubled if a variance array is present in the input
*     NDF.
*
*     The median estimator is much slower than the mean estimator, and
*     is heavily dependent on the smoothing box size.

*  Copyright:
*     Copyright (C) 1990, 1992, 1994 Science & Engineering Research
*     Council.  Copyright (C) 1995, 1998, 2004 Central Laboratory of
*     the Research Councils.  Copyright (C) 2005 Particle Physics &
*     Astronomy Research Council.  .Copyright (C) 2009 Science &
*     Facilities Research Council.  All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     18-SEP-1990 (RFWS):
*        Original version.
*     1990 November 28 (MJC):
*        Corrected call to generic routine for double-precision
*        variance.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 4 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.
*     16-DEC-1994 (DSB):
*        Title propagated by default from input to output NDF.
*        Introduced ESTIMATOR parameter.  Replaced AIF VM calls with PSX
*        calls.
*     1995 March 16 (MJC):
*        Made to operate on one-dimensional arrays.  Enabled the writing
*        of the bad-pixel flags.  Usage and examples to lowercase.
*        Added a "Related Applications" section.  Fixed bug that
*        attempted to create an output variance array when the median
*        estimator was selected.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     23-MAR-2005 (DSB):
*        Added support for smoothing all two-dimensional planes in a
*        three-dimensional cube.
*     2009 October 3 (MJC):
*        Made to operate on n-dimensional data.  Withdraw AXES parameter
*        added at the previous modification.  Added two examples.
*     2022-APR-25 (DSB):
*        Re-written in C.
*     {enter_further_changes_here}

*-
*/

/* Local Variables: */
   ThrWorkForce *wf;     /* Pointer to pool of worker threads */
   char buffer[ 31 ];    /* Text buffer for message */
   char comp[ 14 ];      /* List of components to process */
   char dtype[ NDF__SZFTP + 1 ];   /* Numeric type for output arrays */
   char estim[ 7 ];      /* Method to estimate smoothed values */
   char itype[ NDF__SZTYP + 1 ];   /* Numeric type for processing */
   float wlim;           /* Fraction of good pixels required */
   int bad;              /* Check for bad input pixels? */
   int baddat;           /* Bad values stored in o/p data array? */
   int badout;           /* Bad pixels in output array? */
   int badvar;           /* Bad values stored in o/p var. array? */
   int box[ NDF__MXDIM ];/* Smoothing box size */
   int boxsiz;           /* Number of pixels in smoothing box */
   int dim[ NDF__MXDIM ];/* NDF dimensions */
   int el;               /* Number of mapped array elements */
   int i;                /* Loop counter */
   int ibox[ NDF__MXDIM ];         /* Smoothing box half-size */
   int lbnd[ NDF__MXDIM + 1 ];     /* Lower bounds of NDF pixel axes */
   int ndf1;             /* Identifier for input NDF */
   int ndf2;             /* Identifier for output NDF */
   int ndf2b;            /* Section of output NDF to be filled */
   int ndim;             /* Number of dimensions in the NDF */
   int nlim;             /* Minimum good pixel limit */
   int nval;             /* Number of values obtained */
   int sambad;           /* Propagate bad pixels to same place? */
   int state;            /* State of BOX parameter */
   int ubnd[ NDF__MXDIM + 1 ];     /* Upper bounds of NDF pixel axes */
   int var;              /* Variance array present? */
   int wdim;             /* Dimension of accumulation workspaces */
   size_t cpos;          /* Character position */
   void *pntr1[ 2 ];     /* Pointers for mapped input arrays */
   void *pntr2[ 2 ];     /* Pointers for mapped output arrays */
   void *wpntr1;         /* Mapped workspace pointer */
   void *wpntr2;         /* Mapped workspace pointer */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Find the number of cores/processors available and create a pool of
   threads of the same size. The size is obtained from the environment
   variable if defined, or the number of CPUs/cores otherwise. */
   wf = thrGetWorkforce( thrGetNThread( "KAPPA_THREADS", status ),
                         status );

/* Begin an NDF context. */
   ndfBegin();

/* Get the NDF containing the input data. */
   lpgAssoc( "IN", "READ", &ndf1, status );

/* Obtain the bounds of the NDF. */
   ndfBound( ndf1, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Exit if an error occurred.  This is needed because the significant
   dimensions are used as array indices. */
   if( *status != SAI__OK ) goto L999;

/* Determine its dimensions.  Unused dimensions need to be passed to 1
   for the n-dimensional looping inside the subfunctions that perform the
   smoothing. */
   kpg1Filli( 1, NDF__MXDIM, dim, status );
   for( i = 0; i < ndim; i++ ){
      dim[ i ] = ubnd[ i ] - lbnd[ i ] + 1;
   }

/* Obtain the smoothing box sizes, duplicating the value if fewer values
   than the number of dimensions is supplied.  Since undeclared values
   adopt the final value, inform the user of the NDF's dimensionality.
   It is not needed for vectors as "parGdrvi" will warn if too many
   values are presented.  Prefer not to use "parPromt", as this
   undermines the interface-file concept.

   Each box size must be a positive odd number, so derive "ibox" so that
   "box" = 2*"ibox"+1 is rounded up if necessary. */
   lpgState( "BOX", &state, status );
   if( ndim > 1 && state != PAR__ACTIVE ) {
      star_strlcpy( buffer, " ", sizeof( buffer ) );
      cpos = 1;
      chrAppnd( "The data have", buffer, sizeof( buffer ), &cpos );
      cpos++;
      chrPuti( ndim, buffer, sizeof(buffer), &cpos );
      chrAppnd( " dimensions.", buffer, sizeof( buffer ), &cpos );
      msgOutif( MSG__NORM, "BLOCK_DIMS", buffer, status );
   }

   parGdrvi( "BOX", ndim, 1, VAL__BADI, box, &nval, status );
   if( *status != SAI__OK ) goto L999;
   if( nval < ndim ) {
      for( i = nval; i < ndim; i++ ){
         box[ i ] = box[ nval - 1 ];
      }
   }

   boxsiz = 1;
   for( i = 0; i < ndim; i++ ){
      if( dim[ i ] == 1 ) {
         ibox[ i ] = 0;
      } else {
         ibox[ i ] = NDF_MAX( box[ i ], 1 )/2;
      }
      boxsiz = boxsiz*( 2*ibox[ i ] + 1 );
   }

/* Obtain the minimum fraction of good pixels which should be used to
   calculate an output pixel value.  Test if a null value is specified
   and set "sambad" appropriately, annulling the error. */
   errMark();
   sambad = 0;
   parGdr0r( "WLIM", 0.5, 0.0, 1.0, 0, &wlim, status );
   if( *status == PAR__NULL ) {
      sambad = 1;
      errAnnul( status );

/* Derive the minimum number of pixels, using at least one. */
   } else {
      nlim = NDF_MAX( 1, round( (float)( boxsiz )*wlim ) );
   }
   errRlse();

/* Obtain the method to use for estimating the smoothed pixel values. */
   parChoic( "ESTIMATOR", "Mean", "Mean,Median", 0, estim, sizeof(estim),
             status );

/* Determine if a variance component is present and derive a list of
   the components to be processed.  At the moment, VARIANCE components
   can only be processed if the mean estimator is being used. */
   ndfState( ndf1, "Variance", &var, status );
   if( var && !strcmp( estim, "MEAN" ) ) {
      star_strlcpy( comp, "Data,Variance", sizeof( comp ) );
   } else {
      star_strlcpy( comp, "Data", sizeof( comp ) );
   }

/* Determine the numeric type to be used for processing the input
   arrays.  This application supports single- and double-precision
   floating-point processing. */
   ndfMtype( "_REAL,_DOUBLE", ndf1, ndf1, comp, itype, sizeof(itype),
             dtype, sizeof(dtype), status );

/* Create an output NDF based on the input one.  Set an appropriate
   numeric type for the output arrays. */
   lpgProp( ndf1, "WCS,Axis,Quality,Units", "OUT", &ndf2, status );
   ndfStype( dtype, ndf2, comp, status );

/* See if it is necessary to check for bad pixels in the input arrays. */
   ndfMbad( 1, ndf1, ndf1, comp, 0, &bad, status );

/* Obtain workspace arrays for the smoothing algorithm and map them.
   First compute the dimensions of the accumulation arrays.  These
   provide storage for the sums at each dimensionality concatenated
   into single sum and pixel counter arrays. */
   if( !strcmp( estim, "MEAN" ) ) {
      wpntr1 = NULL;
      wpntr2 = NULL;
   } else {
      wpntr1 = astCalloc( boxsiz, kpg1Typsz(itype,status) );
      wpntr2 = astCalloc( boxsiz, VAL__NBI );
   }

/* Only proceed around the loop if everything is satisfactory. */
   if( *status != SAI__OK ) goto L990;

/* Initialise bad flags */
   baddat = 0;
   badvar = 0;

/* Map these input and output arrays. */
   ndfMap( ndf1, comp, itype, "READ", pntr1, &el, status );
   ndfMap( ndf2, comp, itype, "WRITE", pntr2, &el, status );
   if( *status != SAI__OK ) goto L990;

/* Apply smoothing to the mapped data array, using the appropriate
   numeric type of processing.

   Real */
   if( !strcmp( itype, "_REAL" ) ) {

      if( !strcmp( estim, "MEAN" ) ) {
         kpgBlonF( wf, bad, sambad, 0, ndim, dim, pntr1[ 0 ], ibox, nlim,
                   pntr2[ 0 ], &badout, status );
      } else {
         kpgBmdnF( wf, bad, sambad, 1, ndim, dim, pntr1[ 0 ], ibox, nlim,
                   pntr2[ 0 ], &badout, wpntr1, wpntr2, status );
      }

/* Double precision */
   } else if( !strcmp( itype, "_DOUBLE" ) ) {

      if( !strcmp( estim, "MEAN" ) ) {
         kpgBlonD( wf, bad, sambad, 0, ndim, dim, pntr1[ 0 ], ibox, nlim,
                   pntr2[ 0 ], &badout, status );
      } else {
         kpgBmdnD( wf, bad, sambad, 1, ndim, dim, pntr1[ 0 ], ibox, nlim,
                   pntr2[ 0 ], &badout, wpntr1, wpntr2, status );
      }

   }

/* Update the bad-data flag. */
   if( badout ) baddat = 1;

/* If a variance array is present, then also apply smoothing to it.
   At the moment, variances are found only if the mean estimator is
   being used. */
   if( var && !strcmp( estim, "MEAN" ) ) {
      if( !strcmp( itype, "_REAL" ) ) {
         kpgBlonF( wf, bad, sambad, 1, ndim, dim, pntr1[ 1 ], ibox, nlim,
                   pntr2[ 1 ], &badout, status );

      } else if( !strcmp( itype, "_DOUBLE" ) ) {
         kpgBlonD( wf, bad, sambad, 1, ndim, dim, pntr1[ 1 ], ibox, nlim,
                   pntr2[ 1 ], &badout, status );
      }

/* Update the bad data flag. */
      if( badout ) badvar = 1;
   }

/* Indicate whether or not the output data and/or variance array has
   bad  pixels. */
   ndfSbad( baddat, ndf2, "Data", status );
   if( var && !strcmp( estim, "MEAN" ) ) ndfSbad( badvar, ndf2, "Variance", status );

/* Obtain a new title for the output NDF, with the default value
   being the input title. */
   kpg1Ccpro( "TITLE", "Title", ndf1, ndf2, status );

L990:

/* Release the temporary workspace arrays. */
   wpntr1 = astFree( wpntr1 );
   wpntr2 = astFree( wpntr2 );

/* End the NDF context. */
L999:
   ndfEnd( status );

/* If an error occurred, then report contextual information. */
   if( *status != SAI__OK ) {
      errRep( " ", "BLOCK: Error smoothing an NDF using an  "
              "N-dimensional rectangular box filter.", status );
   }

}

