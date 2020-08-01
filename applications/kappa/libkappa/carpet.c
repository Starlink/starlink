#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/kaplibs.h"
#include "star/grp.h"
#include "star/ndg.h"
#include "star/atl.h"
#include "star/one.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include <math.h>
#include <string.h>
#include <stdio.h>

F77_SUBROUTINE(carpet)( INTEGER(STATUS) ){
/*
*+
*  Name:
*     CARPET

*  Purpose:
*     Creates a cube representing a carpet plot of an image.

*  Language:
*     C (designed to be called from Fortran)

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CARPET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new three-dimensional NDF from an
*     existing two-dimensional NDF. The resulting NDF can, for instance,
*     be viewed with the three-dimensional iso-surface facilities of the
*     GAIA image viewer, in order to create a display similar to a
*     "carpet plot" of the image (the iso-surface at value zero represents
*     the input image data values).
*
*     The first two pixel axes (X and Y) in the output cube correspond to the
*     pixel axes in the input image. The third pixel axis (Z) in the output
*     cube is proportional to data value in the input image. The value of
*     a pixel in the output cube measures the difference between the data
*     value implied by its Z-axis position, and the data value of the
*     corresponding pixel in the input image. Two schemes are available
*     (see Parameter MODE): the output pixel values can be either simply
*     the difference between these two data values, or the difference
*     divided by the standard deviation at the corresponding pixel in the
*     input image (as determined either from the VARIANCE component in
*     the input NDF or by Parameter SIGMA).

*  Usage:
*     carpet in out [ndatapix] [range] [mode] [sigma]

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input two-dimensional NDF.
*     MODE = LITERAL (Read)
*        Determines how the pixel values in the output cube are calculated.
*
*        - "Data"  -- the value of each output pixel is equal to the difference
*                     between the data value implied by its position along
*                     the data value axis, and the value of the
*                     corresponding pixel in the input image.
*
*        - "Sigma" -- this is the same as "Data" except that the output
*                     pixel values are divided by the standard deviation
*                     implied either by the VARIANCE component of the
*                     input image, or by the SIGMA parameter.
*
*        ["Data"]
*     NDATAPIX = _INTEGER (Read)
*        The number of pixels to use for the data value axis in the
*        output cube. The pixel origin of this axis will be 1. The
*        dynamic default is the square root of the number of pixels in
*        the input image. This gives a fairly "cubic" output cube.  []
*     OUT = NDF (Write)
*        The output three-dimensional NDF.
*     RANGE = LITERAL (Read)
*        RANGE specifies the range covered by the data value axis (i.e. the
*        third pixel axis) in the output cube. The supplied string should
*        consist of up to three sub-strings, separated by commas.  For all
*        but the option where you give explicit numerical limits, the first
*        sub-string must specify the method to use.  If supplied, the other
*        two sub-strings should be numerical values as described below
*        (default values will be used if these sub-strings are not
*        provided).  The following options are available.
*
*        - lower,upper -- You can supply explicit lower and upper limiting
*        values.  For example, "10,200" would set the lower limit on the
*        output data axis to 10 and its upper limit to 200.  No method name
*        prefixes the two values.  If only one value is supplied, the
*        "Range" method is adopted.  The limits must be within the dynamic
*        range for the data type of the input NDF array component.
*
*        - "Percentiles" -- The default values for the output data axis
*        range are set to the specified percentiles of the input data.  For
*        instance, if the value "Per,10,99" is supplied, then the lowest
*        10% and highest 1% of the data values are beyond the bounds of
*        the output data value axis.  If only one value, p1, is supplied,
*        the second value, p2, defaults to (100 - p1).  If no values are
*        supplied, the values default to "5,95".  Values must be in the
*        range 0 to 100.
*
*        - "Range" -- The minimum and maximum input data values are used.
*        No other sub-strings are needed by this option.  Null (!) is a
*        synonym for the "Range" method.
*
*        - "Sigmas" -- The limits on the output data value axis are set to
*        the specified numbers of standard deviations below and above the
*        mean of the input data.  For instance, if the supplied value is
*        "sig,1.5,3.0", then the data value axis extends from the mean of
*        the input data minus 1.5 standard deviations to the mean plus 3
*        standard deviations.  If only one value is supplied, the second
*        value defaults to the supplied value.  If no values are supplied,
*        both default to "3.0".
*
*        The limits adopted for the data value axis are reported unless
*        Parameter RANGE is specified on the command line.  In this case
*        values are only calculated where necessary for the chosen method.
*
*        The method name can be abbreviated to a single character, and
*        is case insensitive.  The initial default value is "Range".  The
*        suggested defaults are the current values, or ! if these do
*        not exist.  [current value]
*     SIGMA = _REAL (Read)
*        The standard deviation to use if Parameter MODE is set to "Sigma".
*        If a null (!) value is supplied, the standard deviations implied
*        by the VARIANCE component in the input image are used (an error
*        will be reported if the input image does not have a VARIANCE
*        component). If a SIGMA value is supplied, the same value is used
*        to scale all output pixels.  [!]

*  Examples:
*     carpet m31 m31-cube mode=sigma
*        Asssuming the two-dimensional NDF in file m31.sdf contains a
*        VARIANCE component, this will create a three-dimensional NDF called
*        m31-cube in which the third pixel axis corresponds to data value in
*        NDF m31, and each output pixel value is the number of standard
*        deviations of the pixel away from the corresponding input data value.
*        If you then use GAIA to view the cube, an iso-surface at value zero
*        will be a carpet plot of the data values in m31, an iso-surface at
*        value -1.0 will be a carpet plot showing data values one standard
*        deviation below the m31 data values, and an iso-surface at value +1.0
*        will be a carpet plot showing data values one sigma above the
*        m31 data values. This can help to visualise the errors in an image.

*  Implementation Status:
*     -  Any VARIANCE and QUALITY components in the input image are not
*     propagated to the output cube.

*  Copyright:
*     Copyright (C) 2009, 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David S. Berry
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-NOV-2009 (DSB):
*        Original version.
*     2011-03-16 (TIMJ):
*        Use strlcpy rather than strcpy. Use sizeof(*var) for malloc.
*     {enter_further_changes_here}

*-
*/

   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   AstFrame *datafrm;        /* Frame describing data value output axis */
   AstFrameSet *wcs;         /* Pointer to the WCS FrameSet */
   AstWinMap *map;           /* Mapping from GRID to data value */
   Grp *grp;                 /* GRP identifier for configuration settings */
   char cval[ 256 ];         /* Buffer for NDF character component value */
   char mode[ 7 ];           /* Determines nature of output values */
   double *ipdin;            /* Pointer to input Data array */
   double *ipdout;           /* Pointer to output Data array */
   double *ipvin;            /* Pointer to input Variance array */
   double *pin;              /* Pointer to next input Data element */
   double *pout;             /* Pointer to next output Data element */
   double *pvin;             /* Pointer to next input Variance element */
   double *zd;               /* Pointer to next data value for 3rd axis */
   double *zdata;            /* Array of data values for pixels on 3rd axis */
   double dhi;               /* Maximum value for data value axis */
   double dlo;               /* Minimum value for data value axis */
   double ina[ 1 ];          /* Lower GRID limit */
   double inb[ 1 ];          /* Upper GRID limit */
   double outa[ 1 ];         /* Lower data value limit */
   double outb[ 1 ];         /* Upper data value limit */
   double sigma;             /* Stdnard deviation to use */
   double vin;               /* Input variance value */
   int bad;                  /* Any bad pixels present? */
   int dpdef;                /* Dynamic default for NDATAPIX */
   int el;                   /* Number of array elements mapped */
   int hasvar;               /* Does the input NDF have a variance component? */
   int indf1;                /* Identifier for input NDF */
   int indf2;                /* Identifier for output NDF */
   int ix;                   /* Index on first pixel axis */
   int iy;                   /* Index on second pixel axis */
   int iz;                   /* Index on third pixel axis */
   int ndatapix;             /* Number of pixels along output data value axis */
   int sdim[ 3 ];            /* Indices of significant pixel axes */
   int slbnd[ 3 ];           /* Lower pixel bounds of significant pixel axes */
   int subnd[ 3 ];           /* Upper pixel bounds of significant pixel axes */
   size_t size;              /* Size of GRP group */

/* Abort if an error has already occurred. */
   if( *STATUS != SAI__OK ) return;

/* Tell AST to use the supplied status value. */
   astWatch( STATUS );

/* Start AST and NDF contexts. */
   astBegin;
   ndfBegin();

/* Get an identifier for the input image. We use NDG (via kpg1_Rgndf)
   instead of calling ndfAssoc directly since NDF/HDS has problems with
   file names containing spaces, which NDG does not have. */
   kpg1Rgndf( "IN", 1, 1, "", &grp, &size, STATUS );
   ndgNdfas( grp, 1, "READ", &indf1, STATUS );
   grpDelet( &grp, STATUS );

/* See if the NDF has a VARIANCE component. */
   ndfState( indf1, "Variance", &hasvar, STATUS );

/* Now get the WCS FrameSet and significant bounds from the NDF, ensuring
   there are two significant axis in the input NDF. */
   kpg1Asget( indf1, 2, 1, 0, 0, sdim, slbnd, subnd, &wcs, STATUS );

/* Determine what the output pixel values will hold. */
   parChoic( "MODE", "Sigma", "Sigma,Data", 1, mode, 6, STATUS );

/* If required, attempt to get a constant standard deviation value to use
   for all output pixels. */
   if( *STATUS == SAI__OK && !strcmp( "SIGMA", mode ) ) {
      parGdr0d( "SIGMA", -1.0, VAL__SMLD, VAL__MAXD, 0, &sigma, STATUS );
      if( *STATUS == PAR__NULL ) {
         errAnnul( STATUS );
         sigma = 0.0;
         if( ! hasvar ) {
            ndfMsg( "NDF", indf1 );
            *STATUS = SAI__ERROR;
            errRep( " ", "The NDF '^NDF' has no VARIANCE component but a "
                    "null value was supplied for Parameter SIGMA.", STATUS );
         }
      }

/* Mode "Data" is equivalent to mode "Sigma" with a SIGMA value of 1.0. */
   } else {
      sigma = 1.0;
   }

/* Map the input DATA and, if required, VARIANCE component of the input
   image. */
   ndfMap( indf1, "Data", "_DOUBLE", "READ", (void *) &ipdin, &el, STATUS );
   if( sigma == 0.0 ) ndfMap( indf1, "Variance", "_DOUBLE", "READ",
                              (void *) &ipvin, &el, STATUS );

/* Get the number of pixels on the output data-value axis. */
   dpdef = (int) sqrt( (double) el );
   if( dpdef < 2 ) dpdef = 2;
   parGdr0i( "NDATAPIX", dpdef, 2, VAL__MAXI, 1, &ndatapix, STATUS );

/* Get the range for the output data-value axis. */
   bad = 1;
   kpg1Darad( "RANGE", el, ipdin, "Limit,Percentiles,Range,Sigmas",
              &bad, &dlo, &dhi, STATUS );

/* Create the output NDF, initially a 2D NDF propagated from the input
   image. */
   ndfProp( indf1, "Unit", "OUT", &indf2, STATUS );

/* Change its shape to the required 3D shape for the output NDF. */
   slbnd[ 2 ] = 1;
   subnd[ 2 ] = ndatapix;
   ndfSbnd( 3, slbnd, subnd, indf2, STATUS );

/* Create a 1D Frame to represent the data-value axis. */
   datafrm = astFrame( 1, "Domain=DATAVALUE" );

/* If the input NDF has a LABEL component, assign it to the Label
   attribute of this Frame. */
   cval[ 0 ] = '\0';
   ndfCget( indf1, "Label", cval, sizeof(cval), STATUS );
   if( astChrLen( cval ) == 0 ) one_strlcpy( cval, "Data value", sizeof(cval), STATUS );
   astSetC( datafrm, "Label(1)", cval );

/* If the input NDF has a UNIT component, assign it to the Unit
   attribute of this Frame. */
   cval[ 0 ] = '\0';
   ndfCget( indf1, "Unit", cval, sizeof(cval), STATUS );
   if( astChrLen( cval ) > 0 ) astSetC( datafrm, "Unit(1)", cval );

/* Add this Frame into the WCS FrameSet as an extra "data value" axis.
   Map the the GRID coord range [1.0,ndatapix] onto the data value
   range [dlo,dhi]. */
   ina[ 0 ] = 1.0;
   inb[ 0 ] = ndatapix;
   outa[ 0 ] = dlo;
   outb[ 0 ] = dhi;
   map = astWinMap( 1, ina, inb, outa, outb, " " );
   atlAddWcsAxis(  wcs, (AstMapping *) map, datafrm, slbnd + 2, subnd + 2,
                   STATUS );

/* Store the modified WCS FrameSet in the output NDF. */
   ndfPtwcs( wcs, indf2, STATUS );

/* Map the output DATA component as an array of doubles. */
   ndfMap( indf2, "Data", "_DOUBLE", "WRITE", (void *) &ipdout, &el, STATUS );

/* Get an array holding the data value associated with each pixel on the
   new axis. */
   zdata = astMalloc( ndatapix*sizeof( *zdata ) );
   if( *STATUS == SAI__OK ) {
      for( iz = 0; iz < ndatapix; iz++ ) zdata[ iz ] = iz + 1.0;
      astTran1( map, ndatapix, zdata, 1, zdata );

/* Loop round all planes in the output cube. */
      zd = zdata;
      pout = ipdout;
      for( iz = slbnd[ 2 ]; iz <= subnd[ 2 ]; iz++,zd++ ) {

/* We are starting a new output plane. Initialise a pointer to the first
   pixel in the input image. */
         pin = ipdin;
         pvin = ipvin;

/* Loop round all pixels in this output plane, in "Fortran" order. */
         for( iy = slbnd[ 1 ]; iy <= subnd[ 1 ]; iy++ ) {
            for( ix = slbnd[ 0 ]; ix <= subnd[ 0 ]; ix++, pout++, pin++ ) {

/* Store the difference in data value between the input image pixel and the
   data value associated with the current output plane. */
               if( *pin != VAL__BADD ) {
                  *pout = *zd - *pin;

/* If required, scale it by the sigma value for this input pixel. */
                  if( sigma == 0.0 ) {
                     vin = *(pvin++);
                     if( vin != VAL__BADD && vin > 0.0 ) {
                        *pout /= sqrt( vin );
                     } else {
                        *pout = VAL__BADD;
                     }

                  } else {
                     *pout /= sigma;
                  }

               } else {
                  *pout = VAL__BADD;
                  if( sigma == 0.0 ) pvin++;
               }
            }
         }
      }

/* Free resources. */
      zdata = astFree( zdata );
   }

/* End the NDF and AST contexts. */
   ndfEnd( STATUS );
   astEnd;

/* If an error has occurred, issue another error report identifying the
   program which has failed (i.e. this one). */
   if( *STATUS != SAI__OK ) {
      errRep( "CARPET_ERR", "CARPET: Failed to create a cube (representing "
              "a carpet plot) from a two-dimensional image.", STATUS );
   }

}

