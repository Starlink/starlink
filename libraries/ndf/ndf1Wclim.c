#include <math.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "ndf_err.h"
#include "mers.h"
#include <math.h>

void ndf1Wclim( AstFrameSet *iwcs, int nax, int ndim, const hdsdim nlbnd[],
                const hdsdim nubnd[], const int isdef1[], const int isdef2[],
                double value1[], double value2[], int isbnd[], hdsdim lbnd[],
                hdsdim ubnd[], int *status ){
/*
*+
*  Name:
*     ndf1Wclim

*  Purpose:
*     Determine pixel limits for all NDF axes.

*  Synopsis:
*     void ndf1Wclim( AstFrameSet *iwcs, int nax, int ndim,
*                     const hdsdim nlbnd[], const hdsdim nubnd[],
*                     const int isdef1[], const int isdef2[],
*                     double value1[], double value2[], int isbnd[],
*                     hdsdim lbnd[], hdsdim ubnd[], int *status )

*  Description:
*     This function accepts values which have been supplied as WCS
*     axis bounds in a NDF section specification, and calculates the
*     corresponding NDF pixel-index bounds.

*  Parameters:
*     iwcs
*        AST pointer to the NDFs WCS FrameSet.
*     nax
*        The number of WCS axis bound supplied.
*     ndim
*        The number of pixel axes in the NDF.
*     nlbnd
*        The NDF lower pixel bounds. The supplied "nlbnd" array should have
*        at least "ndim" elements.
*     nubnd
*        The NDF upper pixel bounds. The supplied "nubnd" array should have
*        at least "ndim" elements.
*     isdef1
*        Is the value supplied "value1" a default value? The supplied
*        "isdef1" array should have at least "nax" elements.
*     isdef2
*        Is the value supplied "value2" a default value? The supplied
*        "isdef2" array should have at least "nax" elements.
*     value1
*        First value specifying the bound on each WCS axis. On exit, any
*        "centre/width" values are turned into "lbnd/ubnd" values, and the
*        positions are normalised using the "astNorm" method of the current
*        WCS Frame. The supplied "value1" array should have at least "nax"
*        elements.
*     value2
*        Second value specifying the bound on each WCS axis. On exit, any
*        "centre/width" values are turned into "lbnd/ubnd" values. and the
*        positions are normalised using the "astNorm" method of the current
*        WCS Frame. The supplied "value2" array should have at least "nax"
*        elements.
*     isbnd
*        Whether "value1" and "value2" specify the lower and upper bounds
*        directly (as opposed to specifying the centre and width). On exit,
*        any "centre/width" values are turned into "lbnd/ubnd" values. The
*        supplied "isbnd" array should have at least "nax" elements.
*     lbnd
*        Returned holding the lower pixel-index bounds. The supplied "lbnd"
*        array should have at least "ndim" elements.
*     ubnd
*        Returned holding the upper pixel-index bounds. The supplied "ubnd"
*        array should have at least "ndim" elements.
*     *status
*        The global status.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   AstBox *pbox;
   AstBox *wbox;
   AstCmpRegion *cmpreg;
   AstFrame *cfrm;
   AstFrame *pfrm;
   AstMapping *map;
   AstRegion *wboxp;
   double delta;
   double dlbnd;
   double dubnd;
   double ndl[ NDF__MXDIM ];
   double ndu[ NDF__MXDIM ];
   double plbnd[ NDF__MXDIM ];
   double pubnd[ NDF__MXDIM ];
   double v1;
   double v2;
   double xl[ NDF__MXDIM ];
   double xu[ NDF__MXDIM ];
   int def;
   int i;
   int nwcs;
   int temp;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Start an AST context. */
   astBegin;

/* Report an error if insufficient bounds have been supplied. */
   nwcs = astGetI( iwcs, "Nout" );
   if( nax != nwcs ) {
      *status = NDF__BNDIN;
      msgSeti( "NAX", nax );
      msgSeti( "NWCS", nwcs );
      errRep( " ", "Number of axis bounds supplied (^NAX) does not equal "
              "the number of WCS axes (^NWCS) in the NDF.", status );

/* Otherwise, ensure we have upper and lower bounds on all WCS axes.
   Set a flag if any WCS limits were defaulted (but only if there were
   supplied as upper/lower bound). */
   } else {
      cfrm = astGetFrame( iwcs, AST__CURRENT );
      def = 0;

      for( i = 0; i < nwcs; i++ ){

         if( !isbnd[ i ] ) {
            value1[ i ] = astAxOffset( cfrm, i + 1, value1[ i ],
                                       -0.5*value2[ i ] );
            value2[ i ] = astAxOffset( cfrm, i + 1, value1[ i ], value2[ i ] );
            isbnd[ i ] = 1;

         } else if( isdef1[ i ] || isdef2[ i ] ) {
            def = 1;
         }

      }

/* Get the (current Frame)->PIXEL mapping from the FrameSet, and check it
   is defined. Report an error if not. */
      map = astGetMapping( iwcs, AST__CURRENT, 2 );
      if( !astGetL( map, "TranForward" ) ) {
         *status = NDF__BNDIN;
         errRep( " ", "The transformation from the current WCS coordinate "
                 "system to pixel coordinates is undefined, and so the "
                 "supplied NDF section specified cannot be used.", status );

/* Only proceed if the Mapping is defined. */
      } else {

/* If any WCS limit was defaulted, it will have been set to the value
   that encloses the whole NDF. But the non-defaulted limits have
   restricted the area of interest and so these defaults may no longer be
   appropriate. We now find better defaults for any missing limits that
   span only the region implied by the other limits. */
         if( def ) {

/* The AST Box class knows nothing about axis normalisation. To avoid
   problems ensure that the upper and lower axis values are in the same
   "cylce". This applied particularly to RA values where the lower limit
   may have a value of (say) 359 degrees and the upper limit be (say) 2
   degrees. In this example the following code converts the upper limit
   to 361 degrees. */
            for( i = 0; i < nwcs; i++ ){
               delta = astAxDistance( cfrm, i + 1, value1[ i ], value2[ i ] );
               value2[ i ] = value1[ i ] + delta;
            }

/* Create an AST Box describing the original (excesively large) WCS box. */
            wbox = astBox( cfrm, 1, value1, value2, NULL, " " );

/* Map this Box into the PIXEL Frame. The resulting Region will (in
   general) be a rotated box with curvi-linear edges. */
            pfrm = astGetFrame( iwcs, 2 );
            wboxp = astMapRegion( wbox, map, pfrm );

/* Create an AST Box describing the NDF pixel bounds. */
            for( i = 0; i < ndim; i++ ){
               ndl[ i ] = (double)( nlbnd[ i ] ) - 1.0;
               ndu[ i ] = (double)( nubnd[ i ] );
            }
            pbox = astBox( pfrm, 1, ndl, ndu, NULL, " " );

/* Now form a compound region that is the intersection of the two aboves
   Boxes (both now defined in the PIXEL Frame). */
            cmpreg = astCmpRegion( pbox, wboxp, AST__AND, " " );

/* Find the bounds (in PIXEL coords) of the compound Region. */
            astGetRegionBounds( cmpreg, plbnd, pubnd );

/* Use this box to determine new defaults for any WCS limits that were
   originally defaulted. */
            for( i = 0; i < nwcs; i++ ){

/* Pass on to the next axis if this WCS axis did not have either limit
   defaulted. */
               if( isdef1[ i ] || isdef2[ i ] ) {

/* Map the pixel box found above into WCS coords and get the limits of
   the box on this WCS axis. */
                  astMapBox( map, plbnd, pubnd, 0, i + 1, &v1, &v2, xl, xu );

/* Whether a WCS value is a "lower" or "upper" bound is determined not by
   the WCS values themselves but by which one gives the lower or upper
   value on the corresponding pixel axis. Use this criterion to fill in
   values for which ever WCS bound has not been supplied. */
                  if( isdef1[ i ] ) {
                     if( xl[ i ] < xu[ i ] ) {
                        value1[ i ] = v1;
                     } else {
                        value1[ i ] = v2;
                     }
                  }

                  if( isdef2[ i ] ) {
                     if( xl[ i ] > xu[ i ] ) {
                        value2[ i ] = v1;
                     } else {
                        value2[ i ] = v2;
                     }
                  }
               }
            }

         }

/* Now we use the Mapping to find the pixel box enclosing the
   (potentially modified) WCS box. First, normalise the WCS
   coordinates at the two box corners using the "astNorm" method
   associated with the current WCS Frame. */
         astNorm( cfrm, value1 );
         astNorm( cfrm, value2 );

/* Find the extent of the WCS box on each pixel axis in turn. */
         for( i = 0; i < ndim; i++ ){

/* Find the extent of the box on the I"th pixel axis. */
            astMapBox( map, value1, value2, 1, i + 1, &dlbnd, &dubnd, xl, xu );

/* Report an error if the PIXEL box is undefined. */
            if( dlbnd == AST__BAD || dubnd == AST__BAD || fabs( dlbnd ) >
                1.0e7 || fabs( dubnd ) > 1.0e7 ) {
               if( *status == SAI__OK ) {
                  *status = NDF__BNDIN;
                  errRep( " ", "The extent of the requested NDF section in "
                          "pixel coordinates cannot be determined.", status );
               }

/* Otherwise convert to pixel indices */
            } else {
               lbnd[ i ] = NDF_NINT( dlbnd );
               ubnd[ i ] = NDF_NINT( dubnd );
            }
         }
      }

/* Free the Mapping pointer. */
      map = astAnnul( map );

   }

/* If no error has occurred, then ensure that lower bound does not
   exceed the upper bound, swapping the bounds if required. */
   if( *status == SAI__OK ) {
      for( i = 0; i < ndim; i++ ){
         if( lbnd[ i ] > ubnd[ i ] ) {
            temp = lbnd[ i ];
            lbnd[ i ] = ubnd[ i ];
            ubnd[ i ] = temp;
         }
      }
   }

/* End the AST context. */
   astEnd;

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Wclim", status );

}

