#include <math.h>
#include <stdlib.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "ndf_err.h"
#include "prm_par.h"
#include <math.h>
#include <stdlib.h>
#include "mers.h"

void ndf1Wcspm( AstMapping *map, const hdsdim lbnd[], const hdsdim ubnd[],
                int perm[], int *status ){
/*
*+
*  Name:
*     ndf1Wcspm

*  Purpose:
*     Determine pixel limits for all NDF axes.

*  Synopsis:
*     void ndf1Wcspm( AstMapping *map, const hdsdim lbnd[], const hdsdim ubnd[],
*                     int perm[], int *status )

*  Description:
*     This function returns a permutation array that embodies a guess at
*     the correspondance between pixel and WCS axes. In some cases there is
*     no defined correspondance between WCS and pixel axes, and so we need
*     to just guess at what the user means when entering WCS axis values in
*     a certain order (e.g. in an NDF section specified in WCS). For
*     instance in a 2D image of the sky there no way of assigning one pixel
*     axis to the RA axis and the other pixel axis to the Dec axis. To see
*     that this is the case, consider an image in which the WCS axes are
*     linear and at 45 degrees to the pixel axes. Varying either pixel axes
*     independently of the other one will make equal changes to both RA and
*     Dec values.
*
*     Having said that, the 45 degree case is rare and we will usually be
*     able to have some confidence that the user will associate each WCS
*     axis with the "closest" pixel axis.
*
*     In the 45 degree case, the returned permuatation array is based on
*     the assumption that each WCS axis corresponds to the pixel axis with
*     the same index as the WCS axis.

*  Parameters:
*     map
*        AST pointer to the Mapping from pixel coordinates to WCS.
*     lbnd
*        Lower pixel index bounds for the NDF.
*     ubnd
*        Upper pixel index bounds for the NDF.
*     perm
*        The index into this array is pixel index, and the values stored in
*        the array are the corresponding WCS axis indices. A value of 0 is
*        returned for any pixel axes that have no corresponding WCS axis.
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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   AstMapping *tmap;
   double change;
   double delta[ NDF__MXDIM ];
   double maxchange2;
   double maxchange;
   double testpix[ NDF__MXDIM ];
   double testwcs2[ NDF__MXDIM ];
   double testwcs[ NDF__MXDIM ];
   int diff;
   int good;
   int icell;
   int ipix;
   int iwcs;
   int jpix;
   int jwcs;
   int mindiff;
   int more;
   int ncell;
   int ncelltot;
   int npix;
   int ntest;
   int nwcs;
   int used;
   int wcsax[ NDF__MXDIM ];

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Get the number of pixel and WCS axes. */
   npix = astGetI( map, "Nin" );
   nwcs = astGetI( map, "Nout" );

/* First look for any pixel axes that feed one and only one WCS axis.
   For such pixel axes, store the index of the corresponding WCS axis
   in the returned array. For other pixel axes, store zero and set a flag
   to indicate that we have not yet associated each pixel axis with a WCS
   axis. */
   more = 0;
   for( ipix = 0; ipix < npix; ipix++ ){
      astMapSplit( map, 1, &ipix + 1, wcsax, &tmap );
      if( tmap ) {
         if( astGetI( tmap, "Nout" ) == 1 ) {
            perm[ ipix ] = wcsax[ 0 ];
         } else {
            perm[ ipix ] = 0;
            more = 1;
         }
         tmap = astAnnul( tmap );
      } else {
         perm[ ipix ] = 0;
         more = 1;
      }
   }

/*If not all pixel axes have been assigned, we have more work to do. */
   if( more ) {

/* First need to find a pixel that has good world coords. Test an
   increasingly fine square grid of points in pixel space. Initialise the
   number of cells along a pixel axis (the same for all pixel axes), and
   the size of a cell on each pixel axis. We start with one cell covering
   the whole NDF. */
      ncell = 1;
      for( ipix = 0; ipix < npix; ipix++ ){
         delta[ ipix ] = ubnd[ ipix ] - lbnd[ ipix ] + 1.0;
      }

/* Loop until we find a pixel with good WCS coords. Do at most 10000 tests. */
      good = 0;
      ntest = 0;
      while( !good && ntest <= 10000 ){

/* Find the total number of cells required to cover the NDF, and
   initialise the pixel coords at the centre of the first cell. */
         ncelltot = 1;
         for( ipix = 0; ipix < npix; ipix++ ){
            ncelltot *= ncell;
            testpix[ ipix ] = lbnd[ ipix ] + 0.5*delta[ ipix ] - 1.0;
         }

/* Loop round all cells, breaking early if a pixel with good WCS coords
   is found. */
         icell = 1;
         while( !good && icell <= ncelltot ){
            icell++;

/* Increment the number of pixels tested. */
            ntest++;

/* Find the WCS coords of the current cell centre. */
            astTranN( map, 1, npix, 1, testpix, 1, nwcs, 1, testwcs );

/* Set the "good" flag non-zero if all WCS axis values are good. */
            iwcs = 1;
            good = 1;
            while( good && iwcs <= nwcs ){
               good = ( testwcs[ iwcs - 1 ] != AST__BAD );
               iwcs++;
            }

/* Set up the pixel coords at the centre of the next cell. */
            if( !good ) {
               ipix = 1;
               testpix[ ipix - 1 ] += delta[ ipix - 1 ];
               while( ipix < npix && testpix[ ipix - 1 ] > ubnd[ ipix - 1 ] ){
                  testpix[ ipix - 1 ] = lbnd[ ipix - 1 ] + 0.5*delta[ ipix
                                        - 1 ] - 1.0;
                  ipix++;
                  testpix[ ipix - 1 ] += delta[ ipix - 1 ];
               }
            }

         }

/* Double the density of the cells covering the grid. */
         if( !good ) {
            ncell = 2*ncell;
            for( ipix = 0; ipix < npix; ipix++ ){
               delta[ ipix ] = 0.5*delta[ ipix ];
            }
         }

      }

/* Report an error if no good pixel was found. */
      if( !good && *status == SAI__OK ) {
         *status = NDF__WCSIN;
         errRep( " ", "Cannot find a pixel with valid world coordinates.",
                 status );
      } else {

/* Loop round all pixel axes that have not yet been associated with a WCS
   axis. */
         for( ipix = 0; ipix < npix; ipix++ ){
            if( perm[ ipix ] == 0 ) {

/* Move the test pixel position by a tenth of a pixel on the current
   pixel axis. */
               testpix[ ipix ] += 0.1;

/* Find the corresponding WCS position. */
               astTranN( map, 1, npix, 1, testpix, 1, nwcs, 1, testwcs2 );

/* Find the WCS axis that has changed the most from the original "testwcs"
   position. Ignore WCS axes that have already been assined to a pixel axis.
   We keep a note of the second best axis. */
               jwcs = 0;
               maxchange = -1.0;
               maxchange2 = -1.0;
               for( iwcs = 0; iwcs < nwcs; iwcs++ ){

                  used = 0;
                  for( jpix = 0; jpix < npix; jpix++ ){
                     if( perm[ jpix ] == iwcs + 1 ) used = 1;
                  }

                  if( !used ) {
                     change = fabs( testwcs[ iwcs ] - testwcs2[ iwcs ] );
                     if( change > maxchange ) {
                        maxchange2 = maxchange;
                        maxchange = change;
                        jwcs = iwcs + 1;
                     }
                  }

               }

/* Only assign the axis if the best axis was significantly better than
   the second best axis. This means that WCS axes at 45 degrees to the
   pixel axes will not be assigned. */
               if( jwcs >= 1 && jwcs <= nwcs && maxchange > maxchange2*1.1 )
               perm[ ipix ] = jwcs;

/* Reset the test pixel position back to its original value. */
               testpix[ ipix ] -= 0.1;

            }
         }

/* Loop round any pixel axes that have still not been associated with a WCS
   axis. Each is associated with the remaining unassigned WCS axis that
   has the closest index. */
         for( iwcs = 0; iwcs < nwcs; iwcs++ ){
            wcsax[ iwcs ] = 0;
         }

         for( ipix = 0; ipix < npix; ipix++ ){
            if( perm[ ipix ] > 0 ) wcsax[ perm[ ipix ] - 1 ] = ipix + 1;
         }

         for( ipix = 0; ipix < npix; ipix++ ){
            if( perm[ ipix ] == 0 ) {

               mindiff = 2*NDF__MXDIM;
               for( iwcs = 0; iwcs < nwcs; iwcs++ ){
                  if( wcsax[ iwcs ] == 0 ) {
                     diff = abs( iwcs - ipix + 2 );
                     if( diff < mindiff ) {
                        perm[ ipix ] = iwcs + 1;
                        mindiff = diff;
                     }
                  }
               }

               if( perm[ ipix ] > 0 ) wcsax[ perm[ ipix ] - 1 ] = ipix + 1;

            }
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Wcspm", status );

}

