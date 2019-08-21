#include "sae_par.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "ndf_err.h"
#include "prm_par.h"
#include <math.h>
#include "mers.h"

void ndf1Wplim( AstFrameSet *iwcs, int nax, const hdsdim lbndd[],
                const hdsdim ubndd[], double value1[], const double
                value2[], int ispix1[], const int ispix2[],
                const int isbnd[], const int isdef1[], const int isdef2[],
                hdsdim lbnd[], hdsdim ubnd[], int *status ){
/*
*+
*  Name:
*     ndf1Wplim

*  Purpose:
*     Determine pixel limits for all NDF axes.

*  Synopsis:
*     void ndf1Wplim( AstFrameSet *iwcs, int nax, const hdsdim lbndd[],
*                     const hdsdim ubndd[], double value1[],
*                     const double value2[], int ispix1[],
*                     const int ispix2[], const int isbnd[],
*                     const int isdef1[], const int isdef2[], hdsdim lbnd[],
*                     hdsdim ubnd[], int *status )

*  Description:
*     This function accepts values which have been supplied as either pixel
*     indices or WCS axis values in a NDF section specification, and
*     calculates the corresponding NDF pixel-index bounds.

*  Parameters:
*     iwcs
*        AST pointer to the NDFs WCS FrameSet.
*     nax
*        The number of axes for which bounds are supplied.
*     lbndd
*        Lower pixel index bounds for the NDF.
*     ubndd
*        Upper pixel index bounds for the NDF.
*     value1
*        First value specifying the bound on each axis. The supplied
*        "value1" array should have at least "nax" elements.
*     value2
*        Second value specifying the bound on each axis. The supplied
*        "value2" array should have at least "nax" elements.
*     ispix1
*        Whether "value1" is a pixel index (as opposed to a WCS value). The
*        supplied "ispix1" array should have at least "nax" elements.
*     ispix2
*        Whether "value2" is a pixel index (as opposed to a WCS value). The
*        supplied "ispix2" array should have at least "nax" elements.
*     isbnd
*        Whether "value1" and "value2" specify the lower and upper bounds
*        directly (as opposed to specifying the centre and width). The
*        supplied "isbnd" array should have at least "nax" elements.
*     isdef1
*        Is the value supplied "value1" a default value? The supplied
*        "isdef1" array should have at least "nax" elements.
*     isdef2
*        Is the value supplied "value2" a default value? The supplied
*        "isdef2" array should have at least "nax" elements.
*     lbnd
*        Returned holding the lower pixel-index bounds. The supplied "lbnd"
*        array should have at least "nax" elements.
*     ubnd
*        Returned holding the upper pixel-index bounds. The supplied "ubnd"
*        array should have at least "nax" elements.
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
   AstBox *wbox;
   AstCmpRegion *cmpreg;
   AstFrame *cfrm;
   AstFrame *cfrm1;
   AstFrame *pfrm;
   AstInterval *interv;
   AstInterval *pbox;
   AstMapping *fsmap;
   AstMapping *junk;
   AstMapping *map;
   AstMapping *map1;
   AstMapping *map2;
   AstRegion *wboxp;
   double cenpix[ NDF__MXDIM ];
   double cenwcs[ NDF__MXDIM ];
   double delta;
   double dlbndd[ NDF__MXDIM ];
   double dubndd[ NDF__MXDIM ];
   double plbnd2[ NDF__MXDIM ];
   double plbnd[ NDF__MXDIM ];
   double pubnd2[ NDF__MXDIM ];
   double pubnd[ NDF__MXDIM ];
   double v1;
   double v2;
   double wlbnd1[ NDF__MXDIM ];
   double wlbnd[ NDF__MXDIM ];
   double wubnd1[ NDF__MXDIM ];
   double wubnd[ NDF__MXDIM ];
   double xl[ NDF__MXDIM ];
   double xu[ NDF__MXDIM ];
   int allpix;
   int allwcs;
   int i;
   int intprt;
   int j;
   int jj;
   int mixed;
   int nextra;
   int npix;
   int nwcs;
   int perm[ NDF__MXDIM ];
   int pnax1;
   int pnax2;
   int pperm[ NDF__MXDIM ];
   int temp;
   int wnax1;
   int wperm[ NDF__MXDIM ];

//      integer kk

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Begin an AST context. */
   astBegin;

/* Get the pixel->WCS Mapping. Note, the PIXEL Frame is always index 2. */
   fsmap = astGetMapping( iwcs, 2, AST__CURRENT );

/* Find a permutation array that associates each WCS axis with a pixel
   axis. This is really just a guess as to what the user intended, based
   on how closely the WCS and pixel axes align. It could be wrong for
   instance in an image where the WCS axes are linear and exactly at 45
   degrees to the pixel axes. */
   ndf1Wcspm( fsmap, lbndd, ubndd, perm, status );

/* Split this Mapping up into two parallel Mappings, one of which ("map1")
   contains those inputs/outputs that have defined inverse transformations,
   and the other of which ("map2") contains those inputs/outputs that have no
   inverse transformations (either of these Mappings may be AST__NULL). In
   addition this returns a Mapping ("map") that is equivalent to the supplied
   pixel->WCS Mapping except that it will always have a defined inverse
   (even if this means adding in a transformation that returns bad values).
   Arrays are returned that allow the inputs and outputs of the full
   Mapping to be associated with the corresponding inputs and outputs of
   the component Mappings. */
   ndf1Mpspt( fsmap, &map, &map1, &map2, pperm, wperm, status );

/* Invert the Mappings so that their forward transformation goes from the
   current Frame to the pixel Frame. Check that we are inverting a
   different Mapping each time, to avoid inverting the same Mapping twice. */
   astInvert( map );

   if( map1 ) {
      if( !astSame( map1, map ) ) astInvert( map1 );
   }

   if( map2 ) {
      if( !astSame( map2, map ) && !astSame( map2, map1 ) ) astInvert( map2 );
   }

/* Get the number of pixel and WCS axes in the NDF. */
   npix = astGetI( iwcs, "Nin" );
   nwcs = astGetI( iwcs, "Nout" );

/* Get the current WCS coordinate Frame. */
   cfrm = astGetFrame( iwcs, AST__CURRENT );

/* Initialise a box that encloses the required section of WCS space. */
   for( i = 0; i < nwcs; i++ ){
      wlbnd[ i ] = AST__BAD;
      wubnd[ i ] = AST__BAD;
   }

/* Initialise a box that encloses the required section of PIXEL space. */
   for( i = 0; i < npix; i++ ){
      dlbndd[ i ] = (double)( lbndd[ i ] ) - 1.0;
      dubndd[ i ] = (double)( ubndd[ i ] );
      plbnd[ i ] = dlbndd[ i ];
      pubnd[ i ] = dubndd[ i ];
   }

/* Pad out with "1:1" bounds to allow for the section specifying more
   pixel axes than there are in the NDF. */
   for( i = npix; i < NDF__MXDIM; i++ ){
      dlbndd[ i ] = 0.0;
      dubndd[ i ] = 1.0;
      plbnd[ i ] = 0.0;
      pubnd[ i ] = 1.0;
   }

/* If any axis that has been specified using "centre/width" format has a
   WCS width value and a defaulted centre value, then we use a default
   equal to the axis value at the centre of the NDF. */
   cenwcs[ 0 ] = AST__BAD;
   for( i = 0; i < nax; i++ ){
      if( !isbnd[ i ] && ispix1[ i ] && !ispix2[ i ] && isdef1[ i ] ) {

         if( cenwcs[ 0 ] == AST__BAD ) {

            for( j = 0; j < npix; j++ ){
               cenpix[ j ] = 0.5*( ubndd[ i ] + lbndd[ i ] - 1 );
            }

            astTranN( map, 1, nax, 1, cenpix, 0, nwcs, 1, cenwcs );

         }

         if( perm[ i ] > 0 ) {
            value1[ i ] = cenwcs[ perm[ i ] - 1 ];
            ispix1[ i ] = 0;

         } else if( *status == SAI__OK ) {
            *status = NDF__WCSIN;
            msgSeti( "I", i + 1 );
            errRep( " ", "The supplied NDF section cannot be interpreted "
                    "since pixel axis ^I has no corresponding WCS axis.",
                    status );
            goto L999;
         }
      }
   }

/* Indicate that so far we have not found any WCS bounds. */
   allpix = 1;

/* Indicate that so far we have not found any centre/width bounds that
   use a WCS value for one limit and a pixel value for the other. */
   mixed = 0;

/* We now set up the bounds of two boxes; "plbnd"/"pubnd" hold the bounds of
   a box in PIXEL coordinates, and "wlbnd"/"wubnd" hold the bounds of a box
   in WCS current Frame coordinates. Both boxes have edges parallel to
   their respective coordinate axes. Any WCS bounds in the supplied
   "value1"/"value2" arrays are used to set the bounds of the corresponding
   axes of the WCS box. Any pixel index bounds in the supplied
   "value1"/"value2" arrays are used to set the bounds of the corresponding
   axes of the PIXEL box. If any axis has no bounds then defaults are
   used that encompass the whole NDF. For PIXEL axes these default bounds
   are inserted into "plbnd"/"pubnd" immediately. For WCS axes, default bounds
   are only calculated as needed after the supplied WCS bounds have been
   identified. The need for a default WCS bound to be calculated is
   flagged by storing an AST__BAD value in "wlbnd"/"wubnd". Loop round each
   axis for which bounds have been supplied. */
   for( i = 0; i < nax; i++ ){

/* If upper and lower limits have been supplied for this axis... */
      if( isbnd[ i ] ) {

/* If the lower limit was specified as a pixel index, store the
   corresponding pixel coordinate as the lower bound for the pixel
   coordinate box. The lower limit of the WCS box is left set at AST__BAD. */
         if( ispix1[ i ] ) {
            plbnd[ i ] = value1[ i ] - 1.0;

/* Otherwise the lower limit was specified as a WCS value, so store the
   corresponding value as the lower limit of the WCS box, and store the
   lower pixel bound of the NDF as the lower bound of the pixel box. Set
   the "allpix" flag to indicate that at least one bound is specified as a
   WCS value. */
         } else {
            if( i >= nwcs && *status == SAI__OK ) {
               *status = NDF__BNDIN;
               msgSeti( "I", i + 1 );
               msgSeti( "N", nwcs );
               errRep( " ", "Lower bound for axis ^I was specified using a "
                       "WCS value, but there are only ^N axes in the "
                       "current WCS Frame.", status );
               goto L999;

            } else if( perm[ i ] > 0 ) {
               plbnd[ i ] = dlbndd[ i ];
               wlbnd[ perm[ i ] - 1 ] = value1[ i ];
               allpix = 0;

            } else if( *status == SAI__OK ) {
               *status = NDF__WCSIN;
               msgSeti( "I", i + 1 );
               errRep( " ", "The supplied NDF section cannot be "
                       "interpreted since pixel axis ^I has no "
                       "corresponding WCS axis.", status );
               goto L999;
            }
         }

/* Do the same for the upper bound. */
         if( ispix2[ i ] ) {
            pubnd[ i ] = value2[ i ];
         } else {
            if( i >= nwcs && *status == SAI__OK ) {
               *status = NDF__BNDIN;
               msgSeti( "I", i + 1 );
               msgSeti( "N", nwcs );
               errRep( " ", "Upper bound for axis ^I was specified using a "
                       "WCS value, but there are only ^N axes in the "
                       "current WCS Frame.", status );
               goto L999;

            } else if( perm[ i ] > 0 ) {
               pubnd[ i ] = dubndd[ i ];
               wubnd[ perm[ i ] - 1 ] = value2[ i ];
               allpix = 0;

            } else if( *status == SAI__OK ) {
               *status = NDF__WCSIN;
               msgSeti( "I", i + 1 );
               errRep( " ", "The supplied NDF section cannot be "
                       "interpreted since pixel axis ^I has no "
                       "corresponding WCS axis.", status );
               goto L999;
            }
         }

/* If the bounds for this axis are specified by centre and width, and have
   been supplied in the same coordinate system (pixel or WCS)... */
      } else if( ispix1[ i ] == ispix2[ i ] ) {

/* If pixel, then store the bounds of the pixel box and leave the WCS box
   bounds set to AST__BAD. */
         if( ispix1[ i ] ) {
            delta = (double)( NDF_NINT( value2[ i ] )/2 );
            plbnd[ i ] = value1[ i ] - delta - 1.0;
            pubnd[ i ] = plbnd[ i ] + value2[ i ];

/* If WCS, then use the NDF Bounds as the pixel box and store the WCS box
   bounds. */
         } else {
            if( i >= nwcs && *status == SAI__OK ) {
               *status = NDF__BNDIN;
               msgSeti( "I", i + 1 );
               msgSeti( "N", nwcs );
               errRep( " ", "Axis ^I was specified using a WCS centre and "
                       "width, but there are only ^N axes in the current "
                       "WCS Frame.", status );
               goto L999;

            } else if( perm[ i ] > 0 ) {
               delta = 0.5*value2[ i ];
               plbnd[ i ] = dlbndd[ i ];
               pubnd[ i ] = dubndd[ i ];
               wlbnd[ perm[ i ] - 1 ] = value1[ i ] - delta;
               wubnd[ perm[ i ] - 1 ] = value1[ i ] + delta;
               allpix = 0;

            } else if( *status == SAI__OK ) {
               *status = NDF__WCSIN;
               msgSeti( "I", i + 1 );
               errRep( " ", "The supplied NDF section cannot be "
                       "interpreted since pixel axis ^I has no "
                       "corresponding WCS axis.", status );
               goto L999;
            }
         }

/* If the bounds for this axis are specified by centre and width, and the
   centre is a WCS value but the the width is a pixel value... */
      } else if( !ispix1[ i ] && ispix2[ i ] ) {
         if( i >= nwcs && *status == SAI__OK ) {
            *status = NDF__BNDIN;
            msgSeti( "I", i + 1 );
            msgSeti( "N", nwcs );
            errRep( " ", "Axis ^I was specified using a WCS centre, but "
                    "there are only ^N axes in the current WCS Frame.", status );
            goto L999;

         } else if( perm[ i ] > 0 ) {
            allpix = 0;
            mixed = 1;

/* Store the central WCS values as the upper and lower bounds of the WCS
   box. */
            wubnd[ perm[ i ] - 1 ] = value1[ i ];
            wlbnd[ perm[ i ] - 1 ] = value1[ i ];

/* Use defaults for the upper and lower bounds of the pixel box. */
            plbnd[ i ] = dlbndd[ i ];
            pubnd[ i ] = dubndd[ i ];

         } else if( *status == SAI__OK ) {
            *status = NDF__WCSIN;
            msgSeti( "I", i + 1 );
            errRep( " ", "The supplied NDF section cannot be interpreted "
                    "since pixel axis ^I has no corresponding WCS axis.",
                    status );
            goto L999;
         }

/* If the bounds for this axis are specified by centre and width, and the
   centre is a pixel value but the the width is a WCS value... */
      } else {
         if( i >= nwcs && *status == SAI__OK ) {
            *status = NDF__BNDIN;
            msgSeti( "I", i + 1 );
            msgSeti( "N", nwcs );
            errRep( " ", "Axis ^I was specified using a WCS width, but "
                    "there are only ^N axes in the current WCS Frame.", status );
            goto L999;
         } else {
            allpix = 0;
            mixed = 1;

/* Store the central pixel values as the upper and lower bounds of the
   pixel box. Leave the WCS box bounds set to bad so that defaults will
   be found and used. */
            pubnd[ i ] = value1[ i ];
            plbnd[ i ] = value1[ i ];
         }
      }

   }

//     write(*,*) 'A:'
//     write(*,*) '   PLBND: ',(PLBND(KK),kk=1,NDF__MXDIM)
//     write(*,*) '   PUBND: ',(PUBND(KK),kk=1,NDF__MXDIM)
//     write(*,*) '   WLBND: ',(WLBND(KK),kk=1,NWCS)
//     write(*,*) '   WUBND: ',(WUBND(KK),kk=1,NWCS)
//     write(*,*) '   '

/* If any centre/values bounds were specified in which the centre is
   given as a pixel value and the width as a WCS value, then we need
   to convert the centre into a WCS value, so that we can get upper
   and lower bounds. We need to do this before we find the default WCS
   bounds since the WCS bounds depend on the pixel box. */
   if( mixed ) {
      allpix = 1;
      mixed = 0;

/* Find the central positions in the pixel box. */
      for( i = 0; i < nax; i++ ){
         cenpix[ i ] = 0.5*( plbnd[ i ] + pubnd[ i ] );
      }

/* Convert to WCS. */
      astTranN( map, 1, npix, 1, cenpix, 0, nwcs, 1, cenwcs );

/* Rescan the supplied bounds looking for mixed mode centre/value  bounds. */
      for( i = 0; i < nax; i++ ){

/* If the bounds for this axis are specified by centre and width... */
         if( !isbnd[ i ] ) {

/* ...and the width is a WCS value... */
            if( !ispix2[ i ] ) {

/* ...and the centre is a PIXEL value... */
               if( ispix1[ i ] ) {
                  if( perm[ i ] > 0 ) {

/* Re-calculate the WCS bounds using the central WCS value and the
   supplied WCS width. */
                     ispix1[ i ] = 0;
                     value1[ i ] = cenwcs[ perm[ i ] - 1 ];

                     delta = 0.5*value2[ i ];
                     wlbnd[ perm[ i ] - 1 ] = value1[ i ] - delta;
                     wubnd[ perm[ i ] - 1 ] = value1[ i ] + delta;
                     allpix = 0;

/* Store default bounds for the pixel box on this axis. */
                     plbnd[ i ] = dlbndd[ i ];
                     pubnd[ i ] = dubndd[ i ];

                  } else if( *status == SAI__OK ) {
                     *status = NDF__WCSIN;
                     msgSeti( "I", i + 1 );
                     errRep( " ", "The supplied NDF section cannot be "
                             "interpreted since pixel axis ^I has no "
                             "corresponding WCS axis.", status );
                     goto L999;
                  }

/* If the centre was originally a WCS value, we have some WCS bounds. */
               } else {
                  allpix = 0;
               }

/* If the width is a pixel value and the centre is a WCS value, we still
   have a mixed mode bounds including WCS limits. */
            } else if( !ispix1[ i ] ) {
               allpix = 0;
               mixed = 1;
            }

/* Note if we have a lower/upper bounds that includes any non-PIXEL values. */
         } else if( !ispix1[ i ] || !ispix2[ i ] ) {
            allpix = 0;
         }

      }
   }

//     write(*,*) 'B:'
//     write(*,*) '   PLBND: ',(PLBND(KK),kk=1,NDF__MXDIM)
//     write(*,*) '   PUBND: ',(PUBND(KK),kk=1,NDF__MXDIM)
//     write(*,*) '   WLBND: ',(WLBND(KK),kk=1,NWCS)
//     write(*,*) '   WUBND: ',(WUBND(KK),kk=1,NWCS)
//     write(*,*) '   '

/* If all bounds are now specified using pixel indices, then we can pass
   on to cut the required section from the NDF. If some bounds were
   specified using WCS coordinates we need to find the overlap, in pixel
   indices, between the WCS box and the pixel box. */
   if( !allpix && *status == SAI__OK ) {

/* Indicate that we have not yet found any pixel index bounds */
      allwcs = 1;

/* Ensure the WCS box is complete by replacing any AST__BAD values by the
   appropriate limit that encompasses the whole pixel box. Check each WCS
   axis. */
      for( i = 0; i < nwcs; i++ ){

/* Pass on to the next axis if this WCS axis already has upper and lower
   bounds. */
         if( wlbnd[ i ] == AST__BAD || wubnd[ i ] == AST__BAD ) {

/* Indicate that at least one axis was specified by pixel index bounds. */
            allwcs = 0;

/* Map the pixel box into WCS coords and get the limits of the box on
   this WCS axis. */
            astMapBox( map, plbnd, pubnd, 0, i + 1, &v1, &v2, xl, xu );

/* Find the index of the corresponding pixel axis. */
            jj = 0;
            for( j = 0; j < npix; j++ ){
               if( perm[ j ] == i + 1 ) jj = j + 1;
            }

/* Whether a WCS value is a "lower" or "upper" bound is determined not by
   the WCS values themselves but by which one gives the lower or upper
   value on the corresponding pixel axis. Use this criterion to fill in
   values for which ever WCS bound has not been supplied. */
            if( jj > 0 ) {
               if( wlbnd[ i ] == AST__BAD ) {
                  if( xl[ jj - 1 ] < xu[ jj - 1 ] ) {
                     wlbnd[ i ] = v1;
                  } else {
                     wlbnd[ i ] = v2;
                  }
               }

               if( wubnd[ i ] == AST__BAD ) {
                  if( xl[ jj - 1 ] > xu[ jj - 1 ] ) {
                     wubnd[ i ] = v1;
                  } else {
                     wubnd[ i ] = v2;
                  }
               }

            } else if( v1 == v2 ) {
               wlbnd[ i ] = v1;
               wubnd[ i ] = v2;

            } else if( *status == SAI__OK ) {
               *status = NDF__WCSIN;
               msgSeti( "I", i + 1 );
               errRep( " ", "The supplied NDF section cannot be "
                       "interpreted since WCS axis ^I has no corresponding "
                       "pixel axis.", status );
               goto L999;
            }

         }

/* The AST Box class knows nothing about axis normalisation. To avoid
   problems ensure that the upper and lower axis values are in the same
   "cycle". This applied particularly to RA values where the lower limit
   may have a value of (say) 359 degrees and the upper limit be (say) 2
   degrees. In this example the following code converts the upper limit
   to 361 degrees. */
         delta = astAxDistance( cfrm, i + 1, wlbnd[ i ], wubnd[ i ] );
         wubnd[ i ] = wlbnd[ i ] + delta;

      }

/* If any centre/width bounds remain in which the centre is a WCS value
   and the width is a pixel value, then we need to convert them into
   upper and lower bounds now, since this will not have been done earlier. */
      if( mixed ) {
         allpix = 1;

/* Find the central WCS position. */
         for( i = 0; i < nwcs; i++ ){
            cenwcs[ i ] = 0.5*( wlbnd[ i ] + wubnd[ i ] );
         }

/* Convert to pixel. */
         astTranN( map, 1, nwcs, 1, cenwcs, 1, npix, 1, cenpix );

/* Rescan the supplied bounds looking for mixed mode centre/value  bounds. */
         for( i = 0; i < nax; i++ ){

/* If the bounds for this axis are specified by centre and width... */
            if( !isbnd[ i ] ) {

/* ...and the width is a PIXEL value, and the centre is a WCS value... */
               if( !ispix1[ i ] ) {
                  if( ispix2[ i ] ) {

/* Re-calculate the PIXEL bounds using the central PIXEL value and the
   supplied PIXEL width. */
                     if( cenpix[ i ] != AST__BAD ) {
                        ispix1[ i ] = 1;
                        value1[ i ] = cenpix[ i ];

                        delta = value2[ i ]/2.0;
                        plbnd[ i ] = value1[ i ] - delta;
                        pubnd[ i ] = plbnd[ i ] + value2[ i ];

//                          DELTA = DBLE( NINT( VALUE2( I ) )/2 )
//                          PLBND( I ) = VALUE1( I )  - 1.0D0 - DELTA
//                          PUBND( I ) = PLBND( I ) + VALUE2( I )

                     } else if( *status == SAI__OK ) {
                        *status = NDF__BNDIN;
                        errRep( " ", "The WCS coordinates at the centre of "
                                "the requested section are invalid.", status );

                     }
                  } else {
                     allpix = 0;
                  }

               }

            } else if( !ispix1[ i ] || !ispix2[ i ] ) {
               allpix = 0;
            }

         }
      }

//     write(*,*) 'D:'
//     write(*,*) '   PLBND: ',(PLBND(KK),kk=1,NDF__MXDIM)
//     write(*,*) '   PUBND: ',(PUBND(KK),kk=1,NDF__MXDIM)
//     write(*,*) '   WLBND: ',(WLBND(KK),kk=1,NWCS)
//     write(*,*) '   WUBND: ',(WUBND(KK),kk=1,NWCS)
//     write(*,*) '   '

/* If we still need to find the overlap of the WCS and PIXEL boxes, do it
   now. */
      if( !allpix ) {

/* Report an error if no axes had an inverse transformation. */
         if( !map1 ) {
            if( *status == SAI__OK ) {
               *status = NDF__BNDIN;
               errRep( " ", "The transformation from WCS to pixel "
                       "coordinates is undefined.", status );
            }
            goto L999;
         }

/* Define an AST Box within the subset of WCS axes that have an inverse
   transformation, using the bounds stored in "wlbnd"/"wubnd". we will use the
   inverse transformation below to convert the box from WCS to pixel coords. */
         wnax1 = astGetI( map1, "Nin" );
         for( i = 0; i < wnax1; i++ ){
            wlbnd1[ i ] = wlbnd[ wperm[ i ] - 1 ];
            wubnd1[ i ] = wubnd[ wperm[ i ] - 1 ];
         }
         cfrm1 = astPickAxes( cfrm, wnax1, wperm, &junk );
         wbox = astBox( cfrm1, 1, wlbnd1, wubnd1, NULL, " " );

/* Map this region into the PIXEL Frame (we know that this will work
   since we have selected the axes that have the required inverse
   transformation). The resulting Region will (in general) be a rotated
   box with curvi-linear edges. */
         pnax1 = astGetI( map1, "Nout" );
         pfrm = astFrame( pnax1, "Domain=PIXEL" );
         wboxp = astMapRegion( wbox, map1, pfrm );

/* If there are any WCS axes that do not have an inverse transformation,
   then create an Interval describing the entire extent of the
   corresponding pixel axes. Use this Interval to extrude the WBOX1P
   region along the extra pixel axes to form a Prism. */
         if( map2 ) {
            pnax2 = astGetI( map2, "Nout" );

            for( i = 0; i < npix; i++ ){
               j = pperm[ i ] - pnax1;
               if( j > 0 ) {
                  plbnd2[ j - 1 ] = dlbndd[ i ];
                  pubnd2[ j - 1 ] = dubndd[ i ];
               }
            }

            pfrm = astFrame( pnax2, "Domain=PIXEL" );
            interv = astInterval( pfrm, plbnd2, pubnd2, NULL, " " );
            wboxp = (AstRegion *) astPrism( wboxp, interv, " " );
         }

/* Re-arrange the axes so that they correspond to the original order of pixel
   axes. */
         astPermAxes( wboxp, pperm );

/* If all bounds were specified as WCS  values, find the bounds (in
   PIXEL coords) of the above Box, and store in "plbnd"/"pubnd". */
         if( allwcs ) {
            astGetRegionBounds( wboxp, plbnd, pubnd );

/* Otherwise, we restrict the returned section to the overlap of the WCS
   and PIXEL boxes. */
         } else {

/* Replace any defaulted bounds in the pixel box with bad values. This
   causes "astInterval" to ignore the bound, making the axis value
   unlimited. */
            for( i = 0; i < nax; i++ ){
               if( isbnd[ i ] ) {
                  if( isdef1[ i ] || !ispix1[ i ] ) plbnd[ i ] = AST__BAD;
                  if( isdef2[ i ] || !ispix2[ i ] ) pubnd[ i ] = AST__BAD;
               }
            }

/* Define an AST Interval within the PIXEL Frame, using the bounds stored
   in "plbnd"/"pubnd". */
            pfrm = astFrame( npix, "Domain=PIXEL" );
            pbox = astInterval( pfrm, plbnd, pubnd, NULL, " " );

//     write(*,*) 'F:'
//     write(*,*) '   PLBND: ',(PLBND(KK),kk=1,NDF__MXDIM)
//     write(*,*) '   PUBND: ',(PUBND(KK),kk=1,NDF__MXDIM)
//     write(*,*) '   WLBND: ',(WLBND(KK),kk=1,NWCS)
//     write(*,*) '   WUBND: ',(WUBND(KK),kk=1,NWCS)
//     write(*,*) '   '

/* Now form a compound region that is the intersection of the two aboves
   Boxes (both now defined in the PIXEL Frame). */
            cmpreg = astCmpRegion( pbox, wboxp, AST__AND, " " );

/* Find the bounds (in PIXEL coords) of the compound Region, and store in
   "plbnd"/"pubnd". */
            astGetRegionBounds( cmpreg, plbnd, pubnd );

//     write(*,*) 'G:'
//     write(*,*) '   PLBND: ',(PLBND(KK),kk=1,NDF__MXDIM)
//     write(*,*) '   PUBND: ',(PUBND(KK),kk=1,NDF__MXDIM)
//     write(*,*) '   WLBND: ',(WLBND(KK),kk=1,NWCS)
//     write(*,*) '   WUBND: ',(WUBND(KK),kk=1,NWCS)
//     write(*,*) '   '

/* Report an error if the pixel and WCS boxes do not overlap. */
            for( i = 0; i < nax; i++ ){
               if( plbnd[ i ] > pubnd[ i ] && *status == SAI__OK ) {
                  *status = NDF__BNDIN;
                  errRep( " ", "The requested section does not contain any "
                          "pixels.", status );
               }
            }
         }
      }
   }

//     write(*,*) 'H:'
//     write(*,*) '   PLBND: ',(PLBND(KK),kk=1,NDF__MXDIM)
//     write(*,*) '   PUBND: ',(PUBND(KK),kk=1,NDF__MXDIM)
//     write(*,*) '   '

/* Convert the pixel coordinate box to pixel indices. */
   if( *status == SAI__OK ) {
      for( i = 0; i < nax; i++ ){

/* Swap the limits, if required, so that they are the right way round. */
         if( plbnd[ i ] > pubnd[ i ] ) {
            temp = plbnd[ i ];
            plbnd[ i ] = pubnd[ i ];
            pubnd[ i ] = temp;
         }

/* Select the index of the pixel that contains the "pubnd" value. If "pubnd" is
   exactly integer, the axis value is considered to be the upper edge of the
   upper bound. */
         intprt = (int)( pubnd[ i ] );
         if( fabs( pubnd[ i ] - (double)( intprt ) ) < 100*VAL__EPSD ) {
            ubnd[ i ] = intprt;
         } else if( pubnd[ i ] > 0.0 ) {
            ubnd[ i ] = intprt + 1;
         } else {
            ubnd[ i ] = intprt;
         }

/* Select the index of the pixel that contains the "plbnd" value. If "plbnd" is
   exactly integer, the axis value is considered to be the lower edge of the
   lower bound. */
         intprt = (int)( plbnd[ i ] );
         if( fabs( plbnd[ i ] - (double)( intprt ) ) < 100*VAL__EPSD ) {
            lbnd[ i ] = intprt + 1;
         } else if( plbnd[ i ] > 0.0 ) {
            lbnd[ i ] = intprt + 1;
         } else {
            lbnd[ i ] = intprt;
         }

/* Ensure that any supplied pixel bounds are honoured exactly. */
         if( isbnd[ i ] ) {
            if( ispix1[ i ] ) lbnd[ i ] = value1[ i ];
            if( ispix2[ i ] ) ubnd[ i ] = value2[ i ];

/* Ensure that any supplied pixel widths are honoured exactly. */
         } else if( ispix2[ i ] ) {

/* Find out by how many pixels the width of our current interval is short of
   the requested width. Pass on if we already have the right width. */
            nextra = value2[ i ] - ubnd[ i ] + lbnd[ i ] - 1;
            if( nextra != 0 ) {

/* We will expand the current interval by an equal number of pixels at each
   end to achieve the requested width. */
               lbnd[ i ] -= nextra/2;
               ubnd[ i ] += nextra/2;

/* In addition, if we are short by an odd number of pixels, we need to
   expand one of the two ends by an extra pixel. */
               nextra -= 2*( nextra/2 );
               if( nextra != 0 ) {

/* If the centre was specified as a pixel coordinate, we choose the end
   which puts the central pixel closer to the centre of the interval. */
                  if( ispix1[ i ] ) {

/* If the central value is more positive than the interval centre, move
   the interval to more positive values. */
                     if( 2*value1[ i ] > lbnd[ i ] + ubnd[ i ] - 1 ) {

/* If the current width is too little, extend the upper bound. */
                        if( nextra > 0 ) {
                           ubnd[ i ] += nextra;

/* If the current width is too much, retract the lower bound. */
                        } else {
                           lbnd[ i ] -= nextra;
                        }

/* If the central value is more negative than the interval centre, move
   the interval to more negative values. */
                     } else {

/* If the current width is too little, extend the lower bound. */
                        if( nextra > 0 ) {
                           lbnd[ i ] -= nextra;

/* If the current width is too much, retract the upper bound. */
                        } else {
                           ubnd[ i ] += nextra;
                        }
                     }

/* If the centre was not specified as a pixel coordinate, we arbitrarily
   choose to adjust the upper bound. */
                  } else {
                     ubnd[ i ] += nextra;
                  }
               }
            }
         }
      }
   }

/* Arrive here if an error occurs. */
L999:

/* End the AST context. */
   astEnd;

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Wplim", status );

}

