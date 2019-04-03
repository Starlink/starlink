#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include <math.h>
#include "mers.h"

void ndf1Axlim( int iax, NdfACB *acb, double value1, double value2,
                int ispix1, int ispix2, int isbnd, hdsdim *lbnd, hdsdim *ubnd,
                int *status ){
/*
*+
*  Name:
*     ndf1Axlim

*  Purpose:
*     Determine pixel limits for an NDF axis.

*  Synopsis:
*     void ndf1Axlim( int iax, NdfACB *acb, double value1, double value2,
*                     int ispix1, int ispix2, int isbnd, hdsdim *lbnd,
*                     hdsdim *ubnd, int *status )

*  Description:
*     This function accepts two values which have been supplied as
*     dimension bounds in a NDF section specification, and which may refer
*     to coordinates in the NDF's axis coordinate system, and calculates
*     the corresponding NDF pixel-index bounds.

*  Parameters:
*     iax
*        Zero-based index of the NDF axis.
*     acb
*        Pointer to the NDF entry in the ACB.
*     value1
*        First value specifying the NDF dimension bounds.
*     value2
*        Second value specifying the NDF dimension bounds.
*     ispix1
*        Whether "value1" is a pixel index (as opposed to an axis value).
*     ispix2
*        Whether "value2" is a pixel index (as opposed to an axis value).
*     isbnd
*        Whether "value1" and "value2" specify the lower and upper bounds
*        directly (as opposed to specifying the centre and width).
*     *lbnd
*        Returned holding the lower pixel-index bound.
*     *ubnd
*        Returned holding the upper pixel-index bound.
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
   Ary *idd;             /* ID for temporary axis data array */
   Ary *idw;             /* ID for temporary axis width array */
   AryPlace *place;      /* ARY_ system placeholder */
   NdfACB *acb0;         /* Pointer to base NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   double ax[ 2 ];       /* Axis coordinate array */
   double cen;           /* Pixel centre value */
   double cent0[ 2 ];    /* "Lower" pixel centre array */
   double cent1[ 2 ];    /* "Nearest" pixel centre array */
   double space0[ 2 ];   /* Pixel spacing */
   double var;           /* Pixel variance value */
   double varian;        /* Dummy variance array */
   double wid;           /* Pixel width value */
   double width1[ 2 ];   /* "Nearest" pixel width array */
   hdsdim lbnd0[ NDF__MXDIM ];     /* Lower pixel bounds of base NDF */
   hdsdim offs[ NDF__MXDIM ];      /* Pixel offsets for NDF section */
   hdsdim ubnd0[ NDF__MXDIM ];     /* Upper pixel bounds of base NDF */
   int datmap;           /* Axis data array already mapped? */
   int dce;              /* Data conversion errors? */
   int dstate;           /* Logical state of axis data array */
   int inc;              /* Axis values increase? */
   int inpix[ 2 ];       /* Coordinate lies in a pixel? */
   int ipix0[ 2 ];       /* "Lower" pixel index array */
   int ipix1[ 2 ];       /* "Nearest" pixel index array */
   int ndim0;            /* Number of base NDF dimensions */
   int needax;           /* Access to axis values is needed? */
   int widmap;           /* Axis width array already mapped? */
   int wstate;           /* Logical state of axis width array */
   size_t el;            /* Number of elements mapped */
   void *dpntr;          /* Pointer to mapped axis data array */
   void *wpntr;          /* Pointer to mapped axis width array */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise. */
   dstate = 0;
   dpntr = NULL;
   wstate = 0;
   wpntr = NULL;
   acb0 = NULL;

/* Obtain an index to the data object entry in the DCB and obtain the
   bounds of the data object from the ARY_ system identifier for its
   main data array, held in the DCB. */
   dcb = acb->dcb;
   aryBound( dcb->did, NDF__MXDIM, lbnd0, ubnd0, &ndim0, status );

/* Obtain access to the axis arrays if necessary.
   ============================================= */

/* See if access to the NDF's axis arrays will be required. */
   needax = ( ! ( ispix1 && ispix2 ) );

/* If so, then ensure that axis data array information is available in
   the DCB. */
   if( ( *status == SAI__OK ) && needax ) {
      ndf1Dad( iax, dcb, status );
      if( *status == SAI__OK ) {

/* Determine the array's state. If it does not exist, then its state is
   zero.  Otherwise, use its identifier to determine its state. */
         dstate = ( dcb->adid[ iax ] != NULL );
         if( dstate ) aryState( dcb->adid[ iax ], &dstate, status );
      }

/* If no error has occurred, and the axis data array is in a defined
   state, then ensure that axis width array information is also
   available. */
      if( ( *status == SAI__OK ) && dstate ) {
         ndf1Daw( iax, dcb, status );
         if( *status == SAI__OK ) {

/* Determine the state of the axis width array. If it does not exist,
   then its state is zero.  Otherwise, use its identifier to
   determine its state. */
            wstate = ( dcb->awid[ iax ] != NULL );
            if( wstate ) aryState( dcb->awid[ iax ], &wstate, status );
         }
      }

/* If the axis data array is in a defined state, then its values must be
   accessed. */
      if( ( *status == SAI__OK ) && dstate ) {

/* If the NDF is a section, then obtain an index to the data object
   entry in the DCB and create a new base NDF entry in the ACB to refer
   to it.  Note that we must do this so as to access the entire axis
   array, in case extrapolation outside the original NDF bounds is
   required. */
         if( acb->cut ) {
            ndf1Crnbn( dcb, &acb0, status );

/* Map the required axis data array for the base NDF for reading as
   double precision values. */
            ndf1Admap( iax, acb0, "_DOUBLE", "READ", &dpntr, &el, status );

/* If the NDF supplied is a base NDF, then we can access its axis
   arrays directly. First check if the required axis data array is
   already mapped for access. If so, then the currently mapped values
   will be used, but a double precision copy of them must be made. */
         } else if( acb->admap[ iax ] ) {
            datmap = 1;

/* Create and map a temporary array to provide workspace for the copy. */
            aryTemp( &place, status );
            aryNew( "_DOUBLE", 1, lbnd0 + iax, ubnd0 + iax, &place, &idd,
                    status );
            aryMap( idd, "_DOUBLE", "WRITE", &dpntr, &el, status );

/* Convert the mapped values to double precision. */
            ndf1CvtD( 1, el, acb->admtp[ iax ], acb->admpt[ iax ], dpntr,
                      &dce, status );

/* If the axis data array is not mapped, then note this fact and map it
   in the required manner. */
         } else {
            datmap = 0;
            ndf1Admap( iax, acb, "_DOUBLE", "READ", &dpntr, &el, status );
         }

/* If an attempt was made to access the axis data array, but this
   failed, then report contextual information. */
         if( *status != SAI__OK ) {
            errRep( " ", "Unable to access the axis CENTRE array while "
                    "converting axis coordinates to pixel indices.", status );
         }
      }

/* If no error has occurred and the axis width array is in a defined
   state, then its values must be accessed. */
      if( ( *status == SAI__OK ) && wstate ) {

/* If the NDF is a section, then map the required axis width array for
   the base NDF for reading as double precision values. */
         if( acb->cut ) {
            ndf1Awmap( iax, acb0, "_DOUBLE", "READ", &wpntr, &el, status );

/* If the NDF supplied is a base NDF, then we can access its axis
   arrays directly. First check if the required axis width array is
   already mapped for access. If so, then the currently mapped values
   will be used, but a double precision copy of them must be made. */
         } else if( acb->awmap[ iax ] ) {
            widmap = 1;

/* Create and map a temporary array to provide workspace for the copy. */
            aryTemp( &place, status );
            aryNew( "_DOUBLE", 1, lbnd0 + iax, ubnd0 + iax, &place, &idw,
                    status );
            aryMap( idw, "_DOUBLE", "WRITE", &wpntr, &el, status );

/* Convert the mapped values to double precision. */
            ndf1CvtD( 1, el, acb->awmtp[ iax ], acb->awmpt[ iax ], wpntr,
                      &dce, status );

/* If the axis width array is not mapped, then note this fact and map
   it in the required manner. */
         } else {
            widmap = 0;
            ndf1Awmap( iax, acb, "_DOUBLE", "READ", &wpntr, &el, status );
         }

/* If an attempt was made to access the axis width array, but this
   failed, then report contextual information. */
         if( *status != SAI__OK ) {
            errRep( " ", "Unable to access the axis WIDTH array while "
                    "converting axis coordinates to pixel indices.", status );
         }
      }
   }

/* Calculate the pixel limits for the NDF dimension.
   ================================================ */

/* If the values given specify the dimension bounds directly, then
   determine the lower and upper bounds. */
   if( *status == SAI__OK ) {
      if( isbnd ) {

/* Use the lower bound directly if it is a pixel index. */
         if( ispix1 ) {
            *lbnd = NDF_NINT( value1 );

/* Otherwise, convert from an axis value to a pixel index and use this
   new value. */
         } else {
            ax[ 0 ] = value1;
            ndf1A2p( 1, ax, lbnd0[ iax ], ubnd0[ iax ], dstate, wstate,
                     dpntr, wpntr, &inc, ipix0, cent0, space0, inpix,
                     ipix1, cent1, width1, status );
            if( *status == SAI__OK ) *lbnd = ipix1[ 0 ];
         }

/* Similarly, use the upper bound directly if it is a pixel index. */
         if( ispix2 ) {
            *ubnd = NDF_NINT( value2 );

/* Otherwise, convert from an axis value first. */
         } else {
            ax[ 0 ] = value2;
            ndf1A2p( 1, ax, lbnd0[ iax ], ubnd0[ iax ], dstate, wstate,
                     dpntr, wpntr, &inc, ipix0, cent0, space0, inpix,
                     ipix1, cent1, width1, status );
            if( *status == SAI__OK ) *ubnd = ipix1[ 0 ];
         }

/* If the values supplied specify the centre and width of the axis
   range, then each combination of pixel-index/axis value must be
   handled in turn... */
      } else {

/* If both values are pixel indices, then derive the lower and upper
   bounds directly. */
         if( ispix1 && ispix2 ) {
            *lbnd = NDF_NINT( value1 ) - NDF_NINT( value2 )/2;
            *ubnd = NDF_NINT( value1 ) + NDF_NINT( value2 )/2;
            if( *ubnd - *lbnd >= NDF_NINT( value2 ) ) (*lbnd)++;

/* If the first value is an axis value and the second is a pixel index,
   then first convert the axis value to a pixel index. */
         } else if( ( !ispix1 ) && ispix2 ) {
            ax[ 0 ] = value1;
            ndf1A2p( 1, ax, lbnd0[ iax ], ubnd0[ iax ], dstate, wstate,
                     dpntr, wpntr, &inc, ipix0, cent0, space0, inpix,
                     ipix1, cent1, width1, status );

/* Then derive the lower and upper bounds, as above. */
            if( *status == SAI__OK ) {
               *lbnd = ipix1[ 0 ] - NDF_NINT( value2 )/2;
               *ubnd = ipix1[ 0 ] + NDF_NINT( value2 )/2;
               if( *ubnd - *lbnd >= NDF_NINT( value2 ) ) (*lbnd)++;
            }

/* If neither value is a pixel index, then derive the lower and upper
   limits in terms of axis values and convert each of these to pixel
   indices. */
         } else if( ( !ispix1 ) && ( !ispix2 ) ) {
            ax[ 0 ] = value1 - 0.5*value2;
            ax[ 1 ] = value1 + 0.5*value2;
            ndf1A2p( 2, ax, lbnd0[ iax ], ubnd0[ iax ], dstate, wstate,
                     dpntr, wpntr, &inc, ipix0, cent0, space0, inpix,
                     ipix1, cent1, width1, status );

/* Ensure that the pixel indices are in the correct order (remember,
   axis values may decrease as well as increase with increasing pixel
   index). */
            if( *status == SAI__OK ) {
               *lbnd = NDF_MIN( ipix1[ 0 ], ipix1[ 1 ] );
               *ubnd = NDF_MAX( ipix1[ 0 ], ipix1[ 1 ] );
            }

/* If the first value is a pixel index, but the second is not, then
   first convert the first value to an axis value. */
         } else if( ispix1 && ( !ispix2 ) ) {
            ipix1[ 0 ] = NDF_NINT( value1 );
            ndf1P2a( 1, ipix1, lbnd0[ iax ], ubnd0[ iax ], dstate, wstate,
                     0, dpntr, wpntr, &varian, &cen, &wid, &var, status );

/* Derive the lower and upper bounds in terms of axis values and convert
   these to pixel indices. */
            if( *status == SAI__OK ) {
               ax[ 0 ] = cen - 0.5*value2;
               ax[ 1 ] = cen + 0.5*value2;
               ndf1A2p( 2, ax, lbnd0[ iax ], ubnd0[ iax ], dstate, wstate,
                        dpntr, wpntr, &inc, ipix0, cent0, space0, inpix,
                        ipix1, cent1, width1, status );

/* Ensure that the pixel indices are in the correct order. */
               if( *status == SAI__OK ) {
                  *lbnd = NDF_MIN( ipix1[ 0 ], ipix1[ 1 ] );
                  *ubnd = NDF_MAX( ipix1[ 0 ], ipix1[ 1 ] );
               }
            }
         }
      }
   }

/* Release the axis arrays if they were accessed.
   ============================================= */

/* If the axis data array has been accessed, then release it. If the
   NDF is a section, then unmap the base NDF axis data array and annul
   its temporary entry in the ACB. */
   if( needax ) {
      if( dstate ) {
         if( acb->cut ) {
            if( acb0 ) {
               ndf1Adump( iax, acb0, status );
               ndf1Anl( &acb0, status );
            }

/* If access was to a temporary copy of the array, then annul the
   identifier for this temporary copy. Otherwise, simply unmap the
   array. */
         } else if( datmap ) {
            aryAnnul( &idd, status );
         } else {
            ndf1Adump( iax, acb, status );
         }
      }

/* Similarly, if the axis width array has been accessed, then release
   it. If the NDF is a section, then unmap the base NDF axis width
   array and annul its temporary entry in the ACB. */
      if( wstate ) {
         if( acb->cut ) {
            if( acb0 ) {
               ndf1Awump( iax, acb0, status );
               ndf1Anl( &acb0, status );
            }

/* If access was to a temporary copy of the array, then annul the
   identifier for this temporary copy. Otherwise, simply unmap the
   array. */
         } else if( widmap ) {
            aryAnnul( &idw, status );
         } else {
            ndf1Awump( iax, acb, status );
         }
      }
   }

/* Make final adjustments and checks.
   ================================= */

/* If the NDF is a section, then obtain the value of any pixel offset
   between the base NDF and the section, and correct the bounds for this
   offset. */
   if( ( *status == SAI__OK ) && acb->cut ) {
      aryOffs( dcb->did, acb->did, NDF__MXDIM, offs, status );
      if( *status == SAI__OK ) {
         *lbnd += offs[ iax ];
         *ubnd += offs[ iax ];
      }
   }

/* If no error has occurred, then check that the lower bound does not
   exceed the upper bound and report an error if it does. */
   if( *status == SAI__OK ) {
      if( *lbnd > *ubnd ) {
         *status = NDF__BNDIN;
         msgSeti( "LBND", *lbnd );
         msgSeti( "UBND", *ubnd );
         errRep( " ", "Lower pixel bound (^LBND) exceeds the upper bound "
                 "(^UBND).", status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Axlim", status );

}

