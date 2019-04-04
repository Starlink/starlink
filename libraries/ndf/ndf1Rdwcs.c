#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "prm_par.h"
#include <string.h>

#define nstd 4 /* No. standard NDF coordinate systems */

void ndf1Rdwcs( NdfACB *acb, AstFrameSet **iwcs, int *status ){
/*
*+
*  Name:
*     ndf1Rdwcs

*  Purpose:
*     Read WCS information from an "acb" entry.

*  Synopsis:
*     void ndf1Rdwcs( NdfACB *acb, AstFrameSet **iwcs, int *status )

*  Description:
*     This function returns a pointer to an AST_ FrameSet which contains
*     WCS information for an NDF with an entry in the "acb". If the NDF"s
*     WCS component is undefined, default WCS information is provided.
*
*     Account is taken of NDF sections and appropriate adjustments are made
*     to the WCS information returned.

*  Parameters:
*     acb
*        Pointer to the NDF entry in the "acb".
*     *iwcs
*        Returned holding the pointer to a new AST_ FrameSet containing the
*        WCS information. This FrameSet will be derived from a deep copy of
*        the internally-stored information, so may be modified without
*        affecting the subsequent behaviour of the NDF_ library.
*     *status
*        The global status.

*  Notes:
*     - A value of AST__NULL will be returned for the "iwcs" parameter if
*     this function is called with "status" set, or if it should fail for
*     any reason.

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
   Ary *ary;             /* ID of temporary array */
   AryPlace *place;      /* ARY_ placeholder */
   AstCmpMap *cmpmap;    /* Base GRID to section FRACTION Mapping */
   AstFrame *frame;      /* Pointer to Frame */
   AstFrameSet *new;     /* Pointer to new FrameSet */
   AstMapping *axmap;    /* Mapping for NDF axis */
   AstMapping *map;      /* Pointer to Mapping */
   AstMapping *tmpmap;   /* Pointer to temporary Mapping */
   AstUnitMap *unit;     /* Pointer to UnitMap */
   AstWinMap *map0;      /* Pointer to base->section GRID Mapping */
   NdfACB *acbt;         /* Pointer to temporary ACB entry */
   NdfDCB *dcb;          /* Pointer to NDF entry in the DCB */
   char axtype[ NDF__SZTYP + 1 ];  /* Axis data array numeric type */
   double *pntr;         /* Pointer to mapped values */
   double axca[ NDF__MXDIM ];      /* Axis coordinate, 1st point */
   double axcb[ NDF__MXDIM ];      /* Axis coordinate, 2nd point */
   double constants[ NDF__MXDIM ]; /* Constants array for PermMap */
   double eps;           /* Value for LutEpsilon attribute */
   double iaa[ NDF__MXDIM ];       /* Index in ACB array, 1st point */
   double iab[ NDF__MXDIM ];       /* Index in ACB array, 2nd point */
   double ida[ NDF__MXDIM ];       /* Index in DCB array, 1st point */
   double idb[ NDF__MXDIM ];       /* Index in DCB array, 2nd point */
   double maxv;          /* Max value in AXIS data array */
   double mean;          /* Mean value in AXIS data array */
   double minv;          /* Min value in AXIS data array */
   double npca[ NDF__MXDIM ];      /* Normalised Pix. coord., 1st point */
   double npcb[ NDF__MXDIM ];      /* Normalised Pix. coord., 2nd point */
   double pca[ NDF__MXDIM ];       /* Pixel coordinate, 1st point */
   double pcb[ NDF__MXDIM ];       /* Pixel coordinate, 2nd point */
   double rms;           /* RMS value in AXIS data array */
   double sigma;         /* Standard deviation of values in AXIS data array */
   hdsdim lbnda[ NDF__MXDIM ];     /* Lower pixel index bound (ACB entry) */
   hdsdim lbndd[ NDF__MXDIM ];     /* Lower pixel index bound (DCB entry) */
   hdsdim shift[ NDF__MXDIM ];     /* NDF pixel-index shifts */
   hdsdim ubnda[ NDF__MXDIM ];     /* Upper pixel index bound (ACB entry) */
   hdsdim ubndd[ NDF__MXDIM ];     /* Upper pixel index bound (DCB entry) */
   int axstat;           /* NDF axis component present? */
   int dce;              /* Data conversion errors? */
   int i;                /* Loop index */
   int ibase;            /* Index of base Frame */
   int icurr;            /* Index of current Frame */
   int idim;             /* Loop counter for NDF dimensions */
   int iframe;           /* Loop counter for Frame indices */
   int mapped;           /* Axis data mapped? */
   int nconstants;       /* Number of PermMap constants */
   int ndima;            /* No. NDF dimensions (ACB entry) */
   int ndimd;            /* No. NDF dimensions (DCB entry) */
   int perma[ NDF__MXDIM ];        /* Permutation array for ACB axes */
   int permd[ NDF__MXDIM ];        /* Permutation array for DCB axes */
   size_t el;            /* Number of mapped values */
   size_t ngood;         /* Number of good values in AXIS data array */

/* Initialise the returned AST_ pointer. */
   *iwcs = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain the index of the data object in the DCB and ensure that AXIS
   and WCS information is available for it. */
   dcb = acb->dcb;
   ndf1Da( dcb, status );
   ndf1Dw( dcb, status );

/* Obtain the pixel-index bounds of the NDF entry in the "acb" and of the
   data object in the DCB. */
   aryBound( acb->did, NDF__MXDIM, lbnda, ubnda, &ndima, status );
   aryBound( dcb->did, NDF__MXDIM, lbndd, ubndd, &ndimd, status );

/* Obtain the offsets between the pixel indices in these two NDFs. */
   aryOffs( dcb->did, acb->did, NDF__MXDIM, shift, status );

/* Determine if the NDF's AXIS component is in a defined state. */
   if( *status == SAI__OK ) {
      axstat = ( dcb->aloc[ 0 ] != NULL );

/* Obtain raw WCS data.
   --------------------
   If WCS information is available in the DCB, then make a copy of the
   data object's WCS FrameSet. */
      if( dcb->iwcs ) {
         *iwcs = astCopy( dcb->iwcs );

/* Otherwise, we must create a default FrameSet to represent the
   standard NDF coordinate systems. We start by simply adding the
   necessary Frames to a dummy FrameSet, inter-relating the Frames with
   null Mappings (UnitMaps). */
      } else {

/* Start by adding a Frame to represent the data grid coordinate
   system, in which the first NDF pixel is centred at (1,1). */
         frame = astFrame( ndimd, "Domain=GRID" );
         *iwcs = astFrameSet( frame, " " );
         frame = astAnnul( frame );

/* Add a second Frame to represent pixel coordinates, which take
   account of the NDF's lower pixel-index bounds. For now, use a
   UnitMap to relate this to the first Frame. */
         unit = astUnitMap( ndimd, " " );
         frame = astFrame( ndimd, "Domain=PIXEL" );
         astAddFrame( *iwcs, AST__BASE, unit, frame );
         frame = astAnnul( frame );

/* Add a third Frame to represent axis coordinates, which take account
   of data in the NDF's AXIS component. Again, use a UnitMap. */
         frame = astFrame( ndimd, "Domain=AXIS" );
         astAddFrame( *iwcs, AST__BASE, unit, frame );
         frame = astAnnul( frame );

/* Add a fourth Frame to represent normalised pixel coordinates (each
   pixel axis spans a range 0.0 to 1.0 in this Frame). Again, use a
   UnitMap. */
         frame = astFrame( ndimd, "Domain=FRACTION" );
         astAddFrame( *iwcs, AST__BASE, unit, frame );
         frame = astAnnul( frame );

/* Annul the UnitMap pointer. */
         unit = astAnnul( unit );

/* The FRACTION Frame will now be current in our dummy FrameSet. If the
   NDF's AXIS component is defined, change this to make the AXIS
   coordinate Frame current instead. Otherwise, change it to make the
   pixel coordinate Frame current. */
         if( !axstat ) {
            astSetI( *iwcs, "Current", 2 );
         } else {
            astSetI( *iwcs, "Current", 3 );
         }
      }

/* Change dimensionality.
   ----------------------
   If the "acb" entry's dimensionality differs from that of the data
   object in the DCB, then we must modify the FrameSet to allow for
   this. */
      if( ndima != ndimd ) {

/* Create a new FrameSet containing just the new base Frame (with the
   new number of dimensions). */
         frame = astFrame( ndima, "Domain=GRID" );
         new = astFrameSet( frame, " " );
         frame = astAnnul( frame );

/* Add a Frame to represent pixel coordinates, related to the base Frame
   by a UnitMap. */
         unit = astUnitMap( ndima, " " );
         frame = astFrame( ndima, "Domain=PIXEL" );
         astAddFrame( new, AST__BASE, unit, frame );
         frame = astAnnul( frame );

/* Similarly, add a Frame to represent axis coordinates. */
         frame = astFrame( ndima, "Domain=AXIS" );
         astAddFrame( new, AST__BASE, unit, frame );
         frame = astAnnul( frame );

/* Similarly, add a Frame to represent normalised pixel coordinates. */
         frame = astFrame( ndima, "Domain=FRACTION" );
         astAddFrame( new, AST__BASE, unit, frame );
         frame = astAnnul( frame );

/* Annul the UnitMap pointer. */
         unit = astAnnul( unit );

/* We will now set up a PermMap which relates DCB coordinates to "acb"
   coordinates. Loop through each relevant dimension to set up the
   permutation arrays required. */
         nconstants = 0;
         for( idim = 0; idim < NDF_MAX( ndimd, ndima ); idim++ ){

/* Identify DCB dimensions with corresponding dimensions in the "acb", if
   they exist. */
            if( idim < ndimd ) {
               if( idim < ndima ) {
                  permd[ idim ] = idim + 1;

/* Otherwise, flag the dimension so it receives a constant coordinate
   value. Set this value equal to 1, less any pixel-index shift which
   may have been applied in this dimension (this corresponds with the
   standard process of extending bounds by adding new dimensions with
   the value 1, to match dimensionalities). */
               } else {
                  nconstants++;
                  constants[ nconstants - 1 ] = (double)( -shift[ idim ] + 1 );
                  permd[ idim ] = -nconstants;
               }
            }

/* Repeat the process to identify "acb" dimensions with corresponding DCB
   dimensions. In this case, the constant coordinate value used is
   always 1, because any additional pixel-index shifts are accommodated
   by the "ACB offset" and "pixel coordinate system" Mappings set up
   later (see below). */
            if( idim < ndima ) {
               if( idim < ndimd ) {
                  perma[ idim ] = idim + 1;
               } else {
                  nconstants++;
                  constants[ nconstants - 1 ] = (double)( 1 );
                  perma[ idim ] = -nconstants;
               }
            }
         }

/* Create a PermMap to convert between the two dimensionalities. */
         map = (AstMapping *) astPermMap( ndima, perma, ndimd, permd, constants, " " );

/* Obtain the indices of the base and current Frames in the original
   FrameSet. */
         ibase = astGetI( *iwcs, "Base" );
         icurr = astGetI( *iwcs, "Current" );

/* Make the base Frame current and add the original FrameSet to the new
   one we have just created. Their base Frames (i.e. data grid indices)
   are inter-related by the PermMap created above. */
         astSetI( *iwcs, "Current", ibase );
         astAddFrame( new, AST__BASE, map, *iwcs );

/* Annul the PermMap pointer and the original FrameSet pointer. Replace
   the latter with a pointer to the new FrameSet. */
         map = astAnnul( map );
         *iwcs = astAnnul( *iwcs );
         *iwcs = new;

/* Remove the standard Frames from the original "iwcs" FrameSet (allowing
   for their new indices in the new FrameSet), as these have now been
   replaced. */
         for( i = 0; i < nstd; i++ ){
            astRemoveFrame( *iwcs, ibase + nstd );
         }

/* Re-select the original current Frame index (which may now correspond
   to one of the new Frames). */
         astSetI( *iwcs, "Current", icurr );
      }

/* Correct for "acb" offsets.
   ------------------------
   The base Frame now corresponds to the data grid indices in the base
   NDF (from whose DCB entry we obtained the WCS information). However,
   the set of pixels accessed via the "acb" entry may be offset from
   this, so might have a different data grid coordinate system. To
   correct for this, we remap the base Frame. */

/* Set up a Mapping which converts from grid indices in the base NDF
   (first pixel at 1,1) to grid indices in the NDF section (first pixel
   also at 1,1, but corresponding to a different base NDF pixel). In
   doing this, allow for any pixel-index shifts that may have been
   applied, as these will cause the section's pixel index bounds to be
   offset. */
      for( idim = 0; idim < ndima; idim++ ){
         ida[ idim ] = 0.5;
         idb[ idim ] = 1.5;
         iaa[ idim ] = (double)( lbndd[ idim ] - lbnda[ idim ] + shift[
                                 idim ] ) + 0.5;
         iab[ idim ] = (double)( lbndd[ idim ] - lbnda[ idim ] + shift[
                                 idim ] ) + 1.5;
      }
      map0 = astWinMap( ndima, ida, idb, iaa, iab, " " );

/* Remap the base Frame to reflect the change of data grid origin (so
   that all other coordinate systems described by the FrameSet remain
   attached to the same actual data pixels). */
      astRemapFrame( *iwcs, AST__BASE, map0 );

/* Set up pixel coordinate system.
   -------------------------------
   Set up a Mapping which converts from grid indices in the base NDF
   (the coordinate system to which the pixel coordinate Frame is still
   attached) and pixel coordinates corresponding with the "acb" entry. */
      for( idim = 0; idim < ndima; idim++ ){
         ida[ idim ] = 0.5;
         idb[ idim ] = 1.5;
         pca[ idim ] = (double)( lbndd[ idim ] + shift[ idim ] ) - 1.0;
         pcb[ idim ] = (double)( lbndd[ idim ] + shift[ idim ] );
      }

/* Use this Mapping to remap the second Frame to define the pixel
   coordinate system. */
      map = (AstMapping *) astWinMap( ndima, ida, idb, pca, pcb, " " );
      astRemapFrame( *iwcs, 2, map );

/* Set up axis coordinate system.
   ------------------------------
   If the NDF's AXIS component is in an undefined state, then remap the
   Frame representing the axis coordinate system using the same Mapping
   as above. This makes the default axis coordinate system identical to
   the pixel coordinate system. */
      if( !axstat ) astRemapFrame( *iwcs, 3, map );

/* Annul the Mapping used. */
      map = astAnnul( map );

/* If the AXIS component is defined, then we must obtain access to the
   axis data arrays in order to set up this coordinate system. */
      if( axstat ) {

/* If the "acb" entry describes an NDF section, create a temporary "acb"
   entry to describe the base NDF, through which we will access its
   axis data. This is necessary to prevent any truncation or
   extrapolation of the axis data occurring. */
         if( acb->cut ) ndf1Crnbn( dcb, &acbt, status );

/* Loop to access the axis centre array for each "acb" axis. */
         for( idim = 0; idim < ndima; idim++ ){

/* If the NDF is a section, map the required axis data array for
   reading as double precision values via the temporary "acb" entry. */
            if( acb->cut ) {
               ndf1Admap( idim + 1, acbt, "_DOUBLE", "READ", (void **) &pntr,
                          &el, status );

/* If it is not a section, check if the required axis data array is
   already mapped for access. If so, then the currently mapped values
   will be used, but a double precision copy of them must be made. */
            } else if( acb->admap[ idim ] ) {
               mapped = 1;

/* Create and map a temporary ARY_ array to provide workspace for the
   copy. */
               aryTemp( &place, status );
               aryNew( "_DOUBLE", 1, lbndd + idim, ubndd + idim, &place,
                       &ary, status );
               aryMap( ary, "_DOUBLE", "WRITE", (void **) &pntr, &el, status );

/* Convert the mapped values to double precision. */
               ndf1CvtD( 1, el, acb->admtp[ idim ], acb->admpt[ idim ],
                         pntr, &dce, status );

/* If the axis data array is not already mapped, then note this fact
   and map it in the required manner. */
            } else {
               mapped = 0;
               ndf1Admap( idim + 1, acb, "_DOUBLE", "READ", (void **) &pntr,
                          &el, status );
            }

/* If 2 or more axis centre values have been mapped, create a LutMap
   containing these values as lookup table entries. This LutMap
   converts from the base NDF's data grid coordinate system (to which
   the AXIS coordinate Frame is still attached) to the axis coordinate
   system along the current axis. */
            if( el > 1 ) {
               axmap = (AstMapping *) astLutMap( el, pntr, 1.0, 1.0, " " );

/* Set an appropriate value for the LutEpsilon attribute (the relative
   error of the values in the table), based on the data type of the AXIS
   structure. */
               ndf1Adtyp( idim + 1, acb, axtype, sizeof( axtype ), status );
               if( !strcmp( axtype, "_DOUBLE" ) ) {
                  eps = VAL__EPSD;
               } else if( !strcmp( axtype, "_REAL" ) ) {
                  eps = VAL__EPSR;

/* For integer type data, calculate a relative error using an absolute
   error of 1.0 and the "rms" data value in the array. */
               } else {
                  ndf1Stats( el, pntr, &maxv, &minv, &mean, &sigma, &rms,
                             &ngood, status );
                  if( rms > 0.0 && rms != VAL__BADD ) {
                     eps = 1.0/rms;
                  } else {
                     eps = 1.0;
                  }
               }

/* Set the relative error of the LutMap. */
               astSetD( axmap, "LutEpsilon", eps );

/* If only one value is available (the size of this NDF dimension is
   only 1 pixel), then copy the mapped value to a double precision
   array so that its value can be accessed. */
            } else {
               axca[ 0 ] = *pntr;

/* Use this value to set up a linear Mapping (for this dimension only)
   that converts from the base NDF's grid index (1) to the required
   axis centre value, with an increment of unity when extrapolating
   outside the NDF along this dimension. */
               axcb[ 0 ] = axca[ 0 ] + 1.0;
               ida[ 0 ] = 1.0;
               idb[ 0 ] = 2.0;
               axmap = (AstMapping *) astWinMap( 1, ida, idb, axca, axcb, " " );
            }

/* Now relinquish access to the axis data array. If it was mapped via
   the temporary "acb" entry, then unmap it. */
            if( acb->cut ) {
               ndf1Adump( idim + 1, acbt, status );

/* If access was to a temporary copy of the array, then annul the
   identifier for the temporary copy. Otherwise, simply unmap the array
   via the current "acb" entry. */
            } else if( mapped ) {
               aryAnnul( &ary, status );
            } else {
               ndf1Adump( idim + 1, acb, status );
            }

/* For the first NDF dimension, use the Mapping produced above directly,
   by cloning its pointer. */
            if( idim + 1 == 1 ) {
               map = astClone( axmap );

/* For subsequent dimensions, accumulate the Mappings by combining them
   in parallel in a CmpMap. Annul the previous accumulated Mapping
   pointer on each occasion and replace it with the new one. */
            } else {
               tmpmap = (AstMapping *) astCmpMap( map, axmap, 0, " " );
               map = astAnnul( map );
               map = tmpmap;
            }

/* Annul the Mapping pointer for the current NDF dimension. */
            axmap = astAnnul( axmap );
         }

/* If a temporary "acb" entry was created, then annul it. */
         if( acb->cut ) ndf1Anl( &acbt, status );

/* Remap the Frame representing the axis coordinate system using the
   Mapping generated above. Then annul the Mapping pointer. */
         astRemapFrame( *iwcs, 3, map );
         map = astAnnul( map );
      }

/* Set up normalised pixel coordinate system.
   ------------------------------------------
   Set up a Mapping which converts from grid indices in the section (note
   section, not base) NDF and pixel coordinates corresponding with the "acb"
   entry. */
      for( idim = 0; idim < ndima; idim++ ){
         iaa[ idim ] = 0.5;
         iab[ idim ] = (double)( ubnda[ idim ] - lbnda[ idim ] ) + 1.5;
         npca[ idim ] = 0.0;
         npcb[ idim ] = 1.0;
      }
      map = (AstMapping *) astWinMap( ndima, iaa, iab, npca, npcb, " " );

/* In order to remap the FRACTION frame, we need the mapping from the
   base NDF (base, not section) to the FRACTION Frame. This Mapping is
   the formed by applying the Mapping from base to section ("map0"),
   followed by the Mapping from section to FRACTION ("map"). */
      cmpmap = astCmpMap( map0, map, 1, " " );

/* Use this Mapping to remap the fourth Frame to define the normalised
   pixel coordinate system. */
      astRemapFrame( *iwcs, 4, cmpmap );

/* Free remaining resource. */
      map = astAnnul( map );
      cmpmap = astAnnul( cmpmap );
      map0 = astAnnul( map0 );

   }

/* Set up Frame attributes.
   ------------------------
   Save the current Frame index and loop through the standard Frames in
   the Frameset, making each current in turn. Initialise each of these
   Frames (this sets up its title, axis labels, etc.). Restore the
   original current Frame afterwards. */
   icurr = astGetI( *iwcs, "Current" );
   for( iframe = 0; iframe < nstd; iframe++ ){
      astSetI( *iwcs, "Current", iframe + 1 );
      ndf1Inifr( acb, *iwcs, status );
   }
   astSetI( *iwcs, "Current", icurr );

/* Simplify the resulting FrameSet. */
   new = astSimplify( *iwcs );
   *iwcs = astAnnul( *iwcs );
   *iwcs = new;

/* If an error occurred, annul the returned FrameSet pointer. */
   if( *status != SAI__OK ) *iwcs = astAnnul( *iwcs );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Rdwcs", status );

}

