#include "atl.h"
#include "ast.h"
#include "mers.h"
#include "sae_par.h"

void atlAddWcsAxis8( AstFrameSet *wcs, AstMapping *map, AstFrame *frm,
                     int64_t *lbnd, int64_t *ubnd, int *status ){
/*
*+
*  Name:
*     atlAddWcsAxis8

*  Purpose:
*     Add one or more axes to an NDFs WCS FrameSet.

*  Language:
*     C.

*  Invocation:
*     void atlAddWcsAxis8(  AstFrameSet *wcs, AstMapping *map, AstFrame *frm,
*                           int64_t *lbnd, int64_t *ubnd, int *status )

*  Description:
*     This function adds one or more new axes to all the Frames in an NDF
*     WCS FrameSet. Frames that are known to be NDF-special (e.g. GRID,
*     AXIS, PIXEL and FRACTION) are expanded to include a number of extra
*     appropriate axes equal to the Nin attribute of the supplied Mapping.
*     All other Frames in the FrameSet are replaced by CmpFrames holding the
*     original Frame and the supplied Frame. These new axes are connected to
*     the new GRID axes using the supplied Mapping.

*  Arguments:
*     wcs
*        A pointer to a FrameSet that is to be used as the WCS FrameSet in
*        an NDF. This imposes the restriction that the base Frame must
*        have Domain GRID.
*     map
*        A pointer to a Mapping. The forward transformation should transform
*        the new GRID axes into the new WCS axes.
*     frm
*        A pointer to a Frame defining the new WCS axes.
*     lbnd
*        An array holding the lower pixel index bounds on the new axes.
*        If a NULL pointer is supplied, a value of 1 is assumed for all
*        the new axes.
*     ubnd
*        An array holding the upper pixel index bounds on the new axes.
*        If a NULL pointer is supplied, any FRACTION Frame in the
*        supplied FrameSet is removed.
*     status
*        Pointer to the global status variable.

*  Notes:
*     - The new axes are appended to the end of the existing axes, so the
*     axis indices associated with the new axes will extend from "nold+1"
*     to "nold+nnew", where "nold" is the number of axes in the original
*     Frame, and "nnew" is the number of new axes.
*     - An error will be reported if the Nout attribute of "map" is
*     different to the Naxes attribute of "frm".

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry  (JAC, HAwaii)
*     {enter_new_authors_here}

*  History:
*     29-OCT-2009 (DSB):
*        Original version.
*     20-NOV-2009 (DSB):
*        Treat all NDF Frames (PIXEL, AXIS and FRACTION in addition to
*        GRID) as special. Requires new arguments "lbnd" and "ubnd".
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*+
*/

/* Local Variables: */
   AstCmpMap *new_map;      /* Mapping from new GRID Frame to new NDF Frame */
   AstFrame *grid_frm;      /* Pointer to original base (GRID) Frame */
   AstFrame *new_frm;       /* Pointer to new NDF-special Frame */
   AstFrame *tfrm;          /* General Frame pointer */
   AstMapping *axismap = NULL;/* Original GRID->AXIS Mapping */
   AstMapping *fracmap = NULL;/* Original GRID->FRACTION Mapping */
   AstMapping *pixmap = NULL; /* Original GRID->PIXEL Mapping */
   char *new_ttl = NULL;    /* Dynamically allocated title buffer */
   char attr[ 20 ];         /* Buffer for full attribute name */
   const char *dom;         /* Pointer to Domain attribute value */
   double fp[ ATL__MXDIM ]; /* First pixel centre coords in NDF-special Frame */
   double gp[ ATL__MXDIM ]; /* First pixel centre coords in GRID Frame */
   double ina[ ATL__MXDIM ];/* Lower bounds in GRID coords */
   double inb[ ATL__MXDIM ];/* Upper bounds in GRID coords */
   double outa[ ATL__MXDIM ];/* Lower bounds in FRACTION coords */
   double outb[ ATL__MXDIM ];/* Upper bounds in FRACTION coords */
   double shifts[ ATL__MXDIM ];/* Shifts to produce required pixel origin */
   int *old_status;         /* Pointer to status value used by AST on entry */
   int axes[ ATL__MXDIM ];  /* Indicies of grid axes to pick */
   int i;                   /* Axis index */
   int iaxis;               /* Index of AXIS Frame in the supplied FrameSet */
   int ibase;               /* Index of original base Frame */
   int icurr;               /* Index of original current Frame */
   int ifrac;               /* Index of FRACTION Frame in the supplied FrameSet */
   int ifrm;                /* Frame index */
   int ipix;                /* Index of PIXEL  Frame in the supplied FrameSet */
   int ln;                  /* Currently used length of string */
   int nfrm;                /* Number of Frames in supplied FrameSet */
   int ngrid_add;           /* Number of additional grid axes */
   int ngrid_new;           /* New number of grid axes */
   int ngrid_old;           /* Original number of grid axes */
   int nwcs_add;            /* Number of WCS axes to add */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status pointer. */
   old_status = astWatch( status );

/* Begin an AST Object context. This means we do not need to annul any
   AST pointers since they will be annulled automatically when the AST
   Object context ends. */
   astBegin;

/* Find the number of new WCS axes to add, and report an error if the
   Mapping has a different number of outputs. */
   nwcs_add = astGetI( frm, "Naxes" );
   if( astGetI( map, "Nout" ) != nwcs_add && *status == SAI__OK ) {
      msgSeti( "NAXES", nwcs_add );
      msgSeti( "NOUT", astGetI( map, "Nout" ) );
      *status = SAI__ERROR;
      errRep( "", "atlAddWcsAxis8: The supplied Mapping has ^NOUT outputs "
              "but the supplied Frame has ^NAXES axes (programming error).",
              status );
   }

/* Get a pointer to the original base Frame in the supplied WCS FrameSet. */
   grid_frm = astGetFrame( wcs, AST__BASE );

/* Report an error if the Domain is not GRID. */
   dom = astGetC( grid_frm, "Domain" );
   if( dom && strcmp( dom, "GRID" ) && *status == SAI__OK ) {
      msgSetc( "DOM", dom );
      *status = SAI__ERROR;
      errRep( "", "atlAddWcsAxis8: The base Frame in the supplied FrameSet has "
              "Domain '^DOM'. It should be 'GRID' (programming error).", status );
   }

/* Get the indices of the PIXEL, AXIS and FRACTION Frames within the
   FrameSet (we already know that the GRID Frame is the base Frame, so we
   do not need to check for GRID). Frame indices are one-based. Also save
   the Mappings from the GRID Frame to each of these other NDF Frames. */
   iaxis = AST__NOFRAME;
   ipix = AST__NOFRAME;
   ifrac = AST__NOFRAME;

   nfrm = astGetI( wcs, "NFrame" );
   for( ifrm = 1; ifrm <= nfrm; ifrm++ ) {
      tfrm = astGetFrame( wcs, ifrm );
      dom = astGetC( tfrm, "Domain" );
      if( dom ) {
         if( !strcmp( dom, "AXIS" ) ) {
            iaxis = ifrm;
            axismap = astGetMapping( wcs, AST__BASE, ifrm );

         } else if( !strcmp( dom, "PIXEL" ) ) {
            ipix = ifrm;
            pixmap = astGetMapping( wcs, AST__BASE, ifrm );

         } else if( !strcmp( dom, "FRACTION" ) ) {
            ifrac = ifrm;
            fracmap = astGetMapping( wcs, AST__BASE, ifrm );
         }
      }
      tfrm = astAnnul( tfrm );
   }

/* Append the supplied Frame to every Frame in the FrameSet, including the
   NDF-special Frames. These new axes in each Frame are inter-connected using
   UnitMaps. */
   astAddFrame( wcs, AST__ALLFRAMES, NULL, frm );

/* We now create corrected versions of the NDF-special Frames and add
   them into the FrameSet. We need to be able to re-instate the original
   current Frame at the end, so record its index now, before it is changed. */
   icurr = astGetI( wcs, "Current" );

/* Fix up the GRID Frame first. Find the original number of GRID axes, and
   the number of new GRID axes to add. */
/* ----------------------------------------------------------------------- */

/* Find the original number of GRID axes, and the number of new GRID axes to
   add. */
   ngrid_old = astGetI( grid_frm, "Naxes" );
   ngrid_add = astGetI( map, "Nin" );

/* Use astPickAxes to create a new GRID Frame from the old GRID Frame,
   expanding it to have the required additional number of axes. */
   ngrid_new = ngrid_old + ngrid_add;
   for( i = 0; i < ngrid_old; i++ ) axes[ i ] = i + 1;
   for( ; i < ngrid_new; i++ ) axes[ i ] = 0;
   new_frm = astPickAxes( grid_frm, ngrid_new, axes, NULL );

/* Set up the attributes for the new axes. These values are copied from
   ndf1_inifr.f  */
   for( i = ngrid_old + 1; i <= ngrid_new; i++ ) {
      astSet( new_frm, "Format(%d)=%%3.1f", i );
      astSet( new_frm, "Label(%d)=Data grid index %d", i, i );
      astSet( new_frm, "Symbol(%d)=g%d", i, i );
      astSet( new_frm, "Unit(%d)=pixel", i );
   }

/* Create and store a new Frame title following the scheme in ndf1_inifr. */
   for( i = 0; i < ngrid_new; i++ ) gp[ i ] = 1.0;
   ln = 0;
   new_ttl = astAppendString( new_ttl, &ln, "Data grid indices; first "
                              "pixel at (" );
   for( i = 0; i < ngrid_new; i++ ) {
      if( i ) new_ttl = astAppendString( new_ttl, &ln, "," );
      new_ttl = astAppendString( new_ttl, &ln, astFormat( new_frm, i + 1,
                                                          gp[ i ] ) );
   }
   new_ttl = astAppendString( new_ttl, &ln, ")" );
   astSetC( new_frm, "Title", new_ttl );

/* Now construct a Mapping that transforms the base Frame in the modified
   FrameSet into the expanded GRID Frame created above. This is a parallel
   CmpMap that uses a UnitMap to transform the original grid axes, and the
   inverse of the supplied Mapping to transform the newly added grid axes. */
   new_map = astCmpMap( astUnitMap( ngrid_old, " " ), map, 0, " " );
   astInvert( new_map );

/* Add the new GRID Frame into the FrameSet. It becomes the current
   Frame. */
   astAddFrame( wcs, AST__BASE, new_map, new_frm );

/* Note the original base Frame index, and make the new GRID Frame (just
   added) the base Frame. */
   ibase = astGetI( wcs, "Base" );
   astSetI( wcs, "Base", astGetI( wcs, "Current" ) );


/* Now fix up the PIXEL Frame, if it exists in the supplied FrameSet. */
/* ----------------------------------------------------------------- */
   if( ipix != AST__NOFRAME ) {

/* Construct a Mapping that transforms the new GRID Frame in the modified
   FrameSet into the new PIXEL Frame. This is a parallel CmpMap combining
   the original GRID->PIXEL Mapping with a new ShiftMap that produces the
   required origin on the new PIXEL axes. */
      for( i = 0; i < ngrid_add; i++ ) {
         shifts[ i ] = ( lbnd ? lbnd[ i ] : 1 ) - 1.5;
      }
      new_map = astCmpMap( pixmap, astShiftMap( ngrid_add, shifts, " " ),
                           0, " " );

/* Use astPickAxes to create a new PIXEL Frame from the old PIXEL Frame,
   expanding it to have the required additional number of axes. */
      new_frm = astPickAxes( astGetFrame( wcs, ipix ), ngrid_new, axes, NULL );

/* Set up the attributes for the new axes. These values are copied from
   ndf1_inifr.f  */
      for( i = ngrid_old + 1; i <= ngrid_new; i++ ) {
         astSet( new_frm, "Format(%d)=%%3.1f", i );
         astSet( new_frm, "Label(%d)=Pixel coordinate %d", i, i );
         astSet( new_frm, "Symbol(%d)=p%d", i, i );
         astSet( new_frm, "Unit(%d)=pixel", i );
      }

/* Create and store a new Frame title following the scheme in ndf1_inifr. */
      astTranN( new_map, 1, ngrid_new, 1, gp, 1, ngrid_new, 1, fp );
      ln = 0;
      new_ttl = astAppendString( new_ttl, &ln, "Pixel coordinates; first "
                                 "pixel at (" );
      for( i = 0; i < ngrid_new; i++ ) {
         if( i ) new_ttl = astAppendString( new_ttl, &ln, "," );
         new_ttl = astAppendString( new_ttl, &ln, astFormat( new_frm, i + 1,
                                                             fp[ i ] ) );
      }
      new_ttl = astAppendString( new_ttl, &ln, ")" );
      astSetC( new_frm, "Title", new_ttl );

/* Add the new PIXEL Frame into the FrameSet. */
      astAddFrame( wcs, AST__BASE, new_map, new_frm );
   }

/* Now fix up the FRACTION Frame, if it exists in the supplied FrameSet,
   and if upper bounds for the new axes have been supplied. */
/* ----------------------------------------------------------------- */
   if( ifrac != AST__NOFRAME && ubnd ) {

/* Construct a Mapping that transforms the new GRID Frame in the modified
   FrameSet into the new FRACTION Frame. This is a parallel CmpMap combining
   the original GRID->FRACTION Mapping with a new WinMap. */
      for( i = 0; i < ngrid_add; i++ ) {
         ina[ i ] = 0.5;
         inb[ i ] = ubnd[ i ] - ( lbnd ? lbnd[ i ] : 1 ) + 1.5;
         outa[ i ] = 0.0;
         outb[ i ] = 1.0;
      }
      new_map = astCmpMap( fracmap, astWinMap( ngrid_add, ina, inb, outa,
                                               outb, " " ),
                           0, " " );

/* Use astPickAxes to create a new FRACTION Frame from the old FRACTION Frame,
   expanding it to have the required additional number of axes. */
      new_frm = astPickAxes( astGetFrame( wcs, ifrac ), ngrid_new, axes, NULL );

/* Set up the attributes for the new axes. These values are copied from
   ndf1_inifr.f  */
      for( i = ngrid_old + 1; i <= ngrid_new; i++ ) {
         astSet( new_frm, "Format(%d)=%%5.4f", i );
         astSet( new_frm, "Label(%d)=Normalised pixel coordinate %d", i, i );
         astSet( new_frm, "Symbol(%d)=f%d", i, i );
         astSet( new_frm, "Unit(%d)= ", i );
      }

/* Create and store a new Frame title following the scheme in ndf1_inifr. */
      astTranN( new_map, 1, ngrid_new, 1, gp, 1, ngrid_new, 1, fp );
      ln = 0;
      new_ttl = astAppendString( new_ttl, &ln, "Normalised pixel coordinates; "
                                 "first pixel at (" );
      for( i = 0; i < ngrid_new; i++ ) {
         if( i ) new_ttl = astAppendString( new_ttl, &ln, "," );
         new_ttl = astAppendString( new_ttl, &ln, astFormat( new_frm, i + 1,
                                                             fp[ i ] ) );
      }
      new_ttl = astAppendString( new_ttl, &ln, ")" );
      astSetC( new_frm, "Title", new_ttl );

/* Add the new FRACTION Frame into the FrameSet. */
      astAddFrame( wcs, AST__BASE, new_map, new_frm );
   }

/* Now fix up the AXIS Frame, if it exists in the supplied FrameSet. */
/* ----------------------------------------------------------------- */
   if( iaxis != AST__NOFRAME ) {

/* The NDF library does not place any restrictions on the Mapping from
   GRID to AXIS (it throws away the AXIS Frame anyway when it writes out
   the WCS component to disk). So the new GRID -> AXIS Mapping is just the
   old one in parallel with the new one. */
      new_map = astCmpMap( axismap, map, 0, " " );

/* Use astPickAxes to create a new AXIS Frame from the old AXIS Frame,
   expanding it to have the required additional number of axes. */
      new_frm = astPickAxes( astGetFrame( wcs, iaxis ), ngrid_new, axes, NULL );

/* Set up the attributes for the new axes. These values are copied from
   the supplied Frame. */
      for( i = ngrid_old + 1; i <= ngrid_new; i++ ) {
         sprintf( attr, "Format(%d)", i - ngrid_old );
         astSet( new_frm, "Format(%d)=%s", i, astGetC( frm, attr ) );

         sprintf( attr, "Label(%d)", i - ngrid_old );
         astSet( new_frm, "Label(%d)=%s", i, astGetC( frm, attr ) );

         sprintf( attr, "Symbol(%d)", i - ngrid_old );
         astSet( new_frm, "Symbol(%d)=%s", i, astGetC( frm, attr ) );

         sprintf( attr, "Unit(%d)", i - ngrid_old );
         astSet( new_frm, "Unit(%d)=%s", i, astGetC( frm, attr ) );
      }

/* Create and store a new Frame title following the scheme in ndf1_inifr. */
      astTranN( new_map, 1, ngrid_new, 1, gp, 1, ngrid_new, 1, fp );
      ln = 0;
      new_ttl = astAppendString( new_ttl, &ln, "Axis coordinates; first "
                                 "pixel at (" );
      for( i = 0; i < ngrid_new; i++ ) {
         if( i ) new_ttl = astAppendString( new_ttl, &ln, "," );
         new_ttl = astAppendString( new_ttl, &ln, astFormat( new_frm, i + 1,
                                                             fp[ i ] ) );
      }
      new_ttl = astAppendString( new_ttl, &ln, ")" );
      astSetC( new_frm, "Title", new_ttl );

/* Add the new AXIS Frame into the FrameSet. */
      astAddFrame( wcs, AST__BASE, new_map, new_frm );
   }


/* Clean up */
/* -------- */

/* Re-instate the original current Frame. If the original current Frame
   was the base (i.e. GRID) Frame, use the new base Frame as the current
   Frame. */
   if( ibase != icurr ) {
      astSetI( wcs, "Current", icurr );
   } else {
      astSetI( wcs, "Current", astGetI( wcs, "Base" ) );
   }

/* Remove the old NDF-special Frames, taking care to adjust the indices
   of the remainingg special Frames each time. */
   if( ipix != AST__NOFRAME ) {
      astRemoveFrame( wcs, ipix );
      if( iaxis != AST__NOFRAME && iaxis > ipix ) iaxis--;
      if( ifrac != AST__NOFRAME && ifrac > ipix ) ifrac--;
      if( ibase > ipix ) ibase--;
   }

   if( iaxis != AST__NOFRAME ) {
      astRemoveFrame( wcs, iaxis );
      if( ifrac != AST__NOFRAME && ifrac > iaxis ) ifrac--;
      if( ibase > iaxis) ibase--;
   }

   if( ifrac != AST__NOFRAME ) {
      astRemoveFrame( wcs, ifrac );
      if( ibase > ifrac) ibase--;
   }

   astRemoveFrame( wcs, ibase );

/* Free memory. */
   new_ttl = astFree( new_ttl );

/* End the AST Object context. This annulls all AST objects created since
   the matching call to astBegin. */
   astEnd;

/* Revert to using the old AST status pointer. */
   astWatch( old_status );
}

