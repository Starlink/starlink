#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

#define NSTD 4 /* No. standard NDF coordinate systems */

void ndf1Vwcs( NdfACB *acb, AstFrameSet *iwcs1, AstFrameSet **iwcs2, int *status ){
/*
*+
*  Name:
*     ndf1Vwcs

*  Purpose:
*     Validate and strip WCS information.

*  Synopsis:
*     void ndf1Vwcs( NdfACB *acb, AstFrameSet *iwcs1, AstFrameSet **iwcs2,
*                    int *status )

*  Description:
*     This function accepts a pointer to an AST_ FrameSet containing WCS
*     information which is to be written to an NDF with an entry in the
*     ACB. It validates the WCS information and produces a copy of the
*     FrameSet. It then strips out of this copy any information which can
*     be recovered from other NDF information (thus minimising storage
*     requirements).
*
*     If the WCS information is valid, a pointer to the validated and
*     stripped copy of the FrameSet is returned. Otherwise, a pointer value
*     of NULL is returned and an appropriate error is reported.

*  Parameters:
*     acb
*        Pointer to the NDF entry in the ACB.
*     iwcs1
*        An AST_ pointer to the FrameSet containing the WCS information to
*        be validated.
*     *iwcs2
*        Returned holding the an AST_ pointer to the FrameSet containing
*        the validated and stripped copy of the WCS information.
*     *status
*        The global status.

*  Notes:
*     - A value of NULL will be returned for the "iwcs2" parameter if
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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/


/* Local Variables: */
   AstFrame *frame;      /* Pointer to Frame */
   AstFrameSet *new;     /* Pointer to new FrameSet */
   AstUnitMap *unit;     /* Pointer to UnitMap */
   const char *class;    /* Object Class string */
   const char *domain;   /* Frame Domain string */
   hdsdim lbnd[ NDF__MXDIM ]; /* NDF lower bounds */
   hdsdim ubnd[ NDF__MXDIM ]; /* NDF upper bounds */
   int ibase;            /* Base Frame index */
   int icurr;            /* Current Frame index */
   int iframe;           /* Loop counter for Frame indices */
   int naxes;            /* Number of base Frame axes */
   int ndim;             /* Number of NDF dimensions */
   int nframe;           /* Number of Frames */

/* Initialise the returned pointer. */
   *iwcs2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Make a copy of the AST_ object supplied. */
   *iwcs2 = astCopy( iwcs1 );

/* Validate the Object supplied.
   -----------------------------
   Obtain the Object's Class and check that it is "FrameSet". Report an
   error if it is not. */
   class = astGetC( *iwcs2, "Class" );
   if( *status == SAI__OK ) {
      if( strcmp( class, "FrameSet" ) ) {
         *status = NDF__WCSIN;
         msgSetc( "CLASS", class );
         errRep( " ", "Invalid ^CLASS pointer supplied when a FrameSet "
                 "pointer is required (possible programming error).", status );
      }
   }

/* If OK, obtain the Domain of the base Frame and check that this is
   "GRID". Report an error if it is not. */
   if( *status == SAI__OK ) {
      frame = astGetFrame( *iwcs2, AST__BASE );
      domain = astGetC( frame, "Domain" );
      frame = astAnnul( frame );
      if( *status == SAI__OK ) {
         if( strcmp( domain, "GRID" ) ) {
            *status = NDF__WCSIN;
            msgSetc( "DOMAIN", domain );
            errRep( " ", "The base Frame of the FrameSet supplied has a "
                    "Domain value of '^DOMAIN'; this should be 'GRID' "
                    "(possible programming error).", status );
         }
      }
   }

/* If OK, obtain the number of base Frame axes. If an ACB indesx has been
   supplied, also obtain the bounds and number of dimensions of the NDF. */
   if( *status == SAI__OK ) {
      frame = astGetFrame( *iwcs2, AST__BASE );
      naxes = astGetI( frame, "Naxes" );
      frame = astAnnul( frame );

      if( acb ) {
         aryBound( acb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Check that the number of axes matches the number of NDF dimensions.
   Report an error if it does not. */
         if( *status == SAI__OK ) {
            if( naxes != ndim ) {
               *status = NDF__NAXIN;
               msgSeti( "NAXES", naxes );
               msgSeti( "NDIM", ndim );
               errRep( " ", "The base Frame of the FrameSet supplied has "
                       "^NAXES axes; this does not match the number of NDF "
                       "dimensions, ^NDIM (possible programming error).",
                       status );
            }
         }
      }
   }

/* Strip away unwanted information.
   --------------------------------
   We do not need to hold information about any of the standard NDF
   coordinate systems as part of the FrameSet stored internally, since
   this can be re-generated from other NDF information when required.
   However, we must still have Frames present to represent these
   coordinate systems, so they can be selected as current. To allow
   this, we remove all the redundant information, but replace it with 4
   dummy Frames, all inter-related by null Mappings (UnitMaps). */
   if( *status == SAI__OK ) {

/* Start building a new FrameSet containing a base Frame which is a
   placeholder for the data grid coordinate system. We do not set any
   unnecessary attributes for it, since we do not want to store these. */
      frame = astFrame( naxes, "Domain=GRID" );
      new = astFrameSet( frame, " " );
      frame = astAnnul( frame );

/* Add a second Frame as a placeholder for the pixel coordinate system,
   related to the base Frame by a UnitMap. */
      unit = astUnitMap( naxes, " " );
      frame = astFrame( naxes, "Domain=PIXEL" );
      astAddFrame( new, AST__BASE, unit, frame );
      frame = astAnnul( frame );

/* Similarly, add a third Frame as a placeholder for the axis
   coordinate system. */
      frame = astFrame( naxes, "Domain=AXIS" );
      astAddFrame( new, AST__BASE, unit, frame );
      frame = astAnnul( frame );

/* Similarly, add a fourth Frame as a placeholder for the normalised
   pixel coordinate system. */
      frame = astFrame( naxes, "Domain=FRACTION" );
      astAddFrame( new, AST__BASE, unit, frame );
      frame = astAnnul( frame );

/* Obtain the base and current Frame indices from the original FrameSet. */
      ibase = astGetI( *iwcs2, "Base" );
      icurr = astGetI( *iwcs2, "Current" );

/* Make the base Frame current and add the original FrameSet to the new
   one, related to the new base Frame by a UnitMap.  Annul the UnitMap
   pointer. */
      astSetI( *iwcs2, "Current", ibase );
      astAddFrame( new, AST__BASE, unit, *iwcs2 );
      unit = astAnnul( unit );

/* Annul the original FrameSet pointer and replace it with a pointer to
   the new one. */
      (void) astAnnul( *iwcs2 );
      *iwcs2 = new;

/* Restore the original current Frame (allowing for its new index). */
      astSetI( *iwcs2, "Current", icurr + NSTD );

/* If the current Frame describes the data grid coordinate system,
   change it to be the new base Frame instead. Otherwise, if it
   describes the pixel coordinate Frame, then change it to be the new
   one instead. Similarly for the axis coordinate system. This process
   records which coordinate system was current, even if we subsequently
   remove the actual Frame. */
      domain = astGetC( *iwcs2, "Domain" );
      if( !strcmp( domain, "GRID" ) ) {
         astSet( *iwcs2, "Current=1" );
      } else if( !strcmp( domain, "PIXEL" ) ) {
         astSet( *iwcs2, "Current=2" );
      } else if( !strcmp( domain, "AXIS" ) ) {
         astSet( *iwcs2, "Current=3" );
      } else if( !strcmp( domain, "FRACTION" ) ) {
         astSet( *iwcs2, "Current=4" );
      }

/* Loop through all the Frames acquired from the original FrameSet. */
      nframe = astGetI( *iwcs2, "Nframe" );
      iframe = NSTD + 1;
      while( iframe <= nframe ){

/* Obtain each Frame's Domain. */
         frame = astGetFrame( *iwcs2, iframe );
         domain = astGetC( frame, "Domain" );
         frame = astAnnul( frame );

/* Remove any Frame with a Domain associated with any of the standard
   NDF coordinate systems, and decrement the Frame count (we assume
   there might be more than one Frame with the same domain, although
   this shouldn't normally happen). */
         if( ( !strcmp( domain, "GRID" ) ) || ( !strcmp( domain, "PIXEL" ) ) ||
             ( !strcmp( domain, "AXIS" ) ) || ( !strcmp( domain, "FRACTION" ) ) ) {
            astRemoveFrame( *iwcs2, iframe );
            nframe--;

/* Leave all other Frames in place. */
         } else {
            iframe++;
         }
      }
   }

/* If an error occurred, annul the returned AST_ pointer. */
   if( *status != SAI__OK ) *iwcs2 = astAnnul( *iwcs2 );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vwcs", status );

}

