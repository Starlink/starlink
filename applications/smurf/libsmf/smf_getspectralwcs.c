/*
*+
*  Name:
*     smf_getspectralwcs

*  Purpose:
*     Get the spectral WCS from a FrameSet.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_getspectralwcs( AstFrameSet *fset, int type, AstFrameSet **specwcs,
*                         int *status );

*  Arguments:
*     fset = AstFrameSet * (Given)
*        A pointer to an AST FrameSet that describes a collection of
*        coordinate systems and the Mappings that are used to transform
*        positions from one system to another. This is read from the WCS
*        component of an NDF.
*     type = int (Given)
*        Indicates the type of spectral Frame to be searched for:
*           0 - Either a dual sideband spectrum of a simple spectrum
*           1 - A dual sideband spectrum only
*           2 - A simple spectrum only
*     specwcs = AstFrameSet ** (Returned)
*        A pointer to a location at which to return a pointer to a new AST
*        FrameSet describing the spectral axis of the reference NDF. The
*        base Frame will be a 1D Frame with Domain PIXEL, and the current
*        Frame will be a SpecFrame or DSBSpecFrame. If the supplied "fset"
*        FrameSet does not contain a spectral axis, a NULL pointer will be
*        returned but no error will be reported.
*     status = int * (Given and Returned)
*        The inherited status.

*  Description:
*     This function searches each coordinate system in the supplied
*     FrameSet for a system that includes a spectral axis (either dual
*     sideband or simple as indicated by "type"). If found, a new FrameSet
*     is created and returned describing a 1D pixel coordinate system, the
*     1D spectral coordinate system, and the Mapping between them. This
*     FrameSet can be used subsequently for transforming pixel positions
*     into spectral positions or vice-versa.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     17-JAN-2011 (DSB):
*        Initial version (extracted from smf_getrefwcs).
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "ast.h"
#include "par.h"
#include "mers.h"
#include "sae_par.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"


void smf_getspectralwcs( AstFrameSet *fset, int type, AstFrameSet **specwcs,
                         int *status ){

/* Local Variables */
   AstFrame *bfrm = NULL;       /* Frame describing full PIXEL coords */
   AstFrame *cfrm = NULL;       /* Frame describing required WCS coords */
   AstFrame *gfrm = NULL;       /* Frame describing required PIXEL coords */
   AstFrame *template = NULL;   /* A Frame defining what we are looking for */
   AstFrameSet *fs = NULL;      /* A conversion FrameSet */
   AstMapping *map = NULL;      /* Mapping from full wcs to PIXEL coords */
   AstMapping *splitmap = NULL; /* Mapping from required wcs to PIXEL coords */
   const char *domain;          /* Pointer to Frame's Domain string */
   int ibase;                   /* Original index of base Frame in "fset" */
   int iframe;                  /* Index of next Frame to check */
   int inax[ 2 ];               /* Indices of required WCS axes */
   int ipass;                   /* Indicates type of spectral Frame to use */
   int nframe;                  /* No. of Frames in supplied FrameSet */
   int outax[ 7 ];              /* Indices of corresponding PIXEL axes */

/* Initialise the returned values. */
   *specwcs = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST context. */
   astBegin;

/* Check the supplied "type" value is valid. */
   if( type < 0 || type > 2 ) {
      *status = SAI__ERROR;
      msgSeti( "T", type );
      errRep( " ", "smf_getspectralwcs: Illegal value (^T) supplied for "
              "function parameter 'type'.", status );
   }

/* Get the number of coordinate systems (i.e. Frames) described by the
   supplied FrameSet. */
   nframe = astGetI( fset, "NFrame" );

/* Record the original index of the base Frame in the FrameSet, so re can
   re-instate it before leaving this function. */
   ibase = astGetI( fset, "Base" );

/* Ensure that the base Frame (i.e. coordinate system) in the supplied
   FrameSet is the PIXEL Frame. To do this, we check each Frame in turn
   until one is found that has the Domain name "PIXEL", and then set the
   Frame as the base Frame in the FrameSet. */
   for( iframe = 1; iframe <= nframe; iframe++ ) {
      bfrm = astGetFrame( fset, iframe );
      domain = astGetC( bfrm, "Domain" );
      if( domain && !strcmp( domain, "PIXEL" ) ) {
         astSetI( fset, "Base", iframe );
         break;
      }
      bfrm = astAnnul( bfrm );
   }

/* If a PIXEL Frame was found, the above will have left "bfrm" pointing to
   the PIXEL Frame. If not, report an error. */
   if( !bfrm && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( " ", "smf_getspectralwcs: No PIXEL Frame found in supplied "
              "NDF.", status );
   }

/* Now search the supplied FrameSet for a Frame containing a spectral axis.
   We search first for dual sideband axis (described by a DSBSpecFrame
   object). If required we will then search for a simple spectral axis
   (described by a SpecFrame object). */
   for( ipass = 0; ipass < 2; ipass++ ) {

/* We will use the astFindFrame method to search for the required Frame.
   It uses a "template" Frame to define the properties of the Frame to
   be searched for. So first create a DSBSpecFrame (pass 0) or SpecFrame
   (pass 1) that we can use as a template. Set a high value for the MaxAxes
   attribute so that spectral Frames can be found within compound Frames
   (CmpFrames) which will have more than 1 axis. */
      template = NULL;
      if( ipass == 0 ) {
         if( type == 0 || type == 1 ) {
            template = (AstFrame *) astDSBSpecFrame( "MaxAxes=7" );
         }
      } else {
         if( type == 0 || type == 2 ) {
            template = (AstFrame *) astSpecFrame( "MaxAxes=7" );
         }
      }

/* Only search for the Frame if the caller is interested in the type of
   Frame searched for on this pass. */
      if( template ) {

/* Use astFindFrame to search the supplied FrameSet for a spectral Frame.
   This search includes the component Frames contained within CmpFrames. */
         fs = astFindFrame( fset, template, " " );

/* If a spectral Frame was found... */
         if( fs ) {

/* Get the Mapping from the spectral coordinate system to the PIXEL
   coordinate system. */
            map = astGetMapping( fs, AST__CURRENT, AST__BASE );

/* Get the spectral coord Frame. This will be a DSBSpecFrame, but its
   attributes will be inherited from the supplied FrameSet rather than
   the template DSBSpecFrame. */
            cfrm = astGetFrame( fs, AST__CURRENT );

/* Get the PIXEL Frame. If the supplied FrameSet describes a cube this will
   be a 3D Frame. If it describes a single spectrum it will be a 1D Frame. */
            bfrm = astGetFrame( fs, AST__BASE );

/* Since, for a cube, the mapping above will include spatial axes, see if we
   can split off the spectral axis axes from the total Mapping. If we can,
   this will give us the Mapping from 1D spectral coords to 1D PIXEL coords. */
            inax[ 0 ] = 1;
            astMapSplit( map, 1, inax, outax, &splitmap );
            if( splitmap && astGetI( splitmap, "Nout" ) == 1 ) {

/* Pick the corresponding axis form the (potentially 3D) PIXEL Frame. */
               gfrm = astPickAxes( bfrm, 1, outax, NULL );

/* Create the returned spectral FrameSet. */
               *specwcs = astFrameSet( gfrm, " " );
               astInvert( splitmap );
               astAddFrame( *specwcs, AST__BASE, splitmap, cfrm );
            }
         }
      }
   }

/* If no error has occurred, export the returned FrameSet pointer from
   the current AST context so that it will not be annulled when the AST
   context is ended. Otherwise, ensure a null pointer is returned. */
   if( *status == SAI__OK ) {
      if( *specwcs ) astExport( *specwcs );
   } else {
      if( *specwcs ) *specwcs = astAnnul( *specwcs );
   }

/* Re-instate the original index of the base Frame in the FrameSet. */
   astSetI( fset, "Base", ibase );

/* End the AST context. This will annul all AST objects created within the
   context (except for those that have been exported from the context). */
   astEnd;

}
