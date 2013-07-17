/*
*+
*  Name:
*     smf_getrefwcs

*  Purpose:
*     Get the spatial and/or spectral WCS from a reference NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_getrefwcs( const char *param, Grp *igrp, AstFrameSet **specwcs,
*                    AstFrameSet **spacewcs, int *status );

*  Arguments:
*     param = const char * (Given)
*        The name of the ADAM parameter used to get the reference NDF. A
*        value of "JSA" may also be provided for the parameter, in which
*        case the WCS defined by the JSA all-sky pixel grid is returned.
*     igrp = Grp * (Given)
*        A group holding the paths to the input science files. The first
*        of these is used to determine the instrument in use in the case
*        that WCS defined by the JSA all-sky pixel grid is requested. A
*        NULL value may be supplied, but an error will then be reported
*        if the user requests JSA all-sky WCS.
*     specwcs = AstFrameSet ** (Returned)
*        A pointer to a location at which to return a pointer to an AST
*        FrameSet describing the spectral axis of the reference NDF. The
*        base Frame will be a 1D Frame with Domain PIXEL, and the current
*        Frame will be a DSBSpecFrame. If no reference NDF is supplied,
*        or if the reference NDF has no spectral axis, a NULL pointer
*        will be returned.
*     spacewcs = AstFrameSet ** (Returned)
*        A pointer to a location at which to return a pointer to an AST
*        FrameSet describing the spatial axes of the reference NDF. The
*        base Frame will be a 2D Frame with Domain PIXEL, and the current
*        Frame will be a SkyFrame. If no reference NDF is supplied,
*        or if the reference NDF has no spatial axes, a NULL pointer
*        will be returned.
*     status = int * (Given and Returned)
*        The inherited status.

*  Description:
*     This function determines the spatial and spectral WCS to be used by
*     the output map/cube on the basis of the value obtained for a
*     specified ADAM parameter. If the string "JSA" is obtained for the
*     parameter, the returned spatial WCS describes the JSA all-sky pixel
*     grid (no spectral WCS is returned in this case). Otherwise, a reference
*     NDF should be supplied for the parameter. The WCS from this NDF is
*     split into 2 parallel parts; one describing the spatial axes and one
*     describing the spectral axis. It is legal for the NDF to contain only
*     one of these two sets of WCS axes (so an output cube can be aligned
*     with a apatial image or a 1D spectrum).

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     18-DEC-2007 (DSB):
*        Initial version.
*     17-JAN-2011 (DSB):
*        Move the spectral axis code out into a separate function
*        (smf_getspectralwcs). The same could be done with the code for
*        the spatial axes if required.
*     17-JUL-2013 (DSB):
*        Allow JSA all-sky WCS to be requested, instead of supplying an
*        NDF.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007,2013 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "ast.h"
#include "par.h"
#include "mers.h"
#include "grp.h"
#include "sae_par.h"
#include "star/subpar.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "libsmf/jsatiles.h"

void smf_getrefwcs( const char *param, Grp *igrp, AstFrameSet **specwcs,
                    AstFrameSet **spacewcs, int *status ){

/* Local Variables */
   AstFrame *bfrm = NULL;       /* Frame describing full PIXEL coords */
   AstFrame *cfrm = NULL;       /* Frame describing required WCS coords */
   AstFrame *frm = NULL;
   AstFrame *gfrm = NULL;       /* Frame describing required PIXEL coords */
   AstFrame *template = NULL;   /* A Frame defining what we are looking for */
   AstFrameSet *fs = NULL;      /* A conversion FrameSet */
   AstFrameSet *refwcs = NULL;  /* The WCS FrameSet from the reference NDF */
   AstMapping *map = NULL;      /* Mapping from full wcs to PIXEL coords */
   AstMapping *splitmap = NULL; /* Mapping from required wcs to PIXEL coords */
   char text[ 255 ];            /* Parameter value */
   int i;
   int inax[ 2 ];               /* Indices of required WCS axes */
   int lbnd[2];                 /* Lower pixel index bounds of mid tile */
   int outax[ 7 ];              /* Indices of corresponding PIXEL axes */
   int refndf;                  /* NDF identifier for the refence NDF */
   int ubnd[2];                 /* Upper pixel index bounds of mid tile */
   smfData *data = NULL;        /* Structure describing 1st input file */
   smfJSATiling skytiling;
   smf_inst_t inst = SMF__INST_NONE;
   smf_subinst_t subinst;
   size_t code;

/* Initialise the returned values. */
   *specwcs = NULL;
   *spacewcs = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST context. */
   astBegin;

/* First get the parameter value as a string. Use subpar to avoid problem
   caused by interpretion of the text within the parameter system. */
   subParFindpar( param, &code, status );
   subParGetname( code, text, sizeof(text), status );

/* If no value was supplied, annul the error and do nothing more. */
   if( *status == PAR__NULL ) {
      errAnnul( status );

/* If it is "JSA", we return WCS that describes the JSA all-sky pixel grid. */
   } else if( *status == SAI__OK ) {
      if( astChrMatch( text, "JSA" ) ) {

/* Report an error if the instrument cannot be determined. */
         if( !igrp ) {
            *status = SAI__ERROR;
            errRep( "", "smf_getrefwcs: Cannot use the JSA all-sky pixel "
                    "grid since no input group has been supplied (possibly "
                    "programming error).", status );
         } else {

/* Open the first input file. */
            smf_open_file( igrp, 1, "READ", SMF__NOCREATE_DATA, &data,
                           status );
            if( *status == SAI__OK ) {

/* Get the instrument. */
               if( data->hdr->instrument == INST__SCUBA2 ) {
                  subinst = smf_calc_subinst( data->hdr, status );
                  if( subinst == SMF__SUBINST_850 ) {
                     inst = SMF__INST_SCUBA_2_850;
                  } else {
                     inst = SMF__INST_SCUBA_2_450;
                  }

               } else if( data->hdr->instrument == INST__ACSIS ) {
                  inst = SMF__INST_HARP;

               } else if( *status == SAI__OK ) {
                  *status = SAI__ERROR;
                  if( data->file ) {
                     smf_smfFile_msg( data->file, "FILE", 1, "one or more of "
                                      "the input data files" );
                  } else {
                     msgSetc( "FILE", "one or more of the input data files" );
                  }
                  errRep( "", "No tiles are yet defined for the instrument that "
                          "created ^FILE.", status );
               }

/* Get the parameters that define the layout of sky tiles for the
   instrument. */
               smf_jsatiling( inst, &skytiling, status );

/* Get the WCS FrameSet for the central tile - since we do not use the
   "local-origin" option, the WCS for every tile is identical.The base
   Frame will be GRID coords within the tile, and the current Frame will
   be ICRS (RA,Dec). */
               smf_jsatile( skytiling.ntiles/2, &skytiling, 0,
                            NULL, spacewcs, NULL, lbnd, ubnd, status );

/* Change the base Frame to be PIXEL. */
               for( i = 1; i <= astGetI( *spacewcs, "NFrame" ); i++ ) {
                  frm = astGetFrame( *spacewcs, i );
                  if( astChrMatch( astGetC( frm, "Domain" ), "PIXEL" ) ) {
                     astSetI( *spacewcs, "Base", i );
                  }
                  frm = astAnnul( frm );
               }
            }

/* Close the current input data file. */
            smf_close_file( &data, status);
         }

/* If it is not "JSA", get the parameter value as an NDF. */
      } else {
         ndfAssoc( param, "READ", &refndf, status );

/* Get the WCS FrameSet from the reference NDF. */
         ndfGtwcs( refndf, &refwcs, status );

/* We no longer need the NDF so annul it. */
         ndfAnnul( &refndf, status );

/* We want astFindFrame to return us the conversion from PIXEL coords to
   celestial or spectral coords, so we need to make the PIXEL Frame the
   base Frame in the reference WCS FrameSet. The NDF libray ensures that
   the PIXEL Frame is always frame 2. */
         astSetI( refwcs, "Base", 2 );

/* First look for the spatial WCS. Create a SkyFrame that we can use as a
   template for searching the reference WCS. Set a high value for MaxAxes
   so that SkyFrames can be found within CmpFrames (which will have
   more than 2 axes). We also set PreserveAxes true so that the order of
   the sky axes in the reference WCS is preserved. */
         template = (AstFrame *) astSkyFrame( "MaxAxes=7,PreserveAxes=1" );

/* Use astFindFrame to search the reference WCS for a SkyFrame. This search
   includes the component Frames contained within CmpFrames. */
         fs = astFindFrame( refwcs, template, " " );

/* If a SkyFrame was found... */
         if( fs ) {

/* Get the Mapping from sky coords to PIXEL coords. */
            map = astGetMapping( fs, AST__CURRENT, AST__BASE );

/* Get the sky coords Frame. This will be a SkyFrame, but its attributes
   will be inherited form the reference WCS rather than the template
   SkyFrame. */
            cfrm = astGetFrame( fs, AST__CURRENT );

/* Get the PIXEL Frame. If the reference NDF is a cube this will be a 3D
   Frame. */
            bfrm = astGetFrame( fs, AST__BASE );

/* Since the mappiong above may include a spectral axis, see if we can split
   off the sky axes from the total Mapping. If we can, this will give us the
   Mapping from 2D sky coords to 2D PIXEL coords. */
            inax[ 0 ] = 1;
            inax[ 1 ] = 2;
            astMapSplit( map, 2, inax, outax, &splitmap );
            if( splitmap && astGetI( splitmap, "Nout" ) == 2 ) {

/* Pick the corresponding 2 axes form the (potentially 3D) PIXEL Frame. */
               gfrm = astPickAxes( bfrm, 2, outax, NULL );

/* Create the returned spatial FrameSet. */
               *spacewcs = astFrameSet( gfrm, " " );
               astInvert( splitmap );
               astAddFrame( *spacewcs, AST__BASE, splitmap, cfrm );
            }
         }

/* Now look for the spectral WCS (described by a DSBSpecFrame). */
         smf_getspectralwcs( refwcs, 1, specwcs, status );
      }
   }

/* If no error has occurred, export any returned FrameSet pointers from
   the current AST context so that it will not be annulled when the AST
   context is ended. Otherwise, ensure a null pointer is returned. */
   if( *status == SAI__OK ) {
      if( *spacewcs ) astExport( *spacewcs );
      if( *specwcs ) astExport( *specwcs );
   } else {
      if( *spacewcs ) *spacewcs = astAnnul( *spacewcs );
      if( *specwcs ) *specwcs = astAnnul( *specwcs );
   }

/* End the AST context. This will annul all AST objects created within the
   context (except for those that have been exported from the context). */
   astEnd;

}
