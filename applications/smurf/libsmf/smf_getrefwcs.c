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
*                    AstFrameSet **spacewcs, int *isjsa, int *status );

*  Arguments:
*     param = const char * (Given)
*        The name of the ADAM parameter used to get the reference NDF. A
*        value of "JSA" may also be provided for the parameter, in which
*        case the WCS defined by the SMF__JSA_HPX all-sky projection is
*        returned. The suitable JSA projection is chosen to ensure that
*        the map does not cross any discontinuities. Alternatively,
*        "HPX", "HPX12", "XPHS" or "XPHN" can be supplied if it is
*        necessary to specifiy the JSA projection explicitly.
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
*     isjsa = int * (Returned)
*        Is the map being made on the JSA all-sky pixel grid?
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
*     DSB: David S Berry (JAC, UCLan)
*     GSB: Graham Bell (JAC, Hawaii)
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
*     1-NOV-2013 (GSB):
*        Change the index of the central tile to take account of the
*        change in tile numbering scheme from raster to nested.
*     8-NOV-2013 (DSB):
*        Allow the REF parameter to be over-ridden by the JSATILES
*        parameter.
*     25-NOV-2013 (DSB):
*        Added argument "isjsa".
*     20-AUG-2014 (DSB):
*        Convert mapregion circle to ICRS before checking to see if it
*        crosses any of the JSA ICRS discontinuities.
*     26-AUG-2014 (DSB):
*        Allow some tolerance for rounding errors when checking whether the
*        observation instersects an HPX discontinuity.
*     1-OCT-2014 (DSB):
*        Added support for HPX projections centred on RA=12h.
*     11-NOV-2014 (DSB):
*        Re-write of the NDF code to ensure that the returned ref FrameSet
*        always has sky axes in the order (lon,lat).
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007,2013-2014 Science & Technology Facilities Council.
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
#include "star/atl.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "libsmf/jsatiles.h"

void smf_getrefwcs( const char *param, Grp *igrp, AstFrameSet **specwcs,
                    AstFrameSet **spacewcs, int *isjsa, int *status ){

/* Local Variables */
   AstFrame *frm = NULL;
   AstFrameSet *refwcs = NULL;  /* The WCS FrameSet from the reference NDF */
   AstRegion *circle;
   char text[ 255 ];            /* Parameter value */
   int *tiles;
   int i;
   int jsatiles;
   int lbnd[2];                 /* Lower pixel index bounds of mid tile */
   int ntile;
   int perm[ 2 ];
   int refndf;                  /* NDF identifier for the refence NDF */
   int ubnd[2];                 /* Upper pixel index bounds of mid tile */
   size_t code;
   smfData *data = NULL;        /* Structure describing 1st input file */
   smfJSATiling skytiling;
   smf_inst_t inst = SMF__INST_NONE;
   smf_jsaproj_t proj;          /* Specific JSA projection to use */
   smf_subinst_t subinst;

/* Initialise the returned values. */
   *specwcs = NULL;
   *spacewcs = NULL;
   *isjsa = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST context. */
   astBegin;

/* If the JSAILES parameter is TRUE, then we use the JSA all-sky pixel
   grid regardless of the setting of REF. */
   parGet0l( "JSATILES", &jsatiles, status );
   if( jsatiles ) {
      strcpy( text, "JSA" );
      *isjsa = 1;

/* Otherwise, first get the parameter value as a string. Use subpar to avoid problem
   caused by interpretion of the text within the parameter system. */
   } else {
      subParFindpar( param, &code, status );
      subParGetname( code, text, sizeof(text), status );
   }

/* If no value was supplied, annul the error and do nothing more. */
   if( *status == PAR__NULL ) {
      errAnnul( status );

/* If it is "JSA", or one of the JSA projection codes, we return WCS that
   describes one of the the JSA all-sky pixel grids. */
   } else if( *status == SAI__OK ) {
      proj = smf_jsaproj_fromstr( text, 0, status );
      if( astChrMatch( text, "JSA" ) || proj != SMF__JSA_NULL ) {
         *isjsa = 1;

/* Report an error if the instrument cannot be determined. */
         if( !igrp ) {
            *status = SAI__ERROR;
            errRep( "", "smf_getrefwcs: Cannot use the JSA all-sky pixel "
                    "grid since no input group has been supplied (possibly "
                    "programming error).", status );
         } else {

/* Open the first input file. */
            smf_open_file( NULL, igrp, 1, "READ", SMF__NOCREATE_DATA, &data,
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
                  inst = SMF__INST_ACSIS;

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

/* For "JSA" - choose the best projection. */
               if( astChrMatch( text, "JSA" ) ) {

/* Use the FITS headers in the first raw data file to create an AST Circle
   describing the approximate area of the observation within the tracking
   system. */
                  circle = smf_mapregion_approx( igrp, status );

/* Convert the circle to ICRS (as used by the JSA all-sky grid). */
                  astSetC( circle, "System", "ICRS" );

/* Get a list of the tiles that touch this circle. */
                  tiles = smf_jsatiles_region( circle, &skytiling,
                                               &ntile, status );

/* Choose the best projection (i.e. the projection that puts the circle
   furthest away from any singularities). */
                  proj = smf_jsaproj( ntile, tiles, &skytiling, status);

/* Free resources. */
                  tiles = astFree( tiles );
                  circle = astAnnul( circle );

/* If a good projection was specified, use it. Otherwise report an error. */
               } else if( proj == SMF__JSA_NULL && *status == SAI__OK ) {
                  *status = SAI__ERROR;
                  errRepf( "", "Bad value '%s' supplied for parameter %s.",
                           status, text, param );
               }

/* Report the projection type. */
               msgOutf( " ", "The %s will be created on the JSA %s "
                        "pixel grid.", status,
                        (data->hdr->instrument==INST__ACSIS)?"cube":"map",
                        smf_jsaproj_tostr( proj ) );

/* All tiles within the same JSA projection use the same WCS, so we get
   the WCS FrameSet for an arbitrary central tile, and use it for the
   full map. The exception is that tiles within the HPX facet that is
   split between bottom-left and top-right, use a different WCS (they
   have different reference points). But our choice of projection should
   mean that the map never falls in that facet. The base Frame will be
   GRID coords within the tile, and the current Frame will be ICRS
   (RA,Dec). */
               smf_jsatile( ((skytiling.ntpf * skytiling.ntpf - 1) * 2) / 3,
                            &skytiling, 0, proj, NULL, spacewcs, NULL, lbnd,
                            ubnd, status );

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
            smf_close_file( NULL, &data, status);
         }

/* Otherwise get the parameter value as an NDF. */
      } else {
         ndfAssoc( param, "READ", &refndf, status );

/* Get the WCS FrameSet from the reference NDF. */
         ndfGtwcs( refndf, &refwcs, status );

/* Attempt to extract a new FrameSet from this WCS FrameSet, in which the
   current Frame is a SkyFrame, and the base Frame is a 2D PIXEL Frame.
   Since the NDF library sets the GRID Frame to be the Base Frame, we need
   to make the PIXEL Frame the base Frame first. The NDF library ensures
   that the pixel Frame is Frame 2. */
         astSetI( refwcs, "Base", 2 );
         *spacewcs = atlFrameSetSplit( refwcs, "SKY", NULL, NULL, status );
         if( !(*spacewcs) ) {
            if( *status == SAI__OK ) {
               ndfMsg( "N", refndf );
               *status = SAI__ERROR;
               errRep( "", "The supplied reference NDF (^N) either has no "
                       "celestial WCS axes, or the celestial axes cannot "
                       "be separated from the non-celestial axes.", status );
            }

/* The rest of makemap assumes that the sky frame axes are in the default
   order (lon,lat). If this is not the case, permute them. */
         } else if( astGetI( *spacewcs, "IsLatAxis(1)" ) ) {
            perm[ 0 ] = 2;
            perm[ 1 ] = 1;
            astPermAxes( *spacewcs, perm );
         }

/* Now look for the spectral WCS (described by a DSBSpecFrame). */
         smf_getspectralwcs( refwcs, 1, specwcs, status );

/* We no longer need the NDF so annul it. */
         ndfAnnul( &refndf, status );
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
