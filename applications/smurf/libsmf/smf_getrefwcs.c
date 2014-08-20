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
*        case the WCS defined by the JSA all-sky pixel grid is returned.
*        The choice of JSA projection (HPX, XPHS or XPHN) is based on
*        whether the map crosses an HPX discontinuity. Alternatively,
*        "JSA-HPX", "JSA-XPHS" or "JSA-XPHN" can be supplied if it is
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
                    AstFrameSet **spacewcs, int *isjsa, int *status ){

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
   AstRegion *circle;
   char text[ 255 ];            /* Parameter value */
   double centre[ 2 ];
   double hpxlat[] = {0.0, 90.0, 180.0, 270.0 };
   double p0[ 2 ];
   double radius;
   int i;
   int inax[ 2 ];               /* Indices of required WCS axes */
   int jsatiles;
   int lbnd[2];                 /* Lower pixel index bounds of mid tile */
   int outax[ 7 ];              /* Indices of corresponding PIXEL axes */
   int refndf;                  /* NDF identifier for the refence NDF */
   int ubnd[2];                 /* Upper pixel index bounds of mid tile */
   int usexph;
   size_t code;
   smfData *data = NULL;        /* Structure describing 1st input file */
   smfJSATiling skytiling;
   smf_inst_t inst = SMF__INST_NONE;
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

/* If it is "JSA", we return WCS that describes the JSA all-sky pixel grid. */
   } else if( *status == SAI__OK ) {
      if( astChrMatchN( text, "JSA", 3 ) ) {
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

/* Assume the HPX projection is to be used. */
               usexph = 0;

/* For "JSA" - choose the best projection. */
               if( astChrMatch( text, "JSA" ) ) {

/* Use the FITS headers in the first raw data file to create an AST Circle
   describing the approximate area of the observation within the tracking
   system. */
                  circle = smf_mapregion_approx( igrp, 1, status );

/* Convert the circle to ICRS (as used by the JSA all-sky grid). */
                  astSetC( circle, "System", "ICRS" );

/* Get the centre and radius of this circle. */
                  astCirclePars( circle, centre, &radius, NULL );
                  if( ( centre[ 0 ] == AST__BAD || centre[ 1 ] == AST__BAD ||
                        radius == AST__BAD ) && *status == SAI__OK ) {
                     *status = SAI__ERROR;
                     errRep( "", "smf_getrefwcs: the centre and/or radius of "
                             "the map is undefined.", status );
                  }

/* See if the observation may straddle the gap between two polar cusps of
   the standard JSA HPX-projected all-sky grid. If it does, we need to
   swap to use an XPH (polar-HEALPix) projection instead since otherwise
   the map will probably be too big (in pixels) to create. The gaps are
   defined by the sections of meridians at longitude=0, 90, 180, 270 that
   have absolute latitude greater than 41.8103 degrees (the latitude of
   the transition between the equatorial and polar regions of the HPX
   projection). If the offset in longitude between the circle centre and
   any of these meridians is less than the radius, then we may have an
   intersection. Check each meridian in turn. */
                  usexph = 0;
                  for( i = 0; i < 4; i++ ) {

/* Get the coords of the point on the meridian which is at the same
   latitude as the circle centre. */
                     p0[ 0 ] = AST__DD2R*hpxlat[ i ];
                     p0[ 1 ] = centre[ 1 ];

/* If the arc-distance between this point and the circle centre is less
   than the radius, the circle intersects the meridian. */
                     if( fabs( astDistance( circle, centre, p0 ) <= radius ) ) {

/* If the circle centre is above the northern transitional latitude, we
   need to use the XPH projection centred on the north pole. */
                        if( centre[ 1 ] > AST__DD2R*SMF__HPX_TRANS ) {
                           msgOutif( MSG__VERB, "", "The Map will be created "
                                     "using an XPH projection centred on the "
                                     "north pole.", status );
                           usexph = 1;
                           break;

/* If the circle centre is below the southern transitional latitude, we
   need to use the XPH projection centred on the south pole. */
                        } else if( centre[ 1 ] < -AST__DD2R*SMF__HPX_TRANS ) {
                           msgOutif( MSG__VERB, "", "The Map will be created "
                                     "using an XPH projection centred on the "
                                     "south pole.", status );
                           usexph = -1;
                           break;
                        }
                     }
                  }

/* If a projection was specified, use it. */
               } else if( astChrMatch( text, "JSA-HPX" ) ) {
                  usexph = 0;

               } else if( astChrMatch( text, "JSA-XPHN" ) ) {
                  usexph = 1;

               } else if( astChrMatch( text, "JSA-XPHS" ) ) {
                  usexph = -1;

               } else if( *status == SAI__OK ) {
                  *status = SAI__ERROR;
                  errRepf( "", "Bad value '%s' supplied for parameter %s.",
                           status, text, param );
               }

/* Get the WCS FrameSet for what was the central tile in the old
   raster-based indexing scheme - since we do not use the "local-origin"
   option, the WCS for every tile is identical. The base Frame will be
   GRID coords within the tile, and the current Frame will be ICRS
   (RA,Dec).  This used to be the central tile in numerical order, not
   in spatial position. In other words, it was the lower of the two middle
   tiles in numerical order. This tile is in the north west corner of
   HPX facet 0, so in the new nested scheme, it has number 0 * ntpf^2 + all
   the odd bits of the index within the tile. */
               smf_jsatile( ((skytiling.ntpf * skytiling.ntpf - 1) * 2) / 3,
                            &skytiling, 0, usexph,
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
            smf_close_file( NULL, &data, status);
         }

/* Otherwise get the parameter value as an NDF. */
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
