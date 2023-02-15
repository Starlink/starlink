/*
*+
*  Name:
*     smf_jsatileheader

*  Purpose:
*     Return a FITS header describing a sky tile for a JCMT instrument.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     AstFitsChan *smf_jsatileheader( int itile, smfJSATiling *skytiling,
*                                     int local_origin, smf_jsaproj_t proj,
*                                     int *move, int *status )

*  Arguments:
*     itile = int (Given)
*        The zero-based tile index, or -1 for the whole sky.
*     skytiling = smfJSATiling * (Given)
*        Pointer to a structure holding parameters describing the tiling
*        scheme used for the required JCMT instrument, as returned by
*        function smf_jsatiling.
*     local_origin = int (Given)
*        If non-zero, then the returned header will include values for
*        projection parameters PVi_1 and PVi_2 that causes the origin of
*        grid coordinates to be moved to the centre of the tile. If zero,
*        the origin of pixel coordinates will be at RA=0, Dec=0.
*     proj = smf_jsaproj_t (Given)
*        The projection to use. This can be an HPX projection centred
*        either on RA=0 or RA=12h, or an XPH projection centred either
*        on the north pole or the south pole. When creating an individual
*        JSA tile, SMF__JSA_HPX (HPX centred on RA=0) should always be
*        used. When creating a mosaic of JSA tiles, the projection should
*        be chosen so that no discontinuities cross the mosaic.
*     move = int * (Returned)
*        Pointer to an int which is returned non-zero if the tile is in
*        the bottom left part of the bottom left tile of an HPX projection
*        (truncated by RA=12 hours (SMF__JSA_HPX) or RA=0 hours
*        (SMF__JSA_HPX12) ). Such tiles should be moved up to the top right
*        corner of the all-sky grid in pixel coords. May be NULL. Returned
*        equal to zero if "proj" is SMF__JSA_XPHN or SMF__XHPS
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Description:
*     This function returns a FITS header describing a specified sky tile
*     holding data from a specified JCMT instrument (or, if "itile" is
*     -1, a FITS header describing the whole collection of tiles).
*
*     The whole sky is divided into 12 basic HEALPix facets. Each
*     facet is then divided up into NxN tiles, where N is given by
*     "skytiling->ntpf". Each tile is then divided into PxP pixels,
*     where P is given by "skytiling->ppt".
*
*     HEALPix facets are numbered from 0 to 11 as defined in the HEALPix
*     paper (Gorsky et. al. 2005 ApJ 622, 759). Facet zero contains tiles
*     zero to N*N-1, facet one contains tiles N*N to 2*N*N-1, etc. Within
*     a facet tiles are indexed using the "nested" scheme described in
*     the HEALPix paper. This starts with the lowest tile number in the
*     southern corner of the facet.  The even bits number the position in
*     the north-east direction and the odd bits number the position in
*     the north-west direction.
*
*     Four flavours of HEALPix projection can be used to map these tiles
*     onto a pixel grid. Two are based on the HPX projection (see the A&A
*     paper "Mapping on the HEALPix grid" by Calabretta and Roukema), and
*     two on the XPH projection ( see the PASA paper "Representing the
*     ‘Butterfly’ Projection in FITS—Projection Co" by Calabretta
*     and Lowe). Transforming a tile between any pair of these four
*     projections just involves a shift and a rotation within pixel
*     coordinates. The rotation is always a multiple of 90 degrees, and
*     so the transformation can be performed without degradation using
*     nearest neighbour resampling or rebinning.
*
*     The SMF__JSA_HPX projection is an HPX projection in which the
*     reference point is at RA=0 Dec=0. The SMF__JSA_HPX12 projection is
*     an HPX projection in which the reference point is at RA=12h Dec=0.
*     The SMF__JSA_XPHN projection is an XPH projection in which the
*     reference point is at the north pole. The SMF__JSA_XPHS projection
*     is an XPH projection in which the reference point is at the south
*     pole.
*
*     Each of these projections include discontinuities, but these
*     discontinuities will be at different places on the sky for each
*     one. When creating a header to describe a large area of the sky
*     (i.e. larger than one tile), the projection should be chosen to
*     ensure that the mapped area does not include any discontinuities.
*     For instance, areas close to the north pole should use SMF__JSA_XPHN,
*     areas close to the south pole should use SMF__JSA_XPHS, areas close
*     to RA=0 should use SMF__JSA_HPX, areas close to RA=12h should use
*     SMF__JSA_HPX12, etc.
*
*     Note, in all four projections, the discontinuities run between
*     tiles, never across tiles. When creating a header for a single
*     tile, the SMF__JSA_HPX projection should always be used.
*
*     The reference point (native lon.=0, native lat.=0) of the
*     SMF__JSA_HPX projection is put at (RA,Dec)=(0,0) [except for facet
*     six which has a reference point of (12h,0)], so that native coords
*     are equivalent to celestial coords. Note, facet six occupies the
*     bottom left corner of the SMF__JSA_HPX projection plane and covers
*     an RA range of 9h to 15h - the top right corner of the projection
*     covers the same area on the sky but has no corresponding tile.
*     The projection plane is rotated by 45 degrees so that the edges
*     of each facet are parallel to X and Y (as in Fig.3 of the
*     Calabretta and Roukema paper).
*
*     The bottom left facet of the SMF__JSA_HPX12 projection is split by
*     the RA=0h line in a similar way.
*
*     The projection reference point is at the origin of native longitude
*     and latitude, which corresponds to the origin of (RA,Dec). However,
*     this means that for most tiles the reference point is not actually
*     contained within the tile, and also results in jsatiles having huge
*     values for CRPIX1/2. To avoid this, the feature described in
*     FITS-WCS paper II section 2.5 ("User specified phi_0,theta_0") may
*     be used (see the "local_origin" argument). This causes the CRVAL and
*     CRPIX values in the returned header do not give the celestial and
*     pixel coords of the projection reference point, but instead give
*     the celestial and pixel coords of an arbitrary point (specified by
*     projection parameters PVi_1 and PVi_2 for longitude axis "i"). The
*     point actually used is the centre centre of the requested tile.

*  Authors:
*     DSB: David S Berry (JAC, UCLan)
*     GSB: Graham Bell (JAC)
*     {enter_new_authors_here}

*  History:
*     28-FEB-2011 (DSB):
*        Initial version.
*     16-JUL-2013 (DSB):
*        Added argument "local_origin".
*     29-OCT-2013 (GSB):
*        Use zero-based numbering for JSA tiles and number facets
*        as defined by HEALPix.  Removed code duplicating that in
*        smf_jsatilei2xy.
*     30-OCT-2013 (GSB):
*        Use nested numbering scheme for JSA tiles.
*     10-JUN-2014 (DSB):
*        - Ensure the returned FITS header include the telescope position
*        (keywords "OBSGEO-X/Y/Z").
*        - Added argument "move".
*     12-JUN-2014 (DSB):
*        Added argument "usexph".
*     30-SEP-2014 (DSB):
*        Include a Frame representing JSA all-sky HPX pixel coordinates
*        within the all-sky WCS.
*     1-OCT-2014 (DSB):
*        Allow an HPX projection centred on RA=12 to be used.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011,2013-2014 Science & Technology Facilities Council.
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
#include "sae_par.h"
#include "mers.h"
#include "star/atl.h"
#include "star/pal.h"

/* SMURF includes */
#include "libsmf/smf.h"


#include "libsmf/jsatiles.h"

/* Prototypes for private functions. */
static AstFitsChan *smfMakeFC( int nx, int ny, int n, int p, double crpix1,
                               double crpix2, double crval1, double crval2,
                               int usexph, int *status );

AstFitsChan *smf_jsatileheader( int itile, smfJSATiling *skytiling,
                                int local_origin, smf_jsaproj_t proj,
                                int *move, int *status ){

/* Local Variables: */
   AstFitsChan *fc = NULL;
   AstFrameSet *fs = NULL;
   char card[ 81 ];   /* FITS header card */
   double dec_cen;    /* DEC at tile centre */
   double dec_ref;    /* DEC at reference point */
   double gx_cen;     /* X grid coord at tile centre */
   double gx_ref;     /* X grid coord at reference point */
   double gy_cen;     /* Y grid coord at tile centre */
   double gy_ref;     /* Y grid coord at reference point */
   double ra_cen;     /* RA at tile centre */
   double ra_ref;     /* RA at reference point */
   int fi;            /* Zero-based facet index in range [0,11] */
   int m;             /* The number of pixels along one edge of a facet */
   int ng;            /* The number of tile along one edge of the FITS grid */
   int usexph;        /* Use an XPH projection? */
   int xt;          /* X offset to the requested tile */
   int yt;          /* Y offset to the requested tile */

/* Initialise */
   if( move ) *move = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return fc;

/* Initialise things for a SMF__JSA_HPX projection. */
   usexph = 0;
   ra_ref = 0.0;
   dec_ref = 0.0;
   gx_ref = 0.0;
   gy_ref = 0.0;

/* If the tile index is -1, produce a header for the whole sky (one
   pixel per tile). */
   if( itile == -1 ) {

      if( proj == SMF__JSA_HPX ) {
         ng = 5*skytiling->ntpf;

      } else if( proj == SMF__JSA_HPX12 ) {
         ng = 5*skytiling->ntpf;
         ra_ref = AST__DPI;

      } else if( proj == SMF__JSA_XPHN ) {
         ng = 4*skytiling->ntpf;
         dec_ref = AST__DPIBY2;
         usexph = 1;

      } else if( proj == SMF__JSA_XPHS ) {
         ng = 4*skytiling->ntpf;
         dec_ref = -AST__DPIBY2;
         usexph = 1;

      } else if( *status == SAI__OK ){
         ng = 0;
         *status = SAI__ERROR;
         errRepf( "", "smf_jsatileheader: Unsupported projection '%d' "
                  "requested.", status, proj );
      }

      gx_ref = gy_ref = 0.5*( ng + 1 );
      fc = smfMakeFC( ng, ng, skytiling->ntpf, 1, gx_ref, gy_ref, ra_ref,
                      dec_ref, usexph, status );

/* For SMF__JSA_HPX, add in an alternate axis descriptions describing the
   JSA pixel (x,y) coordinate grid. */
      if(  proj == SMF__JSA_HPX ) {
         astPutFits( fc, "CTYPE1A = 'XJSA'", 1 );
         astPutFits( fc, "CTYPE2A = 'YJSA'", 1 );
         astPutFits( fc, "CRVAL1A = -1.0", 1 );
         astPutFits( fc, "CRVAL2A = -1.0", 1 );
         sprintf( card, "CRPIX1A = %.15g", gx_ref );
         astPutFits( fc, card, 1 );
         sprintf( card, "CRPIX2A = %.15g", gy_ref );
         astPutFits( fc, card, 1 );
         sprintf( card, "CDELT1A = %d",skytiling->ppt );
         astPutFits( fc, card, 1 );
         sprintf( card, "CDELT2A = %d",skytiling->ppt );
         astPutFits( fc, card, 1 );
      }

/* Otherwise, get the number of pixels along one edge of a facet. */
   } else {
      m = skytiling->ntpf*skytiling->ppt;

/* Convert the supplied tile index into a pair of X and Y offsets that
   give the gaps along the X and Y axes, in tiles, between the bottom left
   tile in the SMF__JSA_HPX projection plane and the required tile. This
   function includes a check that the tile number is valid. It also
   returns the index of the HEALPix facet containing the tile. It does
   *not* flip the bottom left half-facet up to the top right. */
      smf_jsatilei2xy( itile, skytiling, &xt, &yt, &fi, status );
      if( *status != SAI__OK ) return fc;

/* Convert the (x,y) offsets of the tile within the SMF__JSA_HPX grid to
   the corresponding offsets in the requested projection. Note, tile
   (x,y) indices are currently in the "raw" mode, in which the lower left
   facet is *not* split (i.e. (0,0) is a valid tile). */
      smf_jsatilexyconv( skytiling, proj, xt, yt, 1, &xt, &yt, status );
      if( ( xt == VAL__BADI || yt == VAL__BADI ) && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( " ", "smf_jsatileheader: converted tile index is bad "
                "(programming error).", status );
         return fc;
      }

/* Choose the sky and pixel coords at the reference point. First deal with
   XPH projections. */
      if( proj == SMF__JSA_XPHN || proj == SMF__JSA_XPHS ) {
         usexph = 1;

/* The reference point is the north or south pole. */
         dec_ref = ( proj == SMF__JSA_XPHN ) ? AST__DPIBY2 : -AST__DPIBY2;

/* Get the grid coordinates (within the grid frame of the requested tile),
   of the projection reference point. The entire XPH projection plane is
   spanned by four facets along each edge. */
         gx_ref = 0.5*( 4.0*m + 1.0 ) - xt*skytiling->ppt;
         gy_ref = 0.5*( 4.0*m + 1.0 ) - yt*skytiling->ppt;

/* Now deal with SMF_JSA_HPX projections. */
      } else if( proj == SMF__JSA_HPX ) {

/* Note the RA at the reference point. For SMF__JSA_HPX projections, this
   is 0 hours except for tiles within facet six. */
         if( fi != 6 ) {

/* Get the grid coordinates (within the grid frame of the current tile),
   of the projection reference point. The entire HPX projection plane is
   spanned by five facets along each edge. */
            gx_ref = 0.5*( 5.0*m + 1.0) - xt*skytiling->ppt;
            gy_ref = 0.5*( 5.0*m + 1.0) - yt*skytiling->ppt;

/* The seventh facet (i.e bottom right, fi==6) in the SMF__JSA_HPX
   projection plane is a problem because it is split by the ra = 12 hours
   line into two halfs. When a WcsMap is used transform (x,y) points in
   the lower left half of this facet, the resulting RA an Dec values will
   be AST__BAD. To avoid this, we use (ra,dec)=(12h,0) as the reference
   point for the seventh facet. Note the RA at the reference point. */
         } else {
            ra_ref = AST__DPI;

/* Get the grid coordinates (within the grid frame of the current tile),
   of the projection reference point (which is at the centre of the first
   facet in this case). */
            gx_ref = 0.5*( m + 1.0) - xt*skytiling->ppt;
            gy_ref = 0.5*( m + 1.0) - yt*skytiling->ppt;

/* Indicate if the tile is in the lower left section (the section that
   needs to be moved up to the top right in pixel coords). */
            if( move )  *move = yt < skytiling->ntpf - 1 && xt < skytiling->ntpf - 1 - yt;
         }

/* Now deal with SMF_JSA_HPX12 projections. */
      } else if( proj == SMF__JSA_HPX12 ) {

/* Note the RA at the reference point. For SMF__JSA_HPX12 projections,
   this is 12 hours except for tiles within facet four (the equivalent of
   facet 6 in HPX). */
         if( fi != 4 ) {
            ra_ref = AST__DPI;

/* Get the grid coordinates (within the grid frame of the current tile),
   of the projection reference point. The entire HPX projection plane is
   spanned by five facets along each edge. */
            gx_ref = 0.5*( 5.0*m + 1.0) - xt*skytiling->ppt;
            gy_ref = 0.5*( 5.0*m + 1.0) - yt*skytiling->ppt;

/* The fifth facet (i.e bottom right, fi==4) in the SMF__JSA_HPX12
   projection plane is a problem because it is split by the ra = 0 hours
   line into two halfs. When a WcsMap is used transform (x,y) points in
   the lower left half of this facet, the resulting RA an Dec values will
   be AST__BAD. To avoid this, we use (ra,dec)=(0h,0) as the reference
   point for the fifth facet. Note the RA at the reference point. */
         } else {

/* Get the grid coordinates (within the grid frame of the current tile),
   of the projection reference point (which is at the centre of the first
   facet in this case). */
            gx_ref = 0.5*( m + 1.0) - xt*skytiling->ppt;
            gy_ref = 0.5*( m + 1.0) - yt*skytiling->ppt;

/* Indicate if the tile is in the lower left section (the section that
   needs to be moved up to the top right in pixel coords). */
            if( move )  *move = yt < skytiling->ntpf - 1 && xt < skytiling->ntpf - 1 - yt;
         }

/* Report an error for an unknown projection. */
      } else if( *status == SAI__OK ){
         *status = SAI__ERROR;
         errRepf( "", "smf_jsatileheader: Unsupported projection '%d' "
                  "requested.", status, proj );
      }

/* Put the basic header into a FitsChan. */
      fc = smfMakeFC( 1, 1, skytiling->ntpf, skytiling->ppt, gx_ref, gy_ref,
                      ra_ref, dec_ref, usexph, status );

/* If we want a local pixel origin... */
      if( local_origin ) {

/* Read a FrameSet from the FitsChan, then annul the FitsChan. */
         astClear( fc, "Card" );
         fs = astRead( fc );
         fc = astAnnul( fc );

/* Store the grid coords at the tile centre. */
         gx_cen = gy_cen = 0.5*( skytiling->ppt + 1.0 );

/* Transform these into WCS coords to get the ra,dec (in radians) at
   the centre of the tile. Then annull the FrameSet. */
         astTran2( fs, 1, &gx_cen, &gy_cen, 1, &ra_cen, &dec_cen );
         fs = astAnnul( fs );

/* To avoid the header including huge CRPIX values and CRVAL values that
   are (in general) way off the tile (i.e. the origin of (RA,Dec)), we
   use the option described in FITS-WCS paper II section 2.5 to specify
   CRVAL as the (RA,Dec) at the centre of the tile. Create a FitsCHan
   holding the header. */
         fc = smfMakeFC( 1, 1, skytiling->ntpf, skytiling->ppt, gx_cen, gy_cen,
                         ra_cen, dec_cen, usexph, status );

/* Set PVi_0 to a non-zero value in the header to indicate that the CRPIX
   grid coords correspond to the CRVAL sky coords, and not to the projection
   reference point (i.e. (ra,dec)=(0,0) or (ra,dec)=(180,0)). */
         astPutFits( fc, "PV1_0   = 1.0", 1 );

/* We now need to store the native coords corresponding to CRVAL in the
   PVi_1/PVi_2 keywords. For all but the first tile, native coords and
   (RA,Dec) are equivalent so these are just the same as the values stored
   for CRVAL. For the first tile (which uses ra=180 as the projection
   reference point), we need to shift the RA value by 180 degrees. */
         if( fi == 0 ) ra_cen -= AST__DPI;
         sprintf( card,  "PV1_1   = %.15g", ra_cen*AST__DR2D );
         astPutFits( fc, card, 1 );
         sprintf( card,  "PV1_2   = %.15g", dec_cen*AST__DR2D );
         astPutFits( fc, card, 1 );
      }

/* Add the zero based tile index into the header. */
      sprintf( card, "JSATILE = %d", itile );
      astPutFits( fc, card, 1 );

/* Add the JSA projection type into the header. */
      sprintf( card, "JSAPROJ = '%s'", smf_jsaproj_tostr( proj ) );
      astPutFits( fc, card, 1 );
   }

/* Return the FitsChan. */
   return fc;
}

static AstFitsChan *smfMakeFC( int nx, int ny, int n, int p, double crpix1,
                               double crpix2, double crval1, double crval2,
                               int usexph, int *status ) {
/*
*  Name:
*     smfMakeFC

*  Purpose:
*     Create a FitsChan holding an HPX header.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     AstFitsChan *smfMakeFC( int nx, int ny, int n, int p, double crpix1,
*                             double crpix2, double crval1, double crval2,
*                             int usexph, int *status )

*  Arguments:
*     nx = int (Given)
*        Number of X tiles within the FITS grid.
*     ny = int (Given)
*        Number of Y tiles within the FITS grid.
*     n = int (Given)
*        Number of tiles along each edge of an HEALPix facet.
*     p = int (Given)
*        Number of pixels along each edge of a tile.
*     crpix1 = double (Given)
*        Value for CRPIX1 header.
*     crpix2 = double (Given)
*        Value for CRPIX2 header.
*     crval1 = double (Given)
*        Value for CRVAL1 header.
*     crval2 = double (Given)
*        Value for CRVAL2 header.
*     usexph = int (Given)
*        If non-zero use "XPH" projection. Otherwise, use "HPX".
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Description:
*     This function returns a FitsChan holding a HPX header created
*     from the supplied values.

*/

/* Local Variables: */
   AstFitsChan *fc = NULL;
   char card[ 81 ];
   char retname[41];
   char shortname[11];
   double height;
   double lat;
   double lon;
   double xyz[3];
   int m;

/* Check inherited status */
   if( *status != SAI__OK ) return fc;

/* Get the geodetic longitude and latitude, and altitude of JCMT. */
   (void) palObs( 0, "JCMT", shortname, sizeof(shortname), retname,
                  sizeof(retname), &lon, &lat, &height );

/* Convert the above observatory spherical coords to Cartesian form as
   required by FITS-WCS. */
   smf_terr( lat, height, -lon, xyz );

/* Create the returned FitsChan. */
   fc = astFitsChan( NULL, NULL, " " );

/* Get the number of pixels along one edge of a facet. */
   m = n*p;

/* Store the cards in the FitsChan. */
   sprintf( card, "NAXIS1  = %d", p*nx );
   astPutFits( fc, card, 1 );

   sprintf( card, "NAXIS2  = %d", p*ny );
   astPutFits( fc, card, 1 );

   sprintf( card, "CTYPE1  = 'RA---%s'", usexph ? "XPH" : "HPX" );
   astPutFits( fc, card, 1 );

   sprintf( card, "CTYPE2  = 'DEC--%s'", usexph ? "XPH" : "HPX" );
   astPutFits( fc, card, 1 );

   sprintf( card, "CRVAL1  = %.15g", crval1*AST__DR2D );
   astPutFits( fc, card, 1 );

   sprintf( card, "CRVAL2  = %.15g", crval2*AST__DR2D );
   astPutFits( fc, card, 1 );

   sprintf( card, "CRPIX1  = %.15g", crpix1 );
   astPutFits( fc, card, 1 );

   sprintf( card, "CRPIX2  = %.15g", crpix2 );
   astPutFits( fc, card, 1 );

   sprintf( card, "CDELT1  = -%.15g", 90.0/(sqrtf(2.0)*m) );
   astPutFits( fc, card, 1 );

   sprintf( card, "CDELT2  = %.15g", 90.0/(sqrtf(2.0)*m) );
   astPutFits( fc, card, 1 );

   if( !usexph ) {
      sprintf( card, "PC1_1   = 0.7071068" );
      astPutFits( fc, card, 1 );

      sprintf( card, "PC1_2   = 0.7071068" );
      astPutFits( fc, card, 1 );

      sprintf( card, "PC2_1   = -0.7071068" );
      astPutFits( fc, card, 1 );

      sprintf( card, "PC2_2   = 0.7071068" );
      astPutFits( fc, card, 1 );
   }

   sprintf( card, "RADESYS = 'ICRS'" );
   astPutFits( fc, card, 1 );

   sprintf( card, "OBSGEO-X= %.16g", xyz[ 0 ] );
   astPutFits( fc, card, 1 );

   sprintf( card, "OBSGEO-Y= %.16g", xyz[ 1 ] );
   astPutFits( fc, card, 1 );

   sprintf( card, "OBSGEO-Z= %.16g", xyz[ 2 ] );
   astPutFits( fc, card, 1 );

/* Return the result. */
   return fc;
}








