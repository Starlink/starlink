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
*                                     int local_origin, int *status )

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
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Description:
*     This function returns a FITS header describing a specified sky tile
*     holding data from a specified JCMT instrument (or, if "itile" is
*     -1, a FITS header describing the whole collection of tiles).
*
*     The whole sky is covered by an HPX (HEALPix) projection containing
*     12 basic facets,the reference point (native lon.=0, native lat.=0)
*     of the projection is put at (RA,Dec)=(0,0) [except for facet six
*     which has a reference point of (12h,0)], so that native coords
*     are equivalent to celestial coords. The projection plane is rotated
*     by 45 degrees so that the edges of each facet are parallel to X and
*     Y (as in Fig.3 of the A&A paper "Mapping on the HEALPix grid" by
*     Calabretta and Roukema). Each facet is then divided up into NxN
*     tiles, where N is given by "skytiling->ntpf". Each tile is then
*     divided into PxP pixels, where P is given by "skytiling->ppt".
*
*     The projection reference point is at the origin of native longitude
*     and latitude, which corresponds to the origin of (RA,Dec). However,
*     this means that for most tiles the reference point is not actually
*     contained within the tile, and also results in jsatiles.having huge
*     values for CRPIX1/2. To avoid this, the feature described in
*     FITS-WCS paper II section 2.5 ("User specified phi_0,theta_0") may
*     be used (see the "local_origin" argument). This causes the CRVAL and
*     CRPIX values in the returned header do not give the celestial and
*     pixel coords of the projection reference point, but instead give
*     the celestial and pixel coords of an arbitrary point (specified by
*     projection parameters PVi_1 and PVi_2 for longitude axis "i"). The
*     point actually used is the centre centre of the requested tile.
*
*     HEALPix facets are numbered from 0 to 11 as defined in the HEALPix
*     paper (Gorsky et. al. 2005 ApJ 622, 759) (note, facet six
*     occupies the bottom left corner of the projection plane and covers
*     an RA range of 9h to 15h - the top right corner of the projection
*     covers the same area on the sky but has no corresponding tile). Facet
*     zero contains tiles zero to N*N-1, facet one contains tiles N*N to
*     2*N*N-1, etc. Within a facet tiles are indexed using the "nested"
*     scheme described in the HEALPix paper. This starts with pixel
*     zero in the southern corner of the facet.  The even bits number
*     the position in the north-east direction and the odd bits number
*     the position in the north-west direction.

*  Authors:
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     28-FEB-2011 (DSB):
*        Initial version.
*     16-JUL-2013 (DSB):
*        Added argument "local_origin".
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011,2013 Science & Technology Facilities Council.
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

/* SMURF includes */
#include "libsmf/smf.h"


#include "libsmf/jsatiles.h"   /* Move this to smf_typ.h and smf.h when done */

/* Prototypes for private functions. */
static AstFitsChan *smfMakeFC( int nx, int ny, int n, int p, double crpix1,
                               double crpix2, double crval1, double crval2,
                               int *status );

AstFitsChan *smf_jsatileheader( int itile, smfJSATiling *skytiling,
                                int local_origin, int *status ){

/* Local Variables: */
   AstFitsChan *fc = NULL;
   AstFrameSet *fs = NULL;
   char card[ 81 ];   /* FITS header card */
   double dec_ref;    /* DEC at reference point */
   double gx_ref;     /* X grid coord at reference point */
   double gy_ref;     /* Y grid coord at reference point */
   double ra_ref;     /* RA at reference point */
   double dec_cen;    /* DEC at tile centre */
   double gx_cen;     /* X grid coord at tile centre */
   double gy_cen;     /* Y grid coord at tile centre */
   double ra_cen;     /* RA at tile centre */
   int fi;            /* Zero-based facet index in range [0,11] */
   int m;             /* The number of pixels along one edge of a facet */
   int ng;            /* The number of tile along one edge of the FITS grid */
   int xt;            /* X offset to the requested tile */
   int yt;            /* Y offset to the requested tile */

/* Check inherited status */
   if( *status != SAI__OK ) return fc;

/* If the tile index is -1, produce a header for the whole sky (one
   pixel per tile). */
   if( itile == -1 ) {
      ng = 5*skytiling->ntpf;
      gx_ref = gy_ref = 0.5*( ng + 1);
      ra_ref = dec_ref = 0.0;
      fc = smfMakeFC( ng, ng, skytiling->ntpf, 1, gx_ref, gy_ref, ra_ref,
                      dec_ref, status );

   } else {
/* Get the number of pixels along one edge of a facet. */
      m = skytiling->ntpf*skytiling->ppt;

/* Convert the supplied tile index into a pair of X and Y offsets that
   give the gaps along the X and Y axes, in tiles, between the bottom left
   tile in the projection plane and the required tile. This function
   includes a check that the tile number is valid. */
      smf_jsatilei2xy(itile, skytiling, &xt, &yt, &fi, status);
      if( *status != SAI__OK ) return fc;

/* Note the RA at the reference point. This is 0 hours except for tiles
   within facet six. */
      if( fi != 6 ) {
         ra_ref = 0.0;

/* Get the grid coordinates (within the grid frame of the current tile),
   of the projection reference point. The entire projection plane is spanned
   by five facets along each edge. */
         gx_ref = 0.5*( 5.0*m + 1.0) - xt*skytiling->ppt;
         gy_ref = 0.5*( 5.0*m + 1.0) - yt*skytiling->ppt;

/* The seventh facet (i.e bottom right, fi==6) in the projection plane is a
   problem because it is split by the ra = 12hours line into two halfs.
   When a WcsMap is used transform (x,y) points in the lower left half of
   this facet, the resulting RA an Dec values will be AST__BAD. To avoid
   this, we use (ra,dec)=(12h,0) as the reference point for the seventh
   facet. Note the RA at the reference point. */
      } else {
         ra_ref = AST__DPI;

/* Get the grid coordinates (within the grid frame of the current tile),
   of the projection reference point (which is at the centre of the first
   facet in this case). */
         gx_ref = 0.5*( m + 1.0) - xt*skytiling->ppt;
         gy_ref = 0.5*( m + 1.0) - yt*skytiling->ppt;
      }

/* The Declination at the reference point is zero for all tiles. */
      dec_ref = 0.0;

/* Put the basic header into a FitsChan. */
      fc = smfMakeFC( 1, 1, skytiling->ntpf, skytiling->ppt, gx_ref, gy_ref, ra_ref,
                      dec_ref, status );

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
                         ra_cen, dec_cen, status );

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
      sprintf( card, "JCMTTILE= %d", itile );
      astPutFits( fc, card, 1 );
   }

/* Return the FitsChan. */
   return fc;
}

static AstFitsChan *smfMakeFC( int nx, int ny, int n, int p, double crpix1,
                               double crpix2, double crval1, double crval2,
                               int *status ) {
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
*                             int *status )

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
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Description:
*     This function returns a FitsChan holding a HPX header created
*     from the supplied values.

*/

/* Local Variables: */
   AstFitsChan *fc = NULL;
   char card[ 81 ];
   int m;

/* Check inherited status */
   if( *status != SAI__OK ) return fc;

/* Create the returned FitsChan. */
   fc = astFitsChan( NULL, NULL, " " );

/* Get the number of pixels along one edge of a facet. */
   m = n*p;

/* Store the cards in the FitsChan. */
   sprintf( card, "NAXIS1  = %d", p*nx );
   astPutFits( fc, card, 1 );

   sprintf( card, "NAXIS2  = %d", p*ny );
   astPutFits( fc, card, 1 );

   sprintf( card, "CTYPE1  = 'RA---HPX'" );
   astPutFits( fc, card, 1 );

   sprintf( card, "CTYPE2  = 'DEC--HPX'" );
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

   sprintf( card, "PC1_1   = 0.7071068" );
   astPutFits( fc, card, 1 );

   sprintf( card, "PC1_2   = 0.7071068" );
   astPutFits( fc, card, 1 );

   sprintf( card, "PC2_1   = -0.7071068" );
   astPutFits( fc, card, 1 );

   sprintf( card, "PC2_2   = 0.7071068" );
   astPutFits( fc, card, 1 );

   sprintf( card, "RADESYS = 'ICRS'" );
   astPutFits( fc, card, 1 );

   sprintf( card, "EPOCH   = 2013" );
   astPutFits( fc, card, 1 );

/* Return the result. */
   return fc;
}








