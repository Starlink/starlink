/*
*+
*  Name:
*     smf_maketanmap.c

*  Purpose:
*     Create a tangent plane mapping

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_maketanmap( double lon, double lat, AstMapping *cache[ 2 ],
*                     int *status )

*  Arguments:
*     lon = double (Given)
*        Celestial longitude at ref point (rads)
*     lat = double (Given)
*        Celestial latitude at ref point (rads)
*     cache = AstMapping* (Given and Returned)
*        Cached Mappings (supply as NULL on 1st call)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     The forward transformation of the returned Mapping transforms
*     cartesian tangent plane offsets in radians, into celestial longitude
*     and latitude values, in radians. The reference point of the tangent
*     plane is put at the supplied longitude and latitude position. It is
*     assumed that the second cartesian input axis is parallel to celestial
*     north.

*     The "cache" array should be filled with NULL values before the first
*     call to this function. It will be returned holding AST pointers to
*     Mappings which will be needed on subsequent calls (these pointers are
*     exempted from AST context handling).

*  Authors:
*     D.S.Berry (dsb@ast.man.ac.uk)
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-02-10 (dsb)
*       Initial version
*     2006-07-17 (dsb)
*       Modified to remove Nasmyth rotation
*     2008-09-1 (dsb)
*       Set the SphMap attribute "UnitRadius" so that SphMaps can be
*       simplified.
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
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
#include "ndf.h"
#include "sae_par.h"

/* SMURF includes */
#include "smf.h"

AstMapping *smf_maketanmap( double lon, double lat, AstMapping *cache[ 2 ],
                            int *status ) {

  /* Local Variables */

  AstMapping *result;
  AstMatrixMap *matmap;
  AstCmpMap *m1;
  AstWcsMap *wcsmap;
  double t1, ct, cn, st, sn, mat[ 9 ];

  /* Check the inherited status. */
  if ( *status != SAI__OK ) return NULL;

  /* If required, create a SphMap for converting spherical cartesian
     (x,y,z) positions to (lon,lat) positions. */
  if( !cache[ 0 ] ) {
    cache[ 0 ] = (AstMapping *) astSphMap( "UnitRadius=1" );
    astExempt( cache[ 0 ] );
  }

  /* If required, create a CmpMap holding a WcsMap followed by an inverted
     SphMap. */
  if( !cache[ 1 ] ) {
    wcsmap = astWcsMap( 2, AST__TAN, 1, 2, " " );
    astInvert( wcsmap );
    astInvert( cache[ 0 ] );
    cache[ 1 ] = (AstMapping *) astCmpMap( wcsmap, cache[ 0 ], 1, " " );
    astInvert( cache[ 0 ] );
    wcsmap = astAnnul( wcsmap );
    astExempt( cache[ 1 ] );
  }

  /* The required Mapping consists of the "cache[ 1 ]" Mapping, followed by
     a suitable MatrixMap which rotates the tangent point to the requested
     spherical (az,el) coordinates, followed by the "cache[ 0 ]" Mapping.
     The logic follows that of FITS-WCS paper II (which is what the WcsMap
     class assumes). The reference point of the TAN projection is at the
     north pole of the "native" spherical coordinate system. The matrix map
     first rotates about the Y axis by (pi/2-lat) (to rotate the reference
     point from the native north pole to the celestial north pole), then
     rotates about the Z axis by -lon (to rotate the prime native meridian
     to the prime celestial meridian). Here the Z axis points to the north
     pole, the X axis points to (lon,lat)=(0,0) and the Y axis points to
     (lon,lat) = (90 degs,0) (the slalib convention). */

  ct = cos( lat );
  st = sin( lat );
  cn = cos( lon );
  sn = sin( lon );

  t1 = sn*ct;

  mat[ 0 ] = cn*st + st*t1;
  mat[ 1 ] = -t1*ct;
  mat[ 2 ] = ct*cn;
  mat[ 3 ] = st*sn;
  mat[ 4 ] = cn;
  mat[ 5 ] = t1;
  mat[ 6 ] = -ct;
  mat[ 7 ] = 0;
  mat[ 8 ] = st;

  matmap = astMatrixMap( 3, 3, 0, mat, " " );

  /* Create the required compound Mapping. */
  m1 = astCmpMap( cache[ 1 ], matmap, 1, " " );
  result = (AstMapping *) astCmpMap( m1, cache[ 0 ], 1, " " );
  matmap = astAnnul( matmap );
  m1 = astAnnul( m1 );

  /* Return the required Mapping.*/
  return result;

}
