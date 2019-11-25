#include "star/hds.h"
#include "star/hds_fortran.h"
#include "f77.h"
#include "ast.h"
#include "atl.h"
#include "sae_par.h"

F77_SUBROUTINE(atl_getpixelparams)( INTEGER(FSET), INTEGER_ARRAY(DIMS),
                                    LOGICAL(DEGS), DOUBLE_ARRAY(CRPIX),
                                    DOUBLE_ARRAY(CRVAL), DOUBLE_ARRAY(CDELT),
                                    DOUBLE(CROTA), INTEGER(STATUS) ) {
/*
*+
*  Name:
*     ATL_GETPIXELPARAMS

*  Purpose:
*     Find typical values for "FITS-like" parameters describing a FrameSet.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL ATL_GETPIXELPARAMS( FSET, DIMS, DEGS, CRPIX, CRVAL, CDELT,
*                              CROTA, STATUS )

*  Description:
*     This function finds values that resemble the the FITS keywords
*     CRVAL1/2/3.., CRPIX1/2/3..., CRDELT1/2/3... and CROTA2, on the
*     assumption that the base Frame in the supplied FrameSet describe
*     GRID coords (i.e. FITS pixel coords), and the current Frame describe
*     the required WCS.  It is not restricted to 2D FrameSets.
*
*     If the FrameSet can be written to a FitsChan successfully using
*     FITS-WCS encoding, the the resulting keyword values are returned.
*     Otherwise, the values are estimated by transforming closely spaced
*     pixel positions along each axis. If the current Frame contains a
*     SkyFrame, and the SkyFrame has a defined reference position, then
*     this position specifies the returned CRVAL values. Otherwise, the
*     reference position is assumed to be at the central pixel.

*  Arguments:
*     FSET = INTEGER (Given)
*        The FrameSet.
*     DIMS(*) = INTEGER (Given)
*        An array supplied holding the number of pixels along each
*        edge of the pixel array. The number of elements in this array
*        should match the number of axes in the base Frame of FSET.
*     DEGS = LOGICAL (Given)
*        If .TRUE., then the CRVAL, CDELT and CROTA values for sky axes
*        are returned in units of degrees. Otherwise they are returned in
*        radians.
*     CRPIX(*) = DOUBLE PRECISION (Returned)
*        An array returned holding the position of the reference pixel
*        in the base Frame of FSET. The number of elements in this
*        array should match the number of axes in the base Frame of FSET.
*     CRVAL(*) = DOUBLE PRECISION (Returned)
*        An array returned holding the position of the reference pixel
*        in the current Frame of FSET. The number of elements in this
*        array should match the number of axes in the current Frame of
*        FSET.
*     CDELT(*) = DOUBLE PRECISION (Returned)
*        An array returned holding the geodesic distance along each
*        edge of the reference pixel, measured within the current
*        Frame of FSET. The number of elements in this array should
*        match the number of axes in the base Frame of FSET.
*     CROTA = DOUBLE PRECISION (Returned)
*        The angle from north in the current frame of FSET to the
*        second spatial pixel axis, measured positive through east.
*        This will be returned set to AST__BAD if the current frame
*        of FSET does not contain a SkyFrame.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-DEC-2013 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
   GENPTR_INTEGER(FSET)
   GENPTR_INTEGER_ARRAY(DIMS)
   GENPTR_LOGICAL(DEGS)
   GENPTR_DOUBLE_ARRAY(CRPIX)
   GENPTR_DOUBLE_ARRAY(CRVAL)
   GENPTR_DOUBLE_ARRAY(CDELT)
   GENPTR_DOUBLE(CROTA)
   GENPTR_INTEGER(STATUS)

   atlGetPixelParams( astI2P( *FSET ), DIMS, *DEGS, CRPIX,
                      CRVAL, CDELT, CROTA, STATUS );

}
