      SUBROUTINE IRQ1_QLST8( BIT, LISTED, SET, NDIM, NCOORD, LIST, LBND,
     :                       UBND, SIZE, QUAL, STATUS )
*+
*  Name:
*     IRQ1_QLST8

*  Purpose:
*     Set a QUALITY bit for pixels determined by a list.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_QLST8( BIT, LISTED, SET, NDIM, NCOORD, LIST, LBND,
*                      UBND, SIZE, QUAL, STATUS )

*  Description:
*     If SET is true then selected pixels in the QUALITY vector have
*     the specified bit set. If SET is false, then the selected pixels
*     have the specified bit cleared. If LISTED is true then pixels are
*     selected if they appear in the supplied list of pixel
*     coordinates. If LISTED is false then pixels are selected if they
*     do not appear in the list. Listed pixels which lie outside the
*     supplied pixel bounds are ignored.

*  Arguments:
*     BIT = INTEGER (Given)
*        The bit number within the QUALITY component. The least
*        significant bit is called bit 1 (not bit 0).
*     LISTED = LOGICAL (Given)
*        If true, then the operation specified by SET is performed on
*        all the pixels in the list of coordinates given by LIST.  If
*        false, then the operation specified by SET is performed on all
*        the pixels which are not in the list of coordinates given by
*        LIST.
*     SET = LOGICAL (Given)
*        If true, then the selected pixels have the given bit set,
*        indicating that the corresponding quality is held.  If false,
*        then the selected pixels have the given bit cleared,
*        indicating that the corresponding quality is not held.
*     NDIM = INTEGER (Given)
*        The number of dimensions. This determines how many coordinates
*        values are stored for each pixel in the list, and also
*        determines the number of upper and lower bounds supplied in
*        LBND and UBND.
*     NCOORD = INTEGER*8 (Given)
*        The number of pixels in the input list. Each pixel has NDIM
*        coordinate values to describe its position.
*     LIST( NDIM, NCOORD ) = INTEGER*8 (Given)
*        The list of pixel positions.
*     LBND( NDIM ) = INTEGER*8 (Given)
*        The lower bound on each axis of the pixel space.
*     UBND( NDIM ) = INTEGER*8 (Given)
*        The upper bound on each axis of the pixel space.
*     SIZE = INTEGER*8 (Given)
*        The size of the QUAL vector. This should be equal to the
*        product of the dimensions implied by LBND and UBND.
*     QUAL( SIZE ) = BYTE (Given and Returned)
*        The QUALITY vector.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  VAX-specific features used:
*     -  Uses BYTE arrays.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-OCT-2019 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER BIT
      LOGICAL LISTED
      LOGICAL SET
      INTEGER NDIM
      INTEGER*8 NCOORD
      INTEGER*8 LIST( NDIM, NCOORD )
      INTEGER*8 LBND( NDIM )
      INTEGER*8 UBND( NDIM )
      INTEGER*8 SIZE

*  Arguments Given and Returned:
      BYTE QUAL( SIZE )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*8 NEL              ! No. of elements in mapped workspace.
      INTEGER*8 NOK              ! No. of listed pixel positions which
                                 ! satisfy the bounds constraints.
      INTEGER PNT                ! Pointer to mapped workspace.
      CHARACTER WLOC*(DAT__SZLOC)! Locator to workspace.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get workspace to hold the vectorised addresses corresponding to the
*  listed pixel positions.
      CALL IRQ1_TEMP8( '_INTEGER*8', NCOORD, WLOC, STATUS )

*  Map the temporary workspace as a 1D vector.
      CALL DAT_MAPV8( WLOC, '_INTEGER*8', 'WRITE', PNT, NEL, STATUS )

*  Convert the N dimensional coordinates to the corresponding 1D vector
*  addresses, excluding any coordinates which fall outside the bounds.
      CALL IRQ1_NDTOV8( NDIM, NCOORD, LIST, LBND, UBND,
     :                  %VAL( CNF_PVAL( PNT ) ), NOK, STATUS )

*  Perform the required operation on the selected pixels.
      CALL IRQ1_QLST28( BIT, LISTED, SET, NOK, %VAL( CNF_PVAL( PNT ) ),
     :                  SIZE, QUAL, STATUS )

*  Annul the temporary workspace.
      CALL IRQ1_ANTMP( WLOC, STATUS )

      END
