      SUBROUTINE NDF1_ID2AC( INDF, IACB )
*+
*  Name:
*     NDF1_ID2AC

*  Purpose:
*     Convert an NDF identifier into the associated ACB index.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ID2AC( INDF, IACB )

*  Description:
*     The routine converts an NDF identifier, previously issued by
*     NDF1_EXPID, into an index to the appropriate entry in the ACB.
*     The identifier supplied is fully checked and a value of zero is
*     returned if it is not valid.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     IACB = INTEGER (Returned)
*        Index to an entry in the ACB, or zero of the identifier
*        supplied was not valid.

*  Notes:
*     -  This routine does not perform error checking or reporting.

*  Algorithm:
*     -  Check that the INDF value supplied is positive and return a
*     value of zero if it is not.
*     -  Decode the INDF value into an index for the ACB.
*     -  Check that the identifier value matches the value originally
*     issued for the ACB slot and that the slot is still in use.
*     -  If everything is OK, then return the ACB index. Otherwise,
*     return zero.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CHK( NDF__MXACB ) = INTEGER (Read)
*           Identifier value originally issued for each ACB slot.
*        ACB_USED( NDF__MXACB ) = LOGICAL (Read)
*           Whether the ACB slot is in use.

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      INTEGER IACB

*  External References:
      EXTERNAL NDF1_INIT         ! Initialise common blocks

*  Local Variables:
      INTEGER I                  ! Temporary variable for ACB index

*.

*  Check that the identifier value is positive, and return a value of
*  zero if it is not.
      IF ( INDF .LE. 0 ) THEN
         IACB = 0

*  Decode the INDF value into an index for the ACB (the reverse of the
*  process used by NDF1_EXPID to encode it).
      ELSE
         I = MOD( INDF, NDF__MXACB )
         IF ( I .EQ. 0 ) I = NDF__MXACB

*  Check that the identifier matches the value originally issued for
*  this ACB slot and that the slot is still in use. If OK, then return
*  the ACB index. Otherwise, return zero.
         IF ( ( ACB_CHK( I ) .EQ. INDF ) .AND. ACB_USED( I ) ) THEN
            IACB = I
         ELSE
            IACB = 0
         END IF
      END IF

      END
