      SUBROUTINE GRP1_ID2SL( IGRP, SLOT )
*+
*  Name:
*     GRP1_ID2SL

*  Purpose:
*     Convert a GRP identifier into the associated common array index.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_ID2SL( IGRP, SLOT )

*  Description:
*     The routine converts a GRP identifier, previously issued by
*     GRP1_EXPID, into an index to the appropriate entry in the common
*     arrays.  The identifier supplied is fully checked and a value of
*     zero is returned if it is not valid.

*  Arguments:
*     IGRP = INTEGER (Given)
*        GRP identifier.
*     SLOT = INTEGER (Returned)
*        Index to an entry in the common arrays, or zero of the
*        identifier supplied was not valid.

*  Notes:
*     -  This routine does not perform error checking or reporting.

*  Algorithm:
*     -  Check that the IGRP value supplied is positive and return a
*     value of zero if it is not.
*     -  Decode the IGRP value into an index for the common arrays.
*     -  Check that the identifier value matches the value originally
*     issued for the common arrays slot and that the slot is still in
*     use.
*     -  If everything is OK, then return the common arrays index.
*     Otherwise, return zero.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP_ constants

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP_ common blocks.
*        CMN_CHK( GRP__MAXG ) = INTEGER (Read)
*           Identifier value originally issued for each slot.
*        CMN_USED( GRP__MAXG ) = LOGICAL (Read)
*           Whether the slot is in use.

*  Arguments Given:
      INTEGER IGRP

*  Arguments Returned:
      INTEGER SLOT

*  External References:
      EXTERNAL GRP1_INIT         ! Initialise common blocks

*  Local Variables:
      INTEGER I                  ! Temporary variable for common arrays index

*.

*  Check that the identifier value is positive, and return a value of
*  zero if it is not.
      IF ( IGRP .LE. 0 ) THEN
         SLOT = 0

*  Decode the IGRP value into an index for the common arrays (the
*  reverse of the process used by GRP1_EXPID to encode it).
      ELSE
         I = MOD( IGRP, GRP__MAXG )
         IF ( I .EQ. 0 ) I = GRP__MAXG

*  Check that the identifier matches the value originally issued for
*  this slot and that the slot is still in use. If OK, then return
*  the common arrays index. Otherwise, return zero.
         IF ( ( CMN_CHK( I ) .EQ. IGRP ) .AND. CMN_USED( I ) ) THEN
            SLOT = I
         ELSE
            SLOT = 0
         END IF
      END IF

      END
