      SUBROUTINE GRP1_IMPID( IGRP, SLOT, STATUS )
*+
*  Name:
*     GRP1_IMPID

*  Purpose:
*     Import an identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  Description:
*     The routine converts a GRP identifier, previously issued by
*     GRP1_EXPID, into an index to the appropriate entry in the common
*     arrays.  The identifier value is fully checked and an error is
*     reported if it is not valid.

*  Arguments:
*     IGRP = INTEGER (Given)
*        GRP identifier.
*     SLOT = INTEGER (Returned)
*        Index to an entry in the common arrays.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*      -  Set an initial value for the SLOT argument before checking
*      the inherited status.
*     -  Decode the IGRP value into an index for the common arrays.
*     -  If a valid index was not obtained, then report an error.

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

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_ERR'          ! GRP_ error codes

*  Arguments Given:
      INTEGER IGRP

*  Arguments Returned:
      INTEGER SLOT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Set an initial value for the SLOT argument.
      SLOT = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert the identifier to an common arrays index.
      CALL GRP1_ID2SL( IGRP, SLOT )

*  If a valid index was not returned, then report an error.
      IF ( SLOT .LE. 0 ) THEN
         STATUS = GRP__INVID
         CALL MSG_SETI( 'IGRP', IGRP )
         CALL ERR_REP( 'GRP1_IMPID_ERR1',
     :   'GRP1_IMPID: GRP identifier invalid; its value is ^IGRP '//
     :   '(possible programming error).', STATUS )
      END IF

      END
