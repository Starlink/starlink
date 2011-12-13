      SUBROUTINE NDF1_BPPB( EL, ARRAY, BAD, STATUS )
*+
*  Name:
*     NDF1_BPPB

*  Purpose:
*     Determine if bad pixels are present in a vectorised BYTE array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_BPPB( EL, ARRAY, BAD, STATUS )

*  Description:
*     The routine examines the values in a vectorised BYTE array and
*     returns a logical result BAD indicating whether any element of the
*     array contains the "bad" pixel value VAL__BADB.

*  Arguments:
*     EL = INTEGER (Given)
*        Number of elements in the vectorised array.
*     ARRAY( EL ) = BYTE (Given)
*        The array to be examined.
*     BAD = LOGICAL (Returned)
*        Whether any ARRAY element had the value VAL__BADB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise.
*     -  Loop to examine each array element.
*     -  If a bad value is found, then set BAD=.TRUE. and quit checking.

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
*     18-DEC-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER EL
      BYTE ARRAY( EL )

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      BAD = .FALSE.

*  Loop to examine each array element.
      DO 1 I = 1, EL

*  If a bad value is found, set BAD=.TRUE. and quit checking.
         IF ( ARRAY( I ) .EQ. VAL__BADB ) THEN
            BAD = .TRUE.
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_BPPB',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_BPPUB( EL, ARRAY, BAD, STATUS )
*+
*  Name:
*     NDF1_BPPUB

*  Purpose:
*     Determine if bad pixels are present in a vectorised UNSIGNED BYTE array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_BPPUB( EL, ARRAY, BAD, STATUS )

*  Description:
*     The routine examines the values in a vectorised UNSIGNED BYTE array and
*     returns a logical result BAD indicating whether any element of the
*     array contains the "bad" pixel value VAL__BADUB.

*  Arguments:
*     EL = INTEGER (Given)
*        Number of elements in the vectorised array.
*     ARRAY( EL ) = BYTE (Given)
*        The array to be examined.
*     BAD = LOGICAL (Returned)
*        Whether any ARRAY element had the value VAL__BADUB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise.
*     -  Loop to examine each array element.
*     -  If a bad value is found, then set BAD=.TRUE. and quit checking.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-DEC-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER EL
      BYTE ARRAY( EL )

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      BAD = .FALSE.

*  Loop to examine each array element.
      DO 1 I = 1, EL

*  If a bad value is found, set BAD=.TRUE. and quit checking.
         IF ( ARRAY( I ) .EQ. VAL__BADUB ) THEN
            BAD = .TRUE.
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_BPPUB',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_BPPD( EL, ARRAY, BAD, STATUS )
*+
*  Name:
*     NDF1_BPPD

*  Purpose:
*     Determine if bad pixels are present in a vectorised DOUBLE PRECISION array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_BPPD( EL, ARRAY, BAD, STATUS )

*  Description:
*     The routine examines the values in a vectorised DOUBLE PRECISION array and
*     returns a logical result BAD indicating whether any element of the
*     array contains the "bad" pixel value VAL__BADD.

*  Arguments:
*     EL = INTEGER (Given)
*        Number of elements in the vectorised array.
*     ARRAY( EL ) = DOUBLE PRECISION (Given)
*        The array to be examined.
*     BAD = LOGICAL (Returned)
*        Whether any ARRAY element had the value VAL__BADD.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise.
*     -  Loop to examine each array element.
*     -  If a bad value is found, then set BAD=.TRUE. and quit checking.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-DEC-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER EL
      DOUBLE PRECISION ARRAY( EL )

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      BAD = .FALSE.

*  Loop to examine each array element.
      DO 1 I = 1, EL

*  If a bad value is found, set BAD=.TRUE. and quit checking.
         IF ( ARRAY( I ) .EQ. VAL__BADD ) THEN
            BAD = .TRUE.
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_BPPD',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_BPPI( EL, ARRAY, BAD, STATUS )
*+
*  Name:
*     NDF1_BPPI

*  Purpose:
*     Determine if bad pixels are present in a vectorised INTEGER array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_BPPI( EL, ARRAY, BAD, STATUS )

*  Description:
*     The routine examines the values in a vectorised INTEGER array and
*     returns a logical result BAD indicating whether any element of the
*     array contains the "bad" pixel value VAL__BADI.

*  Arguments:
*     EL = INTEGER (Given)
*        Number of elements in the vectorised array.
*     ARRAY( EL ) = INTEGER (Given)
*        The array to be examined.
*     BAD = LOGICAL (Returned)
*        Whether any ARRAY element had the value VAL__BADI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise.
*     -  Loop to examine each array element.
*     -  If a bad value is found, then set BAD=.TRUE. and quit checking.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-DEC-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER EL
      INTEGER ARRAY( EL )

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      BAD = .FALSE.

*  Loop to examine each array element.
      DO 1 I = 1, EL

*  If a bad value is found, set BAD=.TRUE. and quit checking.
         IF ( ARRAY( I ) .EQ. VAL__BADI ) THEN
            BAD = .TRUE.
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_BPPI',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_BPPR( EL, ARRAY, BAD, STATUS )
*+
*  Name:
*     NDF1_BPPR

*  Purpose:
*     Determine if bad pixels are present in a vectorised REAL array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_BPPR( EL, ARRAY, BAD, STATUS )

*  Description:
*     The routine examines the values in a vectorised REAL array and
*     returns a logical result BAD indicating whether any element of the
*     array contains the "bad" pixel value VAL__BADR.

*  Arguments:
*     EL = INTEGER (Given)
*        Number of elements in the vectorised array.
*     ARRAY( EL ) = REAL (Given)
*        The array to be examined.
*     BAD = LOGICAL (Returned)
*        Whether any ARRAY element had the value VAL__BADR.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise.
*     -  Loop to examine each array element.
*     -  If a bad value is found, then set BAD=.TRUE. and quit checking.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-DEC-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER EL
      REAL ARRAY( EL )

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      BAD = .FALSE.

*  Loop to examine each array element.
      DO 1 I = 1, EL

*  If a bad value is found, set BAD=.TRUE. and quit checking.
         IF ( ARRAY( I ) .EQ. VAL__BADR ) THEN
            BAD = .TRUE.
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_BPPR',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_BPPW( EL, ARRAY, BAD, STATUS )
*+
*  Name:
*     NDF1_BPPW

*  Purpose:
*     Determine if bad pixels are present in a vectorised WORD array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_BPPW( EL, ARRAY, BAD, STATUS )

*  Description:
*     The routine examines the values in a vectorised WORD array and
*     returns a logical result BAD indicating whether any element of the
*     array contains the "bad" pixel value VAL__BADW.

*  Arguments:
*     EL = INTEGER (Given)
*        Number of elements in the vectorised array.
*     ARRAY( EL ) = INTEGER*2 (Given)
*        The array to be examined.
*     BAD = LOGICAL (Returned)
*        Whether any ARRAY element had the value VAL__BADW.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise.
*     -  Loop to examine each array element.
*     -  If a bad value is found, then set BAD=.TRUE. and quit checking.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-DEC-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER EL
      INTEGER*2 ARRAY( EL )

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      BAD = .FALSE.

*  Loop to examine each array element.
      DO 1 I = 1, EL

*  If a bad value is found, set BAD=.TRUE. and quit checking.
         IF ( ARRAY( I ) .EQ. VAL__BADW ) THEN
            BAD = .TRUE.
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_BPPW',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_BPPUW( EL, ARRAY, BAD, STATUS )
*+
*  Name:
*     NDF1_BPPUW

*  Purpose:
*     Determine if bad pixels are present in a vectorised UNSIGNED WORD array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_BPPUW( EL, ARRAY, BAD, STATUS )

*  Description:
*     The routine examines the values in a vectorised UNSIGNED WORD array and
*     returns a logical result BAD indicating whether any element of the
*     array contains the "bad" pixel value VAL__BADUW.

*  Arguments:
*     EL = INTEGER (Given)
*        Number of elements in the vectorised array.
*     ARRAY( EL ) = INTEGER*2 (Given)
*        The array to be examined.
*     BAD = LOGICAL (Returned)
*        Whether any ARRAY element had the value VAL__BADUW.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise.
*     -  Loop to examine each array element.
*     -  If a bad value is found, then set BAD=.TRUE. and quit checking.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-DEC-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER EL
      INTEGER*2 ARRAY( EL )

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      BAD = .FALSE.

*  Loop to examine each array element.
      DO 1 I = 1, EL

*  If a bad value is found, set BAD=.TRUE. and quit checking.
         IF ( ARRAY( I ) .EQ. VAL__BADUW ) THEN
            BAD = .TRUE.
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_BPPUW',
     :                                            STATUS )

      END
