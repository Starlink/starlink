      SUBROUTINE NDF1_AVEB( UPPER, PIX0, LBNDA, UBNDA, AVAR, STATUS )
*+
*  Name:
*     NDF1_AVEB

*  Purpose:
*     Assign extrapolated values to a BYTE axis variance array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AVEB( UPPER, PIX0, LBNDA, UBNDA, AVAR, STATUS )

*  Description:
*     The routine assigns extrapolated values to a BYTE axis variance
*     array. It is intended for assigning values to those axis variance
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is zero.

*  Arguments:
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     LBNDA = INTEGER (Given)
*        The lower bound of the axis variance array.
*     UBNDA = INTEGER (Given)
*        The upper bound of the axis variance array.
*     AVAR( LBNDA : UBNDA ) = BYTE (Given and Returned)
*        The BYTE axis variance array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Loop to assign extrapolated values (zero) to the array
*     elements.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     17-OCT-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER PIX0
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      BYTE AVAR( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      BYTE ZERO                ! Zero
      PARAMETER ( ZERO = 0  )

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBNDA
      ELSE
         L = LBNDA
         U = PIX0
      END IF

*  Loop to assign extrapolated values (zero) to the array elements.
      DO 1 I = L, U
         AVAR( I ) = ZERO
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AVEB',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AVEUB( UPPER, PIX0, LBNDA, UBNDA, AVAR, STATUS )
*+
*  Name:
*     NDF1_AVEUB

*  Purpose:
*     Assign extrapolated values to a UNSIGNED BYTE axis variance array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AVEUB( UPPER, PIX0, LBNDA, UBNDA, AVAR, STATUS )

*  Description:
*     The routine assigns extrapolated values to a UNSIGNED BYTE axis variance
*     array. It is intended for assigning values to those axis variance
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is zero.

*  Arguments:
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     LBNDA = INTEGER (Given)
*        The lower bound of the axis variance array.
*     UBNDA = INTEGER (Given)
*        The upper bound of the axis variance array.
*     AVAR( LBNDA : UBNDA ) = BYTE (Given and Returned)
*        The UNSIGNED BYTE axis variance array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Loop to assign extrapolated values (zero) to the array
*     elements.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-OCT-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER PIX0
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      BYTE AVAR( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      BYTE ZERO                ! Zero
      PARAMETER ( ZERO = 0  )

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBNDA
      ELSE
         L = LBNDA
         U = PIX0
      END IF

*  Loop to assign extrapolated values (zero) to the array elements.
      DO 1 I = L, U
         AVAR( I ) = ZERO
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AVEUB',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AVED( UPPER, PIX0, LBNDA, UBNDA, AVAR, STATUS )
*+
*  Name:
*     NDF1_AVED

*  Purpose:
*     Assign extrapolated values to a DOUBLE PRECISION axis variance array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AVED( UPPER, PIX0, LBNDA, UBNDA, AVAR, STATUS )

*  Description:
*     The routine assigns extrapolated values to a DOUBLE PRECISION axis variance
*     array. It is intended for assigning values to those axis variance
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is zero.

*  Arguments:
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     LBNDA = INTEGER (Given)
*        The lower bound of the axis variance array.
*     UBNDA = INTEGER (Given)
*        The upper bound of the axis variance array.
*     AVAR( LBNDA : UBNDA ) = DOUBLE PRECISION (Given and Returned)
*        The DOUBLE PRECISION axis variance array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Loop to assign extrapolated values (zero) to the array
*     elements.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-OCT-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER PIX0
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      DOUBLE PRECISION AVAR( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION ZERO                ! Zero
      PARAMETER ( ZERO = 0.0D0 )

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBNDA
      ELSE
         L = LBNDA
         U = PIX0
      END IF

*  Loop to assign extrapolated values (zero) to the array elements.
      DO 1 I = L, U
         AVAR( I ) = ZERO
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AVED',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AVEI( UPPER, PIX0, LBNDA, UBNDA, AVAR, STATUS )
*+
*  Name:
*     NDF1_AVEI

*  Purpose:
*     Assign extrapolated values to a INTEGER axis variance array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AVEI( UPPER, PIX0, LBNDA, UBNDA, AVAR, STATUS )

*  Description:
*     The routine assigns extrapolated values to a INTEGER axis variance
*     array. It is intended for assigning values to those axis variance
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is zero.

*  Arguments:
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     LBNDA = INTEGER (Given)
*        The lower bound of the axis variance array.
*     UBNDA = INTEGER (Given)
*        The upper bound of the axis variance array.
*     AVAR( LBNDA : UBNDA ) = INTEGER (Given and Returned)
*        The INTEGER axis variance array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Loop to assign extrapolated values (zero) to the array
*     elements.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-OCT-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER PIX0
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      INTEGER AVAR( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER ZERO                ! Zero
      PARAMETER ( ZERO = 0  )

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBNDA
      ELSE
         L = LBNDA
         U = PIX0
      END IF

*  Loop to assign extrapolated values (zero) to the array elements.
      DO 1 I = L, U
         AVAR( I ) = ZERO
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AVEI',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AVER( UPPER, PIX0, LBNDA, UBNDA, AVAR, STATUS )
*+
*  Name:
*     NDF1_AVER

*  Purpose:
*     Assign extrapolated values to a REAL axis variance array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AVER( UPPER, PIX0, LBNDA, UBNDA, AVAR, STATUS )

*  Description:
*     The routine assigns extrapolated values to a REAL axis variance
*     array. It is intended for assigning values to those axis variance
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is zero.

*  Arguments:
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     LBNDA = INTEGER (Given)
*        The lower bound of the axis variance array.
*     UBNDA = INTEGER (Given)
*        The upper bound of the axis variance array.
*     AVAR( LBNDA : UBNDA ) = REAL (Given and Returned)
*        The REAL axis variance array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Loop to assign extrapolated values (zero) to the array
*     elements.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-OCT-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER PIX0
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      REAL AVAR( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL ZERO                ! Zero
      PARAMETER ( ZERO = 0.0E0 )

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBNDA
      ELSE
         L = LBNDA
         U = PIX0
      END IF

*  Loop to assign extrapolated values (zero) to the array elements.
      DO 1 I = L, U
         AVAR( I ) = ZERO
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AVER',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AVEW( UPPER, PIX0, LBNDA, UBNDA, AVAR, STATUS )
*+
*  Name:
*     NDF1_AVEW

*  Purpose:
*     Assign extrapolated values to a WORD axis variance array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AVEW( UPPER, PIX0, LBNDA, UBNDA, AVAR, STATUS )

*  Description:
*     The routine assigns extrapolated values to a WORD axis variance
*     array. It is intended for assigning values to those axis variance
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is zero.

*  Arguments:
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     LBNDA = INTEGER (Given)
*        The lower bound of the axis variance array.
*     UBNDA = INTEGER (Given)
*        The upper bound of the axis variance array.
*     AVAR( LBNDA : UBNDA ) = INTEGER*2 (Given and Returned)
*        The WORD axis variance array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Loop to assign extrapolated values (zero) to the array
*     elements.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-OCT-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER PIX0
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      INTEGER*2 AVAR( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER*2 ZERO                ! Zero
      PARAMETER ( ZERO = 0  )

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBNDA
      ELSE
         L = LBNDA
         U = PIX0
      END IF

*  Loop to assign extrapolated values (zero) to the array elements.
      DO 1 I = L, U
         AVAR( I ) = ZERO
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AVEW',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AVEUW( UPPER, PIX0, LBNDA, UBNDA, AVAR, STATUS )
*+
*  Name:
*     NDF1_AVEUW

*  Purpose:
*     Assign extrapolated values to a UNSIGNED WORD axis variance array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AVEUW( UPPER, PIX0, LBNDA, UBNDA, AVAR, STATUS )

*  Description:
*     The routine assigns extrapolated values to a UNSIGNED WORD axis variance
*     array. It is intended for assigning values to those axis variance
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is zero.

*  Arguments:
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     LBNDA = INTEGER (Given)
*        The lower bound of the axis variance array.
*     UBNDA = INTEGER (Given)
*        The upper bound of the axis variance array.
*     AVAR( LBNDA : UBNDA ) = INTEGER*2 (Given and Returned)
*        The UNSIGNED WORD axis variance array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Loop to assign extrapolated values (zero) to the array
*     elements.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-OCT-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER PIX0
      INTEGER LBNDA
      INTEGER UBNDA

*  Arguments Given and Returned:
      INTEGER*2 AVAR( LBNDA : UBNDA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER*2 ZERO                ! Zero
      PARAMETER ( ZERO = 0  )

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBNDA
      ELSE
         L = LBNDA
         U = PIX0
      END IF

*  Loop to assign extrapolated values (zero) to the array elements.
      DO 1 I = L, U
         AVAR( I ) = ZERO
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AVEUW',
     :                                            STATUS )

      END
