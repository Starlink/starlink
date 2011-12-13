      SUBROUTINE NDF1_AWEB( UPPER, PIX0, WIDTH, LBND, UBND, AWIDTH,
     :                        STATUS )
*+
*  Name:
*     NDF1_AWEB

*  Purpose:
*     Assign extrapolated values to a BYTE axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWEB( UPPER, PIX0, WIDTH, LBND, UBND, AWIDTH,
*                       STATUS )

*  Description:
*     The routine assigns extrapolated values to a BYTE axis width
*     array. It is intended for assigning values to those axis width
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is a constant, and is supplied as an
*     argument.

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
*     WIDTH = DOUBLE PRECISION (Given)
*        The extrapolated width value to be assigned.
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     AWIDTH( LBND : UBND ) = BYTE (Given and Returned)
*        The BYTE axis width array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because the extrapolated value cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Check if the extrapolation value can be represented using the
*     numeric type of the axis width array. If not, then report an
*     error.
*     -  Assign a bad value to all the affected array pixels.
*     -  If OK, then loop to assign the extrapolated value to the array
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
*     9-NOV-1990 (RFWS):
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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER PIX0
      DOUBLE PRECISION WIDTH
      INTEGER LBND
      INTEGER UBND

*  Arguments Given and Returned:
      BYTE AWIDTH( LBND : UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      BYTE W                   ! Width value after type conversion
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBND
      ELSE
         L = LBND
         U = PIX0
      END IF

*  Check if the extrapolation value can be represented using the
*  numeric type of the axis width array. If not, then report an error.
      IF ( ( WIDTH .GT. NUM_BTOD( NUM__MAXB ) ) .OR.
     :     ( WIDTH .LT. NUM_BTOD( NUM__MINB ) ) ) THEN
         CALL MSG_SETI( 'PIXEL', PIX0 )
         CALL ERR_REP( 'NDF1_AWEB_AXVAL',
     :                 'Overflow occurred while calculating ' //
     :                 'an extrapolated value for pixel ^PIXEL of ' //
     :                 'an axis width array.', STATUS )

*  Assign a bad value to all the affected array pixels.
         DO 1 I = L, U
            AWIDTH( I ) = VAL__BADB
 1       CONTINUE

*  If OK, then loop to assign the extrapolated value to the array
*  elements.
      ELSE
         W = NUM_DTOB( WIDTH )
         DO 2 I = L, U
            AWIDTH( I ) = W
2        CONTINUE
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWEB',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AWEUB( UPPER, PIX0, WIDTH, LBND, UBND, AWIDTH,
     :                        STATUS )
*+
*  Name:
*     NDF1_AWEUB

*  Purpose:
*     Assign extrapolated values to a UNSIGNED BYTE axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWEUB( UPPER, PIX0, WIDTH, LBND, UBND, AWIDTH,
*                       STATUS )

*  Description:
*     The routine assigns extrapolated values to a UNSIGNED BYTE axis width
*     array. It is intended for assigning values to those axis width
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is a constant, and is supplied as an
*     argument.

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
*     WIDTH = DOUBLE PRECISION (Given)
*        The extrapolated width value to be assigned.
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     AWIDTH( LBND : UBND ) = BYTE (Given and Returned)
*        The UNSIGNED BYTE axis width array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because the extrapolated value cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Check if the extrapolation value can be represented using the
*     numeric type of the axis width array. If not, then report an
*     error.
*     -  Assign a bad value to all the affected array pixels.
*     -  If OK, then loop to assign the extrapolated value to the array
*     elements.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-NOV-1990 (RFWS):
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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER PIX0
      DOUBLE PRECISION WIDTH
      INTEGER LBND
      INTEGER UBND

*  Arguments Given and Returned:
      BYTE AWIDTH( LBND : UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      BYTE W                   ! Width value after type conversion
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBND
      ELSE
         L = LBND
         U = PIX0
      END IF

*  Check if the extrapolation value can be represented using the
*  numeric type of the axis width array. If not, then report an error.
      IF ( ( WIDTH .GT. NUM_UBTOD( NUM__MAXUB ) ) .OR.
     :     ( WIDTH .LT. NUM_UBTOD( NUM__MINUB ) ) ) THEN
         CALL MSG_SETI( 'PIXEL', PIX0 )
         CALL ERR_REP( 'NDF1_AWEUB_AXVAL',
     :                 'Overflow occurred while calculating ' //
     :                 'an extrapolated value for pixel ^PIXEL of ' //
     :                 'an axis width array.', STATUS )

*  Assign a bad value to all the affected array pixels.
         DO 1 I = L, U
            AWIDTH( I ) = VAL__BADUB
 1       CONTINUE

*  If OK, then loop to assign the extrapolated value to the array
*  elements.
      ELSE
         W = NUM_DTOUB( WIDTH )
         DO 2 I = L, U
            AWIDTH( I ) = W
2        CONTINUE
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWEUB',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AWED( UPPER, PIX0, WIDTH, LBND, UBND, AWIDTH,
     :                        STATUS )
*+
*  Name:
*     NDF1_AWED

*  Purpose:
*     Assign extrapolated values to a DOUBLE PRECISION axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWED( UPPER, PIX0, WIDTH, LBND, UBND, AWIDTH,
*                       STATUS )

*  Description:
*     The routine assigns extrapolated values to a DOUBLE PRECISION axis width
*     array. It is intended for assigning values to those axis width
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is a constant, and is supplied as an
*     argument.

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
*     WIDTH = DOUBLE PRECISION (Given)
*        The extrapolated width value to be assigned.
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     AWIDTH( LBND : UBND ) = DOUBLE PRECISION (Given and Returned)
*        The DOUBLE PRECISION axis width array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because the extrapolated value cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Check if the extrapolation value can be represented using the
*     numeric type of the axis width array. If not, then report an
*     error.
*     -  Assign a bad value to all the affected array pixels.
*     -  If OK, then loop to assign the extrapolated value to the array
*     elements.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-NOV-1990 (RFWS):
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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER PIX0
      DOUBLE PRECISION WIDTH
      INTEGER LBND
      INTEGER UBND

*  Arguments Given and Returned:
      DOUBLE PRECISION AWIDTH( LBND : UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION W                   ! Width value after type conversion
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBND
      ELSE
         L = LBND
         U = PIX0
      END IF

*  Check if the extrapolation value can be represented using the
*  numeric type of the axis width array. If not, then report an error.
      IF ( ( WIDTH .GT. NUM_DTOD( NUM__MAXD ) ) .OR.
     :     ( WIDTH .LT. NUM_DTOD( NUM__MIND ) ) ) THEN
         CALL MSG_SETI( 'PIXEL', PIX0 )
         CALL ERR_REP( 'NDF1_AWED_AXVAL',
     :                 'Overflow occurred while calculating ' //
     :                 'an extrapolated value for pixel ^PIXEL of ' //
     :                 'an axis width array.', STATUS )

*  Assign a bad value to all the affected array pixels.
         DO 1 I = L, U
            AWIDTH( I ) = VAL__BADD
 1       CONTINUE

*  If OK, then loop to assign the extrapolated value to the array
*  elements.
      ELSE
         W = NUM_DTOD( WIDTH )
         DO 2 I = L, U
            AWIDTH( I ) = W
2        CONTINUE
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWED',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AWEI( UPPER, PIX0, WIDTH, LBND, UBND, AWIDTH,
     :                        STATUS )
*+
*  Name:
*     NDF1_AWEI

*  Purpose:
*     Assign extrapolated values to a INTEGER axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWEI( UPPER, PIX0, WIDTH, LBND, UBND, AWIDTH,
*                       STATUS )

*  Description:
*     The routine assigns extrapolated values to a INTEGER axis width
*     array. It is intended for assigning values to those axis width
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is a constant, and is supplied as an
*     argument.

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
*     WIDTH = DOUBLE PRECISION (Given)
*        The extrapolated width value to be assigned.
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     AWIDTH( LBND : UBND ) = INTEGER (Given and Returned)
*        The INTEGER axis width array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because the extrapolated value cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Check if the extrapolation value can be represented using the
*     numeric type of the axis width array. If not, then report an
*     error.
*     -  Assign a bad value to all the affected array pixels.
*     -  If OK, then loop to assign the extrapolated value to the array
*     elements.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-NOV-1990 (RFWS):
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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER PIX0
      DOUBLE PRECISION WIDTH
      INTEGER LBND
      INTEGER UBND

*  Arguments Given and Returned:
      INTEGER AWIDTH( LBND : UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER W                   ! Width value after type conversion
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBND
      ELSE
         L = LBND
         U = PIX0
      END IF

*  Check if the extrapolation value can be represented using the
*  numeric type of the axis width array. If not, then report an error.
      IF ( ( WIDTH .GT. NUM_ITOD( NUM__MAXI ) ) .OR.
     :     ( WIDTH .LT. NUM_ITOD( NUM__MINI ) ) ) THEN
         CALL MSG_SETI( 'PIXEL', PIX0 )
         CALL ERR_REP( 'NDF1_AWEI_AXVAL',
     :                 'Overflow occurred while calculating ' //
     :                 'an extrapolated value for pixel ^PIXEL of ' //
     :                 'an axis width array.', STATUS )

*  Assign a bad value to all the affected array pixels.
         DO 1 I = L, U
            AWIDTH( I ) = VAL__BADI
 1       CONTINUE

*  If OK, then loop to assign the extrapolated value to the array
*  elements.
      ELSE
         W = NUM_DTOI( WIDTH )
         DO 2 I = L, U
            AWIDTH( I ) = W
2        CONTINUE
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWEI',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AWER( UPPER, PIX0, WIDTH, LBND, UBND, AWIDTH,
     :                        STATUS )
*+
*  Name:
*     NDF1_AWER

*  Purpose:
*     Assign extrapolated values to a REAL axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWER( UPPER, PIX0, WIDTH, LBND, UBND, AWIDTH,
*                       STATUS )

*  Description:
*     The routine assigns extrapolated values to a REAL axis width
*     array. It is intended for assigning values to those axis width
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is a constant, and is supplied as an
*     argument.

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
*     WIDTH = DOUBLE PRECISION (Given)
*        The extrapolated width value to be assigned.
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     AWIDTH( LBND : UBND ) = REAL (Given and Returned)
*        The REAL axis width array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because the extrapolated value cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Check if the extrapolation value can be represented using the
*     numeric type of the axis width array. If not, then report an
*     error.
*     -  Assign a bad value to all the affected array pixels.
*     -  If OK, then loop to assign the extrapolated value to the array
*     elements.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-NOV-1990 (RFWS):
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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER PIX0
      DOUBLE PRECISION WIDTH
      INTEGER LBND
      INTEGER UBND

*  Arguments Given and Returned:
      REAL AWIDTH( LBND : UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL W                   ! Width value after type conversion
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBND
      ELSE
         L = LBND
         U = PIX0
      END IF

*  Check if the extrapolation value can be represented using the
*  numeric type of the axis width array. If not, then report an error.
      IF ( ( WIDTH .GT. NUM_RTOD( NUM__MAXR ) ) .OR.
     :     ( WIDTH .LT. NUM_RTOD( NUM__MINR ) ) ) THEN
         CALL MSG_SETI( 'PIXEL', PIX0 )
         CALL ERR_REP( 'NDF1_AWER_AXVAL',
     :                 'Overflow occurred while calculating ' //
     :                 'an extrapolated value for pixel ^PIXEL of ' //
     :                 'an axis width array.', STATUS )

*  Assign a bad value to all the affected array pixels.
         DO 1 I = L, U
            AWIDTH( I ) = VAL__BADR
 1       CONTINUE

*  If OK, then loop to assign the extrapolated value to the array
*  elements.
      ELSE
         W = NUM_DTOR( WIDTH )
         DO 2 I = L, U
            AWIDTH( I ) = W
2        CONTINUE
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWER',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AWEW( UPPER, PIX0, WIDTH, LBND, UBND, AWIDTH,
     :                        STATUS )
*+
*  Name:
*     NDF1_AWEW

*  Purpose:
*     Assign extrapolated values to a WORD axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWEW( UPPER, PIX0, WIDTH, LBND, UBND, AWIDTH,
*                       STATUS )

*  Description:
*     The routine assigns extrapolated values to a WORD axis width
*     array. It is intended for assigning values to those axis width
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is a constant, and is supplied as an
*     argument.

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
*     WIDTH = DOUBLE PRECISION (Given)
*        The extrapolated width value to be assigned.
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     AWIDTH( LBND : UBND ) = INTEGER*2 (Given and Returned)
*        The WORD axis width array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because the extrapolated value cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Check if the extrapolation value can be represented using the
*     numeric type of the axis width array. If not, then report an
*     error.
*     -  Assign a bad value to all the affected array pixels.
*     -  If OK, then loop to assign the extrapolated value to the array
*     elements.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-NOV-1990 (RFWS):
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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER PIX0
      DOUBLE PRECISION WIDTH
      INTEGER LBND
      INTEGER UBND

*  Arguments Given and Returned:
      INTEGER*2 AWIDTH( LBND : UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*2 W                   ! Width value after type conversion
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBND
      ELSE
         L = LBND
         U = PIX0
      END IF

*  Check if the extrapolation value can be represented using the
*  numeric type of the axis width array. If not, then report an error.
      IF ( ( WIDTH .GT. NUM_WTOD( NUM__MAXW ) ) .OR.
     :     ( WIDTH .LT. NUM_WTOD( NUM__MINW ) ) ) THEN
         CALL MSG_SETI( 'PIXEL', PIX0 )
         CALL ERR_REP( 'NDF1_AWEW_AXVAL',
     :                 'Overflow occurred while calculating ' //
     :                 'an extrapolated value for pixel ^PIXEL of ' //
     :                 'an axis width array.', STATUS )

*  Assign a bad value to all the affected array pixels.
         DO 1 I = L, U
            AWIDTH( I ) = VAL__BADW
 1       CONTINUE

*  If OK, then loop to assign the extrapolated value to the array
*  elements.
      ELSE
         W = NUM_DTOW( WIDTH )
         DO 2 I = L, U
            AWIDTH( I ) = W
2        CONTINUE
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWEW',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_AWEUW( UPPER, PIX0, WIDTH, LBND, UBND, AWIDTH,
     :                        STATUS )
*+
*  Name:
*     NDF1_AWEUW

*  Purpose:
*     Assign extrapolated values to a UNSIGNED WORD axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWEUW( UPPER, PIX0, WIDTH, LBND, UBND, AWIDTH,
*                       STATUS )

*  Description:
*     The routine assigns extrapolated values to a UNSIGNED WORD axis width
*     array. It is intended for assigning values to those axis width
*     array elements which are not present in an actual NDF data
*     structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is a constant, and is supplied as an
*     argument.

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
*     WIDTH = DOUBLE PRECISION (Given)
*        The extrapolated width value to be assigned.
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     AWIDTH( LBND : UBND ) = INTEGER*2 (Given and Returned)
*        The UNSIGNED WORD axis width array to be extrapolated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because the extrapolated value cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Determine the lower and upper bounds of the array pixels to
*     have values assigned.
*     -  Check if the extrapolation value can be represented using the
*     numeric type of the axis width array. If not, then report an
*     error.
*     -  Assign a bad value to all the affected array pixels.
*     -  If OK, then loop to assign the extrapolated value to the array
*     elements.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-NOV-1990 (RFWS):
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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER PIX0
      DOUBLE PRECISION WIDTH
      INTEGER LBND
      INTEGER UBND

*  Arguments Given and Returned:
      INTEGER*2 AWIDTH( LBND : UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*2 W                   ! Width value after type conversion
      INTEGER I                  ! Loop counter for array elements
      INTEGER L                  ! Lower pixel index to consider
      INTEGER U                  ! Upper pixel index to consider

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion routines
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the lower and upper bounds of the array pixels to have
*  values assigned.
      IF ( UPPER ) THEN
         L = PIX0
         U = UBND
      ELSE
         L = LBND
         U = PIX0
      END IF

*  Check if the extrapolation value can be represented using the
*  numeric type of the axis width array. If not, then report an error.
      IF ( ( WIDTH .GT. NUM_UWTOD( NUM__MAXUW ) ) .OR.
     :     ( WIDTH .LT. NUM_UWTOD( NUM__MINUW ) ) ) THEN
         CALL MSG_SETI( 'PIXEL', PIX0 )
         CALL ERR_REP( 'NDF1_AWEUW_AXVAL',
     :                 'Overflow occurred while calculating ' //
     :                 'an extrapolated value for pixel ^PIXEL of ' //
     :                 'an axis width array.', STATUS )

*  Assign a bad value to all the affected array pixels.
         DO 1 I = L, U
            AWIDTH( I ) = VAL__BADUW
 1       CONTINUE

*  If OK, then loop to assign the extrapolated value to the array
*  elements.
      ELSE
         W = NUM_DTOUW( WIDTH )
         DO 2 I = L, U
            AWIDTH( I ) = W
2        CONTINUE
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWEUW',
     :                                            STATUS )

      END
