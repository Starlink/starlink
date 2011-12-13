      SUBROUTINE NDF1_V2SB( BAD, EL, ARRAY, DCE, STATUS )
*+
*  Name:
*     NDF1_V2SB

*  Purpose:
*     Convert BYTE variance values to standard deviations.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_V2SB( BAD, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised BYTE array of variance values
*     into standard deviations by taking the square root. It checks for
*     "bad" values if required. If a negative variance value is found,
*     then STATUS is set to NDF__NGVAR, an error is reported, and a
*     "bad" value is assigned to the affected array element - however,
*     the routine continues to process the entire array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     ARRAY( EL ) = BYTE (Given and Returned)
*        On input, an array of BYTE variance values is supplied. On
*        output, they are replaced by the standard deviation values.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in the
*        introduction of new bad values into the array. This will be
*        due to replacement of illegal negative variance values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine works through the array backwards. This is to
*     minimise page faulting on a virtual memory machine, since it will
*     usually be followed by a data-processing loop which passes
*     forwards through the same array.

*  Algorithm:
*     -  Initialise the count of negative variance values and the data
*     conversion error flag.
*     -  If required, loop through the array without checking for bad
*     values.
*     -  If the data type is not unsigned, then check if each value is
*     negative. If so, then increment the negative variance count, set
*     the data conversion error flag, and set a bad value for the
*     affected array element.
*     -  Otherwise, take the square root.
*     -  If required, loop through the array checking for bad values.
*     -  Perform the bad value check on each array element, processing
*     only those which are not bad in the same way as above.
*     -  If negative variance values were encountered, then report an
*     error.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     11-DEC-1989 (RFWS):
*        Original version.
*     20-DEC-1989 (RFWS):
*        Fixed bug in comparing array elements to zero.
*     17-OCT-1990 (RFWS):
*        Added the first offending value to the error message.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER EL

*  Arguments Given and Returned:
      BYTE ARRAY( EL )

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      BYTE ZERO                ! Zero
      PARAMETER ( ZERO = 0  )

*  Local Variables:
      BYTE ERRVAL              ! Value causing last error
      INTEGER I                  ! Loop counter for array elements
      INTEGER NNEG               ! Number of negative variance values

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_B'      ! NUM_ BYTE functions

      INCLUDE 'NUM_DEF_CVT'      ! Declare the functions...
      INCLUDE 'NUM_DEF_B'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the count of negative variance values and the data
*  conversion error flag.
      NNEG = 0
      DCE = .FALSE.

*  If required, loop through the array without checking for bad values.
      IF ( .NOT. BAD ) THEN
         DO 1 I = EL, 1, -1

*  If the data type is not an unsigned quantity, then check if the
*  variance value is negative.
            IF ( ( 'B' .NE. 'UB' ) .AND.
     :           ( 'B' .NE. 'UW' ) .AND.
     :           ( NUM_LTB( ARRAY( I ), ZERO ) ) ) THEN

*  If it is negative, then count it, note a data conversion error and
*  assign a bad value to the array element.
               NNEG = NNEG + 1
               DCE = .TRUE.
               ERRVAL = ARRAY( I )
               ARRAY( I ) = VAL__BADB

*  Otherwise, take the square root.
            ELSE
               ARRAY( I ) = NUM_SQRTB( ARRAY( I ) )
            END IF
1        CONTINUE

*  If required, loop through the array checking for bad values.
      ELSE
         DO 2 I = EL, 1, -1

*  Perform the bad value check.
            IF ( ARRAY( I ) .NE. VAL__BADB ) THEN

*  Check the value is not negative.
               IF ( ( 'B' .NE. 'UB' ) .AND.
     :              ( 'B' .NE. 'UW' ) .AND.
     :              ( NUM_LTB( ARRAY( I ), ZERO ) ) ) THEN
                  NNEG = NNEG + 1
                  DCE = .TRUE.
                  ERRVAL = ARRAY( I )
                  ARRAY( I ) = VAL__BADB

*  Take the square root.
               ELSE
                  ARRAY( I ) = NUM_SQRTB( ARRAY( I ) )
               END IF
            END IF
2        CONTINUE
      END IF

*  If negative variance values were encountered, then report an error.
      IF ( NNEG .NE. 0 ) THEN
         STATUS = NDF__NGVAR
         CALL MSG_SETI( 'NNEG', NNEG )
         CALL MSG_SETR( 'ERRVAL', NUM_BTOR( ERRVAL ) )
         CALL ERR_REP( 'NDF1_V2SB_NEG',
     :   '^NNEG illegal negative variance value(s) encountered ' //
     :   '(first offending value was ^ERRVAL).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_V2SB',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_V2SUB( BAD, EL, ARRAY, DCE, STATUS )
*+
*  Name:
*     NDF1_V2SUB

*  Purpose:
*     Convert UNSIGNED BYTE variance values to standard deviations.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_V2SUB( BAD, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised UNSIGNED BYTE array of variance values
*     into standard deviations by taking the square root. It checks for
*     "bad" values if required. If a negative variance value is found,
*     then STATUS is set to NDF__NGVAR, an error is reported, and a
*     "bad" value is assigned to the affected array element - however,
*     the routine continues to process the entire array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     ARRAY( EL ) = BYTE (Given and Returned)
*        On input, an array of UNSIGNED BYTE variance values is supplied. On
*        output, they are replaced by the standard deviation values.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in the
*        introduction of new bad values into the array. This will be
*        due to replacement of illegal negative variance values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine works through the array backwards. This is to
*     minimise page faulting on a virtual memory machine, since it will
*     usually be followed by a data-processing loop which passes
*     forwards through the same array.

*  Algorithm:
*     -  Initialise the count of negative variance values and the data
*     conversion error flag.
*     -  If required, loop through the array without checking for bad
*     values.
*     -  If the data type is not unsigned, then check if each value is
*     negative. If so, then increment the negative variance count, set
*     the data conversion error flag, and set a bad value for the
*     affected array element.
*     -  Otherwise, take the square root.
*     -  If required, loop through the array checking for bad values.
*     -  Perform the bad value check on each array element, processing
*     only those which are not bad in the same way as above.
*     -  If negative variance values were encountered, then report an
*     error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1989 (RFWS):
*        Original version.
*     20-DEC-1989 (RFWS):
*        Fixed bug in comparing array elements to zero.
*     17-OCT-1990 (RFWS):
*        Added the first offending value to the error message.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER EL

*  Arguments Given and Returned:
      BYTE ARRAY( EL )

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      BYTE ZERO                ! Zero
      PARAMETER ( ZERO = 0  )

*  Local Variables:
      BYTE ERRVAL              ! Value causing last error
      INTEGER I                  ! Loop counter for array elements
      INTEGER NNEG               ! Number of negative variance values

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_UB'      ! NUM_ UNSIGNED BYTE functions

      INCLUDE 'NUM_DEF_CVT'      ! Declare the functions...
      INCLUDE 'NUM_DEF_UB'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the count of negative variance values and the data
*  conversion error flag.
      NNEG = 0
      DCE = .FALSE.

*  If required, loop through the array without checking for bad values.
      IF ( .NOT. BAD ) THEN
         DO 1 I = EL, 1, -1

*  If the data type is not an unsigned quantity, then check if the
*  variance value is negative.
            IF ( ( 'UB' .NE. 'UB' ) .AND.
     :           ( 'UB' .NE. 'UW' ) .AND.
     :           ( NUM_LTUB( ARRAY( I ), ZERO ) ) ) THEN

*  If it is negative, then count it, note a data conversion error and
*  assign a bad value to the array element.
               NNEG = NNEG + 1
               DCE = .TRUE.
               ERRVAL = ARRAY( I )
               ARRAY( I ) = VAL__BADUB

*  Otherwise, take the square root.
            ELSE
               ARRAY( I ) = NUM_SQRTUB( ARRAY( I ) )
            END IF
1        CONTINUE

*  If required, loop through the array checking for bad values.
      ELSE
         DO 2 I = EL, 1, -1

*  Perform the bad value check.
            IF ( ARRAY( I ) .NE. VAL__BADUB ) THEN

*  Check the value is not negative.
               IF ( ( 'UB' .NE. 'UB' ) .AND.
     :              ( 'UB' .NE. 'UW' ) .AND.
     :              ( NUM_LTUB( ARRAY( I ), ZERO ) ) ) THEN
                  NNEG = NNEG + 1
                  DCE = .TRUE.
                  ERRVAL = ARRAY( I )
                  ARRAY( I ) = VAL__BADUB

*  Take the square root.
               ELSE
                  ARRAY( I ) = NUM_SQRTUB( ARRAY( I ) )
               END IF
            END IF
2        CONTINUE
      END IF

*  If negative variance values were encountered, then report an error.
      IF ( NNEG .NE. 0 ) THEN
         STATUS = NDF__NGVAR
         CALL MSG_SETI( 'NNEG', NNEG )
         CALL MSG_SETR( 'ERRVAL', NUM_UBTOR( ERRVAL ) )
         CALL ERR_REP( 'NDF1_V2SUB_NEG',
     :   '^NNEG illegal negative variance value(s) encountered ' //
     :   '(first offending value was ^ERRVAL).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_V2SUB',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_V2SD( BAD, EL, ARRAY, DCE, STATUS )
*+
*  Name:
*     NDF1_V2SD

*  Purpose:
*     Convert DOUBLE PRECISION variance values to standard deviations.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_V2SD( BAD, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised DOUBLE PRECISION array of variance values
*     into standard deviations by taking the square root. It checks for
*     "bad" values if required. If a negative variance value is found,
*     then STATUS is set to NDF__NGVAR, an error is reported, and a
*     "bad" value is assigned to the affected array element - however,
*     the routine continues to process the entire array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     ARRAY( EL ) = DOUBLE PRECISION (Given and Returned)
*        On input, an array of DOUBLE PRECISION variance values is supplied. On
*        output, they are replaced by the standard deviation values.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in the
*        introduction of new bad values into the array. This will be
*        due to replacement of illegal negative variance values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine works through the array backwards. This is to
*     minimise page faulting on a virtual memory machine, since it will
*     usually be followed by a data-processing loop which passes
*     forwards through the same array.

*  Algorithm:
*     -  Initialise the count of negative variance values and the data
*     conversion error flag.
*     -  If required, loop through the array without checking for bad
*     values.
*     -  If the data type is not unsigned, then check if each value is
*     negative. If so, then increment the negative variance count, set
*     the data conversion error flag, and set a bad value for the
*     affected array element.
*     -  Otherwise, take the square root.
*     -  If required, loop through the array checking for bad values.
*     -  Perform the bad value check on each array element, processing
*     only those which are not bad in the same way as above.
*     -  If negative variance values were encountered, then report an
*     error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1989 (RFWS):
*        Original version.
*     20-DEC-1989 (RFWS):
*        Fixed bug in comparing array elements to zero.
*     17-OCT-1990 (RFWS):
*        Added the first offending value to the error message.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER EL

*  Arguments Given and Returned:
      DOUBLE PRECISION ARRAY( EL )

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION ZERO                ! Zero
      PARAMETER ( ZERO = 0.0D0 )

*  Local Variables:
      DOUBLE PRECISION ERRVAL              ! Value causing last error
      INTEGER I                  ! Loop counter for array elements
      INTEGER NNEG               ! Number of negative variance values

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_D'      ! NUM_ DOUBLE PRECISION functions

      INCLUDE 'NUM_DEF_CVT'      ! Declare the functions...
      INCLUDE 'NUM_DEF_D'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the count of negative variance values and the data
*  conversion error flag.
      NNEG = 0
      DCE = .FALSE.

*  If required, loop through the array without checking for bad values.
      IF ( .NOT. BAD ) THEN
         DO 1 I = EL, 1, -1

*  If the data type is not an unsigned quantity, then check if the
*  variance value is negative.
            IF ( ( 'D' .NE. 'UB' ) .AND.
     :           ( 'D' .NE. 'UW' ) .AND.
     :           ( NUM_LTD( ARRAY( I ), ZERO ) ) ) THEN

*  If it is negative, then count it, note a data conversion error and
*  assign a bad value to the array element.
               NNEG = NNEG + 1
               DCE = .TRUE.
               ERRVAL = ARRAY( I )
               ARRAY( I ) = VAL__BADD

*  Otherwise, take the square root.
            ELSE
               ARRAY( I ) = NUM_SQRTD( ARRAY( I ) )
            END IF
1        CONTINUE

*  If required, loop through the array checking for bad values.
      ELSE
         DO 2 I = EL, 1, -1

*  Perform the bad value check.
            IF ( ARRAY( I ) .NE. VAL__BADD ) THEN

*  Check the value is not negative.
               IF ( ( 'D' .NE. 'UB' ) .AND.
     :              ( 'D' .NE. 'UW' ) .AND.
     :              ( NUM_LTD( ARRAY( I ), ZERO ) ) ) THEN
                  NNEG = NNEG + 1
                  DCE = .TRUE.
                  ERRVAL = ARRAY( I )
                  ARRAY( I ) = VAL__BADD

*  Take the square root.
               ELSE
                  ARRAY( I ) = NUM_SQRTD( ARRAY( I ) )
               END IF
            END IF
2        CONTINUE
      END IF

*  If negative variance values were encountered, then report an error.
      IF ( NNEG .NE. 0 ) THEN
         STATUS = NDF__NGVAR
         CALL MSG_SETI( 'NNEG', NNEG )
         CALL MSG_SETR( 'ERRVAL', NUM_DTOR( ERRVAL ) )
         CALL ERR_REP( 'NDF1_V2SD_NEG',
     :   '^NNEG illegal negative variance value(s) encountered ' //
     :   '(first offending value was ^ERRVAL).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_V2SD',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_V2SI( BAD, EL, ARRAY, DCE, STATUS )
*+
*  Name:
*     NDF1_V2SI

*  Purpose:
*     Convert INTEGER variance values to standard deviations.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_V2SI( BAD, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised INTEGER array of variance values
*     into standard deviations by taking the square root. It checks for
*     "bad" values if required. If a negative variance value is found,
*     then STATUS is set to NDF__NGVAR, an error is reported, and a
*     "bad" value is assigned to the affected array element - however,
*     the routine continues to process the entire array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     ARRAY( EL ) = INTEGER (Given and Returned)
*        On input, an array of INTEGER variance values is supplied. On
*        output, they are replaced by the standard deviation values.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in the
*        introduction of new bad values into the array. This will be
*        due to replacement of illegal negative variance values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine works through the array backwards. This is to
*     minimise page faulting on a virtual memory machine, since it will
*     usually be followed by a data-processing loop which passes
*     forwards through the same array.

*  Algorithm:
*     -  Initialise the count of negative variance values and the data
*     conversion error flag.
*     -  If required, loop through the array without checking for bad
*     values.
*     -  If the data type is not unsigned, then check if each value is
*     negative. If so, then increment the negative variance count, set
*     the data conversion error flag, and set a bad value for the
*     affected array element.
*     -  Otherwise, take the square root.
*     -  If required, loop through the array checking for bad values.
*     -  Perform the bad value check on each array element, processing
*     only those which are not bad in the same way as above.
*     -  If negative variance values were encountered, then report an
*     error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1989 (RFWS):
*        Original version.
*     20-DEC-1989 (RFWS):
*        Fixed bug in comparing array elements to zero.
*     17-OCT-1990 (RFWS):
*        Added the first offending value to the error message.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER EL

*  Arguments Given and Returned:
      INTEGER ARRAY( EL )

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER ZERO                ! Zero
      PARAMETER ( ZERO = 0  )

*  Local Variables:
      INTEGER ERRVAL              ! Value causing last error
      INTEGER I                  ! Loop counter for array elements
      INTEGER NNEG               ! Number of negative variance values

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_I'      ! NUM_ INTEGER functions

      INCLUDE 'NUM_DEF_CVT'      ! Declare the functions...
      INCLUDE 'NUM_DEF_I'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the count of negative variance values and the data
*  conversion error flag.
      NNEG = 0
      DCE = .FALSE.

*  If required, loop through the array without checking for bad values.
      IF ( .NOT. BAD ) THEN
         DO 1 I = EL, 1, -1

*  If the data type is not an unsigned quantity, then check if the
*  variance value is negative.
            IF ( ( 'I' .NE. 'UB' ) .AND.
     :           ( 'I' .NE. 'UW' ) .AND.
     :           ( NUM_LTI( ARRAY( I ), ZERO ) ) ) THEN

*  If it is negative, then count it, note a data conversion error and
*  assign a bad value to the array element.
               NNEG = NNEG + 1
               DCE = .TRUE.
               ERRVAL = ARRAY( I )
               ARRAY( I ) = VAL__BADI

*  Otherwise, take the square root.
            ELSE
               ARRAY( I ) = NUM_SQRTI( ARRAY( I ) )
            END IF
1        CONTINUE

*  If required, loop through the array checking for bad values.
      ELSE
         DO 2 I = EL, 1, -1

*  Perform the bad value check.
            IF ( ARRAY( I ) .NE. VAL__BADI ) THEN

*  Check the value is not negative.
               IF ( ( 'I' .NE. 'UB' ) .AND.
     :              ( 'I' .NE. 'UW' ) .AND.
     :              ( NUM_LTI( ARRAY( I ), ZERO ) ) ) THEN
                  NNEG = NNEG + 1
                  DCE = .TRUE.
                  ERRVAL = ARRAY( I )
                  ARRAY( I ) = VAL__BADI

*  Take the square root.
               ELSE
                  ARRAY( I ) = NUM_SQRTI( ARRAY( I ) )
               END IF
            END IF
2        CONTINUE
      END IF

*  If negative variance values were encountered, then report an error.
      IF ( NNEG .NE. 0 ) THEN
         STATUS = NDF__NGVAR
         CALL MSG_SETI( 'NNEG', NNEG )
         CALL MSG_SETR( 'ERRVAL', NUM_ITOR( ERRVAL ) )
         CALL ERR_REP( 'NDF1_V2SI_NEG',
     :   '^NNEG illegal negative variance value(s) encountered ' //
     :   '(first offending value was ^ERRVAL).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_V2SI',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_V2SR( BAD, EL, ARRAY, DCE, STATUS )
*+
*  Name:
*     NDF1_V2SR

*  Purpose:
*     Convert REAL variance values to standard deviations.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_V2SR( BAD, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised REAL array of variance values
*     into standard deviations by taking the square root. It checks for
*     "bad" values if required. If a negative variance value is found,
*     then STATUS is set to NDF__NGVAR, an error is reported, and a
*     "bad" value is assigned to the affected array element - however,
*     the routine continues to process the entire array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     ARRAY( EL ) = REAL (Given and Returned)
*        On input, an array of REAL variance values is supplied. On
*        output, they are replaced by the standard deviation values.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in the
*        introduction of new bad values into the array. This will be
*        due to replacement of illegal negative variance values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine works through the array backwards. This is to
*     minimise page faulting on a virtual memory machine, since it will
*     usually be followed by a data-processing loop which passes
*     forwards through the same array.

*  Algorithm:
*     -  Initialise the count of negative variance values and the data
*     conversion error flag.
*     -  If required, loop through the array without checking for bad
*     values.
*     -  If the data type is not unsigned, then check if each value is
*     negative. If so, then increment the negative variance count, set
*     the data conversion error flag, and set a bad value for the
*     affected array element.
*     -  Otherwise, take the square root.
*     -  If required, loop through the array checking for bad values.
*     -  Perform the bad value check on each array element, processing
*     only those which are not bad in the same way as above.
*     -  If negative variance values were encountered, then report an
*     error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1989 (RFWS):
*        Original version.
*     20-DEC-1989 (RFWS):
*        Fixed bug in comparing array elements to zero.
*     17-OCT-1990 (RFWS):
*        Added the first offending value to the error message.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER EL

*  Arguments Given and Returned:
      REAL ARRAY( EL )

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL ZERO                ! Zero
      PARAMETER ( ZERO = 0.0E0 )

*  Local Variables:
      REAL ERRVAL              ! Value causing last error
      INTEGER I                  ! Loop counter for array elements
      INTEGER NNEG               ! Number of negative variance values

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_R'      ! NUM_ REAL functions

      INCLUDE 'NUM_DEF_CVT'      ! Declare the functions...
      INCLUDE 'NUM_DEF_R'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the count of negative variance values and the data
*  conversion error flag.
      NNEG = 0
      DCE = .FALSE.

*  If required, loop through the array without checking for bad values.
      IF ( .NOT. BAD ) THEN
         DO 1 I = EL, 1, -1

*  If the data type is not an unsigned quantity, then check if the
*  variance value is negative.
            IF ( ( 'R' .NE. 'UB' ) .AND.
     :           ( 'R' .NE. 'UW' ) .AND.
     :           ( NUM_LTR( ARRAY( I ), ZERO ) ) ) THEN

*  If it is negative, then count it, note a data conversion error and
*  assign a bad value to the array element.
               NNEG = NNEG + 1
               DCE = .TRUE.
               ERRVAL = ARRAY( I )
               ARRAY( I ) = VAL__BADR

*  Otherwise, take the square root.
            ELSE
               ARRAY( I ) = NUM_SQRTR( ARRAY( I ) )
            END IF
1        CONTINUE

*  If required, loop through the array checking for bad values.
      ELSE
         DO 2 I = EL, 1, -1

*  Perform the bad value check.
            IF ( ARRAY( I ) .NE. VAL__BADR ) THEN

*  Check the value is not negative.
               IF ( ( 'R' .NE. 'UB' ) .AND.
     :              ( 'R' .NE. 'UW' ) .AND.
     :              ( NUM_LTR( ARRAY( I ), ZERO ) ) ) THEN
                  NNEG = NNEG + 1
                  DCE = .TRUE.
                  ERRVAL = ARRAY( I )
                  ARRAY( I ) = VAL__BADR

*  Take the square root.
               ELSE
                  ARRAY( I ) = NUM_SQRTR( ARRAY( I ) )
               END IF
            END IF
2        CONTINUE
      END IF

*  If negative variance values were encountered, then report an error.
      IF ( NNEG .NE. 0 ) THEN
         STATUS = NDF__NGVAR
         CALL MSG_SETI( 'NNEG', NNEG )
         CALL MSG_SETR( 'ERRVAL', NUM_RTOR( ERRVAL ) )
         CALL ERR_REP( 'NDF1_V2SR_NEG',
     :   '^NNEG illegal negative variance value(s) encountered ' //
     :   '(first offending value was ^ERRVAL).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_V2SR',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_V2SW( BAD, EL, ARRAY, DCE, STATUS )
*+
*  Name:
*     NDF1_V2SW

*  Purpose:
*     Convert WORD variance values to standard deviations.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_V2SW( BAD, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised WORD array of variance values
*     into standard deviations by taking the square root. It checks for
*     "bad" values if required. If a negative variance value is found,
*     then STATUS is set to NDF__NGVAR, an error is reported, and a
*     "bad" value is assigned to the affected array element - however,
*     the routine continues to process the entire array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     ARRAY( EL ) = INTEGER*2 (Given and Returned)
*        On input, an array of WORD variance values is supplied. On
*        output, they are replaced by the standard deviation values.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in the
*        introduction of new bad values into the array. This will be
*        due to replacement of illegal negative variance values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine works through the array backwards. This is to
*     minimise page faulting on a virtual memory machine, since it will
*     usually be followed by a data-processing loop which passes
*     forwards through the same array.

*  Algorithm:
*     -  Initialise the count of negative variance values and the data
*     conversion error flag.
*     -  If required, loop through the array without checking for bad
*     values.
*     -  If the data type is not unsigned, then check if each value is
*     negative. If so, then increment the negative variance count, set
*     the data conversion error flag, and set a bad value for the
*     affected array element.
*     -  Otherwise, take the square root.
*     -  If required, loop through the array checking for bad values.
*     -  Perform the bad value check on each array element, processing
*     only those which are not bad in the same way as above.
*     -  If negative variance values were encountered, then report an
*     error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1989 (RFWS):
*        Original version.
*     20-DEC-1989 (RFWS):
*        Fixed bug in comparing array elements to zero.
*     17-OCT-1990 (RFWS):
*        Added the first offending value to the error message.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER EL

*  Arguments Given and Returned:
      INTEGER*2 ARRAY( EL )

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER*2 ZERO                ! Zero
      PARAMETER ( ZERO = 0  )

*  Local Variables:
      INTEGER*2 ERRVAL              ! Value causing last error
      INTEGER I                  ! Loop counter for array elements
      INTEGER NNEG               ! Number of negative variance values

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_W'      ! NUM_ WORD functions

      INCLUDE 'NUM_DEF_CVT'      ! Declare the functions...
      INCLUDE 'NUM_DEF_W'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the count of negative variance values and the data
*  conversion error flag.
      NNEG = 0
      DCE = .FALSE.

*  If required, loop through the array without checking for bad values.
      IF ( .NOT. BAD ) THEN
         DO 1 I = EL, 1, -1

*  If the data type is not an unsigned quantity, then check if the
*  variance value is negative.
            IF ( ( 'W' .NE. 'UB' ) .AND.
     :           ( 'W' .NE. 'UW' ) .AND.
     :           ( NUM_LTW( ARRAY( I ), ZERO ) ) ) THEN

*  If it is negative, then count it, note a data conversion error and
*  assign a bad value to the array element.
               NNEG = NNEG + 1
               DCE = .TRUE.
               ERRVAL = ARRAY( I )
               ARRAY( I ) = VAL__BADW

*  Otherwise, take the square root.
            ELSE
               ARRAY( I ) = NUM_SQRTW( ARRAY( I ) )
            END IF
1        CONTINUE

*  If required, loop through the array checking for bad values.
      ELSE
         DO 2 I = EL, 1, -1

*  Perform the bad value check.
            IF ( ARRAY( I ) .NE. VAL__BADW ) THEN

*  Check the value is not negative.
               IF ( ( 'W' .NE. 'UB' ) .AND.
     :              ( 'W' .NE. 'UW' ) .AND.
     :              ( NUM_LTW( ARRAY( I ), ZERO ) ) ) THEN
                  NNEG = NNEG + 1
                  DCE = .TRUE.
                  ERRVAL = ARRAY( I )
                  ARRAY( I ) = VAL__BADW

*  Take the square root.
               ELSE
                  ARRAY( I ) = NUM_SQRTW( ARRAY( I ) )
               END IF
            END IF
2        CONTINUE
      END IF

*  If negative variance values were encountered, then report an error.
      IF ( NNEG .NE. 0 ) THEN
         STATUS = NDF__NGVAR
         CALL MSG_SETI( 'NNEG', NNEG )
         CALL MSG_SETR( 'ERRVAL', NUM_WTOR( ERRVAL ) )
         CALL ERR_REP( 'NDF1_V2SW_NEG',
     :   '^NNEG illegal negative variance value(s) encountered ' //
     :   '(first offending value was ^ERRVAL).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_V2SW',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_V2SUW( BAD, EL, ARRAY, DCE, STATUS )
*+
*  Name:
*     NDF1_V2SUW

*  Purpose:
*     Convert UNSIGNED WORD variance values to standard deviations.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_V2SUW( BAD, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised UNSIGNED WORD array of variance values
*     into standard deviations by taking the square root. It checks for
*     "bad" values if required. If a negative variance value is found,
*     then STATUS is set to NDF__NGVAR, an error is reported, and a
*     "bad" value is assigned to the affected array element - however,
*     the routine continues to process the entire array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     ARRAY( EL ) = INTEGER*2 (Given and Returned)
*        On input, an array of UNSIGNED WORD variance values is supplied. On
*        output, they are replaced by the standard deviation values.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in the
*        introduction of new bad values into the array. This will be
*        due to replacement of illegal negative variance values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine works through the array backwards. This is to
*     minimise page faulting on a virtual memory machine, since it will
*     usually be followed by a data-processing loop which passes
*     forwards through the same array.

*  Algorithm:
*     -  Initialise the count of negative variance values and the data
*     conversion error flag.
*     -  If required, loop through the array without checking for bad
*     values.
*     -  If the data type is not unsigned, then check if each value is
*     negative. If so, then increment the negative variance count, set
*     the data conversion error flag, and set a bad value for the
*     affected array element.
*     -  Otherwise, take the square root.
*     -  If required, loop through the array checking for bad values.
*     -  Perform the bad value check on each array element, processing
*     only those which are not bad in the same way as above.
*     -  If negative variance values were encountered, then report an
*     error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1989 (RFWS):
*        Original version.
*     20-DEC-1989 (RFWS):
*        Fixed bug in comparing array elements to zero.
*     17-OCT-1990 (RFWS):
*        Added the first offending value to the error message.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public_constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER EL

*  Arguments Given and Returned:
      INTEGER*2 ARRAY( EL )

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER*2 ZERO                ! Zero
      PARAMETER ( ZERO = 0  )

*  Local Variables:
      INTEGER*2 ERRVAL              ! Value causing last error
      INTEGER I                  ! Loop counter for array elements
      INTEGER NNEG               ! Number of negative variance values

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_UW'      ! NUM_ UNSIGNED WORD functions

      INCLUDE 'NUM_DEF_CVT'      ! Declare the functions...
      INCLUDE 'NUM_DEF_UW'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the count of negative variance values and the data
*  conversion error flag.
      NNEG = 0
      DCE = .FALSE.

*  If required, loop through the array without checking for bad values.
      IF ( .NOT. BAD ) THEN
         DO 1 I = EL, 1, -1

*  If the data type is not an unsigned quantity, then check if the
*  variance value is negative.
            IF ( ( 'UW' .NE. 'UB' ) .AND.
     :           ( 'UW' .NE. 'UW' ) .AND.
     :           ( NUM_LTUW( ARRAY( I ), ZERO ) ) ) THEN

*  If it is negative, then count it, note a data conversion error and
*  assign a bad value to the array element.
               NNEG = NNEG + 1
               DCE = .TRUE.
               ERRVAL = ARRAY( I )
               ARRAY( I ) = VAL__BADUW

*  Otherwise, take the square root.
            ELSE
               ARRAY( I ) = NUM_SQRTUW( ARRAY( I ) )
            END IF
1        CONTINUE

*  If required, loop through the array checking for bad values.
      ELSE
         DO 2 I = EL, 1, -1

*  Perform the bad value check.
            IF ( ARRAY( I ) .NE. VAL__BADUW ) THEN

*  Check the value is not negative.
               IF ( ( 'UW' .NE. 'UB' ) .AND.
     :              ( 'UW' .NE. 'UW' ) .AND.
     :              ( NUM_LTUW( ARRAY( I ), ZERO ) ) ) THEN
                  NNEG = NNEG + 1
                  DCE = .TRUE.
                  ERRVAL = ARRAY( I )
                  ARRAY( I ) = VAL__BADUW

*  Take the square root.
               ELSE
                  ARRAY( I ) = NUM_SQRTUW( ARRAY( I ) )
               END IF
            END IF
2        CONTINUE
      END IF

*  If negative variance values were encountered, then report an error.
      IF ( NNEG .NE. 0 ) THEN
         STATUS = NDF__NGVAR
         CALL MSG_SETI( 'NNEG', NNEG )
         CALL MSG_SETR( 'ERRVAL', NUM_UWTOR( ERRVAL ) )
         CALL ERR_REP( 'NDF1_V2SUW_NEG',
     :   '^NNEG illegal negative variance value(s) encountered ' //
     :   '(first offending value was ^ERRVAL).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_V2SUW',
     :                                            STATUS )

      END
