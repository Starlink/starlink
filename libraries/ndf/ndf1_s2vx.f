      SUBROUTINE NDF1_S2VB( BAD, EL, ARRAY, DCE, STATUS )
*+
*  Name:
*     NDF1_S2VB

*  Purpose:
*     Convert BYTE standard deviation values to variances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_S2VB( BAD, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised BYTE array of standard
*     deviation values into variances by squaring them. It checks for
*     "bad" values if required and handles numerical overflow errors,
*     replacing them with bad values. If a negative standard deviation
*     is found, then STATUS is set to NDF__NGSTD, an error is reported
*     and a bad value is assigned to the affected array element -
*     however, the routine continues to process the entire array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     ARRAY( EL ) = BYTE (Given and Returned)
*        On input, an array of BYTE standard deviation values is
*        supplied. On output, they are replaced by the variance values.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in new bad
*        values being produced. This may result either from numerical
*        overflow (which the routine handles) or from replacement of
*        illegal negative standard deviations.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine works through the array backwards. This is to
*     minimise page faulting on a virtual memory machine, since it will
*     usually be followed by a data-processing loop which passes
*     forwards through the same array.

*  Copyright:
*     Copyright (C) 1989, 1990, 1993 Science & Engineering Research Council.
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
*     17-OCT-1990 (RFWS):
*        Added the offending value to the error message.
*     8-SEP-1993 (RFWS):
*        Re-written to avoid the use of traps to detect overflow.
*     30-SEP-1993 (RFWS):
*        Eliminated arithmetic operations which may not compile
*        correctly for all integer data types. Use NUM_ functions
*        instead.
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
      BYTE ONE                 ! One
      PARAMETER ( ONE = 1  )
      BYTE TWO                 ! Two
      PARAMETER ( TWO = 2  )

*  Local Variables:
      BYTE A                   ! Number to which 1 can't be added
      BYTE B                   ! Number which can be added to A
      BYTE BASE                ! Base of floating point numbers
      BYTE ERRVAL              ! Value causing last error
      BYTE HI                  ! Maximum value that can be squared
      BYTE NEW                 ! New estimate of square root
      BYTE TEST                ! Test value for finding number base
      INTEGER I                  ! Loop counter for array elements
      INTEGER ITER               ! Loop counter for iterations
      INTEGER NNEG               ! Number negative standard deviations
      LOGICAL FIRST              ! First invocation of this routine?

      SAVE FIRST
      SAVE HI

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_B'      ! NUM_ BYTE functions

      INCLUDE 'NUM_DEF_CVT'      ! Define the functions...
      INCLUDE 'NUM_DEF_B'

*  Local Data:
      DATA FIRST / .TRUE. /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If this is the first invocation of this routine, we must first
*  calculate the limit HI, which is the largest input value which can be
*  squared without causing overflow.
      IF ( FIRST ) THEN

*  If we are dealing with a floating point type, then obtain an initial
*  estimate of HI from the square root of the largest positive number.
         IF ( ( 'B' .EQ. 'R' ) .OR. ( 'B' .EQ. 'D' ) ) THEN
            HI = NUM_SQRTB( NUM__MAXB )

*  Since this may not always achieve the full machine precision, we now
*  apply up to 10 iterations to refine the result, using Newton's
*  formula to estimate each new value. Note we cannot use arithmetic
*  operators directly, as the code must also compile correctly for
*  integer data types which may not support arithmetic (even though it
*  will never be executed in such cases).
            DO 1 ITER = 1, 10
               NEW = NUM_DIVB(
     :               NUM_ADDB( HI, NUM_DIVB( NUM__MAXB, HI ) ),
     :               TWO )

*  Quit looping if the result is no longer changing.
               IF ( NUM_EQB( NEW, HI ) ) GO TO 2
               HI = NEW
 1          CONTINUE
 2          CONTINUE

*  The current value of HI may still be too large (i.e. may overflow if
*  squared) if the result has been rounded upwards. To test for this,
*  we must square a known fraction of HI and check that it does not
*  exceed the expected value. For this to work, however, the fraction
*  chosen must be derived using the exponent base for the floating
*  point number representation in use (so that the mantissa is not
*  changed). Thus we must first determine this base.

*  Starting with unity, repeatedly double until a value A is found to
*  which 1 cannot be added without losing precision.
            A = ONE
 3          CONTINUE
            A = NUM_ADDB( A, A )
            TEST = NUM_ADDB( A, ONE )
            IF ( NUM_EQB( NUM_SUBB( TEST, A ), ONE ) ) GO TO 3

*  Again starting with unity, repeatedly double until a value B is
*  found that can be added to A to give a result that differs from A.
            B = ONE
 4          CONTINUE
            B = NUM_ADDB( B, B )
            TEST = NUM_ADDB( A, B )
            IF ( NUM_EQB( TEST, A ) ) GO TO 4

*  The number base is now given by the difference between A and the
*  next representable number that differs from it (TEST).
            BASE = NUM_SUBB( TEST, A )

*  Now test if HI is too large, using an appropriate fraction of it to
*  prevent overflow occurring. If it is too large, we decrement it to
*  obtain the next smaller representable value.
            IF ( NUM_GTB( NUM_MULB( NUM_DIVB( HI, BASE ),
     :                                  NUM_DIVB( HI, BASE ) ),
     :                      NUM_DIVB( NUM__MAXB,
     :                                  NUM_MULB( BASE, BASE ) ) ) )
     :         HI = NUM_SUBB( HI, NUM_MULB( HI, VAL__EPSB ) )

*  If we are dealing with an integer type, then find the square root of
*  the largest positive number using double precision arithmetic. Round
*  the result down to an integer and convert back to the required type.
         ELSE
            HI = NUM_ITOB( INT( SQRT( NUM_BTOD( NUM__MAXB ) ) ) )
         END IF

*  Note we do not need to repeat these calculations.
         FIRST = .FALSE.
      END IF

*  Initialise the count of negative standard deviations and the data
*  conversion error flag.
      NNEG = 0
      DCE = .FALSE.

*  If required, loop through the array without checking for bad values.
      IF ( .NOT. BAD ) THEN
         DO 5 I = EL, 1, -1

*  If the data values are not unsigned, then check if the standard
*  deviation is negative.
            IF ( ( 'B' .NE. 'UB' ) .AND.
     :           ( 'B' .NE. 'UW' ) .AND.
     :           NUM_LTB( ARRAY( I ), ZERO ) ) THEN

*  If so, then increment the negative standard deviation count, note a
*  data conversion error and assign a bad value to the affected array
*  element.
               NNEG = NNEG + 1
               DCE = .TRUE.
               ERRVAL = ARRAY( I )
               ARRAY( I ) = VAL__BADB

*  Otherwise, check that the value will not overflow when squared. If
*  not, then square it.
            ELSE IF ( NUM_LEB( ARRAY( I ), HI ) ) THEN
               ARRAY( I ) = NUM_MULB( ARRAY( I ), ARRAY( I ) )

*  If it would overflow, then note a data conversion error and assign a
*  bad result.
            ELSE
               DCE = .TRUE.
               ARRAY( I ) = VAL__BADB
            END IF
 5       CONTINUE

*  If required, loop through the array checking for bad values.
      ELSE
         DO 6 I = EL, 1, -1

*  Perform the bad value check.
            IF ( ARRAY( I ) .NE. VAL__BADB ) THEN

*  Check the value is not negative.
               IF ( ( 'B' .NE. 'UB' ) .AND.
     :              ( 'B' .NE. 'UW' ) .AND.
     :              NUM_LTB( ARRAY( I ), ZERO ) ) THEN
                  NNEG = NNEG + 1
                  DCE = .TRUE.
                  ERRVAL = ARRAY( I )
                  ARRAY( I ) = VAL__BADB

*  Check for overflow and square the standard deviation.
               ELSE IF ( NUM_LEB( ARRAY( I ), HI ) ) THEN
                  ARRAY( I ) = NUM_MULB( ARRAY( I ), ARRAY( I ) )

*  Deal with overflows.
               ELSE
                  DCE = .TRUE.
                  ARRAY( I ) = VAL__BADB
               END IF
            END IF
 6       CONTINUE
      END IF

*  If negative standard deviation values were encountered, then report
*  an error.
      IF ( NNEG .NE. 0 ) THEN
         STATUS = NDF__NGSTD
         CALL MSG_SETI( 'NNEG', NNEG )
         CALL MSG_SETR( 'ERRVAL', NUM_BTOR( ERRVAL ) )
         CALL ERR_REP( 'NDF1_S2VB_NEG',
     :   '^NNEG illegal negative standard deviation value(s) ' //
     :   'encountered; first offending value was ^ERRVAL ' //
     :   '(possible programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_S2VB',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_S2VUB( BAD, EL, ARRAY, DCE, STATUS )
*+
*  Name:
*     NDF1_S2VUB

*  Purpose:
*     Convert UNSIGNED BYTE standard deviation values to variances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_S2VUB( BAD, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised UNSIGNED BYTE array of standard
*     deviation values into variances by squaring them. It checks for
*     "bad" values if required and handles numerical overflow errors,
*     replacing them with bad values. If a negative standard deviation
*     is found, then STATUS is set to NDF__NGSTD, an error is reported
*     and a bad value is assigned to the affected array element -
*     however, the routine continues to process the entire array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     ARRAY( EL ) = BYTE (Given and Returned)
*        On input, an array of UNSIGNED BYTE standard deviation values is
*        supplied. On output, they are replaced by the variance values.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in new bad
*        values being produced. This may result either from numerical
*        overflow (which the routine handles) or from replacement of
*        illegal negative standard deviations.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine works through the array backwards. This is to
*     minimise page faulting on a virtual memory machine, since it will
*     usually be followed by a data-processing loop which passes
*     forwards through the same array.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1989 (RFWS):
*        Original version.
*     17-OCT-1990 (RFWS):
*        Added the offending value to the error message.
*     8-SEP-1993 (RFWS):
*        Re-written to avoid the use of traps to detect overflow.
*     30-SEP-1993 (RFWS):
*        Eliminated arithmetic operations which may not compile
*        correctly for all integer data types. Use NUM_ functions
*        instead.
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
      BYTE ONE                 ! One
      PARAMETER ( ONE = 1  )
      BYTE TWO                 ! Two
      PARAMETER ( TWO = 2  )

*  Local Variables:
      BYTE A                   ! Number to which 1 can't be added
      BYTE B                   ! Number which can be added to A
      BYTE BASE                ! Base of floating point numbers
      BYTE ERRVAL              ! Value causing last error
      BYTE HI                  ! Maximum value that can be squared
      BYTE NEW                 ! New estimate of square root
      BYTE TEST                ! Test value for finding number base
      INTEGER I                  ! Loop counter for array elements
      INTEGER ITER               ! Loop counter for iterations
      INTEGER NNEG               ! Number negative standard deviations
      LOGICAL FIRST              ! First invocation of this routine?

      SAVE FIRST
      SAVE HI

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_UB'      ! NUM_ UNSIGNED BYTE functions

      INCLUDE 'NUM_DEF_CVT'      ! Define the functions...
      INCLUDE 'NUM_DEF_UB'

*  Local Data:
      DATA FIRST / .TRUE. /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If this is the first invocation of this routine, we must first
*  calculate the limit HI, which is the largest input value which can be
*  squared without causing overflow.
      IF ( FIRST ) THEN

*  If we are dealing with a floating point type, then obtain an initial
*  estimate of HI from the square root of the largest positive number.
         IF ( ( 'UB' .EQ. 'R' ) .OR. ( 'UB' .EQ. 'D' ) ) THEN
            HI = NUM_SQRTUB( NUM__MAXUB )

*  Since this may not always achieve the full machine precision, we now
*  apply up to 10 iterations to refine the result, using Newton's
*  formula to estimate each new value. Note we cannot use arithmetic
*  operators directly, as the code must also compile correctly for
*  integer data types which may not support arithmetic (even though it
*  will never be executed in such cases).
            DO 1 ITER = 1, 10
               NEW = NUM_DIVUB(
     :               NUM_ADDUB( HI, NUM_DIVUB( NUM__MAXUB, HI ) ),
     :               TWO )

*  Quit looping if the result is no longer changing.
               IF ( NUM_EQUB( NEW, HI ) ) GO TO 2
               HI = NEW
 1          CONTINUE
 2          CONTINUE

*  The current value of HI may still be too large (i.e. may overflow if
*  squared) if the result has been rounded upwards. To test for this,
*  we must square a known fraction of HI and check that it does not
*  exceed the expected value. For this to work, however, the fraction
*  chosen must be derived using the exponent base for the floating
*  point number representation in use (so that the mantissa is not
*  changed). Thus we must first determine this base.

*  Starting with unity, repeatedly double until a value A is found to
*  which 1 cannot be added without losing precision.
            A = ONE
 3          CONTINUE
            A = NUM_ADDUB( A, A )
            TEST = NUM_ADDUB( A, ONE )
            IF ( NUM_EQUB( NUM_SUBUB( TEST, A ), ONE ) ) GO TO 3

*  Again starting with unity, repeatedly double until a value B is
*  found that can be added to A to give a result that differs from A.
            B = ONE
 4          CONTINUE
            B = NUM_ADDUB( B, B )
            TEST = NUM_ADDUB( A, B )
            IF ( NUM_EQUB( TEST, A ) ) GO TO 4

*  The number base is now given by the difference between A and the
*  next representable number that differs from it (TEST).
            BASE = NUM_SUBUB( TEST, A )

*  Now test if HI is too large, using an appropriate fraction of it to
*  prevent overflow occurring. If it is too large, we decrement it to
*  obtain the next smaller representable value.
            IF ( NUM_GTUB( NUM_MULUB( NUM_DIVUB( HI, BASE ),
     :                                  NUM_DIVUB( HI, BASE ) ),
     :                      NUM_DIVUB( NUM__MAXUB,
     :                                  NUM_MULUB( BASE, BASE ) ) ) )
     :         HI = NUM_SUBUB( HI, NUM_MULUB( HI, VAL__EPSUB ) )

*  If we are dealing with an integer type, then find the square root of
*  the largest positive number using double precision arithmetic. Round
*  the result down to an integer and convert back to the required type.
         ELSE
            HI = NUM_ITOUB( INT( SQRT( NUM_UBTOD( NUM__MAXUB ) ) ) )
         END IF

*  Note we do not need to repeat these calculations.
         FIRST = .FALSE.
      END IF

*  Initialise the count of negative standard deviations and the data
*  conversion error flag.
      NNEG = 0
      DCE = .FALSE.

*  If required, loop through the array without checking for bad values.
      IF ( .NOT. BAD ) THEN
         DO 5 I = EL, 1, -1

*  If the data values are not unsigned, then check if the standard
*  deviation is negative.
            IF ( ( 'UB' .NE. 'UB' ) .AND.
     :           ( 'UB' .NE. 'UW' ) .AND.
     :           NUM_LTUB( ARRAY( I ), ZERO ) ) THEN

*  If so, then increment the negative standard deviation count, note a
*  data conversion error and assign a bad value to the affected array
*  element.
               NNEG = NNEG + 1
               DCE = .TRUE.
               ERRVAL = ARRAY( I )
               ARRAY( I ) = VAL__BADUB

*  Otherwise, check that the value will not overflow when squared. If
*  not, then square it.
            ELSE IF ( NUM_LEUB( ARRAY( I ), HI ) ) THEN
               ARRAY( I ) = NUM_MULUB( ARRAY( I ), ARRAY( I ) )

*  If it would overflow, then note a data conversion error and assign a
*  bad result.
            ELSE
               DCE = .TRUE.
               ARRAY( I ) = VAL__BADUB
            END IF
 5       CONTINUE

*  If required, loop through the array checking for bad values.
      ELSE
         DO 6 I = EL, 1, -1

*  Perform the bad value check.
            IF ( ARRAY( I ) .NE. VAL__BADUB ) THEN

*  Check the value is not negative.
               IF ( ( 'UB' .NE. 'UB' ) .AND.
     :              ( 'UB' .NE. 'UW' ) .AND.
     :              NUM_LTUB( ARRAY( I ), ZERO ) ) THEN
                  NNEG = NNEG + 1
                  DCE = .TRUE.
                  ERRVAL = ARRAY( I )
                  ARRAY( I ) = VAL__BADUB

*  Check for overflow and square the standard deviation.
               ELSE IF ( NUM_LEUB( ARRAY( I ), HI ) ) THEN
                  ARRAY( I ) = NUM_MULUB( ARRAY( I ), ARRAY( I ) )

*  Deal with overflows.
               ELSE
                  DCE = .TRUE.
                  ARRAY( I ) = VAL__BADUB
               END IF
            END IF
 6       CONTINUE
      END IF

*  If negative standard deviation values were encountered, then report
*  an error.
      IF ( NNEG .NE. 0 ) THEN
         STATUS = NDF__NGSTD
         CALL MSG_SETI( 'NNEG', NNEG )
         CALL MSG_SETR( 'ERRVAL', NUM_UBTOR( ERRVAL ) )
         CALL ERR_REP( 'NDF1_S2VUB_NEG',
     :   '^NNEG illegal negative standard deviation value(s) ' //
     :   'encountered; first offending value was ^ERRVAL ' //
     :   '(possible programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_S2VUB',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_S2VD( BAD, EL, ARRAY, DCE, STATUS )
*+
*  Name:
*     NDF1_S2VD

*  Purpose:
*     Convert DOUBLE PRECISION standard deviation values to variances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_S2VD( BAD, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised DOUBLE PRECISION array of standard
*     deviation values into variances by squaring them. It checks for
*     "bad" values if required and handles numerical overflow errors,
*     replacing them with bad values. If a negative standard deviation
*     is found, then STATUS is set to NDF__NGSTD, an error is reported
*     and a bad value is assigned to the affected array element -
*     however, the routine continues to process the entire array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     ARRAY( EL ) = DOUBLE PRECISION (Given and Returned)
*        On input, an array of DOUBLE PRECISION standard deviation values is
*        supplied. On output, they are replaced by the variance values.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in new bad
*        values being produced. This may result either from numerical
*        overflow (which the routine handles) or from replacement of
*        illegal negative standard deviations.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine works through the array backwards. This is to
*     minimise page faulting on a virtual memory machine, since it will
*     usually be followed by a data-processing loop which passes
*     forwards through the same array.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1989 (RFWS):
*        Original version.
*     17-OCT-1990 (RFWS):
*        Added the offending value to the error message.
*     8-SEP-1993 (RFWS):
*        Re-written to avoid the use of traps to detect overflow.
*     30-SEP-1993 (RFWS):
*        Eliminated arithmetic operations which may not compile
*        correctly for all integer data types. Use NUM_ functions
*        instead.
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
      DOUBLE PRECISION ONE                 ! One
      PARAMETER ( ONE = 1.0D0 )
      DOUBLE PRECISION TWO                 ! Two
      PARAMETER ( TWO = 2.0D0 )

*  Local Variables:
      DOUBLE PRECISION A                   ! Number to which 1 can't be added
      DOUBLE PRECISION B                   ! Number which can be added to A
      DOUBLE PRECISION BASE                ! Base of floating point numbers
      DOUBLE PRECISION ERRVAL              ! Value causing last error
      DOUBLE PRECISION HI                  ! Maximum value that can be squared
      DOUBLE PRECISION NEW                 ! New estimate of square root
      DOUBLE PRECISION TEST                ! Test value for finding number base
      INTEGER I                  ! Loop counter for array elements
      INTEGER ITER               ! Loop counter for iterations
      INTEGER NNEG               ! Number negative standard deviations
      LOGICAL FIRST              ! First invocation of this routine?

      SAVE FIRST
      SAVE HI

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_D'      ! NUM_ DOUBLE PRECISION functions

      INCLUDE 'NUM_DEF_CVT'      ! Define the functions...
      INCLUDE 'NUM_DEF_D'

*  Local Data:
      DATA FIRST / .TRUE. /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If this is the first invocation of this routine, we must first
*  calculate the limit HI, which is the largest input value which can be
*  squared without causing overflow.
      IF ( FIRST ) THEN

*  If we are dealing with a floating point type, then obtain an initial
*  estimate of HI from the square root of the largest positive number.
         IF ( ( 'D' .EQ. 'R' ) .OR. ( 'D' .EQ. 'D' ) ) THEN
            HI = NUM_SQRTD( NUM__MAXD )

*  Since this may not always achieve the full machine precision, we now
*  apply up to 10 iterations to refine the result, using Newton's
*  formula to estimate each new value. Note we cannot use arithmetic
*  operators directly, as the code must also compile correctly for
*  integer data types which may not support arithmetic (even though it
*  will never be executed in such cases).
            DO 1 ITER = 1, 10
               NEW = NUM_DIVD(
     :               NUM_ADDD( HI, NUM_DIVD( NUM__MAXD, HI ) ),
     :               TWO )

*  Quit looping if the result is no longer changing.
               IF ( NUM_EQD( NEW, HI ) ) GO TO 2
               HI = NEW
 1          CONTINUE
 2          CONTINUE

*  The current value of HI may still be too large (i.e. may overflow if
*  squared) if the result has been rounded upwards. To test for this,
*  we must square a known fraction of HI and check that it does not
*  exceed the expected value. For this to work, however, the fraction
*  chosen must be derived using the exponent base for the floating
*  point number representation in use (so that the mantissa is not
*  changed). Thus we must first determine this base.

*  Starting with unity, repeatedly double until a value A is found to
*  which 1 cannot be added without losing precision.
            A = ONE
 3          CONTINUE
            A = NUM_ADDD( A, A )
            TEST = NUM_ADDD( A, ONE )
            IF ( NUM_EQD( NUM_SUBD( TEST, A ), ONE ) ) GO TO 3

*  Again starting with unity, repeatedly double until a value B is
*  found that can be added to A to give a result that differs from A.
            B = ONE
 4          CONTINUE
            B = NUM_ADDD( B, B )
            TEST = NUM_ADDD( A, B )
            IF ( NUM_EQD( TEST, A ) ) GO TO 4

*  The number base is now given by the difference between A and the
*  next representable number that differs from it (TEST).
            BASE = NUM_SUBD( TEST, A )

*  Now test if HI is too large, using an appropriate fraction of it to
*  prevent overflow occurring. If it is too large, we decrement it to
*  obtain the next smaller representable value.
            IF ( NUM_GTD( NUM_MULD( NUM_DIVD( HI, BASE ),
     :                                  NUM_DIVD( HI, BASE ) ),
     :                      NUM_DIVD( NUM__MAXD,
     :                                  NUM_MULD( BASE, BASE ) ) ) )
     :         HI = NUM_SUBD( HI, NUM_MULD( HI, VAL__EPSD ) )

*  If we are dealing with an integer type, then find the square root of
*  the largest positive number using double precision arithmetic. Round
*  the result down to an integer and convert back to the required type.
         ELSE
            HI = NUM_ITOD( INT( SQRT( NUM_DTOD( NUM__MAXD ) ) ) )
         END IF

*  Note we do not need to repeat these calculations.
         FIRST = .FALSE.
      END IF

*  Initialise the count of negative standard deviations and the data
*  conversion error flag.
      NNEG = 0
      DCE = .FALSE.

*  If required, loop through the array without checking for bad values.
      IF ( .NOT. BAD ) THEN
         DO 5 I = EL, 1, -1

*  If the data values are not unsigned, then check if the standard
*  deviation is negative.
            IF ( ( 'D' .NE. 'UB' ) .AND.
     :           ( 'D' .NE. 'UW' ) .AND.
     :           NUM_LTD( ARRAY( I ), ZERO ) ) THEN

*  If so, then increment the negative standard deviation count, note a
*  data conversion error and assign a bad value to the affected array
*  element.
               NNEG = NNEG + 1
               DCE = .TRUE.
               ERRVAL = ARRAY( I )
               ARRAY( I ) = VAL__BADD

*  Otherwise, check that the value will not overflow when squared. If
*  not, then square it.
            ELSE IF ( NUM_LED( ARRAY( I ), HI ) ) THEN
               ARRAY( I ) = NUM_MULD( ARRAY( I ), ARRAY( I ) )

*  If it would overflow, then note a data conversion error and assign a
*  bad result.
            ELSE
               DCE = .TRUE.
               ARRAY( I ) = VAL__BADD
            END IF
 5       CONTINUE

*  If required, loop through the array checking for bad values.
      ELSE
         DO 6 I = EL, 1, -1

*  Perform the bad value check.
            IF ( ARRAY( I ) .NE. VAL__BADD ) THEN

*  Check the value is not negative.
               IF ( ( 'D' .NE. 'UB' ) .AND.
     :              ( 'D' .NE. 'UW' ) .AND.
     :              NUM_LTD( ARRAY( I ), ZERO ) ) THEN
                  NNEG = NNEG + 1
                  DCE = .TRUE.
                  ERRVAL = ARRAY( I )
                  ARRAY( I ) = VAL__BADD

*  Check for overflow and square the standard deviation.
               ELSE IF ( NUM_LED( ARRAY( I ), HI ) ) THEN
                  ARRAY( I ) = NUM_MULD( ARRAY( I ), ARRAY( I ) )

*  Deal with overflows.
               ELSE
                  DCE = .TRUE.
                  ARRAY( I ) = VAL__BADD
               END IF
            END IF
 6       CONTINUE
      END IF

*  If negative standard deviation values were encountered, then report
*  an error.
      IF ( NNEG .NE. 0 ) THEN
         STATUS = NDF__NGSTD
         CALL MSG_SETI( 'NNEG', NNEG )
         CALL MSG_SETR( 'ERRVAL', NUM_DTOR( ERRVAL ) )
         CALL ERR_REP( 'NDF1_S2VD_NEG',
     :   '^NNEG illegal negative standard deviation value(s) ' //
     :   'encountered; first offending value was ^ERRVAL ' //
     :   '(possible programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_S2VD',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_S2VI( BAD, EL, ARRAY, DCE, STATUS )
*+
*  Name:
*     NDF1_S2VI

*  Purpose:
*     Convert INTEGER standard deviation values to variances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_S2VI( BAD, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised INTEGER array of standard
*     deviation values into variances by squaring them. It checks for
*     "bad" values if required and handles numerical overflow errors,
*     replacing them with bad values. If a negative standard deviation
*     is found, then STATUS is set to NDF__NGSTD, an error is reported
*     and a bad value is assigned to the affected array element -
*     however, the routine continues to process the entire array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     ARRAY( EL ) = INTEGER (Given and Returned)
*        On input, an array of INTEGER standard deviation values is
*        supplied. On output, they are replaced by the variance values.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in new bad
*        values being produced. This may result either from numerical
*        overflow (which the routine handles) or from replacement of
*        illegal negative standard deviations.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine works through the array backwards. This is to
*     minimise page faulting on a virtual memory machine, since it will
*     usually be followed by a data-processing loop which passes
*     forwards through the same array.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1989 (RFWS):
*        Original version.
*     17-OCT-1990 (RFWS):
*        Added the offending value to the error message.
*     8-SEP-1993 (RFWS):
*        Re-written to avoid the use of traps to detect overflow.
*     30-SEP-1993 (RFWS):
*        Eliminated arithmetic operations which may not compile
*        correctly for all integer data types. Use NUM_ functions
*        instead.
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
      INTEGER ONE                 ! One
      PARAMETER ( ONE = 1  )
      INTEGER TWO                 ! Two
      PARAMETER ( TWO = 2  )

*  Local Variables:
      INTEGER A                   ! Number to which 1 can't be added
      INTEGER B                   ! Number which can be added to A
      INTEGER BASE                ! Base of floating point numbers
      INTEGER ERRVAL              ! Value causing last error
      INTEGER HI                  ! Maximum value that can be squared
      INTEGER NEW                 ! New estimate of square root
      INTEGER TEST                ! Test value for finding number base
      INTEGER I                  ! Loop counter for array elements
      INTEGER ITER               ! Loop counter for iterations
      INTEGER NNEG               ! Number negative standard deviations
      LOGICAL FIRST              ! First invocation of this routine?

      SAVE FIRST
      SAVE HI

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_I'      ! NUM_ INTEGER functions

      INCLUDE 'NUM_DEF_CVT'      ! Define the functions...
      INCLUDE 'NUM_DEF_I'

*  Local Data:
      DATA FIRST / .TRUE. /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If this is the first invocation of this routine, we must first
*  calculate the limit HI, which is the largest input value which can be
*  squared without causing overflow.
      IF ( FIRST ) THEN

*  If we are dealing with a floating point type, then obtain an initial
*  estimate of HI from the square root of the largest positive number.
         IF ( ( 'I' .EQ. 'R' ) .OR. ( 'I' .EQ. 'D' ) ) THEN
            HI = NUM_SQRTI( NUM__MAXI )

*  Since this may not always achieve the full machine precision, we now
*  apply up to 10 iterations to refine the result, using Newton's
*  formula to estimate each new value. Note we cannot use arithmetic
*  operators directly, as the code must also compile correctly for
*  integer data types which may not support arithmetic (even though it
*  will never be executed in such cases).
            DO 1 ITER = 1, 10
               NEW = NUM_DIVI(
     :               NUM_ADDI( HI, NUM_DIVI( NUM__MAXI, HI ) ),
     :               TWO )

*  Quit looping if the result is no longer changing.
               IF ( NUM_EQI( NEW, HI ) ) GO TO 2
               HI = NEW
 1          CONTINUE
 2          CONTINUE

*  The current value of HI may still be too large (i.e. may overflow if
*  squared) if the result has been rounded upwards. To test for this,
*  we must square a known fraction of HI and check that it does not
*  exceed the expected value. For this to work, however, the fraction
*  chosen must be derived using the exponent base for the floating
*  point number representation in use (so that the mantissa is not
*  changed). Thus we must first determine this base.

*  Starting with unity, repeatedly double until a value A is found to
*  which 1 cannot be added without losing precision.
            A = ONE
 3          CONTINUE
            A = NUM_ADDI( A, A )
            TEST = NUM_ADDI( A, ONE )
            IF ( NUM_EQI( NUM_SUBI( TEST, A ), ONE ) ) GO TO 3

*  Again starting with unity, repeatedly double until a value B is
*  found that can be added to A to give a result that differs from A.
            B = ONE
 4          CONTINUE
            B = NUM_ADDI( B, B )
            TEST = NUM_ADDI( A, B )
            IF ( NUM_EQI( TEST, A ) ) GO TO 4

*  The number base is now given by the difference between A and the
*  next representable number that differs from it (TEST).
            BASE = NUM_SUBI( TEST, A )

*  Now test if HI is too large, using an appropriate fraction of it to
*  prevent overflow occurring. If it is too large, we decrement it to
*  obtain the next smaller representable value.
            IF ( NUM_GTI( NUM_MULI( NUM_DIVI( HI, BASE ),
     :                                  NUM_DIVI( HI, BASE ) ),
     :                      NUM_DIVI( NUM__MAXI,
     :                                  NUM_MULI( BASE, BASE ) ) ) )
     :         HI = NUM_SUBI( HI, NUM_MULI( HI, VAL__EPSI ) )

*  If we are dealing with an integer type, then find the square root of
*  the largest positive number using double precision arithmetic. Round
*  the result down to an integer and convert back to the required type.
         ELSE
            HI = NUM_ITOI( INT( SQRT( NUM_ITOD( NUM__MAXI ) ) ) )
         END IF

*  Note we do not need to repeat these calculations.
         FIRST = .FALSE.
      END IF

*  Initialise the count of negative standard deviations and the data
*  conversion error flag.
      NNEG = 0
      DCE = .FALSE.

*  If required, loop through the array without checking for bad values.
      IF ( .NOT. BAD ) THEN
         DO 5 I = EL, 1, -1

*  If the data values are not unsigned, then check if the standard
*  deviation is negative.
            IF ( ( 'I' .NE. 'UB' ) .AND.
     :           ( 'I' .NE. 'UW' ) .AND.
     :           NUM_LTI( ARRAY( I ), ZERO ) ) THEN

*  If so, then increment the negative standard deviation count, note a
*  data conversion error and assign a bad value to the affected array
*  element.
               NNEG = NNEG + 1
               DCE = .TRUE.
               ERRVAL = ARRAY( I )
               ARRAY( I ) = VAL__BADI

*  Otherwise, check that the value will not overflow when squared. If
*  not, then square it.
            ELSE IF ( NUM_LEI( ARRAY( I ), HI ) ) THEN
               ARRAY( I ) = NUM_MULI( ARRAY( I ), ARRAY( I ) )

*  If it would overflow, then note a data conversion error and assign a
*  bad result.
            ELSE
               DCE = .TRUE.
               ARRAY( I ) = VAL__BADI
            END IF
 5       CONTINUE

*  If required, loop through the array checking for bad values.
      ELSE
         DO 6 I = EL, 1, -1

*  Perform the bad value check.
            IF ( ARRAY( I ) .NE. VAL__BADI ) THEN

*  Check the value is not negative.
               IF ( ( 'I' .NE. 'UB' ) .AND.
     :              ( 'I' .NE. 'UW' ) .AND.
     :              NUM_LTI( ARRAY( I ), ZERO ) ) THEN
                  NNEG = NNEG + 1
                  DCE = .TRUE.
                  ERRVAL = ARRAY( I )
                  ARRAY( I ) = VAL__BADI

*  Check for overflow and square the standard deviation.
               ELSE IF ( NUM_LEI( ARRAY( I ), HI ) ) THEN
                  ARRAY( I ) = NUM_MULI( ARRAY( I ), ARRAY( I ) )

*  Deal with overflows.
               ELSE
                  DCE = .TRUE.
                  ARRAY( I ) = VAL__BADI
               END IF
            END IF
 6       CONTINUE
      END IF

*  If negative standard deviation values were encountered, then report
*  an error.
      IF ( NNEG .NE. 0 ) THEN
         STATUS = NDF__NGSTD
         CALL MSG_SETI( 'NNEG', NNEG )
         CALL MSG_SETR( 'ERRVAL', NUM_ITOR( ERRVAL ) )
         CALL ERR_REP( 'NDF1_S2VI_NEG',
     :   '^NNEG illegal negative standard deviation value(s) ' //
     :   'encountered; first offending value was ^ERRVAL ' //
     :   '(possible programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_S2VI',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_S2VR( BAD, EL, ARRAY, DCE, STATUS )
*+
*  Name:
*     NDF1_S2VR

*  Purpose:
*     Convert REAL standard deviation values to variances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_S2VR( BAD, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised REAL array of standard
*     deviation values into variances by squaring them. It checks for
*     "bad" values if required and handles numerical overflow errors,
*     replacing them with bad values. If a negative standard deviation
*     is found, then STATUS is set to NDF__NGSTD, an error is reported
*     and a bad value is assigned to the affected array element -
*     however, the routine continues to process the entire array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     ARRAY( EL ) = REAL (Given and Returned)
*        On input, an array of REAL standard deviation values is
*        supplied. On output, they are replaced by the variance values.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in new bad
*        values being produced. This may result either from numerical
*        overflow (which the routine handles) or from replacement of
*        illegal negative standard deviations.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine works through the array backwards. This is to
*     minimise page faulting on a virtual memory machine, since it will
*     usually be followed by a data-processing loop which passes
*     forwards through the same array.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1989 (RFWS):
*        Original version.
*     17-OCT-1990 (RFWS):
*        Added the offending value to the error message.
*     8-SEP-1993 (RFWS):
*        Re-written to avoid the use of traps to detect overflow.
*     30-SEP-1993 (RFWS):
*        Eliminated arithmetic operations which may not compile
*        correctly for all integer data types. Use NUM_ functions
*        instead.
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
      REAL ONE                 ! One
      PARAMETER ( ONE = 1.0E0 )
      REAL TWO                 ! Two
      PARAMETER ( TWO = 2.0E0 )

*  Local Variables:
      REAL A                   ! Number to which 1 can't be added
      REAL B                   ! Number which can be added to A
      REAL BASE                ! Base of floating point numbers
      REAL ERRVAL              ! Value causing last error
      REAL HI                  ! Maximum value that can be squared
      REAL NEW                 ! New estimate of square root
      REAL TEST                ! Test value for finding number base
      INTEGER I                  ! Loop counter for array elements
      INTEGER ITER               ! Loop counter for iterations
      INTEGER NNEG               ! Number negative standard deviations
      LOGICAL FIRST              ! First invocation of this routine?

      SAVE FIRST
      SAVE HI

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_R'      ! NUM_ REAL functions

      INCLUDE 'NUM_DEF_CVT'      ! Define the functions...
      INCLUDE 'NUM_DEF_R'

*  Local Data:
      DATA FIRST / .TRUE. /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If this is the first invocation of this routine, we must first
*  calculate the limit HI, which is the largest input value which can be
*  squared without causing overflow.
      IF ( FIRST ) THEN

*  If we are dealing with a floating point type, then obtain an initial
*  estimate of HI from the square root of the largest positive number.
         IF ( ( 'R' .EQ. 'R' ) .OR. ( 'R' .EQ. 'D' ) ) THEN
            HI = NUM_SQRTR( NUM__MAXR )

*  Since this may not always achieve the full machine precision, we now
*  apply up to 10 iterations to refine the result, using Newton's
*  formula to estimate each new value. Note we cannot use arithmetic
*  operators directly, as the code must also compile correctly for
*  integer data types which may not support arithmetic (even though it
*  will never be executed in such cases).
            DO 1 ITER = 1, 10
               NEW = NUM_DIVR(
     :               NUM_ADDR( HI, NUM_DIVR( NUM__MAXR, HI ) ),
     :               TWO )

*  Quit looping if the result is no longer changing.
               IF ( NUM_EQR( NEW, HI ) ) GO TO 2
               HI = NEW
 1          CONTINUE
 2          CONTINUE

*  The current value of HI may still be too large (i.e. may overflow if
*  squared) if the result has been rounded upwards. To test for this,
*  we must square a known fraction of HI and check that it does not
*  exceed the expected value. For this to work, however, the fraction
*  chosen must be derived using the exponent base for the floating
*  point number representation in use (so that the mantissa is not
*  changed). Thus we must first determine this base.

*  Starting with unity, repeatedly double until a value A is found to
*  which 1 cannot be added without losing precision.
            A = ONE
 3          CONTINUE
            A = NUM_ADDR( A, A )
            TEST = NUM_ADDR( A, ONE )
            IF ( NUM_EQR( NUM_SUBR( TEST, A ), ONE ) ) GO TO 3

*  Again starting with unity, repeatedly double until a value B is
*  found that can be added to A to give a result that differs from A.
            B = ONE
 4          CONTINUE
            B = NUM_ADDR( B, B )
            TEST = NUM_ADDR( A, B )
            IF ( NUM_EQR( TEST, A ) ) GO TO 4

*  The number base is now given by the difference between A and the
*  next representable number that differs from it (TEST).
            BASE = NUM_SUBR( TEST, A )

*  Now test if HI is too large, using an appropriate fraction of it to
*  prevent overflow occurring. If it is too large, we decrement it to
*  obtain the next smaller representable value.
            IF ( NUM_GTR( NUM_MULR( NUM_DIVR( HI, BASE ),
     :                                  NUM_DIVR( HI, BASE ) ),
     :                      NUM_DIVR( NUM__MAXR,
     :                                  NUM_MULR( BASE, BASE ) ) ) )
     :         HI = NUM_SUBR( HI, NUM_MULR( HI, VAL__EPSR ) )

*  If we are dealing with an integer type, then find the square root of
*  the largest positive number using double precision arithmetic. Round
*  the result down to an integer and convert back to the required type.
         ELSE
            HI = NUM_ITOR( INT( SQRT( NUM_RTOD( NUM__MAXR ) ) ) )
         END IF

*  Note we do not need to repeat these calculations.
         FIRST = .FALSE.
      END IF

*  Initialise the count of negative standard deviations and the data
*  conversion error flag.
      NNEG = 0
      DCE = .FALSE.

*  If required, loop through the array without checking for bad values.
      IF ( .NOT. BAD ) THEN
         DO 5 I = EL, 1, -1

*  If the data values are not unsigned, then check if the standard
*  deviation is negative.
            IF ( ( 'R' .NE. 'UB' ) .AND.
     :           ( 'R' .NE. 'UW' ) .AND.
     :           NUM_LTR( ARRAY( I ), ZERO ) ) THEN

*  If so, then increment the negative standard deviation count, note a
*  data conversion error and assign a bad value to the affected array
*  element.
               NNEG = NNEG + 1
               DCE = .TRUE.
               ERRVAL = ARRAY( I )
               ARRAY( I ) = VAL__BADR

*  Otherwise, check that the value will not overflow when squared. If
*  not, then square it.
            ELSE IF ( NUM_LER( ARRAY( I ), HI ) ) THEN
               ARRAY( I ) = NUM_MULR( ARRAY( I ), ARRAY( I ) )

*  If it would overflow, then note a data conversion error and assign a
*  bad result.
            ELSE
               DCE = .TRUE.
               ARRAY( I ) = VAL__BADR
            END IF
 5       CONTINUE

*  If required, loop through the array checking for bad values.
      ELSE
         DO 6 I = EL, 1, -1

*  Perform the bad value check.
            IF ( ARRAY( I ) .NE. VAL__BADR ) THEN

*  Check the value is not negative.
               IF ( ( 'R' .NE. 'UB' ) .AND.
     :              ( 'R' .NE. 'UW' ) .AND.
     :              NUM_LTR( ARRAY( I ), ZERO ) ) THEN
                  NNEG = NNEG + 1
                  DCE = .TRUE.
                  ERRVAL = ARRAY( I )
                  ARRAY( I ) = VAL__BADR

*  Check for overflow and square the standard deviation.
               ELSE IF ( NUM_LER( ARRAY( I ), HI ) ) THEN
                  ARRAY( I ) = NUM_MULR( ARRAY( I ), ARRAY( I ) )

*  Deal with overflows.
               ELSE
                  DCE = .TRUE.
                  ARRAY( I ) = VAL__BADR
               END IF
            END IF
 6       CONTINUE
      END IF

*  If negative standard deviation values were encountered, then report
*  an error.
      IF ( NNEG .NE. 0 ) THEN
         STATUS = NDF__NGSTD
         CALL MSG_SETI( 'NNEG', NNEG )
         CALL MSG_SETR( 'ERRVAL', NUM_RTOR( ERRVAL ) )
         CALL ERR_REP( 'NDF1_S2VR_NEG',
     :   '^NNEG illegal negative standard deviation value(s) ' //
     :   'encountered; first offending value was ^ERRVAL ' //
     :   '(possible programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_S2VR',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_S2VW( BAD, EL, ARRAY, DCE, STATUS )
*+
*  Name:
*     NDF1_S2VW

*  Purpose:
*     Convert WORD standard deviation values to variances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_S2VW( BAD, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised WORD array of standard
*     deviation values into variances by squaring them. It checks for
*     "bad" values if required and handles numerical overflow errors,
*     replacing them with bad values. If a negative standard deviation
*     is found, then STATUS is set to NDF__NGSTD, an error is reported
*     and a bad value is assigned to the affected array element -
*     however, the routine continues to process the entire array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     ARRAY( EL ) = INTEGER*2 (Given and Returned)
*        On input, an array of WORD standard deviation values is
*        supplied. On output, they are replaced by the variance values.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in new bad
*        values being produced. This may result either from numerical
*        overflow (which the routine handles) or from replacement of
*        illegal negative standard deviations.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine works through the array backwards. This is to
*     minimise page faulting on a virtual memory machine, since it will
*     usually be followed by a data-processing loop which passes
*     forwards through the same array.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1989 (RFWS):
*        Original version.
*     17-OCT-1990 (RFWS):
*        Added the offending value to the error message.
*     8-SEP-1993 (RFWS):
*        Re-written to avoid the use of traps to detect overflow.
*     30-SEP-1993 (RFWS):
*        Eliminated arithmetic operations which may not compile
*        correctly for all integer data types. Use NUM_ functions
*        instead.
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
      INTEGER*2 ONE                 ! One
      PARAMETER ( ONE = 1  )
      INTEGER*2 TWO                 ! Two
      PARAMETER ( TWO = 2  )

*  Local Variables:
      INTEGER*2 A                   ! Number to which 1 can't be added
      INTEGER*2 B                   ! Number which can be added to A
      INTEGER*2 BASE                ! Base of floating point numbers
      INTEGER*2 ERRVAL              ! Value causing last error
      INTEGER*2 HI                  ! Maximum value that can be squared
      INTEGER*2 NEW                 ! New estimate of square root
      INTEGER*2 TEST                ! Test value for finding number base
      INTEGER I                  ! Loop counter for array elements
      INTEGER ITER               ! Loop counter for iterations
      INTEGER NNEG               ! Number negative standard deviations
      LOGICAL FIRST              ! First invocation of this routine?

      SAVE FIRST
      SAVE HI

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_W'      ! NUM_ WORD functions

      INCLUDE 'NUM_DEF_CVT'      ! Define the functions...
      INCLUDE 'NUM_DEF_W'

*  Local Data:
      DATA FIRST / .TRUE. /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If this is the first invocation of this routine, we must first
*  calculate the limit HI, which is the largest input value which can be
*  squared without causing overflow.
      IF ( FIRST ) THEN

*  If we are dealing with a floating point type, then obtain an initial
*  estimate of HI from the square root of the largest positive number.
         IF ( ( 'W' .EQ. 'R' ) .OR. ( 'W' .EQ. 'D' ) ) THEN
            HI = NUM_SQRTW( NUM__MAXW )

*  Since this may not always achieve the full machine precision, we now
*  apply up to 10 iterations to refine the result, using Newton's
*  formula to estimate each new value. Note we cannot use arithmetic
*  operators directly, as the code must also compile correctly for
*  integer data types which may not support arithmetic (even though it
*  will never be executed in such cases).
            DO 1 ITER = 1, 10
               NEW = NUM_DIVW(
     :               NUM_ADDW( HI, NUM_DIVW( NUM__MAXW, HI ) ),
     :               TWO )

*  Quit looping if the result is no longer changing.
               IF ( NUM_EQW( NEW, HI ) ) GO TO 2
               HI = NEW
 1          CONTINUE
 2          CONTINUE

*  The current value of HI may still be too large (i.e. may overflow if
*  squared) if the result has been rounded upwards. To test for this,
*  we must square a known fraction of HI and check that it does not
*  exceed the expected value. For this to work, however, the fraction
*  chosen must be derived using the exponent base for the floating
*  point number representation in use (so that the mantissa is not
*  changed). Thus we must first determine this base.

*  Starting with unity, repeatedly double until a value A is found to
*  which 1 cannot be added without losing precision.
            A = ONE
 3          CONTINUE
            A = NUM_ADDW( A, A )
            TEST = NUM_ADDW( A, ONE )
            IF ( NUM_EQW( NUM_SUBW( TEST, A ), ONE ) ) GO TO 3

*  Again starting with unity, repeatedly double until a value B is
*  found that can be added to A to give a result that differs from A.
            B = ONE
 4          CONTINUE
            B = NUM_ADDW( B, B )
            TEST = NUM_ADDW( A, B )
            IF ( NUM_EQW( TEST, A ) ) GO TO 4

*  The number base is now given by the difference between A and the
*  next representable number that differs from it (TEST).
            BASE = NUM_SUBW( TEST, A )

*  Now test if HI is too large, using an appropriate fraction of it to
*  prevent overflow occurring. If it is too large, we decrement it to
*  obtain the next smaller representable value.
            IF ( NUM_GTW( NUM_MULW( NUM_DIVW( HI, BASE ),
     :                                  NUM_DIVW( HI, BASE ) ),
     :                      NUM_DIVW( NUM__MAXW,
     :                                  NUM_MULW( BASE, BASE ) ) ) )
     :         HI = NUM_SUBW( HI, NUM_MULW( HI, VAL__EPSW ) )

*  If we are dealing with an integer type, then find the square root of
*  the largest positive number using double precision arithmetic. Round
*  the result down to an integer and convert back to the required type.
         ELSE
            HI = NUM_ITOW( INT( SQRT( NUM_WTOD( NUM__MAXW ) ) ) )
         END IF

*  Note we do not need to repeat these calculations.
         FIRST = .FALSE.
      END IF

*  Initialise the count of negative standard deviations and the data
*  conversion error flag.
      NNEG = 0
      DCE = .FALSE.

*  If required, loop through the array without checking for bad values.
      IF ( .NOT. BAD ) THEN
         DO 5 I = EL, 1, -1

*  If the data values are not unsigned, then check if the standard
*  deviation is negative.
            IF ( ( 'W' .NE. 'UB' ) .AND.
     :           ( 'W' .NE. 'UW' ) .AND.
     :           NUM_LTW( ARRAY( I ), ZERO ) ) THEN

*  If so, then increment the negative standard deviation count, note a
*  data conversion error and assign a bad value to the affected array
*  element.
               NNEG = NNEG + 1
               DCE = .TRUE.
               ERRVAL = ARRAY( I )
               ARRAY( I ) = VAL__BADW

*  Otherwise, check that the value will not overflow when squared. If
*  not, then square it.
            ELSE IF ( NUM_LEW( ARRAY( I ), HI ) ) THEN
               ARRAY( I ) = NUM_MULW( ARRAY( I ), ARRAY( I ) )

*  If it would overflow, then note a data conversion error and assign a
*  bad result.
            ELSE
               DCE = .TRUE.
               ARRAY( I ) = VAL__BADW
            END IF
 5       CONTINUE

*  If required, loop through the array checking for bad values.
      ELSE
         DO 6 I = EL, 1, -1

*  Perform the bad value check.
            IF ( ARRAY( I ) .NE. VAL__BADW ) THEN

*  Check the value is not negative.
               IF ( ( 'W' .NE. 'UB' ) .AND.
     :              ( 'W' .NE. 'UW' ) .AND.
     :              NUM_LTW( ARRAY( I ), ZERO ) ) THEN
                  NNEG = NNEG + 1
                  DCE = .TRUE.
                  ERRVAL = ARRAY( I )
                  ARRAY( I ) = VAL__BADW

*  Check for overflow and square the standard deviation.
               ELSE IF ( NUM_LEW( ARRAY( I ), HI ) ) THEN
                  ARRAY( I ) = NUM_MULW( ARRAY( I ), ARRAY( I ) )

*  Deal with overflows.
               ELSE
                  DCE = .TRUE.
                  ARRAY( I ) = VAL__BADW
               END IF
            END IF
 6       CONTINUE
      END IF

*  If negative standard deviation values were encountered, then report
*  an error.
      IF ( NNEG .NE. 0 ) THEN
         STATUS = NDF__NGSTD
         CALL MSG_SETI( 'NNEG', NNEG )
         CALL MSG_SETR( 'ERRVAL', NUM_WTOR( ERRVAL ) )
         CALL ERR_REP( 'NDF1_S2VW_NEG',
     :   '^NNEG illegal negative standard deviation value(s) ' //
     :   'encountered; first offending value was ^ERRVAL ' //
     :   '(possible programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_S2VW',
     :                                            STATUS )

      END
      SUBROUTINE NDF1_S2VUW( BAD, EL, ARRAY, DCE, STATUS )
*+
*  Name:
*     NDF1_S2VUW

*  Purpose:
*     Convert UNSIGNED WORD standard deviation values to variances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_S2VUW( BAD, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised UNSIGNED WORD array of standard
*     deviation values into variances by squaring them. It checks for
*     "bad" values if required and handles numerical overflow errors,
*     replacing them with bad values. If a negative standard deviation
*     is found, then STATUS is set to NDF__NGSTD, an error is reported
*     and a bad value is assigned to the affected array element -
*     however, the routine continues to process the entire array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     ARRAY( EL ) = INTEGER*2 (Given and Returned)
*        On input, an array of UNSIGNED WORD standard deviation values is
*        supplied. On output, they are replaced by the variance values.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in new bad
*        values being produced. This may result either from numerical
*        overflow (which the routine handles) or from replacement of
*        illegal negative standard deviations.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine works through the array backwards. This is to
*     minimise page faulting on a virtual memory machine, since it will
*     usually be followed by a data-processing loop which passes
*     forwards through the same array.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1989 (RFWS):
*        Original version.
*     17-OCT-1990 (RFWS):
*        Added the offending value to the error message.
*     8-SEP-1993 (RFWS):
*        Re-written to avoid the use of traps to detect overflow.
*     30-SEP-1993 (RFWS):
*        Eliminated arithmetic operations which may not compile
*        correctly for all integer data types. Use NUM_ functions
*        instead.
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
      INTEGER*2 ONE                 ! One
      PARAMETER ( ONE = 1  )
      INTEGER*2 TWO                 ! Two
      PARAMETER ( TWO = 2  )

*  Local Variables:
      INTEGER*2 A                   ! Number to which 1 can't be added
      INTEGER*2 B                   ! Number which can be added to A
      INTEGER*2 BASE                ! Base of floating point numbers
      INTEGER*2 ERRVAL              ! Value causing last error
      INTEGER*2 HI                  ! Maximum value that can be squared
      INTEGER*2 NEW                 ! New estimate of square root
      INTEGER*2 TEST                ! Test value for finding number base
      INTEGER I                  ! Loop counter for array elements
      INTEGER ITER               ! Loop counter for iterations
      INTEGER NNEG               ! Number negative standard deviations
      LOGICAL FIRST              ! First invocation of this routine?

      SAVE FIRST
      SAVE HI

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEC_UW'      ! NUM_ UNSIGNED WORD functions

      INCLUDE 'NUM_DEF_CVT'      ! Define the functions...
      INCLUDE 'NUM_DEF_UW'

*  Local Data:
      DATA FIRST / .TRUE. /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If this is the first invocation of this routine, we must first
*  calculate the limit HI, which is the largest input value which can be
*  squared without causing overflow.
      IF ( FIRST ) THEN

*  If we are dealing with a floating point type, then obtain an initial
*  estimate of HI from the square root of the largest positive number.
         IF ( ( 'UW' .EQ. 'R' ) .OR. ( 'UW' .EQ. 'D' ) ) THEN
            HI = NUM_SQRTUW( NUM__MAXUW )

*  Since this may not always achieve the full machine precision, we now
*  apply up to 10 iterations to refine the result, using Newton's
*  formula to estimate each new value. Note we cannot use arithmetic
*  operators directly, as the code must also compile correctly for
*  integer data types which may not support arithmetic (even though it
*  will never be executed in such cases).
            DO 1 ITER = 1, 10
               NEW = NUM_DIVUW(
     :               NUM_ADDUW( HI, NUM_DIVUW( NUM__MAXUW, HI ) ),
     :               TWO )

*  Quit looping if the result is no longer changing.
               IF ( NUM_EQUW( NEW, HI ) ) GO TO 2
               HI = NEW
 1          CONTINUE
 2          CONTINUE

*  The current value of HI may still be too large (i.e. may overflow if
*  squared) if the result has been rounded upwards. To test for this,
*  we must square a known fraction of HI and check that it does not
*  exceed the expected value. For this to work, however, the fraction
*  chosen must be derived using the exponent base for the floating
*  point number representation in use (so that the mantissa is not
*  changed). Thus we must first determine this base.

*  Starting with unity, repeatedly double until a value A is found to
*  which 1 cannot be added without losing precision.
            A = ONE
 3          CONTINUE
            A = NUM_ADDUW( A, A )
            TEST = NUM_ADDUW( A, ONE )
            IF ( NUM_EQUW( NUM_SUBUW( TEST, A ), ONE ) ) GO TO 3

*  Again starting with unity, repeatedly double until a value B is
*  found that can be added to A to give a result that differs from A.
            B = ONE
 4          CONTINUE
            B = NUM_ADDUW( B, B )
            TEST = NUM_ADDUW( A, B )
            IF ( NUM_EQUW( TEST, A ) ) GO TO 4

*  The number base is now given by the difference between A and the
*  next representable number that differs from it (TEST).
            BASE = NUM_SUBUW( TEST, A )

*  Now test if HI is too large, using an appropriate fraction of it to
*  prevent overflow occurring. If it is too large, we decrement it to
*  obtain the next smaller representable value.
            IF ( NUM_GTUW( NUM_MULUW( NUM_DIVUW( HI, BASE ),
     :                                  NUM_DIVUW( HI, BASE ) ),
     :                      NUM_DIVUW( NUM__MAXUW,
     :                                  NUM_MULUW( BASE, BASE ) ) ) )
     :         HI = NUM_SUBUW( HI, NUM_MULUW( HI, VAL__EPSUW ) )

*  If we are dealing with an integer type, then find the square root of
*  the largest positive number using double precision arithmetic. Round
*  the result down to an integer and convert back to the required type.
         ELSE
            HI = NUM_ITOUW( INT( SQRT( NUM_UWTOD( NUM__MAXUW ) ) ) )
         END IF

*  Note we do not need to repeat these calculations.
         FIRST = .FALSE.
      END IF

*  Initialise the count of negative standard deviations and the data
*  conversion error flag.
      NNEG = 0
      DCE = .FALSE.

*  If required, loop through the array without checking for bad values.
      IF ( .NOT. BAD ) THEN
         DO 5 I = EL, 1, -1

*  If the data values are not unsigned, then check if the standard
*  deviation is negative.
            IF ( ( 'UW' .NE. 'UB' ) .AND.
     :           ( 'UW' .NE. 'UW' ) .AND.
     :           NUM_LTUW( ARRAY( I ), ZERO ) ) THEN

*  If so, then increment the negative standard deviation count, note a
*  data conversion error and assign a bad value to the affected array
*  element.
               NNEG = NNEG + 1
               DCE = .TRUE.
               ERRVAL = ARRAY( I )
               ARRAY( I ) = VAL__BADUW

*  Otherwise, check that the value will not overflow when squared. If
*  not, then square it.
            ELSE IF ( NUM_LEUW( ARRAY( I ), HI ) ) THEN
               ARRAY( I ) = NUM_MULUW( ARRAY( I ), ARRAY( I ) )

*  If it would overflow, then note a data conversion error and assign a
*  bad result.
            ELSE
               DCE = .TRUE.
               ARRAY( I ) = VAL__BADUW
            END IF
 5       CONTINUE

*  If required, loop through the array checking for bad values.
      ELSE
         DO 6 I = EL, 1, -1

*  Perform the bad value check.
            IF ( ARRAY( I ) .NE. VAL__BADUW ) THEN

*  Check the value is not negative.
               IF ( ( 'UW' .NE. 'UB' ) .AND.
     :              ( 'UW' .NE. 'UW' ) .AND.
     :              NUM_LTUW( ARRAY( I ), ZERO ) ) THEN
                  NNEG = NNEG + 1
                  DCE = .TRUE.
                  ERRVAL = ARRAY( I )
                  ARRAY( I ) = VAL__BADUW

*  Check for overflow and square the standard deviation.
               ELSE IF ( NUM_LEUW( ARRAY( I ), HI ) ) THEN
                  ARRAY( I ) = NUM_MULUW( ARRAY( I ), ARRAY( I ) )

*  Deal with overflows.
               ELSE
                  DCE = .TRUE.
                  ARRAY( I ) = VAL__BADUW
               END IF
            END IF
 6       CONTINUE
      END IF

*  If negative standard deviation values were encountered, then report
*  an error.
      IF ( NNEG .NE. 0 ) THEN
         STATUS = NDF__NGSTD
         CALL MSG_SETI( 'NNEG', NNEG )
         CALL MSG_SETR( 'ERRVAL', NUM_UWTOR( ERRVAL ) )
         CALL ERR_REP( 'NDF1_S2VUW_NEG',
     :   '^NNEG illegal negative standard deviation value(s) ' //
     :   'encountered; first offending value was ^ERRVAL ' //
     :   '(possible programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_S2VUW',
     :                                            STATUS )

      END
