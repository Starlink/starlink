      DOUBLE PRECISION FUNCTION VAL_UWTOD( BAD, ARG, STATUS )
*+
*  Name:
*     VAL_UWTOD

*  Purpose:
*     Convert a value from UNSIGNED WORD to DOUBLE PRECISION.

*  Language:
*     Starlink Fortran

*  Invocation:
*     RESULT = VAL_UWTOD( BAD, ARG, STATUS )

*  Description:
*     The routine performs type conversion on a value of type UNSIGNED WORD,
*     converting it to the equivalent DOUBLE PRECISION value.  If a numerical
*     error occurs, the value VAL__BADD is returned and a STATUS
*     value is set.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether the argument value (ARG) may be "bad".
*     ARG = INTEGER*2 (Given)
*        The UNSIGNED WORD value to be converted.
*     STATUS = INTEGER (Given & Returned)
*        This should be set to SAI__OK on entry, otherwise the routine
*        returns immediately with the result VAL__BADD.  A STATUS
*        value will be set by this routine if a numerical error occurs
*        during type conversion.

*  Returned Value:
*     VAL_UWTOD = DOUBLE PRECISION
*        Returns the converted DOUBLE PRECISION value.  The value VAL__BADD
*        will be returned under error conditions.

*  Copyright:
*     Copyright (C) 1988, 1991, 1992 Science & Engineering Research Council.
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
*     R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-AUG-1988 (RFWS):
*        Original version.
*     28-OCT-1991 (RFWS):
*        Added LIB$REVERT call.
*     5-NOV-1991 (RFWS):
*        Removed condition handler; use explicit bounds checks instead
*        in the interests of portability.
*     28-JAN-1992 (RFWS):
*        Temporarily removed adjustments to data limits to account for
*        rounding.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INCLUDE 'PRM_PAR'          ! PRM_ public constants

      INCLUDE 'PRM_ERR'          ! PRM_ error codes


*  Arguments Given:
      LOGICAL BAD                ! Bad data flag
      INTEGER*2 ARG                 ! Value to be converted

*  Status:
      INTEGER STATUS             ! Error status

*  Local Variables:
      INTEGER*2 HI                  ! Upper bound on argument
      INTEGER*2 LO                  ! Lower bound on argument
      DOUBLE PRECISION DHI       ! Upper bound on data
      DOUBLE PRECISION DLO       ! Lower bound on data
      LOGICAL FIRST              ! First invocation?

      SAVE FIRST
      SAVE HI
      SAVE LO

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! Declare NUM_ conversion functions

      INCLUDE 'NUM_DEC_UW'      ! Declare NUM_ arithmetic functions

      INCLUDE 'NUM_DEF_CVT'      ! Define NUM_ conversion functions

      INCLUDE 'NUM_DEF_UW'      ! Define NUM_ arithmetic functions


*  Local Data:
      DATA FIRST / .TRUE. /      ! First invocation

*.

*  If the conversion can potentially fail, then on the first invocation
*  set up the lower and upper bounds on the argument values.
      IF ( .FALSE. ) THEN
         IF ( FIRST ) THEN

*  Find the intersection of the ranges of allowed values between the
*  input and output data types. Perform this calculation in double
*  precision.
            DLO = MAX( NUM_UWTOD( NUM__MINUW ),
     :                 NUM_DTOD( NUM__MIND ) )
            DHI = MIN( NUM_UWTOD( NUM__MAXUW ),
     :                 NUM_DTOD( NUM__MAXD ) )

*  Adjust the resulting limits to allow for rounding of both the input
*  and output data types.
C            IF ( DLO .GT. 0.0D0 ) THEN
C               DLO = DLO - 0.5D0 * NUM_DTOD( VAL__EPSD )
C            ELSE
C               DLO = DLO - 0.5D0 * ( NUM_DTOD( VAL__EPSD ) -
C     :                               NUM_UWTOD( VAL__EPSUW ) )
C            END IF
C            IF ( DHI .GT. 0.0D0 ) THEN
C               DHI = DHI + 0.5D0 * ( NUM_DTOD( VAL__EPSD ) -
C     :                               NUM_UWTOD( VAL__EPSUW ) )
C            ELSE
C               DHI = DHI + 0.5D0 * NUM_DTOD( VAL__EPSD )
C            END IF
C
*  Convert the limits to the input data type and note they have been
*  set up.
            LO = NUM_DTOUW( DLO )
            HI = NUM_DTOUW( DHI )
            FIRST = .FALSE.
         END IF
      END IF

*  Check status.  Return the function result VAL__BADD if not OK.
      IF( STATUS .NE. SAI__OK ) THEN
         VAL_UWTOD = VAL__BADD

*  If the bad data flag is set, check if the argument given is bad.
*  Return VAL__BADD if it is.
      ELSE IF( BAD .AND. ( ARG .EQ. VAL__BADUW ) ) THEN
         VAL_UWTOD = VAL__BADD

*  If the conversion can potentially fail, then test if the argument
*  value lies within its allowed bounds.  If not, then return the value
*  VAL__BADD and set a STATUS value.
      ELSE IF( ( .FALSE. ) .AND.
     :         ( NUM_LTUW( ARG, LO ) .OR. NUM_GTUW( ARG, HI ) ) ) THEN
         VAL_UWTOD = VAL__BADD
         STATUS = SAI__OK

*  Otherwise, perform data conversion.
      ELSE
         VAL_UWTOD = NUM_UWTOD( ARG )
      ENDIF

*  Exit routine.
      END
