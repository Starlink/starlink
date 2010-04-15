      SUBROUTINE IRA1_FTOL( TOL, NAME, NC, MAXDP, FIELDS, DP, PFLAGS,
     :                      STATUS )
*+
*  Name:
*     IRA1_FTOL

*  Purpose:
*     suppress the display of insignificant fields.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_FTOL( TOL, NAME, NC, MAXDP, FIELDS, DP, PFLAGS, STATUS )

*  Description:
*     This routine suppresses the display of trailing fields which are
*     insignificant compared with the supplied value of TOL. Trailing
*     fields are suppressed if a change of one in the next most
*     significant field value corresponds to an angular change greater
*     than the required tolerance. The value of leading fields are
*     modified in order to round the displayed value (rather than just
*     truncating the value).  The suppressionof fields is achieved by
*     setting flags indicating if each field should be displayed or
*     not.

*  Arguments:
*     TOL = DOUBLE PRECISION (Given)
*        The required tolerance, in radians.
*     NAME = CHARACTER * ( * ) (Given)
*        The full name of the SCS (without equinox).
*     NC = INTEGER (Given)
*        The axis index, 1 for longitude, or 2 for latitude.
*     MAXDP = INTEGER (Given)
*        The number of decimal places represented by the supplied value
*        of FIELDS( 4 ).
*     FIELDS( 4 ) = INTEGER (Given)
*        The four integer values of the displayed fields; hours,
*        minutes, seconds and fraction of a second, or degrees,
*        arc-minutes, arc-seconds and fraction of an arc-second.
*     DP = INTEGER (Returned)
*        The number of decimal places represented by the returned value
*        of FIELDS( 4 ). Undefined if PFLAGS( 4 ) is false.
*     PFLAGS( 5 ) = LOGICAL (Returned)
*        Flags indicating if each field should be printed or not.
*        Elements 1 to 4 correspond to fields 1 to 4, and element 5
*        corresponds to the sign character. Elements are set false to
*        suppress the printing of the corresponding field, and are set
*        true otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      DOUBLE PRECISION TOL
      CHARACTER NAME*(*)
      INTEGER NC
      INTEGER MAXDP

*  Arguments Given and Returned:
      INTEGER FIELDS( 4 )

*  Arguments Returned:
      INTEGER DP
      LOGICAL PFLAGS( 5 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL      LOGTOL           ! Log of tolerance in 4th field.
      INTEGER   MAX4             ! Maximum integer storable in fourth
                                 ! field, plus one.
      CHARACTER SIGN*1           ! Sign of the tolerance value.
      INTEGER   TFLD( 4 )        ! Tolerance fields corresponding to the
                                 ! coordinate value fields.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the maximum integer in the fourth field (plus 1).
      MAX4 = 10**MAXDP

*  Initially, assume all fields will be printed.
      PFLAGS( 1 ) = .TRUE.
      PFLAGS( 2 ) = .TRUE.
      PFLAGS( 3 ) = .TRUE.
      PFLAGS( 4 ) = .TRUE.
      PFLAGS( 5 ) = .TRUE.

*  If the supplied tolerance is a Right Ascension value, convert it to
*  four separate integer fields holding hours, minutes, seconds, and
*  fraction of a second. The fourth field represents MAXDP decimal
*  places and is the integer to the left of the decimal point.
      IF( NC .EQ. 1 .AND. NAME( : 10 ) .EQ. 'EQUATORIAL' ) THEN
         CALL SLA_DR2TF( MAXDP, TOL, SIGN, TFLD )

*  Do the same for non-RA values. Fields are degrees, arc-minutes,
*  arc-seconds, and fraction of an arc-second.
      ELSE
         CALL SLA_DR2AF( MAXDP, TOL, SIGN, TFLD )

      END IF

*  If the first field of the tolerance value is not zero, then only
*  the first field in the coordinate value is significant.
      IF( TFLD( 1 ) .GT. 0 ) THEN

*  Round the coordinate value to the nearest unit in the first field.
         IF( FIELDS( 4 ) .GT. MAX4/2 ) FIELDS( 3 ) = FIELDS( 3 ) + 1
         FIELDS( 4 ) = 0

         IF( FIELDS( 3 ) .GT. 30 ) FIELDS( 2 ) = FIELDS( 2 ) + 1
         FIELDS( 3 ) = 0

         IF( FIELDS( 2 ) .GT. 30 ) FIELDS( 1 ) = FIELDS( 1 ) + 1
         FIELDS( 2 ) = 0

*  Suppress the printing of all other fields (except the sign).
         PFLAGS( 2 ) = .FALSE.
         PFLAGS( 3 ) = .FALSE.
         PFLAGS( 4 ) = .FALSE.
         DP = 0

*  Otherwise, if the second field of the tolerance value is not zero,
*  then only the first and second fields in the coordinate value are
*  significant.
      ELSE IF( TFLD( 2 ) .GT. 0 ) THEN

*  Round the coordinate value to the nearest unit in the second field.
         IF( FIELDS( 4 ) .GT. MAX4/2 ) FIELDS( 3 ) = FIELDS( 3 ) + 1
         FIELDS( 4 ) = 0

         IF( FIELDS( 3 ) .GT. 30 ) FIELDS( 2 ) = FIELDS( 2 ) + 1
         FIELDS( 3 ) = 0

*  Suppress the printing of all other fields (except the sign).
         PFLAGS( 3 ) = .FALSE.
         PFLAGS( 4 ) = .FALSE.
         DP = 0

*  Otherwise, if the third field of the tolerance value is not zero,
*  then only the fourth field in the coordinate value is
*  insignificant.
      ELSE IF( TFLD( 3 ) .GT. 0 ) THEN

*  Round the coordinate value to the nearest unit in the third field.
         IF( FIELDS( 4 ) .GT. MAX4/2 ) FIELDS( 3 ) = FIELDS( 3 ) + 1
         FIELDS( 4 ) = 0

*  Suppress the printing of the fourth field.
         PFLAGS( 4 ) = .FALSE.
         DP = 0

*  Otherwise, if a fractional field is needed find the minimum number
*  of decimal places needed to give at least the required accuracy.
      ELSE IF( TFLD( 4 ) .GT. 0 ) THEN
         LOGTOL = MAXDP - LOG10( REAL( TFLD( 4 ) ) )
         DP = INT( LOGTOL )
         IF( LOGTOL .NE. REAL( DP ) ) DP = DP + 1
         DP = MAX( 0, MIN( MAXDP, DP ) )

*  Modify the fourth field so that it represent the returned number of
*  decimal places rather than MAXDP decimal places.
         FIELDS( 4 ) = NINT( ( 10.0**DP )*
     :                       REAL( FIELDS( 4 ) )/REAL( MAX4 ) )

*  If the tolerance was zero, print all fields with maximum accuracy.
      ELSE
         DP = MAXDP

      END IF

*  The above rounding could have resulted in a field having a value
*  greater than the maximum value which can be printed for the field.
*  For instance, an RA value of 2h 59m 31.8d with an RA tolerance of
*  0h 2m 0s will result in the field values 2h 60m 0.0s. The 60m field
*  in this case needs carrying forward to the hours field, producing
*  the correct value of 3h 0m 0.0s. First check the fractional seconds
*  field to ensure that its value does not extend beyond the number of
*  decimal places returned in DP. If it does, increment the whole
*  seconds field.
      IF( FIELDS( 4 ) .GE. 10**DP ) THEN
         FIELDS( 4 ) = FIELDS( 4 ) - 10**DP
         FIELDS( 3 ) = FIELDS( 3 ) + 1
      END IF

*  Now check that the whole seconds field does not exceed 59. If so
*  increment the minutes field.
      IF( FIELDS( 3 ) .GE. 60 ) THEN
         FIELDS( 3 ) = FIELDS( 3 ) - 60
         FIELDS( 2 ) = FIELDS( 2 ) + 1
      END IF

*  Now check that the minutes field does not exceed 59. If so
*  increment the hours/degrees field.
      IF( FIELDS( 2 ) .GE. 60 ) THEN
         FIELDS( 2 ) = FIELDS( 2 ) - 60
         FIELDS( 1 ) = FIELDS( 1 ) + 1
      END IF

*  If an hours field exceeds 24, reduce it by 24.
      IF( NC .EQ. 1 .AND. NAME( : 10 ) .EQ. 'EQUATORIAL' ) THEN
         IF( FIELDS( 1 ) .GT. 24 ) FIELDS( 1 ) = FIELDS( 1 ) - 24

*  If a degrees fields exceeds 360, reduce it by 360.
      ELSE
         IF( FIELDS( 1 ) .GT. 360 ) FIELDS( 1 ) = FIELDS( 1 ) - 360
      END IF

      END
