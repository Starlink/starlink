      LOGICAL FUNCTION PARPOS( PAR, POS )
*+
*  Name:
*     PARPOS

*  Purpose:
*     Tests if character string is a "position" (RA or DEC).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = PARPOS( PAR, POS )

*  Description:
*     This routine attempts to interpret the parameter string PAR as a
*     position. It returns the value .TRUE. if successful and .FALSE.
*     otherwise. On exit POS contains the position in seconds if PARPOS
*     is .TRUE. PARPOS does not check the position for range if it is a
*     right ascension but the declination limits are checked.  The RA
*     limits must be tested separately in the calling routine.

*  Arguments:
*     PAR = CHARACTER * ( * ) (Given)
*        The string which might be a "position" value.
*     POS = DOUBLE PRECISION (Returned)
*        The double precision value if conversion is achieved.

*  Returned Value:
*     PARPOS = LOGICAL
*        True if the value has been converted to a number of seconds
*        false otherwise.

*  Side Effects:
*     -  This function modifies the argument POS.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     19-MAY-1983 (?OLAF?):
*        Original version.
*     6-JUN-1994 (PDRAPER):
*        Added prologue and made platform independent.
*     6-DEC-1996 (PDRAPER):
*        Converted format to read STR buffer in one slurp
*        into two reads to work around a problem with a compiler
*        on OSF/1 fortran 4.0, RTL 3.69.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global constants:
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants (VAL__MAXD)

*  Arguments Given:
      CHARACTER * ( * ) PAR

*  Arguments Returned:
      DOUBLE PRECISION POS

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string
      EXTERNAL PARDBL
      LOGICAL PARDBL             ! Character to double precision

*  Local Variables:
      CHARACTER * ( 2 ) STR( 2 ) ! Degrees/minutes or hours/minutes
      CHARACTER * ( 12 ) DIGITS  ! Known digits
      DOUBLE PRECISION SECS      ! Number of seconds
      INTEGER IC                 ! Character counter
      INTEGER IAT                ! Current string position
      INTEGER J                  ! Degrees(1) minutes(2) or seconds
      INTEGER LP                 ! Length of input string
      LOGICAL OK                 ! Input string non-blank
      INTEGER I                  ! Loop variable
      REAL SGN                   ! Sign of value (+/-)
      INTEGER K                  ! Loop variable

*  Local data.
      DATA DIGITS / '+-0123456789' /

*.

*  Set up the initial values.
      PARPOS = .FALSE.
      POS = VAL__BADD
      STR( 1 ) = ' '
      STR( 2 ) = ' '
      IC = 0
      IAT = 1
      J = 1

*  Scan through par to locate the first non-space character.
      LP = CHR_LEN( PAR )
      OK = .FALSE.
      DO 1 I = 1, LP
         IF ( PAR( I : I ) .NE. ' ' ) THEN

*  First non-blank character.
            OK = . TRUE.
            GO TO 100
         END IF
         IAT = IAT + 1
 1    CONTINUE
 100  CONTINUE
      IF ( OK ) THEN

*  Check if the first character is a sign.
         IF ( PAR( IAT : IAT ) .EQ. '-' ) THEN
            SGN = -1.0
            IAT = IAT + 1
         ELSE IF ( PAR( IAT : IAT ) .EQ. '+' ) THEN
            SGN = 1.0
            IAT = IAT + 1
         ELSE
            SGN=1.0

*  Make an early exit if not a digit first
            IF ( INDEX( DIGITS, PAR( IAT : IAT ) ) .EQ. 0 ) GO TO 99
         ENDIF

*  Now scan through the remaining characters and load hm or dm into str.
         DO 3 K = IAT, LP
            IF ( INDEX( DIGITS, PAR( K : K ) ) .EQ. 0 ) THEN

*  Separator encountered, increment the dms counter.
               J = J + 1
               IF ( J .GT. 2 ) THEN

*  Start of the seconds part.
                  GO TO 1001
               END IF

*  Clear the character counter.
               IC = 0
            ELSE IF ( PAR( K : K ) .NE. ' ' ) THEN

*  Character other than a space, load it into STR.
               IC = IC + 1
               IF ( IC .GT. 2 ) THEN

*  String is too long, not a position.
                  GO TO 99
               END IF

*  Load into STR.
               STR( J )( IC : IC ) = PAR( K : K )
            ENDIF
 3       CONTINUE

*  Now attempt to read the seconds part as a double precision number.
 1001    CONTINUE
         IF ( K .GE. LP ) THEN

*  No seconds supplied.
            SECS = 0.0D0
         ELSE IF ( .NOT. PARDBL( PAR( K + 1 : ) , SECS ) ) THEN

*  Not a real number.
            GO TO 99
         ENDIF

*  Check the sign and magnitude of secs.
         IF ( SECS .LT. 0.0D0 .OR. SECS .GE. 60.0D0 ) GO TO 99

*  Now attempt to read the dm/hm in str as integers, exit if the
*  read fails.
*         READ( STR, 800, ERR = 99 ) IAT, J
* 800     FORMAT (BN, 2(I2:/) )
         READ( STR( 1 ), 800, ERR = 99 ) IAT
         READ( STR( 2 ), 800, ERR = 99 ) J
 800     FORMAT(BN,I2)

*  Test the dm/hm in IAT and J.
         IF ( J .LT. 0  .OR.  J .GE. 60 ) GO TO 99
         IF ( IAT .LT. 0  .OR.  IAT. GT. 90  .OR.
     :        IAT .EQ. 90  .AND.  ( J .NE. 0  .OR.  SECS .NE. 0.0D0 ) )
     :      GO TO 99

*  String is a position, return as seconds.
         POS = SIGN( DBLE ( IAT * 3600 + J * 60 ) + SECS, DBLE( SGN ) )
         PARPOS = .TRUE.

*  Exit with error label.
      END IF
 99   CONTINUE
      END
* $Id$
