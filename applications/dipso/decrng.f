      SUBROUTINE DECRNG( TEXT, MINVAL, MAXVAL, BOT, TOP, STATUS )
*+
*  Name:
*     DECRNG

*  Purpose:
*     Read upper and lower limits from a supplied range specifier.
*     Only works for positive integer ranges.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DECRNG( TEXT, MINVAL, MAXVAL, BOT, TOP, STATUS )

*  Description:
*     The general form of the range specifier is 'A-B' where A is the
*     lower limit and B is the upper limit. If A is missing, the
*     supplied minimum value is ued, if B is missing the supplied
*     maximum value is used. If the string consists of a single number
*     (A or B) without any minus sign, the supplied value is used for
*     both limits.  If the supplied range goes outside the range
*     specified by MINVAL and MAXVAL, then an error is reported.  The
*     returned limits are order so that TOP is always greater than or
*     equal to BOT.

*  Arguments:
*     TEXT = CHARACTER * ( * ) (Given)
*        The text containing the range specifier.
*     MINVAL = INTEGER (Given)
*        The minimum allowed value.
*     MAXVAL = INTEGER (Given)
*        The maximum allowed value.
*     BOT = INTEGER (Returned)
*        The lower limit of the range.
*     TOP = INTEGER (Returned)
*        The upper limit of the range.
*     STATUS = INTEGER (Given and Returned)
*        Inherited global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-AUG-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard Starlink constants

*  Arguments Given:
      CHARACTER TEXT*(*)
      INTEGER MINVAL
      INTEGER MAXVAL

*  Arguments Returned:
      INTEGER BOT
      INTEGER TOP

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER
     :        MINUS,             ! Index of the minus sign
     :        NEWBOT,            ! Limited lower limit
     :        NEWTOP,            ! Limited upper limit
     :        TMP                ! Temporary storage

*.

*  Check the global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Look for a minus sign in the text.
      MINUS = INDEX( TEXT, '-' )

*  If found...
      IF( MINUS .NE. 0 ) THEN

*  If the minus is the first character, or if the string in front of the
*  minus is blank, use the minimum value for the lower limit.
         IF( MINUS .EQ. 1 ) THEN
            BOT = MINVAL

         ELSE IF( TEXT( : MINUS - 1 ) .EQ. ' ' ) THEN
            BOT = MINVAL

*  Otherwise, attempt to convert the string infront of the minus sign
*  into an integer value.
         ELSE
            CALL CHR_CTOI( TEXT( : MINUS - 1 ), BOT, STATUS )

         END IF

*  If the minus is the last character, or if the string after the
*  minus is blank, use the maximum value for the upper limit.
         IF( MINUS .EQ. LEN( TEXT ) ) THEN
            TOP = MAXVAL

         ELSE IF( TEXT( MINUS + 1 : ) .EQ. ' ' ) THEN
            TOP = MAXVAL

*  Otherwise, attempt to convert the string aafter the minus sign into
*  an integer value.
         ELSE
            CALL CHR_CTOI( TEXT( MINUS + 1 : ), TOP, STATUS )

         END IF

*  If there is no minus sign in the string...
      ELSE

*  If the string is not blank attempt to convert it into an integer value.
*  Use the same value for both limits.
         IF( TEXT .NE. ' ' ) THEN
            CALL CHR_CTOI( TEXT, TOP, STATUS )
            BOT = TOP

*  If the string is blank, report an error.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'DECRNG_ERR1', 'Blank range specifier given.',
     :                    STATUS )

         END IF

      END IF

*  If no errors have occurred, check the limit values.
      IF( STATUS .EQ. SAI__OK ) THEN

*  Ensure the limits are the right way round.
         IF( TOP .LT. BOT ) THEN
            TMP = TOP
            TOP = BOT
            BOT = TMP
         END IF

*  Report an error if the obtained range goes outside the allowed range,
*  and limit the obtained range.
         IF( TOP .GT. MAXVAL .OR. TOP .LT. MINVAL .OR.
     :       BOT .GT. MAXVAL .OR. BOT .LT. MINVAL ) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MIN', MINVAL )
            CALL MSG_SETI( 'MAX', MAXVAL )
            CALL MSG_SETI( 'BOT', BOT )
            CALL MSG_SETI( 'TOP', TOP )
            CALL ERR_REP( 'DECRNG_ERR2', 'The supplied range ^BOT-'//
     :                    '^TOP goes outside the allowed range '//
     :                    '^MIN-^MAX.', STATUS )

         END IF

      END IF

      END
