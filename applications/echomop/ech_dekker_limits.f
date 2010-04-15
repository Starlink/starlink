      SUBROUTINE ECH_DEKKER_LIMITS( PROFILE, ORDER_SIZE, SUBSTEPS,
     :           MAX_INTEN, DEK_THRESH, MAX_SKY_PIX,
     :           OBJ_ABOVE, OBJ_BELOW, DEK_ABOVE, DEK_BELOW )
*+
*  Name:
*     ECHOMOP - ECH_DEKKER_LIMITS

*  Purpose:
*     Determine or set dekker limits based on supplied profile.

*  Authors:
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     06-SEP-1996 (MJC):
*       Initial version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'

*  Local Constants:
      INTEGER PROF_HWID
      PARAMETER ( PROF_HWID = 1024 )

*  Arguments Given:
      REAL PROFILE( -PROF_HWID : PROF_HWID )

      INTEGER ORDER_SIZE
      INTEGER SUBSTEPS

      REAL MAX_INTEN
      REAL DEK_THRESH

      INTEGER MAX_SKY_PIX
      INTEGER OBJ_ABOVE
      INTEGER OBJ_BELOW

*  Arguments Returned:
      INTEGER DEK_ABOVE
      INTEGER DEK_BELOW

*  Local Variables:
      REAL THRESHOLD

      INTEGER I
      INTEGER NCHAR1

      LOGICAL FOUNDL
      LOGICAL FOUNDU

      CHARACTER*8 REF_STR1
*.

*  Adopt user-defined values if any.
      IF ( ABS( OBJ_BELOW ) .GT. 0 .OR. ABS( OBJ_ABOVE ) .GT. 0 ) THEN
         DEK_BELOW = - OBJ_BELOW
         DEK_ABOVE = OBJ_ABOVE
         FOUNDU = .TRUE.
         FOUNDL = .TRUE.
         CALL ECH_REPORT( 0,
     :        ' Setting dekker limits to user-supplied values.' )
         GO TO 100
      END IF

*  Loop stepping away from trace pixel-by-pixel until intensity drops
*  below threshold.
      THRESHOLD = DEK_THRESH * MAX_INTEN
      FOUNDU = .FALSE.
      FOUNDL = .FALSE.
      I = 0
      DO WHILE ( I .LT. ORDER_SIZE * SUBSTEPS .AND.
     :           ( .NOT. FOUNDL .OR. .NOT. FOUNDU ) )
         IF ( PROFILE( -I ) .LT. THRESHOLD .AND. .NOT. FOUNDL ) THEN
            FOUNDL = .TRUE.
            DEK_BELOW = ( - I - SUBSTEPS / 2 ) / SUBSTEPS - 1
         END IF
         IF ( PROFILE( I ) .LT. THRESHOLD .AND. .NOT. FOUNDU ) THEN
            FOUNDU = .TRUE.
            DEK_ABOVE = ( I + SUBSTEPS / 2 ) / SUBSTEPS  + 1
         END IF
         I = I + 1
      END DO

*  Set dekker limits to edge of order if not found.
      IF ( .NOT. FOUNDL ) THEN
         DEK_BELOW = -ORDER_SIZE
      END IF
      IF ( .NOT. FOUNDU ) THEN
         DEK_ABOVE = ORDER_SIZE
      END IF

  100 DEK_BELOW = MAX( DEK_BELOW, -MAX_SKY_PIX / 2 )

*  Report results.
      CALL CHR_ITOC( -DEK_BELOW, REF_STR1, NCHAR1 )
      IF ( .NOT. FOUNDL ) THEN
         REPORT_STRING =
     :         ' Could not locate lower dekker-limit,' //
     :         ' set to ' // REF_STR1( :NCHAR1 ) //
     :         ' pixels below trace.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         CALL ECH_SET_CONTEXT( 'PROBLEM', 'Bad lower dekker' )

      ELSE
         REPORT_STRING = ' Lower dekker-limit' //
     :         ' set to ' // REF_STR1( :NCHAR1 ) //
     :         ' pixels below trace.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF

      DEK_ABOVE = MIN( DEK_ABOVE, MAX_SKY_PIX / 2 )
      CALL CHR_ITOC( DEK_ABOVE, REF_STR1, NCHAR1 )
      IF ( .NOT. FOUNDU ) THEN
         REPORT_STRING =
     :         ' Could not locate upper dekker-limit,' //
     :         ' set to ' // REF_STR1( :NCHAR1 ) //
     :         ' pixels above trace.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         CALL ECH_SET_CONTEXT( 'PROBLEM', 'Bad upper dekker' )

      ELSE
         REPORT_STRING = ' Upper dekker-limit' //
     :         ' set to ' // REF_STR1( :NCHAR1 ) //
     :         ' pixels above trace.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF

      END
