      SUBROUTINE ECH_OBJECT_LIMITS( PROFILE, SUBSTEPS, SKY_LOLIM,
     :           MIN_INTEN, MAX_INTEN, Y_PEAK_INDEX,
     :           OBJ_ABOVE, OBJ_BELOW, DEK_ABOVE, DEK_BELOW,
     :           SKY_ABOVE, SKY_BELOW, STATUS )
*+
*  Name:
*     ECHOMOP - ECH_OBJECT_LIMITS

*  Purpose:
*     Determine or set object limits based on supplied profile.

*  Authors:
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     09-SEP-1996 (MJC):
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
      INTEGER SUBSTEPS
      REAL MIN_INTEN
      REAL MAX_INTEN
      REAL SKY_LOLIM
      INTEGER Y_PEAK_INDEX
      INTEGER DEK_ABOVE
      INTEGER DEK_BELOW

      INTEGER OBJ_ABOVE
      INTEGER OBJ_BELOW

* Arguments Returned:
      INTEGER SKY_ABOVE
      INTEGER SKY_BELOW

* Status:
      INTEGER STATUS

*  Local Variables:
      REAL THRESHOLD

      INTEGER I
      INTEGER ISIZE
      INTEGER NCHAR1

      LOGICAL FOUNDL
      LOGICAL FOUNDU

      CHARACTER*8 REF_STR1
*.

*  Find intensity at MODE point in profile.
      ISIZE = ( DEK_ABOVE - DEK_BELOW + 1 ) * SUBSTEPS
      CALL ECH_MEAN_MEDIAN( ISIZE, PROFILE( DEK_BELOW * SUBSTEPS ),
     :     .TRUE., .TRUE., THRESHOLD, STATUS  )

*  Determine threshold from intensity at mode and user-defined limits.
      THRESHOLD = MIN( THRESHOLD * 1.05,
     :   MIN_INTEN + ( MAX_INTEN - MIN_INTEN ) * SKY_LOLIM )

*  Adopt user-defined values if any.
      IF ( ABS( OBJ_BELOW ) .GT. 0 .OR.
     :     ABS( OBJ_ABOVE ) .GT. 0 ) THEN
         SKY_BELOW = - OBJ_BELOW - 1
         SKY_ABOVE = OBJ_ABOVE + 1
         FOUNDU = .TRUE.
         FOUNDL = .TRUE.
         CALL ECH_REPORT( 0,
     :        ' Setting object limits to user-supplied values.' )
         GO TO 100
      END IF

*  Loop stepping away from trace pixel-by-pixel until intensity drops
*  below threshold.
      FOUNDU = .FALSE.
      FOUNDL = .FALSE.
      I = Y_PEAK_INDEX
      DO WHILE ( I .GT. DEK_BELOW * SUBSTEPS .AND. .NOT. FOUNDL )
         IF ( PROFILE( I ) .LT. THRESHOLD ) THEN
            FOUNDL = .TRUE.
            SKY_BELOW = ( I - SUBSTEPS / 2 ) / SUBSTEPS  - 1
         END IF
         I = I - 1
      END DO
      I = Y_PEAK_INDEX
      DO WHILE ( I .LT. DEK_ABOVE * SUBSTEPS .AND. .NOT. FOUNDU )
         IF ( PROFILE( I ) .LT. THRESHOLD ) THEN
            FOUNDU = .TRUE.
            SKY_ABOVE = ( I + SUBSTEPS / 2 ) / SUBSTEPS + 1
         END IF
         I = I + 1
      END DO

*  Set object limits to edges of dekker if not found.
      IF ( .NOT. FOUNDL ) THEN
         SKY_BELOW = DEK_BELOW
      END IF
      IF ( .NOT. FOUNDU ) THEN
         SKY_ABOVE = DEK_ABOVE
      END IF

*  Report results.
  100 CONTINUE
      CALL CHR_ITOC( -SKY_BELOW - 1, REF_STR1, NCHAR1 )
      IF ( .NOT. FOUNDL ) THEN
         REPORT_STRING =
     :         ' Could not locate lower object-limit,' //
     :         ' set to ' // REF_STR1( :NCHAR1 ) //
     :         ' pixels below trace.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         CALL ECH_SET_CONTEXT( 'PROBLEM', 'Bad sky below' )

      ELSE
         REPORT_STRING = ' Lower object-limit' //
     :         ' set to ' // REF_STR1( :NCHAR1 ) //
     :         ' pixels below trace.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF

      CALL CHR_ITOC( SKY_ABOVE, REF_STR1, NCHAR1 )
      IF ( .NOT. FOUNDU ) THEN
         REPORT_STRING =
     :         ' Could not locate upper object-limit,' //
     :         ' set to ' // REF_STR1( :NCHAR1 ) //
     :         ' pixels above trace.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         CALL ECH_SET_CONTEXT( 'PROBLEM', 'Bad sky above' )

      ELSE
         REPORT_STRING = ' Upper object-limit' //
     :         ' set to ' // REF_STR1( :NCHAR1 ) //
     :         ' pixels above trace.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF

      END
