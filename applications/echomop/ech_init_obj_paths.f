      SUBROUTINE ECH_INIT_OBJ_PATHS( FILE_SPEC, STATUS )
*+
*  Name:
*     ECHOMOP - ECH_INIT_OBJ_PATHS

*  Purpose:
*     Load paths to objects in the reduction database.

*  Invocation:
*     CALL ECH_INIT_OBJ_PATHS( FILE_SPEC, STATUS )

*  Arguments:
*     FILE_SPEC = CHARACTER*( * ) (Unknown)
*
*     STATUS = INTEGER (Given and Returned)
*        Global inherited status(?)

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     ??-???-???? (DMILLS):
*       Initial release.
*     22-APR-1996 (MJC):
*       Added prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_USE_RDCTN.INC'

*  Arguments:
      CHARACTER*( * ) FILE_SPEC

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I
      INTEGER I_START
      INTEGER I_END
      INTEGER LUN
      INTEGER NUM_PATHS
      INTEGER NUM_SW_REG_VARS

      LOGICAL LIVE

      CHARACTER*80 SW_OBJ_DIMEN_VARS( MAX_REG_VARS )
      CHARACTER*132 RECORD
      CHARACTER*132 OPENED_NAME
      CHARACTER*1 ACHAR

*.
      STATUS = 0
      CALL ECH_OPEN_FILE( FILE_SPEC, 'TEXT', 'OLD', .FALSE., LUN,
     :     OPENED_NAME, STATUS )
      IF ( STATUS .NE. 0 ) THEN
         GO TO 999
      END IF

      NUM_SW_REG_VARS = 0
      IF ( NUM_REG_VARS .GT. 0 ) THEN
         NUM_SW_REG_VARS = NUM_REG_VARS
         DO I = 1, NUM_SW_REG_VARS
            SW_OBJ_DIMEN_VARS( I ) = OBJ_DIMEN_VARS( I )
            OBJ_DIMEN_VARS( I ) = ' '
         END DO
      END IF
      NUM_REG_VARS = 0
      NUM_PATHS = 0
      LIVE = .TRUE.
      DO WHILE ( ( NUM_PATHS .LT. MAX_OBJECTS ) .AND. LIVE )
         ACHAR = OBJECT_REF_NAME( NUM_PATHS+1 )( 1 : 1 )
         IF ( ( ACHAR .GE. 'A' .AND. ACHAR .LE. 'Z' ) .OR.
     :        ( ACHAR .GE. '0' .AND. ACHAR .LE. '9' ) ) THEN
            NUM_PATHS = NUM_PATHS + 1

         ELSE
            LIVE = .FALSE.
         END IF
      END DO

 1    CONTINUE

*  Read a line from the definition file.
      READ ( LUN, '( A )', END = 999 ) RECORD
      IF ( RECORD .EQ. ' ' ) THEN
         GO TO 1
      END IF

*  Remove leading spaces.
      I = 1
      DO WHILE ( RECORD( I: I ) .EQ. ' ' )
         I = I + 1
      END DO
      IF ( I .NE. 1 ) THEN
         RECORD = RECORD( I: )
      END IF

*  Fold to upper case.
      CALL CHR_UCASE( RECORD )

*  Extract variable name.
      IF ( record( :31 ) .EQ. '**REGISTERED INDEX VARIABLE => ' ) THEN
         NUM_REG_VARS = NUM_REG_VARS + 1
         OBJ_DIMEN_VARS( NUM_REG_VARS ) = RECORD( 32: )

*  Extract path to item in reduction database.
      ELSE IF ( RECORD( :21 ) .EQ. '**REGISTERED PATH => ' ) THEN

*  Find item name.
         I = 22
         DO WHILE ( RECORD( I:I ) .EQ. ' ' )
            I = I + 1
         END DO
         I_START = I
         DO WHILE ( RECORD( I:I ) .NE. ' ' )
            I = I + 1
         END DO
         I_END = I - 1
         NUM_PATHS = NUM_PATHS + 1
         OBJECT_REF_NAME( NUM_PATHS ) = RECORD( I_START:I_END )

*  Find item path.
         DO WHILE ( RECORD( I:I ) .EQ. ' ' )
            I = I + 1
         END DO
         I_START = I
         DO WHILE ( RECORD( I:I ) .NE. ' ' )
            I = I + 1
         END DO
         I_END = I - 1
         OBJECT_PATH( NUM_PATHS ) = RECORD( I_START:I_END )
      END IF
      GO TO 1
 999  CONTINUE

      CALL ECH_OPEN_FILE( FILE_SPEC, 'CLOSE', 'CLOSE', .FALSE., LUN,
     :     OPENED_NAME, STATUS )

      DO I = 1, NUM_SW_REG_VARS
         NUM_REG_VARS = NUM_REG_VARS + 1
         OBJ_DIMEN_VARS( NUM_REG_VARS ) = SW_OBJ_DIMEN_VARS( I )
         SW_OBJ_DIMEN_VARS( I ) = ' '
      END DO

      STATUS = 0

      END
