      SUBROUTINE TASK_DEC0C ( STRING, CVAL, STATUS )
*+
*  Name:
*     TASK_DEC0C

*  Purpose:
*     Decode a character string as a value

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_DEC0C ( STRING, CVAL, STATUS )

*  Description:
*     Copy a given character string into another string, checking for
*     truncation but ignoring trailing spaces

*  Arguments:
*     STRING=CHARACTER*(*) (given)
*           the string to be decoded
*     CVAL=CHARACTER*(*) (returned)
*           the returned value
*     STATUS=INTEGER

*  Algorithm:
*     Use CHR_COPY which checks for truncation/

*  Authors:
*     W.F.Lupton (AAOEPP::WFL)
*     A J Chpperifeld (RLVAD::AJC)
*     {enter_new_authors_here}

*  History:
*     29-APR-1989 (AAOEPP::WFL):
*        Original
*     04-OCT-1992 (RLVAD::AJC):
*        Use CHR for portability
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
 
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'TASK_ERR'
 
*  Arguments Given:
      CHARACTER*(*) STRING  ! the character string to be decoded
 
*  Arguments Returned:
      CHARACTER*(*) CVAL         ! the returned value
 
*  Status:
      INTEGER STATUS
 
*  Local Variables:
      INTEGER ISTAT         ! local (CHR) status
 
*.
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*   Copy with truncation check
      CALL CHR_COPY( STRING, .FALSE., CVAL, ISTAT )
 
*   If value not long enough for STRING - report error
      IF ( ISTAT .NE. 0 ) THEN
         STATUS = TASK__STRFL
         CALL EMS_SETC( 'STR', STRING )
         CALL ERR_REP( 'TSK_DEC0L2',
     :   'TASK_DEC0C: String ^STR overflowed output string',
     :   STATUS )
      ENDIF

      END
