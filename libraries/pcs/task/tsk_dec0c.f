*+  TASK_DEC0C - decode a character string as a value
      SUBROUTINE TASK_DEC0C ( STRING, CVAL, STATUS )
 
*    Description :
*     Copy a given character string into another string, checking for
*     truncation but ignoring trailing spaces
 
*    Invocation :
*     CALL TASK_DEC0C ( STRING, CVAL, STATUS )
 
*    Parameters :
*     STRING=CHARACTER*(*) (given)
*           the string to be decoded
*     CVAL=CHARACTER*(*) (returned)
*           the returned value
*     STATUS=INTEGER
 
*    Method :
*     Use CHR_COPY which checks for truncation/
 
*    Deficiencies :
*     <description of any deficiencies>
 
*    Bugs :
*     <description of any "bugs" which have not been fixed>
 
*    Authors :
*     W.F.Lupton (AAOEPP::WFL)
*     A J Chpperifeld (RLVAD::AJC)
 
*    History :
*     29.04.1989:  original (AAOEPP::WFL)
*      4.10.1992:  use CHR for portability (RLVAD::AJC)
 
*    Type Definitions :
      IMPLICIT NONE
 
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'TASK_ERR'
 
*    Import :
      CHARACTER*(*) STRING  ! the character string to be decoded
 
*    Export :
      CHARACTER*(*) CVAL         ! the returned value
 
*    Status :
      INTEGER STATUS
 
*    Local variables :
      INTEGER ISTAT         ! local (CHR) status
 
*-
 
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
