*+  TASK_DEC0L - decode a character string as a value
      SUBROUTINE TASK_DEC0L ( STRING, LVAL, STATUS )
 
*    Description :
*     Convert the given character string into a value of type
*     LOGICAL and return it in LVAL.
*     A routine exists for each type C, D, L, I, R.
 
*    Invocation :
*     CALL TASK_DEC0L ( STRING, LVAL, STATUS )
 
*    Parameters :
*     STRING=CHARACTER*(*) (given)
*           the string to be decoded
*     LVAL=LOGICAL (returned)
*           the returned value
*     STATUS=INTEGER
 
*    Method :
*     Use CHR_CTOL.
 
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
*      6.09.1993:  remove hagover from GENERIC system (RLVAD::AJC)
 
*    Type Definitions :
      IMPLICIT NONE
 
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'TASK_ERR'
 
*    Import :
      CHARACTER*(*) STRING  ! the character string to be decoded
 
*    Export :
      LOGICAL LVAL         ! the returned value
 
*    Status :
      INTEGER STATUS
 
*    Local variables :
 
*-
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*   Use appropriate CHR routine
      CALL CHR_CTOL( STRING, LVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL EMS_SETC( 'STR', STRING )
         CALL ERR_REP( 'TSK_DEC0L1',
     :   'TASK_DEC0L: Failed to convert ^STR to LOGICAL',
     :    STATUS )
      ENDIF
 
      END
