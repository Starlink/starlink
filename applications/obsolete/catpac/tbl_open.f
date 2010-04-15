      SUBROUTINE TBL_OPEN( TBNAME, MODE, TBDSCR, STATUS )

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_ERR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'TBL_PAR'

*  Arguments Given and Returned:
      CHARACTER*(*) TBDSCR
      CHARACTER*(*) MODE
      CHARACTER*(*) TBNAME

*  Status:
      INTEGER STATUS

*  Local Variables:

*.
      IF (STATUS .NE. SAI__OK) RETURN

      CALL HDS_OPEN(TBNAME, MODE, TBDSCR, STATUS)
      IF (STATUS .NE. SAI__OK) THEN
          STATUS = TBL__ECRTAB
          CALL ERR_REP(' ','Error while opening table',STATUS)
          GOTO 9999
      ENDIF

9999  RETURN
      END
