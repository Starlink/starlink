*+  GFX_CURS - get position, button/key from cursor
      SUBROUTINE GFX_CURS(X,Y,LEFT,RIGHT,CH,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Import/export :
      REAL X,Y
*    Export :
      LOGICAL LEFT,RIGHT
      CHARACTER*(*) CH
*    Global variables :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      LOGICAL ACTIVE,CURSOR
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL GDV_STATUS(ACTIVE,STATUS)
        IF (.NOT.ACTIVE) THEN
          CALL MSG_PRNT('AST_ERR: no device open')
          STATUS=SAI__ERROR
        ELSE
          CALL GDV_CURSOR(CURSOR,STATUS)
          IF (.NOT.CURSOR) THEN
            CALL MSG_PRNT('AST_ERR: no cursor on this device')
            STATUS=SAI__ERROR
          ELSE
            CH=' '
            CALL PGCURSE(X,Y,CH)
            IF (CH.EQ.CHAR(0)) THEN
              CALL MSG_PRNT('AST_ERR: error obtaining cursor position')
              STATUS=SAI__ERROR
            ELSE
              LEFT=(CH.EQ.'1')
              RIGHT=(CH.EQ.'2')
              CALL CHR_UCASE(CH)
            ENDIF
          ENDIF
        ENDIF

      ENDIF

      END
