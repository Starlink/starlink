*+  GFX_CURSOR - get position and button from cursor
      SUBROUTINE GFX_CURSOR(X,Y,LEFT,RIGHT,STATUS)
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
*    Global variables :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*1 CH
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
            CALL PGCURSE(X,Y,CH)
            IF (CH.EQ.CHAR(0)) THEN
              CALL MSG_PRNT('AST_ERR: error obtaining cursor position')
              STATUS=SAI__ERROR
            ELSE
              LEFT=(CH.EQ.CHAR(32).OR.CH.EQ.CHAR(49))
              RIGHT=(CH.EQ.CHAR(13).OR.CH.EQ.CHAR(50))
            ENDIF
          ENDIF
        ENDIF

      ENDIF

      END
