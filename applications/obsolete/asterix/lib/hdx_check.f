*+  HDX_CHECK - check existence and state of named component
      SUBROUTINE HDX_CHECK(LOC,NAME,THERE,SET,STATUS)
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(*) LOC
      CHARACTER*(*) NAME
*    Import-Export :
*    Export :
      LOGICAL THERE,SET
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*(DAT__SZLOC) CLOC
*-
      THERE=.FALSE.
      SET=.FALSE.

      IF (STATUS.EQ.SAI__OK) THEN
        CALL DAT_THERE(LOC,NAME,THERE,STATUS)
        IF (THERE) THEN
          CALL DAT_FIND(LOC,NAME,CLOC,STATUS)
          CALL DAT_STATE(CLOC,SET,STATUS)
          CALL DAT_ANNUL(CLOC,STATUS)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from HDX_CHECK',STATUS)
        ENDIF

      ENDIF

      END
