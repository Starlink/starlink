*+ ARX_INVERT - invert currrent region
      SUBROUTINE ARX_INVERT(ARDID,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Import :
      INTEGER ARDID
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER ID
*-

      IF (STATUS.EQ.SAI__OK) THEN


        CALL ARX_OPEN('WRITE',ID,STATUS)
        CALL ARX_PUT(ID,0,'.NOT.(',STATUS)
        CALL ARX_COPY(ARDID,1,ID,0,STATUS)
        CALL ARX_PUT(ID,0,')',STATUS)
        CALL ARX_CLOSE(ARDID,STATUS)
        ARDID=ID

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_INVERT',STATUS)
        ENDIF

      ENDIF

      END
