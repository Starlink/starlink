*+  GMD_CREMULT - create multiple dataset
      SUBROUTINE GMD_CREMULT(LOC,N,STATUS)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*    Import :
      CHARACTER*(DAT__SZLOC) LOC
      INTEGER N
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL DAT_NEW(LOC,'NDF','NDF',1,N,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMD_CREMULT',STATUS)
        ENDIF
      ENDIF

      END
