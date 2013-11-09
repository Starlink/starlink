*+  GMD_QNDF - returns number of NDFs in multiple dataset
      SUBROUTINE GMD_QNDF(LOC,N,STATUS)

*    Description :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) LOC           ! locator to dataset
*    Import-export :
*    Export :
      INTEGER N				   ! number of NDFs
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      LOGICAL THERE
*-

      N=0

      IF (STATUS.EQ.SAI__OK) THEN

        CALL DAT_THERE(LOC,'NDF',THERE,STATUS)
        IF (THERE) THEN
          CALL CMP_SIZE(LOC,'NDF',N,STATUS)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMD_QNDF',STATUS)
        ENDIF

      ENDIF
      END
