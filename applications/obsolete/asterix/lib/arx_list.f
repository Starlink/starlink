*+  ARX_LIST - list ARD text
      SUBROUTINE ARX_LIST(GRPID,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global Variables :
*    Import :
      INTEGER GRPID
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Functions :
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      CHARACTER*132 ARDOUT
      INTEGER L
      INTEGER I
      INTEGER SIZE
*-
      IF (STATUS.EQ.SAI__OK) THEN


        CALL GRP_GRPSZ(GRPID,SIZE,STATUS)

        DO I=1,SIZE
          CALL GRP_GET(GRPID,I,1,ARDOUT,STATUS)
          L=CHR_LEN(ARDOUT)
          CALL MSG_PRNT(ARDOUT(:L))
        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_LIST',STATUS)
        ENDIF

      ENDIF

      END
