*+  ARX_WRITEF - write ARD text from group to given file
      SUBROUTINE ARX_WRITEF(GRPID,FID,STATUS)
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
      INTEGER FID
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
      LOGICAL THERE
*-
      IF (STATUS.EQ.SAI__OK) THEN


        CALL GRP_GRPSZ(GRPID,SIZE,STATUS)

        DO I=1,SIZE
          CALL GRP_GET(GRPID,I,1,ARDOUT,STATUS)
          L=CHR_LEN(ARDOUT)
          CALL FIO_WRITE(FID,ARDOUT(:L),STATUS)
        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_WRITEF',STATUS)
        ENDIF

      ENDIF

      END
