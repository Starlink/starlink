*+  ARX_WRITE - write ARD text from group to file
      SUBROUTINE ARX_WRITE(PAR,ID,STATUS)
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
      CHARACTER*(*) PAR
      INTEGER ID
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
      INTEGER FID
      INTEGER L
      INTEGER I
      INTEGER SIZE
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  get output file name
        CALL USI_GET0C(PAR,ARDOUT,STATUS)

        CALL FIO_OPEN(ARDOUT,'WRITE','LIST',0,FID,STATUS)

        CALL GRP_GRPSZ(ID,SIZE,STATUS)

        DO I=1,SIZE
          CALL GRP_GET(ID,I,1,ARDOUT,STATUS)
          L=CHR_LEN(ARDOUT)
          CALL FIO_WRITE(FID,ARDOUT(:L),STATUS)
        ENDDO

        CALL FIO_CLOSE(FID,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_WRITE',STATUS)
        ENDIF

      ENDIF

      END
