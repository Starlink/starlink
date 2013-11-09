*+  ARX_COPY - copy ARD text from one group to another
      SUBROUTINE ARX_COPY(INID,INDEXIN,OUTID,INDEXOUT,STATUS)
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
      INTEGER INID,INDEXIN
*    Import-Export :
*    Export :
      INTEGER OUTID,INDEXOUT
*    Status :
      INTEGER STATUS
*    External references :
*    Functions :
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      CHARACTER*132 TEXT
      INTEGER L
      INTEGER I
      INTEGER SIZE
      INTEGER INDEX
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL GRP_GRPSZ(INID,SIZE,STATUS)

        INDEX=INDEXOUT
        DO I=INDEXIN,SIZE
          CALL GRP_GET(INID,I,1,TEXT,STATUS)
          L=CHR_LEN(TEXT)
          CALL GRP_PUT(OUTID,1,TEXT,INDEX,STATUS)
          IF (INDEX.GT.0) THEN
            INDEX=INDEX+1
          ENDIF
        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_COPY',STATUS)
        ENDIF

      ENDIF

      END
