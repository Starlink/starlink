*+  ARX_READ - read ARD text from file or environment
      SUBROUTINE ARX_READ(PAR,ID,STATUS)
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
      INCLUDE 'PAR_ERR'
      INCLUDE 'GRP_PAR'
      INCLUDE 'FIO_ERR'
*    Global Variables :
*    Import :
      CHARACTER*(*) PAR
*    Import-Export :
*    Export :
      INTEGER ID
*    Status :
      INTEGER STATUS
*    External references :
*    Functions :
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      CHARACTER*132 ARDIN
      CHARACTER*1 LC
      INTEGER L
      INTEGER FID
      INTEGER INDEX
      LOGICAL FILE
      LOGICAL THERE
      LOGICAL CONT
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  get ARD input
        CALL USI_GET0C(PAR,ARDIN,STATUS)

*  look for indirection symbol
        IF (ARDIN(1:1).EQ.'^') THEN
          FILE=.TRUE.
          ARDIN=ARDIN(2:)
          INQUIRE(FILE=ARDIN,EXIST=THERE)
          IF (.NOT.THERE) THEN
            CALL MSG_SETC('FIL',ARDIN)
            CALL MSG_PRNT('AST_ERR: file ^FIL does not exist')
            STATUS=SAI__ERROR
          ENDIF
        ELSE
*  if not see if there is a file of that name
          INQUIRE(FILE=ARDIN,EXIST=THERE)
          FILE=THERE
        ENDIF

        IF (STATUS.EQ.SAI__OK) THEN

          CALL GRP_NEW('ARD input',ID,STATUS)

          IF (FILE) THEN
*  read from file
            CALL FIO_OPEN(ARDIN,'READ','NONE',0,FID,STATUS)
            INDEX=0
            DO WHILE (STATUS.EQ.SAI__OK)
              CALL FIO_READF(FID,ARDIN,STATUS)
              IF (STATUS.EQ.SAI__OK) THEN
                INDEX=INDEX+1
                CALL GRP_PUT(ID,1,ARDIN,INDEX,STATUS)
              ENDIF
            ENDDO
            IF (STATUS.EQ.FIO__EOF) THEN
              CALL ERR_ANNUL(STATUS)
            ENDIF
            CALL FIO_CLOSE(FID,STATUS)

*  read from environment
          ELSE
            INDEX=1
            CONT=.TRUE.
            DO WHILE (CONT.AND.STATUS.EQ.SAI__OK)
              L=CHR_LEN(ARDIN)
              LC=ARDIN(L:L)
              IF (LC.EQ.'-'.OR.LC.EQ.'~'.OR.LC.EQ.'\') THEN
                CONT=.TRUE.
                ARDIN(L:L)=' '
              ELSE
                CONT=.FALSE.
              ENDIF
              CALL GRP_PUT(ID,1,ARDIN,INDEX,STATUS)
              IF (CONT) THEN
                INDEX=INDEX+1
                CALL USI_CANCL(PAR,STATUS)
                CALL USI_GET0C(PAR,ARDIN,STATUS)
              ENDIF
            ENDDO
          ENDIF

        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_READ',STATUS)
        ENDIF

      ENDIF

      END
