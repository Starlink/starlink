*+ ARX_SLICE - write definition of slice
      SUBROUTINE ARX_SLICE(ARDID,INDEX,MODE,EXCLUDE,
     :                        XC,YC,LENGTH,WIDTH,ANGLE,STATUS)
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
      INTEGER INDEX
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
      REAL XC,YC,ANGLE,LENGTH,WIDTH
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*80 TEXT
      INTEGER L
*-

      IF (STATUS.EQ.SAI__OK) THEN


        IF (MODE.EQ.'AND') THEN
          TEXT=' .AND.'
          L=7
        ELSEIF (MODE.EQ.'OR'.OR.MODE.EQ.'ADD') THEN
          TEXT=' .OR.'
          L=6
        ELSEIF (MODE.EQ.'XOR') THEN
          TEXT=' .XOR.'
          L=7
        ELSEIF (MODE.EQ.'EQV') THEN
          TEXT=' .EQV.'
          L=7
        ELSE
          TEXT=' '
          L=1
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (ROTBOX( '
        ELSE
          TEXT(L:)=' ROTBOX( '
        ENDIF
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XC)
        CALL MSG_SETR('YC',YC)
        CALL MSG_SETR('LN',LENGTH)
        CALL MSG_SETR('WD',WIDTH)
        CALL MSG_SETR('AN',ANGLE)
        CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^LN , ^WD , ^AN ',
     :                                                    TEXT,L)
        IF (EXCLUDE) THEN
          TEXT(L:)='))'
          L=L+1
        ELSE
          TEXT(L:L)=')'
        ENDIF

        CALL ARX_PUT(ARDID,INDEX,TEXT(:L),STATUS)



        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_SLICE',STATUS)
        ENDIF

      ENDIF

      END
