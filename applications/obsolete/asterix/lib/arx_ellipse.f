*+ ARX_ELLIPSE - write description of ellipse
      SUBROUTINE ARX_ELLIPSE(ARDID,INDEX,MODE,EXCLUDE,
     :                        XC,YC,RMAJ,RMIN,ANGLE,STATUS)
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
      REAL XC,YC,ANGLE,RMAJ,RMIN
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*80 TEXT
      INTEEGER L
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
          TEXT(L:)=' .NOT. (ELLIPSE( '
        ELSE
          TEXT(L:)=' ELLIPSE( '
        ENDIF
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XC)
        CALL MSG_SETR('YC',YC)
        CALL MSG_SETR('LN',RMAJ)
        CALL MSG_SETR('WD',RMIN)
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
          CALL ERR_REP(' ','from ARX_ELLIPSE',STATUS)
        ENDIF

      ENDIF

      END
