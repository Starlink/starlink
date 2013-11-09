*+ ARX_BOX - write box description
      SUBROUTINE ARX_BOX(ARDID,INDEX,MODE,EXCLUDE,XC,YC,XWID,YWID,
     :                                                      STATUS)
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
      REAL XC,YC,XWID,YWID
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
          TEXT=' '
          L=2
        ELSEIF (MODE.EQ.'XOR') THEN
          TEXT=' .XOR.'
          L=7
        ELSEIF (MODE.EQ.'EQV') THEN
          TEXT=' .EQV.'
          L=7
        ELSE
          TEXT=' '
          L=2
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (BOX( '
        ELSE
          TEXT(L:)=' BOX( '
        ENDIF
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XC)
        CALL MSG_SETR('YC',YC)
        CALL MSG_SETR('XW',XWID)
        CALL MSG_SETR('YW',YWID)
        CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^XW , ^YW )',TEXT,L)
        IF (EXCLUDE) THEN
          L=L+1
          TEXT(L:L)=')'
        ENDIF

        CALL ARX_PUT(ARDID,INDEX,TEXT(:L),STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_BOX',STATUS)
        ENDIF

      ENDIF

      END
