*+ ARX_POLYGON - write polygon description
      SUBROUTINE ARX_POLYGON(ARDID,INDEX,MODE,EXCLUDE,NV,XV,YV,STATUS)
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
      INTEGER NV
      REAL XV(NV),YV(NV)
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
      INTEGER NVMAX
      PARAMETER (NVMAX=100)
*    Local variables :
      CHARACTER*80 TEXT
      INTEGER L
      INTEGER I
      INTEGER NPAIR
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
          TEXT(L:)=' .NOT. (POLYGON( '
        ELSE
          TEXT(L:)=' POLYGON( '
        ENDIF
        L=CHR_LEN(TEXT)
        CALL ARX_PUT(ARDID,INDEX,TEXT(:L),STATUS)
        TEXT = ' '
        L = 1
        NPAIR=0


        DO I=1,NV

*  write each vertex allowing for line continuation
          NPAIR=NPAIR+1
          CALL MSG_SETR( 'X', XV(I))
          CALL MSG_SETR( 'Y', YV(I))
          CALL MSG_MAKE( TEXT(:L)//' ^X , ^Y ,', TEXT, L )
          IF (NPAIR.EQ.3.AND.I.LT.NV) THEN
            CALL ARX_PUT(ARDID,0,TEXT(:L),STATUS)
            TEXT = ' '
            L = 1
            NPAIR=0
          ENDIF


        ENDDO

        IF (EXCLUDE) THEN
          TEXT(L:)='))'
          L=L+1
        ELSE
          TEXT(L:L)=')'
        ENDIF

        CALL ARX_PUT(ARDID,0,TEXT(:L),STATUS)



        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_POLYGON',STATUS)
        ENDIF

      ENDIF

      END
