*+ ARX_INVERT - invert currrent region
      SUBROUTINE ARX_INVERT(ARDID,STATUS)
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
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER ID
*-

      IF (STATUS.EQ.SAI__OK) THEN


        CALL ARX_OPEN('WRITE',ID,STATUS)
        CALL ARX_PUT(ID,0,'.NOT.(',STATUS)
        CALL ARX_COPY(ARDID,1,ID,0,STATUS)
        CALL ARX_PUT(ID,0,')',STATUS)
        CALL ARX_CLOSE(ARDID,STATUS)
        ARDID=ID

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_INVERT',STATUS)
        ENDIF

      ENDIF

      END




*+ ARX_QSHAPE - enquire shape
      SUBROUTINE ARX_QSHAPE(ARDID,SHAPE,STATUS)
*    Description :
*      Analyses ARD text and picks out the simple shapes of
*      CIRCLE, ELLIPSE, BOX or ANNULUS - everything else is
*      returned as COMPLEX
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
*    Export :
      CHARACTER*(*) SHAPE
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
      INTEGER STR_OCCUR
*    Local constants :
*    Local variables :
      CHARACTER*132 TEXT
      REAL X1,X2,Y1,Y2,R1,R2
      INTEGER IL,NL
      INTEGER NCIRC,NELL,NBOX,NAND,NNOT
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GRP_SIZE(ARDID,NL,STATUS)
        NCIRC=0
        NELL=0
        NBOX=0
        NAND=0
        NNOT=0
        DO IL=1,NL
          CALL ARX_GET(ARDID,IL,TEXT,STATUS)
          CALL CHR_UCASE(TEXT)
          NCIRC=NCIRC+STR_OCCUR('CIRCLE',TEXT)
          NELL=NELL+STR_OCCUR('ELLIPSE',TEXT)
          NBOX=NBOX+STR_OCCUR('RECT',TEXT)
          NAND=NAND+STR_OCCUR('AND',TEXT)
          NNOT=NNOT+STR_OCCUR('NOT',TEXT)
        ENDDO

        IF (NCIRC.EQ.1) THEN
          SHAPE='CIRCLE'
        ELSEIF (NELL.EQ.1) THEN
          SHAPE='ELLIPSE'
        ELSEIF (NBOX.EQ.1) THEN
          SHAPE='BOX'
        ELSEIF (NCIRC.EQ.2.AND.NAND.EQ.1.AND.NNOT.EQ.1) THEN
          C1=1
          DO IL=1,NL
            CALL ARX_GET(ARDID,IL,TEXT(C1:),STATUS)
            C1=CHR_LEN(TEXT)+1
          ENDDO
          C1=INDEX(TEXT,'(')
          C2=INDEX(TEXT(C1:,')')
          READ(TEXT(C1+1:C2-1),*) X1,Y1,R1
          C1=INDEX(TEXT(C2:),'(')
          C2=INDEX(TEXT(C1:),')')
          READ(TEXT(C1+1:C2-1),*) X2,Y2,R2
          IF (R2.LT.R1.AND.X1.EQ.X2.AND.Y1.EQ.Y2) THEN
            SHAPE='ANNULUS'
          ELSE
            SHAPE='COMPLEX'
          ENDIF
        ELSE
          SHAPE='COMPLEX'
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_QSHAPE',STATUS)
        ENDIF

      ENDIF

      END
