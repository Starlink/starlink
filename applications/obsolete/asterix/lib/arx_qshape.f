*+ ARX_QSHAPE - enquire shape
      SUBROUTINE ARX_QSHAPE(ARDID,SHAPE,PAR,STATUS)
*    Description :
*      Analyses ARD text and picks out the simple shapes of
*      CIRCLE, ELLIPSE, BOX, ANNULUS(circular), ANNULARBOX
*      and ANNULARELLIPSE - everything else is returned as COMPLEX
*
*      The returned parameters are as follows:
*
*      CIRCLE 1 x-cent    BOX 1 x-cent    ELLIPSE 1 x-cent
*             2 y-cent        2 y-cent            2 y-cent
*             3 radius        3 w-width           3 angle
*                             4 y-width           4 semi-maj
*                                                 5 semi-min
*
*      ANNULUS 1 x-cent          ANNULARBOX 1 x-cent
*              2 y-cent                     2 y-cent
*              3 inner-radius               3 inner-x-width
*              4 outer-radius               4 inner-y-width
*                                           5 outer-xwidth
*                                           6 outer-ywidth
*
*      ANNULARELLIPSE 1 x-cent
*                     2 y-cent
*                     3 angle
*                     4 inner-semi-maj
*                     5 inner-semi-min
*                     6 outer-semi-maj
*                     7 outer-semi-min
*
*    Deficiencies :
*       Not totally robust yet.  It was written to cope with sort
*       information written by the likes of XRTSORT.
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
*    Import :
      INTEGER ARDID
*    Export :
      CHARACTER*(*) SHAPE
      REAL PAR(*)
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
      INTEGER STR_OCCUR
*    Local constants :
*    Local variables :
      CHARACTER*132 TEXT
      REAL X1,X2,Y1,Y2,R1,R2
      REAL XW1,XW2,YW1,YW2,XR1,XR2,YR1,YR2,A1,A2
      INTEGER IL,NL
      INTEGER C1,C2,N
      INTEGER NCIRC,NELL,NBOX,NAND,NNOT
      INTEGER ERROR
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  how many lines of text
        CALL GRP_GRPSZ(ARDID,NL,STATUS)
        NCIRC=0
        NELL=0
        NBOX=0
        NAND=0
        NNOT=0

*  scan lines and count occurences of keywords
        DO IL=1,NL
          CALL ARX_GET(ARDID,IL,TEXT,STATUS)
          CALL CHR_UCASE(TEXT)
          CALL CHR_RMBLK(TEXT)
          NCIRC=NCIRC+STR_OCCUR('CIRCLE',TEXT)
          NELL=NELL+STR_OCCUR('ELLIPSE',TEXT)
          NBOX=NBOX+STR_OCCUR('BOX',TEXT)
          NAND=NAND+STR_OCCUR('AND',TEXT)
          NNOT=NNOT+STR_OCCUR('NOT',TEXT)
        ENDDO

        ERROR=0
*  simple circle
        IF (NCIRC.EQ.1) THEN
          SHAPE='CIRCLE'
*  extract the numbers
          CALL ARX_QSHAPE_NUMBERS(TEXT,1,C1,C2,N,STATUS)
          IF (N.EQ.3) THEN
            READ(TEXT(C1:C2),*) PAR(1),PAR(2),PAR(3)
          ELSE
            ERROR=1
          ENDIF

*  simple ellipse
        ELSEIF (NELL.EQ.1) THEN
          SHAPE='ELLIPSE'
*  extract the numbers
          CALL ARX_QSHAPE_NUMBERS(TEXT,1,C1,C2,N,STATUS)
          IF (N.EQ.5) THEN
            READ(TEXT(C1:C2),*) X1,Y1,XR1,YR1,A1
            PAR(1)=X1
            PAR(2)=Y1
            PAR(3)=A1
            PAR(4)=XR1
            PAR(5)=YR1
          ELSE
            ERROR=1
          ENDIF

*  simple rectangular box
        ELSEIF (NBOX.EQ.1) THEN
          SHAPE='BOX'
*  extract the numbers
          CALL ARX_QSHAPE_NUMBERS(TEXT,1,C1,C2,N,STATUS)
          IF (N.EQ.4) THEN
            READ(TEXT(C1:C2),*) PAR(1),PAR(2),PAR(3),PAR(4)
          ELSE
            ERROR=1
          ENDIF

*  circular annulus
        ELSEIF (NCIRC.EQ.2.AND.NAND.EQ.1.AND.NNOT.EQ.1) THEN
          C1=1
*  concatenate into a single string
          DO IL=1,NL
            CALL ARX_GET(ARDID,IL,TEXT(C1:),STATUS)
            CALL CHR_RMBLK(TEXT(C1:))
            C1=CHR_LEN(TEXT)+1
          ENDDO
*  extract the numbers
          CALL ARX_QSHAPE_NUMBERS(TEXT,1,C1,C2,N,STATUS)
          IF (N.EQ.3) THEN
            READ(TEXT(C1:C2),*) X1,Y1,R1
            CALL ARX_QSHAPE_NUMBERS(TEXT,2,C1,C2,N,STATUS)
            IF (N.EQ.3) THEN
              READ(TEXT(C1:C2),*) X2,Y2,R2
              IF (R2.LT.R1.AND.X1.EQ.X2.AND.Y1.EQ.Y2) THEN
                SHAPE='ANNULUS'
                PAR(1)=X1
                PAR(2)=Y1
                PAR(3)=R2
                PAR(4)=R1
              ELSE
                SHAPE='COMPLEX'
              ENDIF

            ELSE
              ERROR=1
            ENDIF
          ELSE
            ERROR=1
          ENDIF

*  annular box
        ELSEIF (NBOX.EQ.2.AND.NAND.EQ.1.AND.NNOT.EQ.1) THEN
          C1=1
*  concatenate into a single string
          DO IL=1,NL
            CALL ARX_GET(ARDID,IL,TEXT(C1:),STATUS)
            CALL CHR_RMBLK(TEXT(C1:))
            C1=CHR_LEN(TEXT)+1
          ENDDO
*  extract the numbers
          CALL ARX_QSHAPE_NUMBERS(TEXT,1,C1,C2,N,STATUS)
          IF (N.EQ.4) THEN
            READ(TEXT(C1:C2),*) X1,Y1,XW1,YW1
            CALL ARX_QSHAPE_NUMBERS(TEXT,2,C1,C2,N,STATUS)
            IF (N.EQ.4) THEN
              READ(TEXT(C1:C2),*) X2,Y2,XW2,YW2
              IF (XW2.LT.XW1.AND.YW1.LT.YW2.AND.
     :                X1.EQ.X2.AND.Y1.EQ.Y2) THEN
                SHAPE='ANNULARBOX'
                PAR(1)=X1
                PAR(2)=Y1
                PAR(3)=XW2
                PAR(4)=YW2
                PAR(5)=XW1
                PAR(6)=YW2
              ELSE
                SHAPE='COMPLEX'
              ENDIF

            ELSE
              ERROR=1
            ENDIF

          ELSE
            ERROR=1
          ENDIF


*  annular ellipse
        ELSEIF (NELL.EQ.2.AND.NAND.EQ.1.AND.NNOT.EQ.1) THEN
          C1=1
*  concatenate into a single string
          DO IL=1,NL
            CALL ARX_GET(ARDID,IL,TEXT(C1:),STATUS)
            CALL CHR_RMBLK(TEXT(C1:))
            C1=CHR_LEN(TEXT)+1
          ENDDO
*  extract the numbers
          CALL ARX_QSHAPE_NUMBERS(TEXT,1,C1,C2,N,STATUS)
          IF (N.EQ.5) THEN
            READ(TEXT(C1:C2),*) X1,Y1,XR1,YR1,A1
            CALL ARX_QSHAPE_NUMBERS(TEXT,2,C1,C2,N,STATUS)
            IF (N.EQ.5) THEN
              READ(TEXT(C1:C2),*) X2,Y2,XR2,YR2,A2
              IF (XR2.LT.XR1.AND.YR1.LT.YR2.AND.X1.EQ.X2.AND.
     :                                Y1.EQ.Y2.AND.A1.EQ.A2) THEN
                SHAPE='ANNULARELLIPSE'
                PAR(1)=X1
                PAR(2)=Y1
                PAR(3)=A1
                PAR(4)=XR2
                PAR(5)=YR2
                PAR(6)=XR1
                PAR(7)=YR1
              ELSE
                SHAPE='COMPLEX'
              ENDIF

            ELSE
              ERROR=1
            ENDIF

          ELSE
            ERROR=1
          ENDIF

*  all other cases considered complex
        ELSE
          SHAPE='COMPLEX'
        ENDIF

*  report any problems
        IF (ERROR.NE.0) THEN
          CALL MSG_PRNT('AST_ERR: invalid ARD syntax')
          CALL MSG_PRNT(TEXT)
          STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ARX_QSHAPE',STATUS)
        ENDIF

      ENDIF

      END



*+ ARX_QSHAPE_NUMBERS
      SUBROUTINE ARX_QSHAPE_NUMBERS(TEXT,SET,CSTART,CEND,NUMS,STATUS)
*    Description :
*       Scans given string and picks out the requested set of numbers
*       by returning the start and stop of the part containing them
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
      CHARACTER*(*) TEXT
      INTEGER SET
*    Export :
      INTEGER CSTART,CEND,NUMS
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*1 C
      INTEGER I,L
      INTEGER ISET
      INTEGER NCOMMA
      LOGICAL NUMERIC,BRACKET
*-

      IF (STATUS.EQ.SAI__OK) THEN

        L=CHR_LEN(TEXT)

        I=1
        DO ISET=1,SET

          NUMS=0
*  locate start of sequence of numbers
          NUMERIC=.FALSE.
          DO WHILE (.NOT.NUMERIC.AND.I.LT.L)
            C=TEXT(I:I)
            IF (C.NE.'-'.AND.C.NE.'+'.AND.C.NE.'.'.AND.
     :                         (C.LT.'0'.OR.C.GT.'9')) THEN
              I=I+1
            ELSEIF (C.EQ.'.'.AND.I.LT.L) THEN
              C=TEXT(I+1:I+1)
              IF (C.GE.'0'.AND.C.LE.'9') THEN
                NUMERIC=.TRUE.
              ELSE
                I=I+1
              ENDIF
            ELSE
              NUMERIC=.TRUE.
            ENDIF
          ENDDO

*  if numbers detected look for delimiting bracket and count commas
          IF (NUMERIC) THEN
            CSTART=I
            NCOMMA=0
            BRACKET=.FALSE.
            DO WHILE (.NOT.BRACKET.AND.I.LE.L)
              C=TEXT(I:I)
              IF (C.EQ.',') THEN
                NCOMMA=NCOMMA+1
                I=I+1
              ELSEIF (C.EQ.')') THEN
                BRACKET=.TRUE.
              ELSE
                I=I+1
              ENDIF
            ENDDO
            IF (BRACKET) THEN
              CEND=I-1
              NUMS=NCOMMA+1
            ENDIF
          ENDIF

        ENDDO

      ENDIF

      END
