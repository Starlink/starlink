*+  GFX_KEY - put key at side of plot
      SUBROUTINE GFX_KEY(MIN,MAX,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'GCB_PAR'
*    Import :
      REAL MIN,MAX
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Global variables :
      INCLUDE 'GFX_PIX_CMN'
*    Functions :
*    Local variables :
      CHARACTER STR*15
      CHARACTER*8 OPT
      REAL X1,X2,Y1,Y2
      REAL XB1,XB2,YB1,YB2
      REAL XWID,YWID
      REAL SIZE
      REAL LEVS(GCB__MXCONTLEV)
      REAL PMIN,PMAX
      REAL SMIN,SMAX
      REAL POS
      REAL AXNUM(10)
      REAL ZVAL
      INTEGER NAXNUM,NAXSUB,IAXNUM
      INTEGER IL,NL
      INTEGER COL(1,256)
      INTEGER BGCOL,FIRST,LAST,NCOL
      INTEGER FONT,BOLD
      INTEGER NN,PP
      INTEGER I,ICOL
      INTEGER STYLE,WIDTH,COLOUR
      LOGICAL OK
      LOGICAL PIX,CONT
      LOGICAL PFLAG,CFLAG
      LOGICAL COLOURDEV
      LOGICAL NUM
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETC('KEY_OPT',OK,OPT,STATUS)
        IF (.NOT.OK) THEN
          PIX=.FALSE.
          CONT=.FALSE.
        ELSE
          CALL CHR_UCASE(OPT)
          PIX=(INDEX(OPT,'P').NE.0)
          CONT=(INDEX(OPT,'C').NE.0)
        ENDIF
        CALL GCB_GETL('CONT_FLAG',OK,CFLAG,STATUS)
        CONT=(CONT.AND.OK.AND.CFLAG)
        CALL GCB_GETL('PIX_FLAG',OK,PFLAG,STATUS)
        IF (.NOT.OK.AND..NOT.CFLAG) THEN
          PFLAG=.TRUE.
        ENDIF
        PIX=(PIX.AND.PFLAG)

*  get contour levels if requested
        IF (CONT) THEN
          CALL GFX_CONTLEV(MIN,MAX,NL,LEVS,STATUS)
          IF (NL.EQ.0) THEN
            CONT=.FALSE.
          ENDIF
        ENDIF

        IF (PIX.OR.CONT) THEN

*  get position of plot
          CALL PGQVP(0,X1,X2,Y1,Y2)

*  set position of key relative to plot
          XWID=X2-X1
          YWID=Y2-Y1
          XB1=X2+XWID*0.05
          XB2=XB1+XWID*0.05
          YB1=Y1+YWID*0.1
          YB2=Y2-YWID*0.1


*  set position of key
          CALL PGVPORT(XB1,XB2,YB1,YB2)

        ENDIF


*  set scale of key bar
        IF (PIX) THEN
*  if showing pixels - set extremes to max and min scaling values
          CALL GCB_GETR('PIX_MIN',OK,SMIN,STATUS)
          IF (.NOT.OK) THEN
            SMIN=MIN
          ENDIF
          CALL GCB_GETR('PIX_MAX',OK,SMAX,STATUS)
          IF (.NOT.OK) THEN
            SMAX=MAX
          ENDIF
        ENDIF

*  set data values at limits of key
        IF (PIX.AND.CONT) THEN
          PMAX=SMAX
          PMIN=SMIN
          DO IL=1,NL
            IF (LEVS(IL).GT.SMAX) THEN
              PMAX=MAX
            ENDIF
            IF (LEVS(IL).LT.SMIN) THEN
              PMIN=MIN
            ENDIF
          ENDDO
          NUM=.FALSE.
          CALL PGWINDOW(0.0,1.0,PMIN,PMAX)
        ELSEIF (CONT.AND..NOT.PIX) THEN
          PMIN=MIN
          PMAX=MAX
          NUM=.FALSE.
          CALL PGWINDOW(0.0,1.0,PMIN,PMAX)
        ELSEIF (PIX.AND..NOT.CONT) THEN
          PMIN=SMIN
          PMAX=SMAX
          NUM=.TRUE.
          CALL PGWINDOW(0.0,1.0,PMIN,PMAX)
        ENDIF


        IF (PIX) THEN
*  how many colours?
          CALL GDV_COLOURS(BGCOL,FIRST,LAST,STATUS)
          NCOL=LAST-FIRST+1

*  true colour or dotty monochrome
          IF (NCOL.LT.8) THEN
            NCOL=8
            FIRST=1
            LAST=8
            COLOURDEV=.FALSE.
          ELSE
            COLOURDEV=.TRUE.
          ENDIF

*  set colours indices
          I=0
          DO ICOL=FIRST,LAST
            I=I+1
            COL(1,I)=ICOL
          ENDDO


*  draw pixel key
          CALL GFX_KEY_COLOURBAR(NCOL,COL,COLOURDEV,PMIN,PMAX,SMIN,SMAX,
     :                                                           STATUS)


        ENDIF
*  put numbers on

        IF (PIX.OR.CONT) THEN
          CALL GCB_SETDEF(STATUS)
          CALL GCB_GETR('KEY_SIZE',OK,SIZE,STATUS)
          IF (OK) THEN
            CALL PGSCH(SIZE)
          ENDIF
          CALL GCB_GETI('KEY_FONT',OK,FONT,STATUS)
          IF (OK) THEN
            CALL PGSCF(FONT)
          ENDIF
          CALL GCB_GETI('KEY_BOLD',OK,BOLD,STATUS)
          IF (OK) THEN
            CALL PGSLW(BOLD)
          ENDIF

          IF (NUM) THEN
*  numbers down side
            CALL GFX_KEY_AXNUM(PMIN,PMAX,NAXNUM,AXNUM,NAXSUB,STATUS)
            DO IAXNUM=1,NAXNUM
              ZVAL=AXNUM(IAXNUM)
              CALL PGMOVE(1.0,ZVAL)
              CALL PGDRAW(0.7,ZVAL)
              ZVAL=(ZVAL-PMIN)/(PMAX-PMIN)
              CALL GFX_KEY_FMT(AXNUM(IAXNUM),STR)
              CALL PGMTEXT('RV',0.5,ZVAL,0.0,STR)
            ENDDO
          ELSE
*  or just mark max and min
            CALL GFX_KEY_FMT(PMAX,STR)
            CALL PGMTEXT('T',0.5,0.5,0.5,STR)
            CALL GFX_KEY_FMT(PMIN,STR)
            CALL PGMTEXT('B',1.5,0.5,0.5,STR)

          ENDIF

        ENDIF

*  mark contour levels
        IF (CONT) THEN

          CALL GCB_SETDEF(STATUS)

          DO IL=1,NL
            CALL GCB_GET1I('CONT_STYLE',IL,1,OK,STYLE,STATUS)
            IF (OK) THEN
              CALL PGSLS(STYLE)
            ENDIF
            CALL GCB_GET1I('CONT_WIDTH',IL,1,OK,WIDTH,STATUS)
            IF (OK) THEN
              CALL PGSLW(WIDTH)
            ENDIF
            CALL GCB_GET1I('CONT_COLOUR',IL,1,OK,COLOUR,STATUS)
            IF (OK) THEN
              CALL PGSCI(COLOUR)
            ENDIF

            CALL PGMOVE(0.0,LEVS(IL))
            CALL PGDRAW(1.0,LEVS(IL))

          ENDDO

*  put numbers on
          CALL GCB_SETDEF(STATUS)
          CALL GCB_GETR('KEY_SIZE',OK,SIZE,STATUS)
          IF (OK) THEN
            CALL PGSCH(SIZE)
          ENDIF
          CALL GCB_GETI('KEY_FONT',OK,FONT,STATUS)
          IF (OK) THEN
            CALL PGSCF(FONT)
          ENDIF
          CALL GCB_GETI('KEY_BOLD',OK,BOLD,STATUS)
          IF (OK) THEN
            CALL PGSLW(BOLD)
          ENDIF

          DO IL=1,NL
            POS=(LEVS(IL)-MIN)/(MAX-MIN)
            IF (LEVS(IL).EQ.0.0) THEN
              CALL PGMTEXT('RV',0.5,POS,0.0,'0.0')
            ELSE
              WRITE(STR,'(G10.3)') LEVS(IL)
              CALL CHR_FANDL(STR,NN,PP)
              CALL PGMTEXT('RV',0.5,POS,0.0,STR(NN:PP))
            ENDIF

          ENDDO

        ENDIF

*  restore transformation for plot
        CALL GTR_RESTORE(STATUS)

*  restore default attributes
        CALL GCB_SETDEF(STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_KEY',STATUS)
        ENDIF

      ENDIF
      END

*+  GFX_KEY_COLOURBAR
      SUBROUTINE GFX_KEY_COLOURBAR(NCOL,COL,CDEV,DMIN,DMAX,SMIN,SMAX,
     :                                                         STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'GCB_PAR'
*    Import :
      INTEGER NCOL,COL(1,NCOL)
      LOGICAL CDEV
      REAL DMIN,DMAX
      REAL SMIN,SMAX
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Global variables :
      INCLUDE 'GFX_PIX_CMN'
*    Functions :
*    Local variables :
      CHARACTER*16 SCALING
      REAL CMIN,CMAX,CRAN
      REAL X(5)/0.0,1.0,1.0,0.0,0.0/
      REAL Y(5)
      INTEGER CYCLES
      INTEGER J
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  get scaling algorithm
        CALL GCB_GETC('PIX_SCALING',OK,SCALING,STATUS)
        IF (.NOT.OK) THEN
          SCALING='LIN'
        ELSE
          CALL CHR_UCASE(SCALING)
          SCALING=SCALING(:3)
        ENDIF
        IF (SCALING.EQ.'CYC') THEN
          CALL GCB_GETI('PIX_CYCLES',OK,CYCLES,STATUS)
          IF (.NOT.OK) THEN
            CYCLES=1
          ENDIF
        ENDIF

*  do scaled part of colour bar

*  cyclic scaling
        IF (SCALING.EQ.'CYC') THEN

          CRAN=(SMAX-SMIN)/REAL(CYCLES)
          CMIN=SMIN
          CMAX=CMIN+CRAN
          DO J=1,CYCLES
            IF (CDEV) THEN
              CALL PGPIXL(COL,1,256,1,1,1,NCOL,0.0,1.0,CMIN,CMAX)
            ELSE
              CALL GFX_PIXEL_DOTTY(COL,1,256,1,1,1,NCOL,0.0,1.0,
     :                                            CMIN,CMAX,STATUS)
            ENDIF
            CMIN=CMAX
            CMAX=CMAX+CRAN
          ENDDO

        ELSE
*  all other scaling types
          DO J=1,NCOL
            IF (CDEV) THEN
              Y(1)=G_BOUNDS(1,J)
              Y(2)=G_BOUNDS(1,J)
              Y(3)=G_BOUNDS(2,J)
              Y(4)=G_BOUNDS(2,J)
              Y(5)=G_BOUNDS(1,J)
              CALL PGSCI(COL(1,J))
              CALL PGPOLY(5,X,Y)
C              CALL PGPIXL(COL,1,256,1,1,J,J,0.0,1.0,
C     :                           G_BOUNDS(1,J),G_BOUNDS(2,J))
            ELSE
              CALL GFX_PIXEL_DOTTY(COL,1,256,1,1,J,J,0.0,1.0,
     :                           G_BOUNDS(1,J),G_BOUNDS(2,J),STATUS)
            ENDIF
          ENDDO

        ENDIF

*  do extensions to full data range if required
        IF (CDEV) THEN
          IF (DMIN.LT.SMIN) THEN
            CALL PGPIXL(COL,1,256,1,1,1,1,0.0,1.0,DMIN,SMIN)
          ENDIF
          IF (DMAX.GT.SMAX) THEN
            CALL PGPIXL(COL,1,256,1,1,NCOL,NCOL,0.0,1.0,SMAX,DMAX)
          ENDIF
        ELSE
          IF (DMIN.LT.SMIN) THEN
            CALL GFX_PIXEL_DOTTY(COL,1,256,1,1,1,1,0.0,1.0,
     :                                           DMIN,SMIN,STATUS)
          ENDIF
          IF (DMAX.GT.SMAX) THEN
            CALL GFX_PIXEL_DOTTY(COL,1,256,1,1,NCOL,NCOL,0.0,1.0,
     :                                            SMAX,DMAX,STATUS)
          ENDIF
        ENDIF

        CALL GCB_SETDEF(STATUS)
        CALL PGBOX('BC',0.0,0,'BC',0.0,0)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_KEY_COLOURBAR',STATUS)
        ENDIF

      ENDIF
      END


*+
      SUBROUTINE GFX_KEY_AXNUM(MIN,MAX,N,NUMS,NSUB,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      REAL MIN,MAX
*    Import-export :
*    Export :
      REAL NUMS(*)
      INTEGER N
      INTEGER NSUB
*    Status :
      INTEGER STATUS
*    Local Constants :
      INTEGER NDIV
      PARAMETER (NDIV=6)
*    Global variables :
*    Functions :
      REAL PGRND
*    Local variables :
      REAL RAN,DIV,TICK
      REAL NICEMIN,NICEDIV
*-

      IF (STATUS.EQ.SAI__OK) THEN

        NICEMIN=PGRND(MIN,NSUB)
        RAN=MAX-MIN
        DIV=RAN/REAL(NDIV)
        NICEDIV=PGRND(DIV,NSUB)

        TICK=NICEMIN
        DO WHILE (TICK.GT.MIN)
          TICK=TICK-NICEDIV
        ENDDO

        N=0
        DO WHILE (TICK.LE.MAX)
          IF (TICK.GE.MIN) THEN
            N=N+1
            NUMS(N)=TICK
          ENDIF
          TICK=TICK+NICEDIV
        ENDDO


      ENDIF

      END



      SUBROUTINE GFX_KEY_FMT(X,STR)

      REAL X
      CHARACTER*(*) STR

      INTEGER NN,PP

      IF (X.EQ.0.0) THEN
        STR='0.0'
      ELSE
        WRITE(STR,'(G10.3)') X
        CALL CHR_FANDL(STR,NN,PP)
        STR=STR(NN:PP)
      ENDIF

      END
