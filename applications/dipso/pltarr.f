       SUBROUTINE PLTARR
     : (X, Y, NPTS, BRKPTS, NBRK, TITLE,
     : ISIZE, XHIST, YHIST)

*   Include graphics stuff
       INCLUDE 'DECLARE_PLTS'
*

       INTEGER SLEN
       REAL X(1), Y(1)
       REAL XP(1), YP(1)
       INTEGER NPTS, BRKPTS(1), NBRK
       REAL XMIN, XMAX, YMIN, YMAX
       LOGICAL DUMLOG
       LOGICAL POLY, HIST
       LOGICAL DRAXIS
       LOGICAL AXMIN, AXMAX, AYMIN, AYMAX
       LOGICAL OPTION
       CHARACTER*100 ZLAB
       INTEGER TITLEN
       INTEGER INCX, INCY
       INTEGER STRLEN
       INTEGER I, J, N, FIRST, IXMIN, IXMAX, LENSTR
       INTEGER I1, I2
       CHARACTER TITLE*(*)
       REAL XLIMITS(2), YLIMITS(2)
       REAL XLIM(2)
       REAL SXMIN, SXMAX, SYMIN, SYMAX

*   Declared for NCAR conversion
       LOGICAL FIRSTBREAK
       REAL XHIST(ISIZE), YHIST(ISIZE)
       INTEGER ASF(13)
       REAL GRAPHW(4), GRIDW(5), GRIDW1(4)
       REAL NMAJOR, NMINOR
       PARAMETER (PI=3.14159265,TWOPI=2.*PI)

*   Include LABELS for axes
       INCLUDE 'DECLARE_LBLS'

*
       REAL MAJTICKSD(2)
       LOGICAL QFILL

*  Local Data:
       DATA POLY/.FALSE./, HIST/.TRUE./
       DATA DRAXIS/.TRUE./
       DATA INCX/0/, INCY/0/
       DATA AXMIN, AXMAX, AYMIN, AYMAX/4*.TRUE./
       DATA XLIM/2*0.0/

       PLTCAL = 0
       TITLEN = MAX(1,LENSTR(TITLE))
       MAXDGM = 79
       TITLEN = MIN(TITLEN,MAXDGM)

       IF (DRAXIS) THEN
          CALL DBOUND(X,NPTS,XMIN,XMAX)
*   XLIMITS (TAKING ACCOUNT OF POSSIBLE AUTOSCALING)
          CALL RANGE(XMIN,AXMIN,XLIM(1),XMAX,AXMAX,XLIM(2))
*   Y RANGE OF DATA TO BE PLOTTED
          XLIM1 = MIN(XLIM(1),XLIM(2))
          XLIM2 = MAX(XLIM(1),XLIM(2))

          YMIN = 1.0E+38
          YMAX = -1.0E+38
          J = 0
          DO 50 I = 1, NPTS
             IF (X(I).GE.XLIM1 .AND. X(I).LE.XLIM2) THEN
                IF (Y(I).LT.YMIN) YMIN = Y(I)
                IF (Y(I).GT.YMAX) YMAX = Y(I)
                J = J + 1
             ENDIF
   50     CONTINUE
          IF (J.EQ.0) GOTO 300
*   YLIMITS
          CALL RANGE(YMIN,AYMIN,YLIM(1),YMAX,AYMAX,YLIM(2))
       ENDIF
*   Ensure that all data in a given plot are scaled by the same amount
       XLIMITS(1) = XLIM1
       XLIMITS(2) = XLIM2
       YLIMITS(1) = YMIN
       YLIMITS(2) = YMAX

       IF (DRAXIS) THEN
          DIVX = 1.0
          DIVY = 1.0
          CALL EXPLABEL
     :    (IFONT,X,NPTS,XLIMITS,XLAB,XLABLN,LABX,IEXPX,4,LOGAXX)
*    If there's no X label, suppress possible exponent
          IF (XLAB(1:XLABLN).EQ.' ') LABX=XLABLN
*
          IF (IEXPX.NE.0) DIVX = 10.0**IEXPX
          CALL EXPLABEL
     :    (IFONT,Y,NPTS,YLIM,YLAB,YLABLN,LABY,IEXPY,3,LOGAXY)
*    If there's no Y label, suppress possible exponent
          IF (YLAB(1:YLABLN).EQ.' ') LABY=YLABLN
*
          IF (IEXPY.NE.0 .AND. IEXPY.NE.38) DIVY = 10.0**IEXPY
       ELSEIF (DIVX.NE.1.0 .OR. DIVY.NE.1.0) THEN
          DO 100 I = 1, NPTS
             X(I) = X(I)/DIVX
             Y(I) = Y(I)/DIVY
  100     CONTINUE
       ENDIF
*
*   PLOT DATA
*
       J = 1
       XLIM1 = MIN(XLIM(1),XLIM(2))
       XLIM2 = MAX(XLIM(1),XLIM(2))
       FIRSTBREAK = .TRUE.
       DO 200 I = 1, NBRK
          K = BRKPTS(I)
          NPLT = K - J + 1
          IF (NPLT.GT.1) THEN
             CALL DBOUND(X(J),NPLT,XMIN,XMAX)
             XMIN = XMIN*DIVX
             XMAX = XMAX*DIVX
             IF (XMIN.LE.XLIM2 .AND. XMAX.GE.XLIM1) THEN
                OPTION = .TRUE.
                PLTCAL = PLTCAL + 1
                IF (X(J).GT.X(K)) OPTION = .FALSE.
                IF (OPTION) THEN
                   DO 105 LL = J, K, +1
                      IF (X(LL).GE.XLIM1) GOTO 110
  105              CONTINUE
                   LL = J
  110              CONTINUE
                   JJ1 = LL
*
                   DO 115 LL = K, J, -1
                      IF (X(LL).LE.XLIM2) GOTO 120
  115              CONTINUE
                   LL = K
  120              CONTINUE
                   KK1 = LL
                   NPLT1 = KK1 - JJ1 + 1
*
                ELSE
                   DO 125 LL = J, K, +1
                      IF (X(LL).LE.XLIM2) GOTO 130
  125              CONTINUE
                   LL = J
  130              CONTINUE
                   JJ1 = LL
*
                   DO 135 LL = K, J, -1
                      IF (X(LL).GE.XLIM1) GOTO 140
  135              CONTINUE
                   LL = K
  140              CONTINUE
                   KK1 = LL
                   NPLT1 = KK1 - JJ1 + 1
                ENDIF

                YLIM1 = YLIM(1)
                YLIM2 = YLIM(2)
*   Define a variable to receive NCAR null/1 value
                CALL AGGETF('NULL/1.',VNULL1)

*   First time
                IF (FIRSTBREAK .AND. DRAXIS) THEN
                   CALL SGS_SELZ(IZBASE,ISTAT)
*   Check if zone graph window is clear
*   Clear screen if BOX is operative and area is not already clear
                   IF (.NOT.ZONECLEAR(NZONEN1) .AND. ERASEBOX) THEN
                      IF (NZONEN1.EQ.0) THEN
                         CALL SGS_CLRZ
                      ELSEIF (.NOT.ZONECLEAR(NZONEN1)) THEN
*   if zone has been plotted in, then page throw
                         CALL SGS_CLRZ
*   All zones are clear
                         DO 142 IZZ = 0, 100
                            ZONECLEAR(IZZ) = .TRUE.
  142                    CONTINUE
                      ELSE
                         CALL SGS_CLRBL(GRID(5),GRID(6),GRID(7),GRID(8))
                      ENDIF
                   ENDIF

*   Select new working zone
                   IF (REALSIZE) THEN
                      CALL SGS_ZSIZE(XGSIZE,YGSIZE,GPOS,IZONID,ISTAT)
                      IF( CROPIT ) CALL CROPER
                   ELSE
                      CALL SGS_ZONE
     :                (GRID(5),GRID(6),GRID(7),GRID(8),IZW,ISTAT)
                   ENDIF
                   CALL SNX_AGWV
                   IF (FIRSTBREAK .AND. DRAXIS) THEN
*   NCAR PLOT

*   Select character set
                      CALL SNX_CHSET(IPREC)
                      CALL SGS_SPREC(IPREC)
*   Suppress joining up of points
                      CALL AGSETI('SET.',-1)
*   Declare the background type
                      CALL AGSETI('BACKGROUND.',IGRIDSTYLE)

*   Position graph window
*   Uses values set up by call to TZONE
*   Default values (0.1,0.9,0.1,0.9)

                      GRAPHW(1) = GRID(5)*XS2NDC
                      GRAPHW(2) = GRID(6)*XS2NDC
                      GRAPHW(3) = GRID(7)*YS2NDC
                      GRAPHW(4) = GRID(8)*YS2NDC
                      GRIDW(1) = (GRID(1)-GRID(5))/(GRID(6)-GRID(5))
                      GRIDW(2) = (GRID(2)-GRID(5))/(GRID(6)-GRID(5))
                      GRIDW(3) = (GRID(3)-GRID(7))/(GRID(8)-GRID(7))
                      GRIDW(4) = (GRID(4)-GRID(7))/(GRID(8)-GRID(7))
                      GRIDW(5) = 0.0

                      IF (REALSIZE) THEN
                         IF (FRZONE) THEN
                            CALL AGSETP('GRID.',GRIDP,5)
                         ENDIF
                      ELSEIF (IGRIDSTYLE.NE.4) THEN
                         CALL AGSETP('GRAPH.',GRAPHW,4)
                         CALL AGSETP('GRID.',GRIDW,5)
                      ELSE
                         GRIDW1(1) = GRID(1)*XS2NDC
                         GRIDW1(2) = GRID(2)*XS2NDC
                         GRIDW1(3) = GRID(3)*YS2NDC
                         GRIDW1(4) = GRID(4)*YS2NDC
                         CALL AGSETP('GRAPH.',GRIDW1,4)
                         CALL AGSETP('GRID.',GRIDW,5)
                      ENDIF

                      CALL SNX_AGCS
*   Define the AUTOGRAPH coordinate system
                      CALL AGSETF('X/MINIMUM.',XLIM1/DIVX)
                      CALL AGSETF('X/MAXIMUM.',XLIM2/DIVX)
                      CALL AGSETF('Y/MINIMUM.',YLIM(1)/DIVY)
                      CALL AGSETF('Y/MAXIMUM.',YLIM(2)/DIVY)
                      XLENGTH = (XLIM2-XLIM1)/DIVX
                      ASPRATIO = XS2NDC/YS2NDC
                      GRIDRATIO = (GRID(2)-GRID(1))/(GRID(4)-GRID(3))
                      IF (IGRIDSTYLE.NE.4) THEN
*   Suppress rotation of axis numbering
                         CALL AGSETI('AXIS/BOTTOM/CONTROL.',2)
                         CALL AGSETI('AXIS/LEFT/CONTROL.',2)

*   Control major tick mark density
                         MAJTICKSD(1) = MAJTICKS(1)/DIVX
                         MAJTICKSD(2) = MAJTICKS(2)/DIVY
*   x-axis
                         IF (ITICKS(1).EQ.0) THEN
*   No x-ticks
                            XTICK = 0.
                         ELSEIF (ITICKS(1).EQ.1) THEN
*   Autotick
                            XTICK = VNULL1
                            MAJTICKSD(1) = VNULL1

                         ELSEIF (ITICKS(1).EQ.2) THEN
*   User tick spacing
                            XTICK = 1.
                         ENDIF
*   y-axis
                         IF (ITICKS(2).EQ.0) THEN
*   No y-ticks
                            YTICK = 0.
                         ELSEIF (ITICKS(2).EQ.1) THEN
*   Autotick
                            YTICK = VNULL1
                            MAJTICKSD(2) = VNULL1

                         ELSEIF (ITICKS(2).EQ.2) THEN
*   User tick spacing
                            YTICK = 1.
                         ENDIF

                         CALL AGSETF
     :                   ('AXIS/BOTTOM/TICKS/MAJOR/SPACING/BASE.',
     :                   MAJTICKSD(1))
                         CALL AGSETF
     :                   ('AXIS/LEFT/TICKS/MAJOR/SPACING/BASE.',
     :                   MAJTICKSD(2))
                         CALL AGSETF
     :                   ('AXIS/RIGHT/TICKS/MAJOR/SPACING/BASE.',
     :                   MAJTICKSD(2))
                         CALL AGSETF
     :                   ('AXIS/TOP/TICKS/MAJOR/SPACING/BASE.',
     :                   MAJTICKSD(1))

                         CALL AGSETF
     :                   ('AXIS/BOTTOM/TICKS/MAJOR/SPACING/TYPE.',
     :                   XTICK)
                         CALL AGSETF
     :                   ('AXIS/LEFT/TICKS/MAJOR/SPACING/TYPE.',
     :                   YTICK)
                         CALL AGSETF
     :                   ('AXIS/RIGHT/TICKS/MAJOR/SPACING/TYPE.',
     :                   YTICK)
                         CALL AGSETF
     :                   ('AXIS/TOP/TICKS/MAJOR/SPACING/TYPE.',
     :                   XTICK)

*   Tick length
                         TICKLENGTH = 0.015*HTFAC( MJTICK )
                         CALL AGSETF
     :                   ('AXIS/BOTTOM/TICKS/MAJOR/LENGTH/INWARD.',
     :                   TICKLENGTH)
                         CALL AGSETF
     :                   ('AXIS/LEFT/TICKS/MAJOR/LENGTH/INWARD.',
     :                   TICKLENGTH)
                         CALL AGSETF
     :                   ('AXIS/RIGHT/TICKS/MAJOR/LENGTH/INWARD.',
     :                   TICKLENGTH)
                         CALL AGSETF
     :                   ('AXIS/TOP/TICKS/MAJOR/LENGTH/INWARD.',
     :                   TICKLENGTH)

*   Control minor tick marks
                         CALL AGSETF
     :                   ('AXIS/BOTTOM/TICKS/MINOR/SPACING.',
     :                   MINTICKS(1))
                         CALL AGSETF('AXIS/LEFT/TICKS/MINOR/SPACING.',
     :                   MINTICKS(2))
                         CALL AGSETF('AXIS/RIGHT/TICKS/MINOR/SPACING.',
     :                   MINTICKS(2))
                         CALL AGSETF('AXIS/TOP/TICKS/MINOR/SPACING.',
     :                   MINTICKS(1))

*   Tick length
                         TICKLENGTH = 0.010*HTFAC( MNTICK )
                         CALL AGSETF
     :                   ('AXIS/BOTTOM/TICKS/MINOR/LENGTH/INWARD.',
     :                   TICKLENGTH)
                         CALL AGSETF
     :                   ('AXIS/LEFT/TICKS/MINOR/LENGTH/INWARD.',
     :                   TICKLENGTH)
                         CALL AGSETF
     :                   ('AXIS/RIGHT/TICKS/MINOR/LENGTH/INWARD.',
     :                   TICKLENGTH)
                         CALL AGSETF
     :                   ('AXIS/TOP/TICKS/MINOR/LENGTH/INWARD.',
     :                   TICKLENGTH)

*   Control presence of numeric labelling
*   Stops labels being shrunk in response to overlap problems

                         CALL AGSETI('LABEL/CONTROL.',1)
                         CALL AGSETI('AXIS/LEFT/CONTROL.',1)
                         CALL AGSETI('AXIS/BOTTOM/CONTROL.',1)


*   Numeric label size
                         SIZEMANT = 0.015*HTFAC( NUMLAB )
                         SIZEEXP = 0.010*HTFAC( NUMLAB )
                         CALL AGSETF
     :                   ('AXIS/BOTTOM/NUMERIC/WIDTH/MANTISSA.',
     :                   SIZEMANT)
                         CALL AGSETF
     :                   ('AXIS/LEFT/NUMERIC/WIDTH/MANTISSA.',
     :                   SIZEMANT)
                         CALL AGSETF
     :                   ('AXIS/BOTTOM/NUMERIC/WIDTH/EXPONENT.',
     :                   SIZEEXP)
                         CALL AGSETF
     :                   ('AXIS/LEFT/NUMERIC/WIDTH/EXPONENT.',
     :                   SIZEEXP)



*   Controls presence of numeric labels
                         IF (NONUM) THEN
                            CALL AGSETF('AXIS/BOTTOM/NUMERIC/TYPE.',
     :                                  0.0)
                            CALL AGSETF('AXIS/LEFT/NUMERIC/TYPE.',0.0)
                            CALL AGSETF('AXIS/RIGHT/NUMERIC/TYPE.',0.0)
                            CALL AGSETF('AXIS/TOP/NUMERIC/TYPE.',0.0)
                         ELSE
                            CALL AGSETF('AXIS/BOTTOM/NUMERIC/TYPE.',
     :                      VNULL1)
                            CALL AGSETF('AXIS/LEFT/NUMERIC/TYPE.',
     :                      VNULL1)
                         ENDIF

                      ENDIF
                      CALL SNX_CHSET(IPREC)
                      CALL SGS_SPREC(IPREC)

*   Logarthmic
                      IF (LOGAXX) THEN
                         CALL AGSETF('X/LOG.',-1.0)
                      ELSE
                         CALL AGSETF('X/LOG.',0.0)
                      ENDIF

                      IF (LOGAXY) THEN
                         CALL AGSETF('Y/LOG.',-1.0)
                      ELSE
                         CALL AGSETF('Y/LOG.',0.0)
                      ENDIF

*   Reverse x-axis ?
                      IF (INCX.EQ.1) THEN
                         CALL AGSETF('X/ORDER.',1.0)
                      ELSE
                         CALL AGSETF('X/ORDER.',0.0)
                      ENDIF
*   Reverse y-axis ?
                      IF (INCY.EQ.1) THEN
                         CALL AGSETF('Y/ORDER.',1.0)
                      ELSE
                         CALL AGSETF('Y/ORDER.',0.0)
                      ENDIF

*   Justify or trim ?
                      IF (TRIMX) THEN
                         CALL AGSETF('X/NICE.',0.0)
                      ELSE
                         CALL AGSETF('X/NICE.',-1.0)
                      ENDIF
                      IF (TRIMY) THEN
                         CALL AGSETF('Y/NICE.',0.0)
                      ELSE
                         CALL AGSETF('Y/NICE.',-1.0)
                      ENDIF
                   ENDIF

                   DO 145 JJ = 1, 13
                      ASF(JJ) = 1
  145              CONTINUE
                   CALL GSASF(ASF)
                ENDIF
                IF (NPLT1.GT.0) THEN
*   Start of plotting
*   Reset extent of clear region first time round
                   IF (FIRSTBREAK) THEN
                      DO 146 IZ = 0, 100
                         IF (ZONEDEF(IZ)) THEN
                            IF ((GRIDS(6,IZ).GT.GRID(5)) .AND.
     :                      (GRIDS(5,IZ).LT.GRID(6)) .AND.
     :                      (GRIDS(8,IZ).GT.GRID(7)) .AND.
     :                      (GRIDS(7,IZ).LT.GRID(8))) THEN
                               ZONECLEAR(IZ) = .FALSE.
                            ENDIF
                         ENDIF
  146                 CONTINUE
                   ENDIF

*   Allow character expansion in titles
                   IF (LABELFLAG) THEN
*   Define labels

                      ZLAB = ' '
                      IF (IFONT.EQ.0 .AND. DEVTYP.EQ.827) THEN
                         DO 148 IZ = 1, LABY
                            J = LABY - IZ + 1
                            ZLAB(J:J) = YLAB(IZ:IZ)
  148                    CONTINUE
                      ELSE
                         ZLAB = YLAB
                      ENDIF
                      GRAPHSIZE = (GRID(2)-GRID(1))/0.85
                      SIZE = DEFHEIGHT*HTFAC( TXTLAB )*GRAPHSIZE
                      CALL AGSETC('LABEL/NAME.','L')
                      CALL AGSETI('LINE/NUMBER.',100)
                      CALL AGSETF('LINE/CHARACTER.',SIZE)
                      CALL AGSETC('LINE/TEXT.',ZLAB(1:LABY))

                      CALL AGSETC('LABEL/NAME.','T')
                      CALL AGSETI('LINE/NUMBER.',100)
                      CALL AGSETF('LINE/CHARACTER.',SIZE*1.3)
                      CALL AGSETC('LINE/TEXT.',TITLE(1:TITLEN))

                      CALL AGSETC('LABEL/NAME.','B')
                      CALL AGSETI('LINE/NUMBER.',-100)
                      CALL AGSETF('LINE/CHARACTER.',SIZE)
                      CALL AGSETC('LINE/TEXT.',XLAB(1:LABX))
                   ENDIF


                   CALL GSLN(1)
                   CALL GSLWSC(XLWIDTH)
                   IF (POLY) THEN
                      IF (FIRSTBREAK .AND. DRAXIS) THEN
                         CALL PLOTIT( 0, 0, 2 )
                         IF (COLOUR) CALL PPALET(1)
                         CALL GSLN(1)
                         CALL GSLWSC(XLWIDTH)

                         IF (LABELFLAG) THEN
                            CALL SNX_EZRXY(X(JJ1),Y(JJ1),NPLT1,
     :                      XLAB(1:LABX),ZLAB(1:LABY),
     :                      TITLE(1:TITLEN))
                         ELSE
                            CALL SNX_EZRXY
     :                      (X(JJ1),Y(JJ1),NPLT1,' ',' ',' ')
                         ENDIF
                         FIRSTBREAK = .FALSE.
                         CALL PLOTIT( 0, 0, 2 )
                         CALL SGS_FLUSH
                      ENDIF
                      CALL GSLN(LNTYPE)
                      CALL GSLWSC(LNWIDTH)
                      IF (COLOUR) CALL PPALET(IPAL)
                      IF (NPLT1.NE.0) CALL CURVE(X(JJ1),Y(JJ1),NPLT1)
                      CALL PLOTIT( 0, 0, 2 )
                      CALL SGS_FLUSH

                   ELSEIF (HIST) THEN
*   Read points into a double sized array to simulate histogram
                      IF (NPLT1.GT.1) THEN
                         XHIST(JJ1*2-1) = (3.0*X(JJ1)-X(JJ1+1))*0.5
                         YHIST(JJ1*2-1) = Y(JJ1)
                         DO 150 L = JJ1, NPLT1 + JJ1 - 2
                            XHIST(2*L) = (X(L)+X(L+1))*0.5
                            XHIST(2*L+1) = XHIST(2*L)
                            YHIST(2*L) = Y(L)
                            YHIST(2*L+1) = Y(L+1)
  150                    CONTINUE
                         L = NPLT1 + JJ1 - 1
                         XHIST(2*L) = (3.0*X(L)-X(L-1))*0.5
                         YHIST(2*L) = Y(L)

                         NPOINTS = 2*NPLT1
                      ELSE
                         NPOINTS = 0
                      ENDIF

                      NSTART = 2*JJ1 - 1

                      IF (FIRSTBREAK .AND. DRAXIS) THEN
                         IF (COLOUR) CALL PPALET(1)
                         CALL GSLN(1)
                         CALL GSLWSC(XLWIDTH)

                         IF (LABELFLAG) THEN
                            CALL SNX_EZRXY(XHIST(NSTART),YHIST(NSTART),
     :                      NPOINTS,XLAB(1:LABX),ZLAB(1:LABY),
     :                      TITLE(1:TITLEN))
                         ELSE
                            CALL SNX_EZRXY(XHIST(NSTART),YHIST(NSTART),
     :                      NPOINTS,' ',' ',' ')
                         ENDIF

                         FIRSTBREAK = .FALSE.
                         CALL PLOTIT( 0, 0, 2 )
                         CALL SGS_FLUSH
                      ENDIF
                      CALL GSLN(LNTYPE)
                      CALL GSLWSC(LNWIDTH)

                      IF (COLOUR) CALL PPALET(IPAL)
                      IF (NPOINTS.NE.0) THEN
                         CALL CURVE(XHIST(NSTART),YHIST(NSTART),NPOINTS)
                      ENDIF

                      CALL SGS_FLUSH
                      IF (COLOUR) CALL PPALET(IPAL)
                   ELSE
                      IF (FIRSTBREAK .AND. DRAXIS) THEN
                         IF (COLOUR) CALL PPALET(1)
                         CALL GSLN(1)
                         CALL GSLWSC(XLWIDTH)

                         IF (LABELFLAG) THEN
                            CALL SNX_EZRXY(X(JJ1),Y(JJ1),NPLT1,
     :                      XLAB(1:LABX),ZLAB(1:LABY),
     :                      TITLE(1:TITLEN))
                         ELSE
                            CALL SNX_EZRXY
     :                      (X(JJ1),Y(JJ1),NPLT1,' ',' ',' ')
                         ENDIF
                         CALL PLOTIT( 0, 0, 2 )
                         CALL SGS_FLUSH
                         FIRSTBREAK = .FALSE.
                      ENDIF
                      CALL GSLN(LNTYPE)
                      CALL GSLWSC(LNWIDTH)

                      IF (COLOUR) CALL PPALET(IPAL)
*   Scale symbol heights (considered unnecessary ?)
                      CALL GQMKSC(IERROR,SYMHEIGHT)
                      IF (DEVTYP.EQ.2600) THEN
                         DEVGRO = 2.0
                      ELSEIF (DEVTYP.EQ.2601) THEN
                         DEVGRO = 2.0
                      ELSE
                         DEVGRO = 1.0
                      ENDIF
                      CALL GSMKSC(SYMHEIGHT*HTFAC( MARKS )*DEVGRO)

                      ANGLE = REAL(NDEGREES)*PI/180.
                      CALL GSLN(1)
                      CALL JSYMBOL(X(JJ1),Y(JJ1),NPLT1,NSIDES,MARKSTYLE,
     :                             IPAL,COLOUR,FILL,HTFAC( MARKS ),
     :                             ANGLE)
                      CALL GSMKSC(SYMHEIGHT)

                   ENDIF
                ENDIF
             ENDIF
          ENDIF
          J = K + 1
  200  CONTINUE


*   End of plotting
       CALL PLOTIT( 0, 0, 2 )
       CALL SGS_FLUSH
       CALL GSLN(1)
       CALL GSLWSC(XLWIDTH)
*   Fix for goof when data are not in bounds on first plot
       IF ((DIVX.NE.1.0 .OR. DIVY.NE.1.0) .AND.
     : (DIVX.NE.0.0 .AND. DIVY.NE.0.0)) THEN
          DO 250 I = 1, NPTS
             X(I) = X(I)*DIVX
             Y(I) = Y(I)*DIVY
  250     CONTINUE
       ENDIF

       GOTO 300


       ENTRY JOBOX
       DRAXIS = .TRUE.
       GOTO 300

       ENTRY INVERT
       INCY = 1 - INCY
       GOTO 300

       ENTRY NOBOX
       DRAXIS = .FALSE.
       GOTO 300

       ENTRY NY
       AYMIN = .TRUE.
       AYMAX = .TRUE.
       GOTO 300

       ENTRY NX
       AXMIN = .TRUE.
       AXMAX = .TRUE.
       GOTO 300

       ENTRY NXY
       AXMIN = .TRUE.
       AXMAX = .TRUE.
       AYMIN = .TRUE.
       AYMAX = .TRUE.
       GOTO 300

       ENTRY REVERS
       INCX = 1 - INCX
       GOTO 300

       ENTRY SETDEV(I1,I2,DUMLOG)
       IF (I.NE.0) THEN
          DUMLOG = .FALSE.
       ENDIF
       GOTO 300

       ENTRY SETHIS
       POLY = .FALSE.
       HIST = .TRUE.
       GOTO 300

       ENTRY SETMAR
       POLY = .FALSE.
       HIST = .FALSE.
       GOTO 300

       ENTRY SETPOL
       POLY = .TRUE.
       HIST = .FALSE.
       GOTO 300

       ENTRY XMINST(X1)
       AXMIN = .FALSE.
       XLIM(1) = X1
       IF (.NOT.AXMAX) THEN
          CALL INCXY(XLIM,INCX)
       ENDIF
       GOTO 300

       ENTRY XMAXST(X2)
       AXMAX = .FALSE.
       XLIM(2) = X2
       IF (.NOT.AXMIN) THEN
          CALL INCXY(XLIM,INCX)
       ENDIF
       GOTO 300

       ENTRY YMINST(Y1)
       AYMIN = .FALSE.
       YLIM(1) = Y1
       IF (.NOT.AYMAX) THEN
          CALL INCXY(YLIM,INCY)
       ENDIF
       GOTO 300

       ENTRY YMAXST(Y2)
       AYMAX = .FALSE.
       YLIM(2) = Y2
       IF (.NOT.AYMIN) THEN
          CALL INCXY(YLIM,INCY)
       ENDIF
       GOTO 300

       ENTRY XRSET(X1,X2)
       AXMIN = .FALSE.
       AXMAX = .FALSE.
       XLIM(1) = X1
       XLIM(2) = X2
       CALL INCXY(XLIM,INCX)
       GOTO 300

       ENTRY YRSET(Y1,Y2)
       AYMIN = .FALSE.
       AYMAX = .FALSE.
       YLIM(1) = Y1
       YLIM(2) = Y2
       CALL INCXY(YLIM,INCY)

  300  CONTINUE

       END
