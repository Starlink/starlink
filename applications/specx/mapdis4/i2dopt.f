*  History:
*     19 Nov 1993 (hme):
*        Disuse the OUTERR_HANDLER error handler.
*        Replace STR$UPCASE with CHR_UCASE.
*     14 Dec 1993 (hme):
*        Attempt to disuse IERASE_LINE, IERASE_PAGE, IPUT_SCREEN,
*        ISET_CURSOR.
*        The WRITE(CHAR,'(A1)') seems to upset things. Use CHR as the
*        variable and the CHAR function to avoid internal write.
*     06 Jan 1994 (rp):
*        Put back calls to nnnn_SCREEN routines (which are now dummies)
*     15 Jan 1994 (rp):
*        Change CHR_UCASE to UUCASE
*     31 Jan 1994 (hme):
*        Disuse <> in formats.
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Don't split strings across lines
C-----------------------------------------------------------------------

      INTEGER FUNCTION I2DOPT (MAP, VALOPT, REPEAT, CONTOURS,
     &                         GREYSCALE, GRLIMITS)

C   General routine to perform interactive graphics options on
C   NCAR type plot

      PARAMETER       (NOPTS = 30 )
      PARAMETER       (XOFF  = 25.)
      PARAMETER       (YOFF  = 40.)

      REAL            MAP(*)
      CHARACTER       VALOPT*(*)
      LOGICAL         REPEAT
      LOGICAL         CONTOURS
      LOGICAL         GREYSCALE
      LOGICAL         XBOUNDS
      REAL            GRLIMITS(2)

      LOGICAL         GOODBOX
      INTEGER*4       VCHAR
      REAL*4          P(2),Q(2),A(2)
      CHARACTER       TXTLIN*64,CHR*1,ICH2*2

      INCLUDE 'FLAGCOMM'
      INCLUDE 'PLOT2D'
      CHARACTER MAPTIT(3)*11, AXTIT(3)*6, UNITS*6
      COMMON /TITLES/ MAPTIT, AXTIT, UNITS

      CHARACTER       LEFT,RIGHT,CR,TOP,BOTTOM,HELP,END,CLEAR,
     &                NEWLIM,ACCEPT,DSPLBOX,QUIT,PLUS,VSIZE,
     &                HSIZE,MARKZ,GETSPEC,VALID,TELLZ,PRINT,
     &                CONTOUR,MAXMIN,INTEG,ONE,TWO,THREE,FOUR,
     &                FIVE,NEWGREY,LOGCOL
      COMMON /CURSOR/ LEFT,RIGHT,CR,TOP,BOTTOM,HELP,END,CLEAR,
     &                NEWLIM,ACCEPT,DSPLBOX,QUIT,PLUS,VSIZE,
     &                HSIZE,MARKZ,GETSPEC,VALID,TELLZ,PRINT,
     &                CONTOUR,MAXMIN,INTEG,ONE,TWO,THREE,FOUR,
     &                FIVE,NEWGREY,LOGCOL

      BYTE            ZCHAR(NOPTS)
      EQUIVALENCE     (LEFT,ZCHAR(1))

C   Functions

      INTEGER   LOC
      INTEGER   GEN_ILEN

C   Include a useful statement function

      LOC (X) = INT (X + 0.5 + SIGN(0.5,X) )

C  Ok, go...

      I2DOPT = 0
      REPEAT = .FALSE.
      IX = LINK(1)
      IY = LINK(2)
      IZ = LINK(3)

      P(1) = XLIM(1)
      P(2) = XLIM(2)
      Q(1) = YLIM(1)
      Q(2) = YLIM(2)

      A(1) = AX1REQ
      A(2) = AX2REQ

C  Initialize characters (DATA statement doesn't work since in common /CURSOR/

      ZCHAR(1) = 076  ! LEFT
      ZCHAR(2) = 082  ! RIGHT
      ZCHAR(3) = 013  ! <CR>
      ZCHAR(4) = 084  ! TOP
      ZCHAR(5) = 066  ! BOTTOM
      ZCHAR(6) = 072  ! HELP
      ZCHAR(7) = 069  ! END
      ZCHAR(8) = 067  ! CLEAR
      ZCHAR(9) = 078  ! NEWLIM
      ZCHAR(10)= 065  ! ACCEPT
      ZCHAR(11)= 068  ! DSPLBOX
      ZCHAR(12)= 081  ! QUIT
      ZCHAR(13)= 043  ! PLUS
      ZCHAR(14)= 094  ! VSIZE
      ZCHAR(15)= 062  ! HSIZE
      ZCHAR(16)= 077  ! MARKZ
      ZCHAR(17)= 071  ! GETSPEC
      ZCHAR(18)= 086  ! VALID
      ZCHAR(19)= 063  ! TELLZ
      ZCHAR(20)= 080  ! PRINT
      ZCHAR(21)= 073  ! CONTOUR
      ZCHAR(22)= 088  ! MAXMIN
      ZCHAR(23)= 083  ! INTEG
      ZCHAR(24)= 049  ! ONE
      ZCHAR(25)= 050  ! TWO
      ZCHAR(26)= 051  ! THREE
      ZCHAR(27)= 052  ! FOUR
      ZCHAR(28)= 053  ! FIVE
      ZCHAR(29)= 087  ! NEWGREY
      ZCHAR(30)= 048  ! LOGCOL

      DX = AXLENX/NXMAP
      DY = AXLENY/NYMAP

      CALL SXGVWINDOW   (XOFF, XOFF+AXLENX, YOFF, YOFF+AXLENY)
      CALL SXGLIMITS    (XLIM(1), XLIM(2), YLIM(1), YLIM(2))
      CALL SXGDEVINFO   (XSIZED, YSIZED)
      CALL SXGVRELOCATE (0.0,    YSIZED)

      DO WHILE (I2DOPT.EQ.0)

C  Read cursor position and write character to screen

        CALL SXGCURSOR    (XPOS, YPOS, VCHAR)
        CHR = CHAR(VCHAR)
        CALL CONFIRM      (CHR,ICH2)
        CALL IERASE_LINE  (1,1)
        CALL IERASE_LINE  (2,1)

C  Decode X and Y positions to plot co-ordinates and confirm

        I = LOC ((XPOS-XOFF)/DX) - 1
        J = LOC ((YPOS-YOFF)/DY) - 1
        NMAP = (NYMAP-J-1)*NXMAP + I + 1

        XBOUNDS = (     I.lt.0 .or. I.ge.NXMAP
     &             .or. J.lt.0 .or. J.ge.NYMAP
     &             .or. NMAP.lt.1 .or. NMAP.gt.NMAPS)

*       Set back to "OK" values of I and J so that calculation
*       of X and Y will give values off the map when out of bounds.

        I = MAX (MIN (I, NXMAP-1), 0)
        J = MAX (MIN (J, NYMAP-1), 0)

        X = XLIM(1) + ((XPOS-XOFF)/DX - I) * (XLIM(2)-XLIM(1))
        Y = YLIM(1) + ((YPOS-YOFF)/DY - J) * (YLIM(2)-YLIM(1))

        IF (XBOUNDS) THEN
          WRITE (TXTLIN, '(''Cursor outside of map area!'')')
          CALL IPUT_SCREEN (TXTLIN, 1, 1, 0)
*         PRINT *,TXTLIN
          CALL ISET_CURSOR (23, 1)

        ELSE

          CALL MAP_LOCATE (MAP, NMAP, X, Y, IM, IN, Z)
          WRITE (TXTLIN,
     &      '(A4''=''F7.2,3X,A4''=''F7.2,3X,'':  MAP-VALUE=''F8.2)',
     $      IOSTAT=IERR) MAPTIT(LINK(1))(:4),X, MAPTIT(LINK(2))(:4),Y,Z
          CALL IPUT_SCREEN (TXTLIN, 1, 1, 0)
*         PRINT *,TXTLIN
        END IF

        CALL IPUT_SCREEN ('GIN character '//ICH2//' ',1,60,2)
*       PRINT *,'GIN character '//ICH2//' '
        CALL ISET_CURSOR (23, 1)

C  Convert lower case letters to upper case

        CALL UUCASE (CHR)
        CALL UUCASE (ICH2)

C  Then decode options

        IF (INDEX(VALOPT,ICH2).EQ.0) THEN
          CALL IPUT_SCREEN('Invalid character '//ICH2,1,60,2)
*         PRINT *,'Invalid character '//ICH2

        ELSE IF (CHR.EQ.END) THEN
          I2DOPT = 2
          CALL SXGCLEAR
          CALL SXGTIDLE

        ELSE IF (CHR.EQ.QUIT) THEN
          I2DOPT = 2
          CALL SXGTIDLE

        ELSE IF (CHR.EQ.PRINT) THEN
          I2DOPT = 6
          REPEAT = .TRUE.

        ELSE IF (CHR.EQ.CR) THEN
          I2DOPT = 1

        ELSE IF (CHR.EQ.ACCEPT) THEN
          CALL DRAW_2DBOX (P,Q)
          I2DOPT = 3

        ELSE IF(CHR.EQ.LEFT) THEN
          P(1) = X
        ELSE IF(CHR.EQ.RIGHT) THEN
          P(2) = X
        ELSE IF (CHR.EQ.BOTTOM) THEN
          Q(1) = Y
        ELSE IF (CHR.EQ.TOP) THEN
          Q(2) = Y

        ELSE IF (CHR.EQ.ONE) THEN
          CALL COLOUR_PLOT (1, IFAIL)
          CALL SET_COLOURS
        ELSE IF (CHR.EQ.TWO) THEN
          CALL COLOUR_PLOT (2, IFAIL)
          CALL SET_COLOURS
        ELSE IF (CHR.EQ.THREE) THEN
          CALL COLOUR_PLOT (3, IFAIL)
          CALL SET_COLOURS
        ELSE IF (CHR.EQ.FOUR) THEN
          CALL COLOUR_PLOT (4, IFAIL)
          CALL SET_COLOURS
        ELSE IF (CHR.EQ.FIVE) THEN
          CALL SETCOL5     (C5START, C5ROTAT, C5EXP)
          CALL COLOUR_PLOT (5, IFAIL)
          CALL SET_COLOURS
        ELSE IF (CHR.EQ.LOGCOL) THEN
          CALL TOGGLE_LOG  (GRLIMITS(1), GRLIMITS(2))
          CALL SET_COLOURS

        ELSE IF (CHR.EQ.HSIZE) THEN
          A(1) = XPOS - XOFF
        ELSE IF (CHR.EQ.VSIZE) THEN
          A(2) = YPOS - YOFF

        ELSE IF (CHR.EQ.CLEAR) THEN
          CALL IERASE_PAGE (1,1)

        ELSE IF (CHR.EQ.HELP) THEN
          CALL LIST_2DHELP (VALOPT)

        ELSE IF (CHR.EQ.NEWLIM) THEN
          CALL NEW_SCALES (A, P, Q)
          CALL SXGCLEAR
          CHANGE_PLOT = .TRUE.
          REPEAT = .TRUE.
          I2DOPT = 4

        ELSE IF (CHR.EQ.DSPLBOX)  THEN
          CALL DRAW_2DBOX (P,Q)

        ELSE IF (CHR.EQ.MARKZ .AND. .NOT.XBOUNDS)  THEN
          CALL SXGLIMITS (XOFF, XOFF+AXLENX, YOFF, YOFF+AXLENY)
          IF (GREYSCALE) THEN
            CALL SETCCOL (Z, GRLIMITS(1), GRLIMITS(2), ICOL)
            CALL SXGSCI  (ICOL)
          END IF
          CALL MARK_POINT (XPOS, YPOS, Z)
          CALL SXGLIMITS  (XLIM(1), XLIM(2), YLIM(1), YLIM(2))
          CALL SXGSCI     (1)

        ELSE IF (CHR.EQ.GETSPEC .AND..NOT.XBOUNDS) THEN
          CALL SXGTIDLE

          IF (IZ.EQ.3) THEN
            IF (IX.EQ.1 .AND. IY.EQ.2) THEN
              CALL GET_SPECTRUM (X, Y, 0, IFAIL)
            ELSE IF (IX.EQ.2 .AND. IY.EQ.1) THEN
              CALL GET_SPECTRUM (Y, X, 0, IFAIL)
            END IF
            IF (IFAIL.EQ.0) THEN
              CALL IPUT_SCREEN ('Spectrum fetched ', 2, 60, 2)
*             PRINT *,'Spectrum fetched '
            ELSE
              CALL IPUT_SCREEN ('No data nearby!  ', 2, 60, 2)
*             PRINT *,'No data nearby!  '
            END IF
          ELSE
            CALL IPUT_SCREEN ('Not a spatial map', 2, 60, 2)
*           PRINT *,'Not a spatial map'
          END IF

          CALL SXGTTGRAPH

        ELSE IF (CHR.EQ.VALID) THEN
          NX = NAX (LINK(1))
          NY = NAX (LINK(2))
          CALL MARK_SAMPLES (MAP)

        ELSE IF (CHR.EQ.PLUS .AND..NOT.XBOUNDS)   THEN
          P(2) = X
          Q(2) = Y
          I2DOPT = 5

        ELSE IF (CHR.EQ.CONTOUR) THEN
          IF (CONTOURS) CALL SXGCLEAR
          IF (GREYSCALE) CONTOURS = .NOT.CONTOURS
          IF (CONTOURS) THEN
            CALL SXGTIDLE
            CALL SET_CONTOURS (IERR)
            CALL SXGTTGRAPH
          END IF
          REPEAT = .TRUE.
          I2DOPT = 4

        ELSE IF (CHR.EQ.NEWGREY) THEN
          CALL SXGTIDLE
          CALL ASK_GREY (AUTOGREY, GREYLIM)
          CALL SXGTTGRAPH
          CALL SXGCLEAR
          REPEAT = .TRUE.
          I2DOPT = 4

        ELSE IF (CHR.EQ.MAXMIN) THEN
          CALL MAP_MAXMIN (MAP, NAXX, NAXY, XLIM, YLIM,
     &                     BADPIX_VAL, AMAPMIN, AMAPMAX)
          CALL SXGTIDLE
          WRITE (TXTLIN,
     &       '(''Map minimum: '', F8.3, 3X,''Map maximum: '', F8.3)',
     &       IOSTAT=IERR) AMAPMIN, AMAPMAX
          CALL IPUT_SCREEN (TXTLIN, 2, 1, 0)
*         PRINT *,TXTLIN
          CALL SXGTTGRAPH

        ELSE IF (CHR.EQ.INTEG) THEN
          CALL SXGTIDLE
          CALL MAP_INTEG (MAP, NAXX, NAXY, P, Q, SIGMA, GOODBOX)
          IF (GOODBOX) THEN
            LU = GEN_ILEN(AXTIT(3))
            WRITE (TXTLIN,
     &        '(''Integrated intensity in box: '', F10.1,'//
     &        ''' K.'',A,''.arcsec**2'')', IOSTAT=IERR)
     &        SIGMA, AXTIT(3)(:LU)
          ELSE
            WRITE (TXTLIN,'(''Undefined data in box!'')')
          END IF
          CALL IPUT_SCREEN (TXTLIN, 2, 1, 0)
*         PRINT *,TXTLIN
          CALL SXGTTGRAPH
          P(1) = XLIM(1)
          P(2) = XLIM(2)
          Q(1) = YLIM(1)
          Q(2) = YLIM(2)

        END IF

      END DO

      RETURN
      END
