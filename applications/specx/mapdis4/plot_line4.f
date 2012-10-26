*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Test ISTAT .NE. 0
*        Unused ILEN, INAME, NC, IFILE, IOSTAT, IZARR
C-----------------------------------------------------------------------

      SUBROUTINE PLOT_LINE4 (IFAIL)

*   Routine to plot up to six maps, of the peak temperature, velocity of
*   the peak, line equivalent width, integrated intensity, centroid and/or
*   second moment of line profile, on desired device

      IMPLICIT NONE

*     Formal parameters:

      INTEGER*4 IFAIL             ! SPECX error return

*     Include files

      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'MAPTITLES'
      INCLUDE 'PLOT2D'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'STACKCOMM'
      INCLUDE 'CNF_PAR'

*     Functions

      INTEGER   IFREEVM
      LOGICAL   SXGCOLOROK
      LOGICAL   SXGGREYOK

*     Other parameters

      LOGICAL   SOME_PLOT
      LOGICAL   REPEAT
      LOGICAL   CONTOURS
      LOGICAL   GREYSCALE
      LOGICAL   FIRST
      INTEGER   I,  J,  K ! General counters
      INTEGER   IERR
      INTEGER   IMAP
      INTEGER   IMX, IMY
      INTEGER   IPTR(6)
      INTEGER   IPTR_MAP
      INTEGER   ISTAT
      INTEGER   K1, K2
      INTEGER   LOCATION
      INTEGER   NBYTES
      INTEGER   NMAPL
      INTEGER   NV, NW
      INTEGER   NZ
      INTEGER   SAVLINK(3)
      REAL      DEVLIMS(2)
      REAL      DX, DY          ! Individual map sizes
      REAL      DZ
      REAL      GRLIMITS(2)
      REAL      XL, YL
      REAL      V1, V2, DV
      REAL      W1, W2, DW
      REAL      XAXLEN, YAXLEN
      REAL      ZMIN,  ZMAX
      REAL      ZMIN1, ZMAX1
      REAL      ZC(32)

      REAL       XMARGIN
      PARAMETER (XMARGIN = 25.)
      REAL       YMARGIN
      PARAMETER (YMARGIN = 40.)

      CHARACTER OPTIONS*64
      CHARACTER PANEL_NAME*32

*  Ok, go....

      IFAIL = 0

*     Contours and/or greyscale?

      CONTOURS  = .NOT.(PLOTGREY.AND..NOT.OVERCONT)
      IF (PLOTGREY .AND..NOT. SXGGREYOK()) THEN
        CALL SXGTIDLE
        PRINT *,'Sorry, greyscaling not available!'
        GREYSCALE = .FALSE.
      ELSE
        GREYSCALE = PLOTGREY
      END IF
      IF (.NOT.(GREYSCALE.OR.CONTOURS)) RETURN

*     Map image file to memory

      CALL MAPIMAGE ('mapplane.tmp', IPTR_MAP, NMAPL, IMX, IMY, ISTAT)
      IF (ISTAT .NE. 0) THEN
        IFAIL = 67
        RETURN
      END IF

      PRINT *,'--- Plot_line_pars ---'
      PRINT *,'    Number of maps in file = ', NMAPL
      PRINT *,'    Each map has size', IMX, ' by', IMY

*     How many maps to plot (and where are they?)

      NMAPS = 0

      DO I = 1, 6
        IF (IPLP(I).NE.0) NMAPS = NMAPS + 1
      END DO

      IF (NMAPS.LE.0) THEN
        IFAIL = 18
        WRITE (ILOUT,*) 'No maps asked for to be plotted!'
        RETURN
      END IF

      NBYTES  = 4*IMX*IMY
      DO I = 1, NMAPL
        IPTR(I) = IPTR_MAP +  (I-1)*NBYTES
*       PRINT *, 'Map #/Memory address = ', I, IPTR(I)
      END DO

*     Copy the default plot values to arrays in /PLOT2D/ as initial values

      IF (.NOT.CHANGE_PLOT .OR. LINK(3).NE.3) THEN
        XLIM(1) = PBEG(1)
        XLIM(2) = PEND(1)
        YLIM(1) = PEND(2)
        YLIM(2) = PBEG(2)
        AX1REQ  = AX1LEN
        AX2REQ  = AX2LEN
        CHANGE_PLOT = .FALSE.
      END IF

*     Initialize device so that SET_DISPLAY_SIZE can work

      CALL ALLOCATE_DEVICE (IDEV, IFAIL)
      IF (IFAIL.NE.0) THEN
        PRINT *,'Trouble allocating plot device!'
        GO TO 998
      END IF

      FIRST = .TRUE.

*     Determine the final size of the montage
*     and find an aesthetic layout for the screen

      XAXLEN = ABS (XLIM(2)-XLIM(1))
      YAXLEN = ABS (YLIM(2)-YLIM(1))

      CALL SXGDEVINFO (DEVLIMS(1), DEVLIMS(2))
      CALL PANELTESS  (DEVLIMS(1)-XMARGIN-5., DEVLIMS(2)-YMARGIN - 8.,
     &                 YAXLEN/XAXLEN, NMAPS, NXMAP,  NYMAP)

      XAXLEN = NXMAP * XAXLEN
      YAXLEN = NYMAP * YAXLEN
      CALL SET_DISPLAY_SIZE (AX1LEN, AX2LEN, XAXLEN,  YAXLEN,
     &                       AXLENX, AXLENY, XMARGIN+5., YMARGIN+8.)

      DX = ABS (AXLENX / NXMAP)
      DY = ABS (AXLENY / NYMAP)

CD    PRINT *,'Map area tesselated',NXMAP,' by',NYMAP
CD    PRINT *,'Windows are ',DX,' by',DY,' mm'

      PLOTLIMS(1) = XMARGIN
      PLOTLIMS(2) = XMARGIN + AXLENX
      PLOTLIMS(3) = YMARGIN
      PLOTLIMS(4) = YMARGIN + AXLENY

*     Find appropriate tick intervals for axes

      V1 = XLIM(1)
      V2 = XLIM(2)
      CALL AUTORANGE (V1,V2,NV)
      DV = ABS (2.*(V2-V1)/NV)

      W1 = YLIM(1)
      W2 = YLIM(2)
      CALL AUTORANGE (W1,W2,NW)
      DW = ABS (2.*(W2-W1)/NW)

*     Save the existing LINK array and set to 1,2,3 for these maps (purely
*     so that the calculation of which data points are valid can proceed)

      DO J = 1,3
        SAVLINK(J) = LINK(J)
        LINK(J)    = J
      END DO

*     Put a label to the screen

      MAP_NAME = MAP_ID

      CALL SXGLTYPE      (0)
      CALL SXGLWEIGHT    (LWEIGHT)
      CALL SXGEXPAND     (CHARHT/3.)
      CALL SXGFONT       (1)

      IF (SHOW_2DHEAD) THEN
        CALL LABEL_MAP_SOURCE (MAP_RA, MAP_DEC)
      END IF

      IF (GREYSCALE) THEN
        CALL SETCOL5     (C5START, C5ROTAT, C5EXP)
        CALL COLOUR_PLOT (COLOUR_TABLE, IFAIL)
        CALL SET_COLOURS
      END IF

*     Now draw each map

      DO J = 1,NYMAP
        DO I = 1,NXMAP

          IMAP     = NXMAP*(J-1) + I
          IF (IMAP.GT.NMAPS) GO TO 997
          LOCATION = IPTR (IPLP(IMAP))

*         Set up window in mm

          XL = XMARGIN +     (I-1)*DX
          YL = YMARGIN + (NYMAP-J)*DY

*         Minimum and maximum on map

*         PRINT *, 'Address of current map = ', LOCATION
          CALL MAP_MAXMIN (%VAL(CNF_PVAL(LOCATION)), 
     :                     NAXX, NAXY, XLIM, YLIM,
     &                     BADPIX_VAL, ZMIN, ZMAX)
          IF (ZMAX.LT.ZMIN) THEN
            CALL SXGTIDLE
            PRINT *,'No good data points on map!'
            IFAIL = 64
            GO TO 998
          END IF

*         Work out contour levels

          ZMIN1 = ZMIN
          ZMAX1 = ZMAX

          IF (PLOTCONT .OR. OVERCONT) THEN
            IF (NMAPS.EQ.1) THEN
              CALL SETCLEVS (ZMIN, ZMAX, ZC, NZ, DZ, K1, K2)

            ELSE
              CALL AUTORANGE (ZMIN1, ZMAX1, NZ)

              NZ = MIN (30, NZ+1)
              DZ = (ZMAX1-ZMIN1)/FLOAT(NZ-1)
              K1 = 1
              K2 = 1

              DO K = 1, NZ
                ZC(K) = ZMIN1 + (K-1)*DZ
                IF (ZC(K) .LT. ZMIN) K1 = K+1
                IF (ZC(K) .LT. ZMAX) K2 = K
              END DO

            END IF

            IF (K2.LT.K1) THEN
              CALL SXGTIDLE
              PRINT *,'Map all one value'
              IFAIL = 18
              GO TO 998
            END IF

            NZ = K2 - K1 + 1

            IF (IDEV.LT.10 .OR. IDEV.GE.20 .OR.
     &          (IDEV.GE.14 .AND. IDEV.LE.17)) THEN
              call sxgtidle
              write (ilout,*) '----------------------------------------'
              write (ilout,*) 'Map number',2*(J-1)+I
              write (ilout,*) 'Contour levels used: (',K1,' to',K2,')'
              write (ilout,'((8(1X,F9.2)))',iostat=ierr) (ZC(K),K=K1,K2)
              write (ilout,*) '----------------------------------------'
              call sxgttgraph
            END IF

          ELSE     ! Contours not being used, but NZ has to be set.
            NZ = 1
          END IF

*         Determine the greyscaling if required

          IF (GREYSCALE) THEN
            IF (NMAPS.EQ.1) THEN
              CALL SETGSCAL (ZMIN, ZMAX, GRLIMITS)
            ELSE
              GRLIMITS(1) = ZMIN
              GRLIMITS(2) = ZMAX
            END IF
          END IF

*         Contour the map on the output device

          CALL SXGLTYPE     (0)
          CALL SXGVWINDOW   (XL, XL+DX, YL, YL+DY)
          CALL SXGLIMITS    (XLIM(1), XLIM(2), YLIM(1), YLIM(2))
*         PRINT *, 'Address of current map = ', LOCATION
*         PRINT *, 'Map size (x,y) = ', NAXX, NAXY
          CALL DRAW_MAP     (ZC(K1), NZ, %VAL(CNF_PVAL(LOCATION)), 
     :                       NAXX, NAXY,
     &                       CBEG(1), CEND(1), CEND(2), CBEG(2),
     &                       NAX(1)-1, NAX(2)-1, LXPIX, LYPIX,
     &                       LTPOS, LTZ, LTNEG, CONTOURS,
     &                       GREYSCALE, GRLIMITS,
     &                       BADPIX_VAL, SOME_PLOT)
          CALL SXGLTYPE     (0)

          IF (IPLP(IMAP).EQ.1) THEN
            PANEL_NAME = 'Tmax'
          ELSE IF (IPLP(IMAP).EQ.2) THEN
            PANEL_NAME = 'Vmax'
          ELSE IF (IPLP(IMAP).EQ.3) THEN
            PANEL_NAME = 'Integ. Int''y'
          ELSE IF (IPLP(IMAP).EQ.4) THEN
            PANEL_NAME = 'V(eq)'
          ELSE IF (IPLP(IMAP).EQ.5) THEN
            PANEL_NAME = 'Centroid'
          ELSE IF (IPLP(IMAP).EQ.6) THEN
            PANEL_NAME = '2nd Moment'
          END IF

          CALL SXGEXPAND    (CHARHT/3.)
          CALL LABEL_MAP_PANEL (PANEL_NAME, ZMIN, ZMAX)

          CALL SXGTICKSIZE  (0.,0.,0.,0.)
          CALL MAP_COORDS   (DMS, .FALSE., 1, 2, XLIM, YLIM,
     &                       MAP_RA, MAP_DEC)

*         Label "bottom" left hand corner map

          IF (I.EQ.1 .AND. J.EQ.NYMAP) THEN
            CALL SXGFONT      (1)
            CALL SXGLTYPE     (0)
            CALL SXGEXPAND    ((CHARHT/3.)/MIN(NXMAP,NYMAP))
            CALL SXGTICKSIZE  (0.,0.,0.,0.)
            CALL MAP_COORDS   (DMS, .TRUE., 1, 2, XLIM, YLIM,
     &                         MAP_RA, MAP_DEC)
          END IF

*         Label each box

          CALL SXGLIMITS    (0., 1., 0., 1.)
          CALL SXGEXPAND    (1.0)
          IF (GREYSCALE) CALL SXGSCI (7)
          IF (IPLP(IMAP).EQ.1) THEN
            CALL SXGLABEL (0.05, 0.87, 'Tmax')
          ELSE IF (IPLP(IMAP).EQ.2) THEN
            CALL SXGLABEL (0.05, 0.87, 'Vmax')
          ELSE IF (IPLP(IMAP).EQ.3) THEN
            CALL SXGLABEL (0.05, 0.87, 'Int T.dv')
          ELSE IF (IPLP(IMAP).EQ.4) THEN
            CALL SXGLABEL (0.05, 0.87, 'Weq')
          ELSE IF (IPLP(IMAP).EQ.5) THEN
            CALL SXGLABEL (0.05, 0.87, 'Centrd')
          ELSE IF (IPLP(IMAP).EQ.6) THEN
            CALL SXGLABEL (0.05, 0.87, '2nd Mom.')
          END IF
          CALL SXGSCI(1)

        END DO
      END DO

*     Scale bar

      IF (GREYSCALE .and. NMAPS.eq.1) THEN
        CALL SCALE_BAR   (GRLIMITS, PLOTLIMS, DEVLIMS, 1, 1, IFAIL)
      END IF

*     Go into interactive mode if required

  997 IF (TERMINAL.AND.INTERACTIVE) THEN
        OPTIONS = ' ? H C Q E G M W'
        IF (GREYSCALE)    OPTIONS=' 0 1 3'//OPTIONS
        IF (SXGCOLOROK()) OPTIONS=' 2 4 5'//OPTIONS
        CALL I2DOPT (%VAL(CNF_PVAL(IPTR(1))), OPTIONS, REPEAT,
     &                CONTOURS, GREYSCALE, GRLIMITS)
      END IF

*     Check that we plotted something on the last map

      IF (.NOT.SOME_PLOT) IFAIL = 42

*     Close the plot file (error return starts here)

  998 CALL ENDPLOT (IDEV, INTERACTIVE, .TRUE.)

*     Restore the LINK array

      DO J = 1,3
        LINK(J) = SAVLINK(J)
      END DO

*     Release virtual memory

  999 CONTINUE

      ISTAT = IFREEVM (IPTR)
      IF (ISTAT.ne.0) THEN
        PRINT *, '--- plot_line4 ---'
        PRINT *, '    error freeing virtual memory: ', ISTAT
      END IF

      RETURN
      END

C-----------------------------------------------------------------------
