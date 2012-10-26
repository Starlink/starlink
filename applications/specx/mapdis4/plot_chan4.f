*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused SAVE_AUTO, K, NPAG, NMAX, NMIN
*---------------------------------------------------------------------------

      SUBROUTINE PLOT_CHAN4 (IFAIL)

*   Routine to plot channel maps of data stored in MAPPLANE.TMP
*   Maps were produced by MAKE_CHANNEL_MAPS

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   IFAIL

*     Include files

      INCLUDE 'CUBE'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'MAPTITLES'
      INCLUDE 'PLOT2D'
      INCLUDE 'STACKCOMM'
      INCLUDE 'CNF_PAR'

*     Local variables:

      LOGICAL   REPEAT
      LOGICAL   SOME_PLOT
      LOGICAL   GREYSCALE
      LOGICAL   CONTOURS
      LOGICAL   FIRST
      LOGICAL   SXGGREYOK
      LOGICAL   SXGCOLOROK
      INTEGER   I,  J
      INTEGER   IX, IY, IZ
      INTEGER   ISTAT
      INTEGER   IMX,         IMY
      INTEGER   BOFF,        IPTR_MAP
      INTEGER   NCONT1,      NCONT2
      INTEGER   NV,   NW,    NZ
      REAL      ARRMIN, ARRMAX
      REAL      DEVLIMS(2)
      REAL      DX,  DY,  DZ
      REAL      GRLIMITS(2)
      REAL      V1,  V2,  DV
      REAL      W1,  W2,  DW
      REAL      XL,  YL
      REAL      XAXLEN, YAXLEN
      REAL      ZMIN,   ZMAX
      REAL      ZC(32)

      REAL       XMARGIN
      PARAMETER (XMARGIN = 25.)
      REAL       YMARGIN
      PARAMETER (YMARGIN = 40.)

      CHARACTER OPTIONS*64

*     Misc common blocks

      INTEGER   NMAP_CM
      INTEGER   NB,   NE
      REAL      BEGZ, ENDZ
      COMMON /CHANMAP/ NMAP_CM, NB, NE, BEGZ, ENDZ

*     Functions:

      INTEGER   IFREEVM

*  Ok, go...

      IFAIL  = 0

*     Map file into virtual memory

      CALL MAPIMAGE ('mapplane.tmp', IPTR_MAP, NMAPS, IMX, IMY, ISTAT)
      IF (ISTAT.ne.0) THEN
        IFAIL = 67
        RETURN
      END IF

      PRINT *, ' --- Plot_chan4 ---'
      PRINT *, '     Number of maps in file = ', NMAPS
      PRINT *, '     Image map size',IMX,' by',IMY

*     Set up these indices for general use (describe which axis is which)

      IX   = LINK(1)
      IY   = LINK(2)
      IZ   = LINK(3)

*     Get number of maps across page

      CALL GEN_GETI4 ('How many maps across page? (0=auto)',
     &                 IXCMAP, 'I2', IXCMAP, ISTAT)

*     Use plot parameters set up by most recent CONTOUR-PLOT unless
*     there has been a DEFINE-MAP or SET-MAP-SCALES in the meantime

      IF (.NOT.CHANGE_PLOT) THEN
        XLIM(1) = PBEG(IX)
        XLIM(2) = PEND(IX)
        YLIM(1) = PEND(IY)
        YLIM(2) = PBEG(IY)
        AX1REQ  = AX1LEN
        AX2REQ  = AX2LEN
      END IF

*     Find appropriate tick intervals for axes

      V1 = XLIM(1)
      V2 = XLIM(2)
      CALL AUTORANGE (V1,V2,NV)
      DV = ABS (2.*(V2-V1)/NV)

      W1 = YLIM(1)
      W2 = YLIM(2)
      CALL AUTORANGE (W1,W2,NW)
      DW = ABS (2.*(W2-W1)/NW)

*     Find cumulative maximum and minimum on maps
*     Initialize max and min of maps

      ARRMAX = -1.E20
      ARRMIN = +1.E20

      DO I = 1, NMAP_CM
        CALL MAP_MAXMIN (%VAL(CNF_PVAL(IPTR_MAP)+4*(I-1)*NAXX*NAXY),
     &       NAXX, NAXY, XLIM, YLIM, BADPIX_VAL, ZMIN, ZMAX)
        ARRMIN = MIN (ARRMIN, ZMIN)
        ARRMAX = MAX (ARRMAX, ZMAX)
      END DO

C   Initialize graphics package now so that SET_DISPLAY_SIZE can work

      CALL ALLOCATE_DEVICE (IDEV,IFAIL)
      IF (IFAIL.NE.0) THEN
        PRINT *,'Trouble allocating plot device'
        GO TO 998
      END IF

      FIRST = .TRUE.

*     Now find an aesthetic layout for the screen
*     Size of each panel in "natural" units...

      XAXLEN = ABS (XLIM(2)-XLIM(1))
      YAXLEN = ABS (YLIM(2)-YLIM(1))

      CALL SXGDEVINFO (DEVLIMS(1), DEVLIMS(2))

      IF (IXCMAP.NE.0) THEN

        NXMAP = IXCMAP
        NYMAP = (NMAP_CM-1)/NXMAP + 1

      ELSE

*       Use an algorithm that (most of the time) maximizes the
*       area of each channel map. Uses actual size of device to
*       do it -- therefore will not do a good job if the map size
*       is set to something else entirely (would have to set number
*       of plots across page manually).

        CALL PANELTESS  (DEVLIMS(1)-XMARGIN-5., DEVLIMS(2)-YMARGIN-8.,
     &                   YAXLEN/XAXLEN, NMAP_CM, NXMAP,  NYMAP)

      END IF

C  Find overall size of the montage

      IF (.NOT.CHANGE_PLOT) THEN
        XAXLEN = NXMAP * XAXLEN
        YAXLEN = NYMAP * YAXLEN
        CALL SET_DISPLAY_SIZE (AX1LEN, AX2LEN, XAXLEN, YAXLEN,
     &                         AXLENX, AXLENY, XMARGIN+5., YMARGIN+8.)
      END IF

C  ...and size of individual maps

      DX = ABS (AXLENX / NXMAP)
      DY = ABS (AXLENY / NYMAP)

*     PRINT *,'Map area tesselated',NXMAP,' by',NYMAP
*     PRINT *,'Windows are ',DX,' by',DY,' mm'

      PLOTLIMS(1) = XMARGIN
      PLOTLIMS(2) = XMARGIN + AXLENX
      PLOTLIMS(3) = YMARGIN
      PLOTLIMS(4) = YMARGIN + AXLENY

C   Greyscale or contours?

      CONTOURS  = .NOT.(PLOTGREY.AND..NOT.OVERCONT)
      IF (PLOTGREY .AND..NOT. SXGGREYOK()) THEN
        PRINT *,'Sorry, greyscaling not available!'
        GREYSCALE = .FALSE.
      ELSE
        GREYSCALE = PLOTGREY
      END IF
      IF (.NOT.(GREYSCALE.OR.CONTOURS)) RETURN

C   Set colour table if grey-scaling

      IF (GREYSCALE .AND. FIRST) THEN
        CALL SETCOL5     (C5START, C5ROTAT, C5EXP)
        CALL COLOUR_PLOT (COLOUR_TABLE, IFAIL)
        CALL SET_COLOURS
        FIRST = .FALSE.
      END IF

      REPEAT = .TRUE.
      DO WHILE (REPEAT)

C  Work out contour levels and greyscale levels

        CALL SETCLEVS (ARRMIN, ARRMAX, ZC, NZ, DZ, NCONT1, NCONT2)
        CALL SETGSCAL (ARRMIN, ARRMAX, GRLIMITS)

C  Put a label to the screen

        CALL SXGLTYPE      (0)
        CALL SXGLWEIGHT    (LWEIGHT)
        CALL SXGEXPAND     (CHARHT/3.)
        CALL SXGFONT       (1)

        IF (SHOW_2DHEAD) THEN
          CALL LABEL_MAP_SOURCE (MAP_RA, MAP_DEC)
          CALL LABEL_MAP_ZRANGE (BEGZ,   ENDZ,    ZWID)

          IF ( .NOT. CLEVELS_SET) THEN
            CALL LABEL_MAP_CONT (ZC, DZ, NZ)
          ELSE
            CALL LABEL_MAP_MCONT(ZC, NZ)
          END IF
        END IF

C  Reset velocity range to first channel values

        PBEG(IZ) = ZZERO - NB*ZWID
        PEND(IZ) = PBEG(IZ) + SIGN (ZWID,(ENDZ-BEGZ))

        DO J = 1, NYMAP
          DO I = 1, MIN (NXMAP, NMAP_CM-(J-1)*NXMAP)

C  Set up window in mm

            XL = XMARGIN + (I-1)*DX
            YL = YMARGIN + (NYMAP-J)*DY

C  Set up pointer to this channel map

            BOFF = 4*NAXX*NAXY*((J-1)*NXMAP + (I-1))

C   Contour the map on the output device and draw a box

            CALL SXGVWINDOW     (XL, XL+DX, YL, YL+DY)
            CALL SXGLIMITS      (XLIM(1), XLIM(2), YLIM(1), YLIM(2))
            CALL DRAW_MAP       (ZC, NZ, %VAL(CNF_PVAL(IPTR_MAP)+BOFF),
     :                           NAXX, NAXY,
     &                           CBEG(IX), CEND(IX), CEND(IY), CBEG(IY),
     &                           NAX(IX)-1, NAX(IY)-1, LXPIX, LYPIX,
     &                           LTPOS, LTZ, LTNEG, CONTOURS,
     &                           GREYSCALE, GRLIMITS, BADPIX_VAL,
     &                           SOME_PLOT)
            CALL SXGTICKSIZE    (0.,0.,0.,0.)
            CALL MAP_COORDS     (DMS, .FALSE.,  IX, IY, XLIM, YLIM,
     &                           MAP_RA, MAP_DEC)
            CALL SXGLTYPE       (0)

C   Label bottom left hand corner map

            IF (I.EQ.1 .AND. J.EQ.NYMAP) THEN
              CALL SXGFONT      (1)
              CALL SXGTICKSIZE  (0.,0.,0.,0.)
              CALL MAP_COORDS   (DMS, .TRUE.,  IX, IY, XLIM, YLIM,
     &                           MAP_RA, MAP_DEC)
            END IF

C   Mark valid sample points if so wished.

            IF (MVPTS) CALL MARK_SAMPLES (%VAL(CNF_PVAL(IPTR_MAP)))

C   Label the velocity range (note ZLABEL: redefines the window)

            IF (GREYSCALE) CALL SXGSCI (7)
            CALL ZLABEL         (PBEG(IZ), PEND(IZ))
            CALL SXGSCI (1)

C   End of iteration over channels

            PBEG(IZ) = PEND(IZ)
            PEND(IZ) = PEND(IZ) + SIGN (ZWID,(ENDZ-BEGZ))

          END DO
        END DO

C   Do scale bar if required

        IF (GREYSCALE) THEN
          CALL SCALE_BAR  (GRLIMITS, PLOTLIMS, DEVLIMS,
     &                     NXMAP,    NYMAP,    IFAIL)
        END IF

C   If interactive mode ON then invoke the cursor

        REPEAT = .FALSE.
        IF (TERMINAL .AND. INTERACTIVE) THEN
          OPTIONS = ' H N Q E C M ? I W'
          IF (IZ.EQ.3)      OPTIONS = ' G'//OPTIONS
          IF (GREYSCALE)    OPTIONS = ' 0 1 3'//OPTIONS
          IF (SXGCOLOROK()) OPTIONS = ' 2 4 5'//OPTIONS
          CALL I2DOPT (%VAL(CNF_PVAL(IPTR_MAP)), OPTIONS,
     &                 REPEAT, CONTOURS, GREYSCALE, GRLIMITS)
        END IF
      END DO

C   If nothing plotted then set the -I- error flag

      IF (.NOT.SOME_PLOT) IFAIL = 42

C   Close the plot

  998 CALL ENDPLOT (IDEV, INTERACTIVE, .TRUE.)

C   Restore the range values for the Z-dimension

      PBEG(IZ) = BEGZ
      PEND(IZ) = ENDZ

C   Close the map file

  999 CONTINUE

      ISTAT = IFREEVM (IPTR_MAP)
      IF (ISTAT .ne. 0) THEN
        PRINT *, '--- plot_chan4 ---'
        PRINT *, '    error freeing virtual memory: ', ISTAT
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      subroutine paneltess (xsiz, ysiz, ratio, n, nxmax, nymax)

*  Subroutine to tesselate arbitrary display surface into *at least*
*  n panels, when size of display surface and aspect ratio of panels
*  are already defined. Search for nice tesselation that maximizes
*  the area of the display surface used.

      implicit none

*     formal parameters:

      real     xsiz, ysiz     ! size of display surface
      real     ratio          ! aspect ratio of individual panel
      integer  n              ! number of panels required
      integer  nxmax, nymax   ! tesselation results

*     local variables

      integer  i
      integer  nx, ny
      real     maxarea
      real     area
      real     scal

*  Ok, go...

CD    PRINT *, ' -- paneltess --'
CD    PRINT *, '    plot surface area (x*y) = ', xsiz, ysiz
CD    PRINT *, '    tesselate with ', n, ' panels, aspect ratio ', ratio

      maxarea = 0.0

      do i = 1, n
        nx = i
        ny = (n-1)/nx + 1
        scal = min (xsiz/float(nx), (ysiz/ratio)/float(ny))
        area = n * ratio * scal**2

CD      print *, 'nx, ny, scal, area: ', nx, ny, scal, area

        if (area .gt. maxarea) then
          nxmax   = nx
          nymax   = ny
          maxarea = area
        end if
      end do

CD    print *, 'final values of nx, ny = ', nxmax, nymax
CD    print *, 'fractional area used = ', maxarea/(xsiz*ysiz)

      return
      end

*-----------------------------------------------------------------------
