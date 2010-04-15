C-----------------------------------------------------------------------

      SUBROUTINE PLOT_MAP (MAP, ZC, DZ, NZ, GRLIMITS,
     &                     ARRMAX, ARRMIN, CONTOURS,
     &                     GREYSCALE, XMARGIN, YMARGIN, IFAIL)

C   Routine to plot given map on desired MONGO device. Information about
C   scales etc is mostly transferred in COMMON blocks.

      IMPLICIT  NONE

C     Common blocks and include files

      INCLUDE  'FLAGCOMM'
      INCLUDE  'MAPHD'
      INCLUDE  'MAPS'
      INCLUDE  'MAPTITLES'
      INCLUDE  'PLOT2D'

C     Formal parameters:

      REAL      MAP(NAXX,NAXY)
      INTEGER   NZ
      REAL      ZC(NZ)
      REAL      DZ
      REAL      GRLIMITS(2)
      REAL      ARRMAX, ARRMIN
      LOGICAL   CONTOURS, GREYSCALE
      REAL      XMARGIN,  YMARGIN
      INTEGER   IFAIL

C     Local variables

      INTEGER   IX, IY, IZ
      INTEGER   NXCELL, NYCELL
      REAL      BCENX,  BCENY
      REAL      DEVLIMS(2)

      LOGICAL   SOME_PLOT

C     Functions:

      INTEGER   GEN_ILEN

C  Ok, go...

      IX = LINK(1)
      IY = LINK(2)
      IZ = LINK(3)
      NXCELL = NAX(IX)-1
      NYCELL = NAX(IY)-1

      CALL SXGEXPAND     (1.0)
      CALL SXGFONT       (1)
      CALL SXGLWEIGHT    (LWEIGHT)

C   Contour the map, including interpolation

      PLOTLIMS(1) = XMARGIN
      PLOTLIMS(2) = XMARGIN + AXLENX
      PLOTLIMS(3) = YMARGIN
      PLOTLIMS(4) = YMARGIN + AXLENY

      CALL SXGVWINDOW   (PLOTLIMS(1), PLOTLIMS(2),
     &                   PLOTLIMS(3), PLOTLIMS(4))
      CALL SXGLIMITS    (XLIM(1), XLIM(2), YLIM(1), YLIM(2))
      CALL DRAW_MAP  (ZC, NZ, MAP, NAXX, NAXY, CBEG(IX), CEND(IX),
     &                CEND(IY), CBEG(IY), NXCELL, NYCELL, LXPIX, LYPIX,
     &                LTPOS, LTZ, LTNEG, CONTOURS, GREYSCALE, GRLIMITS,
     &                BADPIX_VAL, SOME_PLOT)
      IF (MVPTS) CALL MARK_SAMPLES (MAP)

C     Draw a box and label it --- if map is rotated away from RA/Dec axes
C     then SET_MAPTITLE will have been called and the axis names will not
C     be R.A./Dec, which will be picked up in MAP_COORDS

      CALL SXGFONT      (1)
      CALL SXGLTYPE     (0)
      CALL SXGEXPAND    (CHARHT/4.)
      CALL SXGTICKSIZE  (0.,0.,0.,0.)
      CALL SXGPLOTID    (' ',NAMEMP(:GEN_ILEN(NAMEMP)))
      CALL MAP_COORDS   (DMS, .TRUE., IX, IY, XLIM, YLIM,
     &                   MAP_RA, MAP_DEC)

C  Indicate the beam on the map if required

      IF (SHOW_BEAM) THEN
        BCENX = XLIM(1) + SIGN (0.8*BSIZE, XLIM(2)-XLIM(1))
        BCENY = YLIM(1) + SIGN (0.8*BSIZE, YLIM(2)-YLIM(1))
        CALL SXGLWEIGHT     (3)
        CALL SXGRELOCATE    (BCENX-BSIZE/2, BCENY)
        CALL SXGDRAW        (BCENX+BSIZE/2, BCENY)
        CALL SXGRELOCATE    (BCENX, BCENY-BSIZE/2)
        CALL SXGDRAW        (BCENX, BCENY+BSIZE/2)
        CALL SXGLWEIGHT     (1)
        CALL SXGEXPAND      (2.0)
        CALL SXGPOINTS      (1, BCENX, BCENY, 17)
      END IF

C  Put a scale-bar on the plot

      IF (GREYSCALE) THEN
        CALL SXGDEVINFO     (DEVLIMS(1), DEVLIMS(2))
        CALL SCALE_BAR      (GRLIMITS, PLOTLIMS, DEVLIMS, 1, 1, IFAIL)
      END IF

C  Put a label to the screen in the best place

      CALL SXGEXPAND        (CHARHT/3.)
      CALL SXGFONT          (1)

      IF (SHOW_2DHEAD) THEN
        CALL LABEL_MAP_SOURCE (MAP_RA, MAP_DEC)
        CALL LABEL_MAP_Z      (ARRMAX, ARRMIN, PBEG(IZ), PEND(IZ))

        IF (CONTOURS) THEN
           IF (ICAUTO .OR. .NOT. CLEVELS_SET) THEN
              CALL LABEL_MAP_CONT (ZC, DZ, NZ)
           ELSE
              CALL LABEL_MAP_MCONT(ZC, NZ)
           END IF
        END IF

      END IF

      IF (.NOT.SOME_PLOT) IFAIL = 42

      RETURN
      END

C----------------------------------------------------------------------
