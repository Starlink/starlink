*  History:
*     10 Feb 1994 (hme):
*        Fix calls to SXGTICKSIZE: All arguments must be REAL, they were
*        integer constants.
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused GREYRANGE
*-----------------------------------------------------------------------

      SUBROUTINE SCALE_BAR (GREYLIMS, PLOTLIMS, DEVLIMS, NX, NY, IFAIL)

*  Routine to plot a scale bar in the least used margin of the plot

      IMPLICIT  NONE

*     Formal paramters:

      REAL      GREYLIMS(2)     ! Min and max values on grey scale
      REAL      PLOTLIMS(4)     ! Xmin, Xmax and Ymin, Ymax of plot area
      REAL      DEVLIMS(2)      ! Xmax and Ymax of device
      INTEGER   NX, NY          ! Number of maps in X & Y directions
      INTEGER   IFAIL           ! Error return

*     Local variables

      INTEGER   I
      LOGICAL   ABOVE
      REAL      XLEN, YLEN
      REAL      XMID, YMID
      REAL      SCALE (512)

*  Ok, go...

      IFAIL = 0

*     Type out input stuff:

*     PRINT *, ' -- scale_bar --'
*     PRINT *, '    greyscale limits: ', GREYLIMS
*     PRINT *, '    plot limits:      ', PLOTLIMS
*     PRINT *, '    device size:      ', DEVLIMS
*     PRINT *, '    nx and ny:        ', NX, NY

*     Decide whether to put scale bar above plot or to right: depends on
*     useage of plot area: put scale bar where there is most absolute room

      ABOVE = (DEVLIMS(2)-PLOTLIMS(4) .gt. DEVLIMS(1)-PLOTLIMS(2))

*     Fill in scale array --- depends on orientation

      IF (ABOVE) THEN
        DO I = 1, 256
          SCALE(I)     = GREYLIMS(1)
     &                   + (GREYLIMS(2)-GREYLIMS(1))*(I-1)/255.
          SCALE(I+256) = SCALE(I)
        END DO
      ELSE
        DO I = 1, 256
          SCALE(2*I-1) = GREYLIMS(1)
     &                   + (GREYLIMS(2)-GREYLIMS(1))*(I-1)/255.
          SCALE(2*I)   = SCALE(2*I-1)
        END DO
      END IF

*     Window off area and plot scale bar: Make extent the same as that
*     of one map, and centre on the image.

      IF (ABOVE) THEN
        XLEN = (PLOTLIMS(2) - PLOTLIMS(1)) / MIN (2, NX)
        YLEN = 8.0
        XMID = (PLOTLIMS(2) + PLOTLIMS(1)) / 2.
        YMID =  PLOTLIMS(4) + 12.
        CALL SXGVWINDOW  (XMID-XLEN/2., XMID+XLEN/2.,
     &                    YMID-YLEN/2., YMID+YLEN/2.)
        CALL SXGLIMITS   (GREYLIMS(1), GREYLIMS(2), 0., 1.)
        CALL SXGTICKSIZE (0., 0., 2., 2.)
        CALL SXGEXPAND   (2./MAX(2, NX))
        CALL SXGBOX      (1, 0)
        CALL SXGGREY     (SCALE, 256, 2, GREYLIMS(1), GREYLIMS(2),
     &                    0., 1., GREYLIMS)
        CALL SXGEXPAND   (0.)
        CALL SXGBOX      (0, 0)
      ELSE
        XLEN = 8.0
        YLEN = (PLOTLIMS(4) - PLOTLIMS(3)) / MIN (2, NY)
        XMID =  PLOTLIMS(2) + 20.
        YMID = (PLOTLIMS(4) + PLOTLIMS(3)) / 2.
        CALL SXGVWINDOW  (XMID-XLEN/2., XMID+XLEN/2.,
     &                    YMID-YLEN/2., YMID+YLEN/2.)
        CALL SXGLIMITS   (0., 1., GREYLIMS(1), GREYLIMS(2))
        CALL SXGTICKSIZE (2., 2., 0., 0.)
        CALL SXGEXPAND   (2./MAX(2, NY))
        CALL SXGBOX      (0, 2)
        CALL SXGGREY     (SCALE, 2, 256, 0., 1.,
     &                    GREYLIMS(1), GREYLIMS(2), GREYLIMS)
        CALL SXGEXPAND   (0.)
        CALL SXGBOX      (0, 0)
      END IF

      RETURN
      END

*-----------------------------------------------------------------------
