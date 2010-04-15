*     01 Jan 1994 (rp):
*        name of common block GOODPT apparently changed to GOOD_PT.
*        -- assume to avoid conflict with routine name; duplicate change here
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------

      SUBROUTINE MAP_WINDOW (SCALE, NPTS2, IFAIL)

C  Routine to set up windowing function for maps by returning
C  starting position (IOFF+1) and number of points (NAX) for
C  each axis (R.A., Dec., and velocity, in that order)
C  *** REVISED VERSION OF MWINDO -- BETTER DIAGNOSTICS ***
C  RP, 7-Jun-92

C  The reference position for the map is the top left (maybe should have been
C  bottom left!)

C  INVERT_AXIS is a logical array: For each dimension (RA, Dec, Velocity)
C  in order INVERT_AXIS is TRUE if the requested map is not the "normal"
C  way round (R.A. decreasing to right, Dec decreasing down, Velocity in
C  same order as data coming from map file).

      IMPLICIT  NONE

*     Formal parameters

      REAL      SCALE(*)
      INTEGER   NPTS2
      INTEGER   IFAIL

*     Include files

      INCLUDE   'MAPHD'
      INCLUDE   'FLAGCOMM'

*  Ok, go...

      IFAIL=0

C     SETMAX is a function equivalent to SETX to SET Map AXes - generates a
C     SCALE array containing all values from start to end.

*     PRINT *, ' -- map_window --'

*     PRINT *, '    R.A. calculation'
      CALL SETMAX (SCALE, MSTEP, CELL_XSIZE)
      CALL SPECX_AXIDEF (1, SCALE, MSTEP, -CELL_XSIZE,
     &                   PBEG(1), PEND(1), IFAIL)
      IF (IFAIL.NE.0) THEN
        PRINT *, ' -- map_window --'
        PRINT *, '    Error setting windowing for R.A./X-axis'
        PRINT *, '    Requested window from ', PBEG(1), ' to ', PEND(1)
        PRINT *, '    Map extent in this axis is ', SCALE(1), ' to ',
     &               SCALE(MSTEP)
        GO TO 999
      END IF

*     PRINT *, '    Dec. calculation'
      CALL SETMAX (SCALE, NSTEP, CELL_YSIZE)
      CALL SPECX_AXIDEF (2, SCALE, NSTEP, -CELL_YSIZE,
     &                   PBEG(2), PEND(2), IFAIL)
      IF (IFAIL.NE.0) THEN
        PRINT *, ' -- map_window --'
        PRINT *, '    Error setting windowing for Dec./Y-axis'
        PRINT *, '    Requested window from ', PBEG(2), ' to ', PEND(2)
        PRINT *, '    Map extent in this axis is ', SCALE(1), ' to ',
     &               SCALE(NSTEP)
        GO TO 999
      END IF

*     PRINT *, '    Vel. calculation'
      CALL SETXNEW (SCALE, IFAIL)
*     PRINT *, '    # points in X-array = ', npts2
*     PRINT *, '    channel width       = ', xfac(1)

      CALL SPECX_AXIDEF (3, SCALE, NPTS2, XFAC(1),
     &                   PBEG(3), PEND(3), IFAIL)
      IF (IFAIL.NE.0) THEN
        PRINT *, ' -- map_window --'
        PRINT *, '    Error setting windowing for Freq/Vel/etc scale'
        PRINT *, '    Requested window from ', PBEG(3), ' to ', PEND(3)
        PRINT *, '    Map extent in this axis is ', SCALE(1), ' to ',
     &               SCALE(NPTS2)
        GO TO 999
      END IF

  999 CONTINUE

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SPECX_AXIDEF (I, SCALE, NX, FACTOR, PBEG, PEND, IFAIL)

      IMPLICIT  NONE

*     Formal parameters

      INTEGER   I            ! Axis being operated on
      REAL      SCALE(*)     ! Workspace array
      INTEGER   NX           ! Length of SCALE
      REAL      FACTOR       ! Increment per cell in SCALE
      REAL      PBEG, PEND   ! Requested beginning and end point for this axis
      INTEGER   IFAIL        ! Error return

*     Include files

      INCLUDE   'PLOT2D'

*     Common blocks (for memory!)

      LOGICAL*4 INVERT_AXIS(3)
      COMMON /GOOD_PT/ INVERT_AXIS

*     Local variables

      INTEGER  N1, N2
      REAL     X1, X2
      REAL     EPS

*  Ok, go...

      PFAC(I) = FACTOR

      EPS = 1.E-5 * (PEND - PBEG)

      INVERT_AXIS(I) = ((PEND-PBEG)*PFAC(I).LT.0.)
      CALL PLIMITS (SCALE, NX, PFAC(I), PBEG+EPS, PEND-EPS,
     &              N1, N2, PF1(I), PF2(I), X1, X2, IFAIL)

      NAX(I)  = N2 - N1 + 1
      IOFF(I) = N1 - 1
      CBEG(I) = X1
      CEND(I) = X2

*     PRINT *, ' -- specx_axidef --'
*     PRINT *, '    calculation for axis ', I
*     PRINT *, '    plimits return status = ', IFAIL
*     PRINT *, '    Range:     ', PBEG,PEND
*     PRINT *, '    Intervals: ', N1,N2
*     PRINT *, '    Fractions: ', PF1(I),PF2(I)
*     PRINT *, '    Pixel vals:', CBEG(I),CEND(I)
*     PRINT *, '    #pixels:   ', NAX(I)
*     PRINT *, '    Offset:    ', IOFF(I)

   99 CONTINUE

      RETURN
      END

C-----------------------------------------------------------------------
