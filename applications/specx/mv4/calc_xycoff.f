*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------
      SUBROUTINE CALC_XYCOFF (X_OFFSET, Y_OFFSET,
     &                        CELL_XSIZE, CELL_YSIZE,
     &                        MSTEP, NSTEP, XCELL, YCELL,
     &                        IFAIL)

*  Routine to evaluate (X,Y) offsets of pixel centre (in cells).
*  Processing depends on whether there are an even or odd number
*  of pixels on each map axis.

      IMPLICIT  NONE

*     Formal parameters:

      REAL      X_OFFSET
      REAL      Y_OFFSET
      REAL      CELL_XSIZE
      REAL      CELL_YSIZE
      INTEGER   MSTEP,  NSTEP
      REAL      XCELL,  YCELL
      INTEGER   IFAIL

*     Local variables:

      LOGICAL   EVEN
      INTEGER   IXOFF2, IYOFF2
      REAL      XOFF,   YOFF

*  Ok, go...

      IFAIL = 0
CD    PRINT *, ' -- calc_xycoff2 --'

*     Check that we won't end up with a divide by zero...

      IF (CELL_XSIZE.EQ.0.0 .OR. CELL_YSIZE.EQ.0.0) THEN
        PRINT *, '    One or more map cell dimensions are zero!'
        IFAIL = 35
        RETURN
      END IF

*     Locate the nearest pixel centre for the X-axis

      EVEN = MOD (MSTEP,2) .EQ. 0
      XOFF = X_OFFSET/CELL_XSIZE

      IF (EVEN) THEN
        IXOFF2 = 2*NINT (XOFF - SIGN(XOFF,0.5)) + NINT (SIGN(XOFF,1.0))
CD      PRINT *, '    map has even number of pixels in X'
      ELSE
        IXOFF2 = 2*NINT (XOFF)
      END IF

*     ... and the Y-axis

      EVEN = MOD (NSTEP,2) .EQ. 0
      YOFF = Y_OFFSET/CELL_YSIZE

      IF (EVEN) THEN
        IYOFF2 = 2*NINT (YOFF - SIGN(YOFF,0.5)) + NINT (SIGN(YOFF,1.0))
CD      PRINT *, '    map has even number of pixels in Y'
      ELSE
        IYOFF2 = 2*NINT (YOFF)
      END IF

      XCELL = FLOAT(IXOFF2)/2.
      YCELL = FLOAT(IYOFF2)/2.

CD    PRINT *, '    Cell X, Y offsets (cells): ', XCELL, YCELL

      RETURN
      END
