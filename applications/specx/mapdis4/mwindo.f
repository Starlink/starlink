*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------

      SUBROUTINE MWINDO (SCALE, NPTS, IFAIL)

C  Routine to set up windowing function for maps by returning
C  starting position (IOFF+1) and number of points (NAX) for
C  each axis (R.A., Dec., and velocity, in that order)
C  This version revised by RP at 20/12/87
C    - no longer calls ISIZE, ISTEP as argument: gets from MAPHD instead
C    - does not alter values of PBEG, PEND
C    - calculates values of PFAC for inclusion in FLAGCOMM

C  The reference position for the map is the top left (maybe should have been
C  bottom left!)

C  INVERT_AXIS is a logical array: For each dimension (RA, Dec, Velocity)
C  in order INVERT_AXIS is TRUE if the requested map is not the "normal"
C  way round (R.A. decreasing to right, Dec decreasing down, Velocity in
C  same order as data coming from map file).

      IMPLICIT  NONE

*     Formal parameters:

      REAL*4    SCALE(*)
      INTEGER   NPTS
      INTEGER   IFAIL

*     Common blocks/include files

      INCLUDE   'MAPHD'
      INCLUDE   'FLAGCOMM'
      INCLUDE   'PLOT2D'

      LOGICAL*4 INVERT_AXIS(3)
      COMMON /GOOD_PT/ INVERT_AXIS

*     Local variables

      INTEGER    I
      INTEGER    NX
      INTEGER    N1, N2
      REAL       EPS
      REAL       X1, X2

*  Ok, go...

      IFAIL=0

      PFAC(1) = -CELL_XSIZE   !/60.
      PFAC(2) = -CELL_YSIZE   !/60.

C  SETMAX is a function equivalent to SETX to SET Map AXes - generates a
C  SCALE array containing all values from start to end.

CD    PRINT *,'--MWINDO--'
      DO I = 1,3

        IF (I.EQ.1) THEN
          CALL SETMAX (SCALE, MSTEP, CELL_XSIZE)
          NX = MSTEP
CD        PRINT *,'R.A. calculation'
        ELSE IF (I.EQ.2) THEN
          CALL SETMAX (SCALE, NSTEP, CELL_YSIZE)
          NX = NSTEP
CD        PRINT *,'Dec. calculation'
        ELSE
          CALL SETXNEW (SCALE, IFAIL)
          PFAC(3) = XFAC(1)
          NX = NPTS
CD        PRINT *,'Vel. calculation'
        END IF

        IF (IFAIL.NE.0) THEN
          PRINT *,' --- mwindo ---  error in spectral-axis calculation'
          RETURN
        END IF

        EPS = 1.E-5 * (PEND(I) -PBEG(I))

        INVERT_AXIS(I) = ((PEND(I)-PBEG(I))*PFAC(I).LT.0.)
        CALL PLIMITS (SCALE, NX, PFAC(I), PBEG(I)+EPS, PEND(I)-EPS,
     &                 N1, N2, PF1(I), PF2(I), X1, X2, IFAIL)

        NAX(I)  = N2 - N1 + 1
        IOFF(I) = N1 - 1
        CBEG(I) = X1
        CEND(I) = X2

CD      PRINT *,'MWINDO - calculation for axis ',I
CD      PRINT *,'  plimits return status = ', IFAIL
CD      PRINT *,'  Range:     ', PBEG(I),PEND(I)
CD      PRINT *,'  Intervals: ', N1,N2
CD      PRINT *,'  Fractions: ', PF1(I),PF2(I)
CD      PRINT *,'  Pixel vals:', CBEG(I),CEND(I)
CD      PRINT *,'  #pixels:   ', NAX(I)
CD      PRINT *,'  Offset:    ', IOFF(I)

      END DO

   99 CONTINUE

      RETURN
      END

C-----------------------------------------------------------------------

      SUBROUTINE PLIMITS (X, N, DX, V1, V2, N1, N2, F1, F2,
     &                    XN1, XN2, IFAIL)

      IMPLICIT  NONE

C  Formal parameters:
C
      INTEGER    N         ! - Length of array
      REAL       X(N)      ! - array containing values of scale array
      REAL       DX        ! - array increment
      REAL       V1, V2    ! - values to be located in array
      INTEGER    N1, N2    ! - channel in which V1 and V2 fall
      REAL       F1, F2    ! - fraction of end channels (N1,N2) "occupied" by
                           !   interval (V1,V2)
      REAL       XN1,XN2   ! - centres of channels N1 and N2
      INTEGER    IFAIL     ! - SPECX error return: 0 = OK


*     Functions:

      REAL   XLOCATE

*     Local variables:

      REAL   X1, X2

* Ok, go...

      IFAIL = 0

C     XLOCATE is a routine to return x-value in fractional points
C     corresponding to x-value V supplied.

C     X1 is accurate regardless of whether
C     it is actually IN the array or not.

      X1 = XLOCATE (X, N, DX, V1, IFAIL)
      IF (IFAIL.NE.0) THEN
CD       PRINT *,'Problem locating first value in range'
        IFAIL = 0
      END IF

      X2 = XLOCATE (X, N, DX, V2, IFAIL)
      IF (IFAIL.NE.0) THEN
CD      PRINT *,'Problem locating second value in range'
        IFAIL = 0
      END IF

C  Clip values of X1 and X2 to lie inside total range of
C  unit-width intervals centred on X-values.

      X1 = MIN (MAX(X1,0.5), (0.49999+N))
      X2 = MAX (MIN(X2,(0.49999+N)), 0.5)

C     Return X1 and X2 as positions associated with pixels N1 and N2 that
C     contain the range (V1,V2)

      N1  = NINT (X1)
      N2  = NINT (X2)
      XN1 = X(N1)
      XN2 = X(N2)

      IF (X1.EQ.X2) THEN
        IFAIL = 53
        GO TO 99
      END IF

C     Make sure points are right way round so that NAX, IOFF will be +ve
C     Simplest test is whether N1 is greater than N2, but this fails when
C     N1 = N2. So instead have to test V1 and V2 against X(N) and X(1).

      IF ((V2-V1)*(X(N)-X(1)).LT.0.0)  THEN
        CALL SWAP2 (N1,N2)
        CALL SWAP2 (X1,X2)
      END IF

C     Now we have this right we can calculate fractional parts of
C     end pixels "occupied" (depends on knowing that X1 is to "left" of X2)

      F1 = 0.5 - (X1-N1)
      F2 = 0.5 + (X2-N2)

   99 CONTINUE

      RETURN
      END

C-------------------------------------------------------------------

      REAL FUNCTION XLOCATE (XSCALE, N, DX, XBAR, IFAIL)

C   Routine to return x-value in fractional points corresponding to
C   x-value XBAR.  Derived from XSNART

      IMPLICIT  NONE

*     Formal parameters

      REAL    XSCALE(*)
      INTEGER N
      REAL    DX
      REAL    XBAR
      INTEGER IFAIL

*  Ok, go...
      XLOCATE = 1.0

      IF (DX.EQ.0.0) THEN
        IFAIL = 35
        RETURN
      END IF

      XLOCATE = (XBAR-XSCALE(1))/DX + 1.0

      IF (XLOCATE.GT.FLOAT(N)+0.5 .OR. XLOCATE.LT.0.5)   THEN
        IFAIL  = 26
      END IF

CD    PRINT *,'-- XLOCATE --'
CD    PRINT *,'n, dx, xbar: ', n, dx, xbar
CD    PRINT *,'x: ',xscale(1),' ... ',xscale(n)
CD    PRINT *,'xlocate: ', xlocate

      RETURN
      END

C-----------------------------------------------------------------------
