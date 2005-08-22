*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused K
*-----------------------------------------------------------------------

      SUBROUTINE CONVARR (A1, M1, N1, A2, M2, N2, BADPIX, IFAIL)

*   Convolve the array with interpolating function.

      IMPLICIT NONE

C   Formal parameter:

      INTEGER   M1, N1
      INTEGER   M2, N2
      REAL      A1(M1, N1)
      REAL      A2(M2, N2)
      REAL      BADPIX
      INTEGER   IFAIL

C   Parameters:

      INTEGER   IWMAX
      PARAMETER (IWMAX=20)

      REAL      PI
      PARAMETER (PI=3.141592654)

C   Miscellaneous variables

      LOGICAL*4 INTERP
      INTEGER*4 I,J,M,N         ! Counters for output array
      INTEGER*4 II, JJ            ! More counters for input array.
      INTEGER*4 II1, II2          ! Left and right boundaries of original cell
      INTEGER*4 JJ1, JJ2          ! Bottom and top boundaries of original cell
      INTEGER*4 IXMAX, IYMAX      ! Extent of interpolating function in pixels
      INTEGER*4 MMIN, MMAX        ! Interpolation limits in X (pixels)
      INTEGER*4 NMIN, NMAX        ! Interpolation limits in Y (pixels)
      INTEGER*4 INX,  INY         ! Interpolation factor (extra points / pixel)
      REAL*4    CURRENT_WEIGHT    ! Weight value for current sample
      REAL*4    SIGMA_W           ! Sum of weights
      REAL*4    WEIGHT(0:IWMAX,0:IWMAX) ! Value of beam at point
      REAL*4    VAL
      REAL*4    X, Y
      REAL*4    TAPER

C  OK, go...

      IFAIL = 0

C     Find interpolation factors

      INX   = (M2-1)/(M1-1)
      INY   = (N2-1)/(N1-1)
CD    PRINT *, 'Interpolating factors: INX, INY = ', INX, INY

C     How far to interpolate?

      IXMAX = 2*INX
      IYMAX = 2*INY
CD    PRINT *, 'Interp func. radii: IXMAX,IYMAX = ', IXMAX, IYMAX

C   Check that weights array is small enough

      IF (IXMAX.GT.IWMAX .OR. IYMAX.GT.IWMAX) THEN
        PRINT *,'Maximum radius of interp''g function exceeded'
        IFAIL = 18
        RETURN
      END IF

C     Set up the weights array (to save working out lots of trig functions later

      DO I = 0,IXMAX
        X = PI*FLOAT(I)/FLOAT(INX)

        DO J = 0,IYMAX
          Y = PI*FLOAT(J)/FLOAT(INY)

          IF (X.EQ.0.0 .AND. Y.EQ.0.0) THEN
            WEIGHT(I,J) = 1.0
          ELSE IF (X.EQ.0.0) THEN
            WEIGHT(I,J) = SIN(Y)/Y
          ELSE IF (Y.EQ.0.0) THEN
            WEIGHT(I,J) = SIN(X)/X
          ELSE
            WEIGHT(I,J) = SIN(X)*SIN(Y)/(X*Y)
          END IF

          TAPER = EXP(-((FLOAT(I)/IXMAX)**2 + (FLOAT(J)/IYMAX)**2))
          WEIGHT(I,J) = WEIGHT(I,J)*TAPER

        END DO
      END DO

CD    IF (IXMAX.LE.10) THEN
CD      PRINT *
CD      PRINT *,'Weights array:'
CD      Type '(<IXMAX+1>(2X,F9.5))',((WEIGHT(I,J),I=0,IXMAX),J=0,IYMAX)
CD    ELSE
CD      PRINT *,'Weights array: too big to type out!'
CD    END IF

C     Then do the interpolation: M & N are indices in output array.
C     Mmin and Nmin are indices in output array of first and last sample
C      points covered by the interpolating function. Note that we only need
C      the interpolating function at the original sample points, as there are
C      no data in between.

      DO N = 1,N2

        NMIN = MAX (N-IYMAX,  1)
        NMIN = NMIN + (INY-1)- MOD(NMIN+INY-2,INY)

        NMAX = MIN (N+IYMAX, N2)
        NMAX = NMAX - MOD(NMAX-1,INY)

        DO M = 1,M2

          MMIN = MAX (M-IXMAX,  1)
          MMIN = MMIN + (INX-1)- MOD(MMIN+INX-2,INX)

          MMAX = MIN (M+IXMAX, M2)
          MMAX = MMAX - MOD(MMAX-1,INX)

          SIGMA_W = 0.0
          VAL     = 0.0

C         Check that point lies in a box with good data at all corners

          II1 = MAX (1,      (M-1)/INX+1 )
          II2 = MIN (M1, (M+INX-2)/INX+1 )

          JJ1 = MAX (1,      (N-1)/INY+1 )
          JJ2 = MIN (N1, (N+INY-2)/INY+1 )

          IF (       A1(II1,JJ1).NE.BADPIX
     &         .AND. A1(II1,JJ2).NE.BADPIX
     &         .AND. A1(II2,JJ1).NE.BADPIX
     &         .AND. A1(II2,JJ2).NE.BADPIX ) THEN

            DO J = NMIN, NMAX, INY
              DO I = MMIN, MMAX, INX

C               Work out indices in input array

                II = (I-1)/INX + 1
                JJ = (J-1)/INY + 1

                IF (A1(II,JJ).NE.BADPIX) THEN

                  INTERP = .TRUE.

                  CURRENT_WEIGHT = WEIGHT(IABS(M-I),IABS(N-J))
                  SIGMA_W        = SIGMA_W + CURRENT_WEIGHT
                  VAL            = VAL + CURRENT_WEIGHT * A1(II,JJ)

                END IF
              END DO
            END DO
          END IF

C         Go ahead and put properly weighted data back into the cube.

          IF (SIGMA_W.NE.0.) THEN
            A2(M,N) = VAL/SIGMA_W
          ELSE
            A2(M,N) = BADPIX
          END IF

        END DO
      END DO

      RETURN
      END

C-----------------------------------------------------------------------
