*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused K, II1, II2, JJ1, JJ2, X, Y
*-----------------------------------------------------------------------

      SUBROUTINE INTERPARR (A1, M1, N1, A2, M2, N2, BADPIX,
     &                      FWHMX, WMAXX, FWHMY, WMAXY,
     &                      CELLX, CELLY, IFAIL)

*   Interpolate the array with beam-shaped interpolating function.
*   Input array  in A1(M1,N1), output array in A2(M2,N2).
*   Will apply interpolation at both original sample points and at
*   intermediate points (so that contours are smoother...)

      IMPLICIT NONE

C   Formal parameter:

      INTEGER   M1, N1
      INTEGER   M2, N2
      REAL      A1(M1, N1)
      REAL      A2(M2, N2)
      REAL      BADPIX
      REAL      FWHMX, WMAXX    ! Fwhm and max radius of X-beam
      REAL      FWHMY, WMAXY    ! Fwhm and max radius of Y-beam
      REAL      CELLX, CELLY    ! Input map cell X and Y sizes.
      INTEGER   IFAIL

C   Parameters:

      INTEGER   IWMAX
      PARAMETER (IWMAX=20)

      REAL      PI
      PARAMETER (PI=3.141592654)

C   Miscellaneous variables

      LOGICAL   INTERP
      LOGICAL   GXMINUS, GXPLUS
      LOGICAL   GYMINUS, GYPLUS
      INTEGER   I,J,M,N           ! Counters for output array
      INTEGER   II, JJ            ! More counters for input array.
      INTEGER   IXMAX, IYMAX      ! Extent of interpolating function in pixels
      INTEGER   MMIN, MMAX        ! Interpolation limits in X (pixels)
      INTEGER   NMIN, NMAX        ! Interpolation limits in Y (pixels)
      INTEGER   INX,  INY         ! Interpolation factor (extra points / pixel)
      REAL      CURRENT_WEIGHT    ! Weight value for current sample
      REAL      SIGMA_W           ! Sum of weights
      REAL      WEIGHT(0:IWMAX,0:IWMAX) ! Value of beam at point
      REAL      VAL
      REAL      XRAD, YRAD

C  OK, go...

      IFAIL = 0

C     Find interpolation factors

      INX   = (M2-1)/(M1-1)
      INY   = (N2-1)/(N1-1)
CD    PRINT *, 'Interpolating factors: INX, INY = ', INX, INY

C     How far to interpolate?

      IXMAX = (WMAXX/CELLX)*INX
      IYMAX = (WMAXY/CELLY)*INY
CD    PRINT *, 'Interp func. radii: IXMAX,IYMAX = ', IXMAX, IYMAX

C   Check that weights array is small enough

      IF (IXMAX.GT.IWMAX .OR. IYMAX.GT.IWMAX) THEN
        PRINT *,'Maximum radius of interp''g function exceeded'
        IFAIL = 18
        RETURN
      END IF

C     Evaluate beam sizes

      XRAD  = 0.5*FWHMX*INX/CELLX/SQRT(ALOG(2.))
      YRAD  = 0.5*FWHMY*INY/CELLY/SQRT(ALOG(2.))

C     Set up the weights array (to save working out lots of trig functions later

      DO I = 0,IXMAX
        DO J = 0,IYMAX

          IF (XRAD.EQ.0.0 .AND. YRAD.EQ.0.0) THEN
            IF (I.NE.0 .AND. J.NE.0) THEN
              WEIGHT(I,J) = 0.0
            ELSE
              WEIGHT(I,J) = 1.0
            END IF

          ELSE IF (XRAD.EQ.0.0) THEN
            IF (I.NE.0) THEN
              WEIGHT(I,J) = 0.0
            ELSE
              WEIGHT(I,J) = EXP(-(J**2/YRAD**2))
            END IF

          ELSE IF (YRAD.EQ.0.0) THEN
            IF (J.NE.0) THEN
              WEIGHT(I,J) = 0.0
            ELSE
              WEIGHT(I,J) = EXP(-(I**2/XRAD**2))
            END IF

          ELSE
            WEIGHT(I,J) = EXP (-((I**2/XRAD**2)+(J**2/YRAD**2)))
          END IF

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

          GXMINUS = .FALSE.
          GXPLUS  = .FALSE.
          GYMINUS = .FALSE.
          GYPLUS  = .FALSE.

          DO J = NMIN, NMAX, INY
            DO I = MMIN, MMAX, INX

C             Work out indices in input array

              II = (I-1)/INX + 1
              JJ = (J-1)/INY + 1

              IF (A1(II,JJ).NE.BADPIX) THEN

                IF (I.LE.M) GXMINUS = .TRUE.
                IF (I.GE.M) GXPLUS  = .TRUE.
                IF (J.LE.N) GYMINUS = .TRUE.
                IF (J.GE.N) GYPLUS  = .TRUE.

                INTERP = .TRUE.

                CURRENT_WEIGHT = WEIGHT(IABS(M-I),IABS(N-J))
                SIGMA_W        = SIGMA_W + CURRENT_WEIGHT
                VAL            = VAL + CURRENT_WEIGHT * A1(II,JJ)

              END IF
            END DO
          END DO

C         If we have data on all sides of point to be interpolated, then
C         go ahead and put properly weighted data into the output map -
C         else set to bad pixel value.

          IF (       GXMINUS .AND. GXPLUS
     &         .AND. GYMINUS .AND. GYPLUS
     &         .AND. SIGMA_W.NE.0.) THEN
            A2(M,N) = VAL/SIGMA_W
          ELSE
            A2(M,N) = BADPIX
          END IF

        END DO
      END DO

      RETURN
      END

C-----------------------------------------------------------------------
