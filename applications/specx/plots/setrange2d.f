*-----------------------------------------------------------------------

      SUBROUTINE SETRANGE2D (N, X, Y, BAD, XL, XH, EXPAND, NINTS,
     &                       YL, YH, ERR)

*  Routine to find max and min values of Y for which corresponding X
*  is in range (xl, xh).

      IMPLICIT   NONE

*     Formal parameters

      INTEGER    N        ! Given, # points in both X & Y
      REAL       X(N)     ! Given, X-array
      REAL       Y(N)     ! Given, Y-array
      REAL       BAD      ! Bad/invalid pixel value
      REAL       XL, XH   ! Given, useful range in X
      REAL       EXPAND   ! Given, factor to expand range by
      INTEGER    NINTS    ! Given, number of intervals for tick marks
      REAL       YL, YH   ! Return, final range in Y
      INTEGER    ERR      ! Return, status return

*     Local variables

      INTEGER    I
      INTEGER    IST
      REAL       RANGE

*     Functions

*  Ok, go...

*     First find range of Y-values. Can't guarantee that first channel
*     is not BAD (in fact, this is more than likely!) so first need to
*     find first non-bad channel (in allowed range of X)

      I = 1
      DO WHILE ((Y(I).eq.BAD .or. X(I).lt.XL .or. X(I).gt.XH)
     &            .and. I.le.N)
        I = I + 1
      END DO

      IF (I.gt.N) THEN
        ERR = 1
        GO TO 99
      END IF

      IST = I

      YL = Y(IST)
      YH = Y(IST)

      DO I = IST, N
        IF (X(I).ge.XL  .and. X(I).le.XH) THEN
          IF (Y(I).ne.BAD .and. Y(I).gt.YH) YH = Y(I)
          IF (Y(I).ne.BAD .and. Y(I).lt.YL) YL = Y(I)
        END IF
      END DO

*  Expand scales more or less symmetrically if required

      RANGE = YH - YL

      IF (RANGE.GT.0.0) THEN
        IF (EXPAND.ne.1.) THEN
          YL = YL - 0.5*(EXPAND-1.)*RANGE
          YH = YH + 0.5*(EXPAND-1.)*RANGE
        END IF

*       ... and deduce more sensible scaling

        CALL AUTORANGE (YL, YH, NINTS)

      ELSE
        ERR = 35
        Type *,'Error in SETRANGE2D - Range = 0'
        GO TO 99
      END IF

      RETURN

*   Error return

   99 CONTINUE

      RETURN
      END

*-----------------------------------------------------------------------
