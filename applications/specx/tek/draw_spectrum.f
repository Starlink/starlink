C---------------------------------------------------------------------------

      SUBROUTINE DRAW_SPECTRUM (XSCALE, BUF, NQ, IFAIL)

C  Routine to allow baseline (or other plot) to be drawn using cursor on
C  VT100 Retrographics terminal for input

      LOGICAL   REPEAT, FIRST
      REAL*4    XSCALE(1024),BUF(1024)
      REAL*4    P(2),Q(2)

      INCLUDE   'STACKCOMM'
      INCLUDE   'FLAGCOMM'

      IF (.NOT.TERMINAL .OR. .NOT.INTERACTIVE) THEN
        IFAIL = 46
        RETURN
      ELSE
        CALL ALLOCATE_DEVICE (IDEV, IFAIL)
        IF (IFAIL.NE.0) RETURN
      END IF

C  Select points appropriate to this quadrant

      NCH = NPTS(NQ)
      NST = NTOT(NQ-1)
      DO I = 1,NCH
        BUF(I) = DATA(NST+I)
      END DO

C  Put up a plot of current X-register, then push up to create stack space
C  for working in

      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0) RETURN
      CALL PLOTXY  (XSCALE(NST+1), BUF, NCH, IFAIL)
      IF (IFAIL.NE.0) RETURN
      CALL PUSH

      FIRST  = .TRUE.
      REPEAT = .TRUE.

C  Call cursor routine and update synthesized plot

      IFUNC = 0
      DO WHILE (IFUNC.NE.2)
        IFUNC = IVTOPT(IDEV, ' L R T B N E Q C D H +', P, Q, REPEAT)

C  ...Ask for a rescaled plot?

        IF (REPEAT) THEN
          CALL PLOTXY (XSCALE(NST+1), BUF, NCH, IFAIL)

C  ...Else assume character input was a '+'
C     draw a line on the screen

        ELSE IF (IFUNC.NE.2) THEN
          P(1) = XOLD
          Q(1) = YOLD
          IF (.NOT.FIRST) THEN
            CALL SXGCONNECT (P, Q, 2)

C     then find the affected range and reset it in the buffer

            CALL NTRANS (XSCALE(NST+1), NCH, P(1), P(2),
     &                   XFAC(NQ), N1, N2, IFAIL)
            IF (IFAIL.EQ.0) THEN
              CALL CREDAT (P, Q, XSCALE(NST+1), BUF, N1, N2)
            END IF
          END IF

C     reset the P and Q arrays and go back for more

          XOLD  = P(2)
          YOLD  = Q(2)
          FIRST = .FALSE.
        END IF
      END DO

C  Before returning copy new BUF back into appropriate quadrant of STACK

      DO I = 1, NPTS(NQ)
        DATA(NST+I) = BUF(I)
      END DO

      RETURN
      END


