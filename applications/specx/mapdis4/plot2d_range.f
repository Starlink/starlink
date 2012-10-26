*  History:
*     20 July 2000 (ajc):
*        Change TYPE to PRINT
*        Missing commas in format
*-----------------------------------------------------------------------

      SUBROUTINE PLOT2D_RANGE (QBEG, QEND, PBEG, PEND, IX, IY, IZ)

C  Calculate sensible defaults for map scales if requested:

C  Formal parameters:

      REAL*4    QBEG(3)      ! Input;  Start points of axis ranges
      REAL*4    QEND(3)      ! Input;  End points of axis ranges
      REAL*4    PBEG(3)      ! Output; Start points of axis ranges
      REAL*4    PEND(3)      ! Output; End points of axis ranges
      INTEGER*4 IX           ! Input;  Axis corresponding to x-axis of plot
      INTEGER*4 IY           ! Input;  Axis corresponding to y-axis of plot
      INTEGER*4 IZ           ! Input;  Axis over which channels are integrated

      INCLUDE 'MAPHD'
      INCLUDE 'MAPTITLES'
      INCLUDE 'PLOT2D'
      INCLUDE 'CNF_PAR'

C  Investigate INDEX array to find maximum extent of good data

      I_MIN = MSTEP
      I_MAX = 1
      J_MIN = NSTEP
      J_MAX = 1

      DO I = 1,MSTEP
        DO J = 1, NSTEP
          CALL XCOPY (4,
     :         %VAL(CNF_PVAL(INDEX_PTR)+(MSTEP*(J-1) + I - 1)*4),
     :         IPOS)
          IF (IPOS .GE. 0) THEN
            I_MIN = MIN (I_MIN, I)
            I_MAX = MAX (I_MAX, I)
            J_MIN = MIN (J_MIN, J)
            J_MAX = MAX (J_MAX, J)
          END IF
        END DO
      END DO

C  Sort out limits for R.A./X axis of data

      IF (QBEG(1).EQ.0.0 .AND. QEND(1).EQ.0.0 .AND. IZ.NE.1) THEN

        IF (MSTEP.EQ.1) THEN
          PBEG(1) = 0.499999*CELL_XSIZE
          PEND(1) = -PBEG(1)
        ELSE
          PBEG(1) = CELL_XSIZE * (0.5*(MSTEP+1) - I_MIN)
          PEND(1) = CELL_XSIZE * (0.5*(MSTEP+1) - I_MAX)
        END IF

        PRINT '(1X, A11,'' scaled from '',F8.3,'' to '',F8.3)',
     &             MAPTIT(1), PBEG(1), PEND(1)

      ELSE IF (QBEG(1).EQ.QEND(1)) THEN
        PBEG(1) = QBEG(1) - 0.5*MSTEP
        PEND(1) = QEND(1) + 0.5*MSTEP

      ELSE
        PBEG(1) = QBEG(1)
        PEND(1) = QEND(1)

      END IF

C  Limits for Dec./Y axis of data

      IF (QBEG(2).EQ.0.0 .AND. QEND(2).EQ.0.0 .AND. IZ.NE.2) THEN

        IF (NSTEP.EQ.1) THEN
          PBEG(2) = 0.499999*CELL_YSIZE
          PEND(2) = -PBEG(2)
        ELSE
          PBEG(2) = CELL_YSIZE * (0.5*(NSTEP+1) - J_MIN)
          PEND(2) = CELL_YSIZE * (0.5*(NSTEP+1) - J_MAX)
        END IF

        PRINT '(1X, A11, '' scaled from '',F8.3,'' to '',F8.3)',
     &             MAPTIT(2), PBEG(2), PEND(2)

      ELSE IF (QBEG(2).EQ.QEND(2)) THEN
        PBEG(2) = QBEG(2) - 0.5*NSTEP
        PEND(2) = QEND(2) + 0.5*NSTEP

      ELSE
        PBEG(2) = QBEG(2)
        PEND(2) = QEND(2)

      END IF

C  Z-axis is easier, since no "auto" scaling (0,0 limits) and always > 1 pt

      IF (QBEG(3).NE.QEND(3)) THEN
        PBEG(3) = QBEG(3)
        PEND(3) = QEND(3)
      ELSE
        PBEG(3) = QBEG(3) - 0.5*PFAC(3)
        PEND(3) = QEND(3) + 0.5*PFAC(3)
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

