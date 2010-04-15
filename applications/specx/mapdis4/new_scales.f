*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
C------------------------------------------------------------------------------

      SUBROUTINE NEW_SCALES (A, P, Q)

C   Subroutine to set new limits for current plot.

      LOGICAL*4 XINVERT, YINVERT
      LOGICAL*4 XMOD,    YMOD
      REAL*4    A(2),P(2),Q(2)

      INCLUDE  'PLOT2D'
      INCLUDE  'FLAGCOMM'

CD    CALL SXGTIDLE
CD    Print *,'Old values of plot box:'
CD    Print *,'X: ',XLIM
CD    Print *,'Y: ',YLIM
CD    Print *,'Values of PQ box:'
CD    Print *,'P: ',P
CD    Print *,'Q: ',Q
CD    CALL SXGTTGRAPH

      XINVERT = .FALSE.
      YINVERT = .FALSE.

      XOLD1 = XLIM(1)
      XOLD2 = XLIM(2)
      YOLD1 = YLIM(1)
      YOLD2 = YLIM(2)

      IX = LINK(1)
      IY = LINK(2)

C  Set limit back to those requested by S-MA-SC in case cursor was
C  outside plot limits

      XLIM(1) = MIN (PBEG(IX),PEND(IX))
      XLIM(2) = MAX (PBEG(IX),PEND(IX))
      YLIM(1) = MIN (PBEG(IY),PEND(IY))
      YLIM(2) = MAX (PBEG(IY),PEND(IY))

C  Which way round should things be?

      IF (P(1).GT.P(2)) THEN
        XINVERT = .TRUE.
        CALL SWAP2 (P(1),P(2))
      END IF

      XMOD = .FALSE.
      EPSX = .001*ABS(PBEG(IX)-PEND(IX))
      IF (P(1).GE.MIN(XOLD1,XOLD2)) THEN
        XLIM(1) = P(1)+EPSX
        XMOD    = .TRUE.
      END IF
      IF (P(2).LE.MAX(XOLD1,XOLD2)) THEN
        XLIM(2) = P(2)-EPSX
        XMOD    = .TRUE.
      END IF
      IF (XINVERT) CALL SWAP2     (XLIM(1), XLIM(2))
      IF (XMOD)    CALL AUTORANGE (XLIM(1), XLIM(2), NPINT)

      IF (Q(1).GT.Q(2)) THEN
        YINVERT = .TRUE.
        CALL SWAP2 (Q(1),Q(2))
      END IF

      YMOD = .FALSE.
      EPSY = .001*ABS(PBEG(IY)-PEND(IY))
      IF (Q(1).GE.MIN(YOLD1,YOLD2)) THEN
        YLIM(1) = Q(1)+EPSY
        YMOD    = .TRUE.
      END IF
      IF (Q(2).LE.MAX(YOLD1,YOLD2)) THEN
        YLIM(2) = Q(2)-EPSY
        YMOD    = .TRUE.
      END IF
      IF (YINVERT) CALL SWAP2     (YLIM(1),YLIM(2))
      IF (YMOD)    CALL AUTORANGE (YLIM(1), YLIM(2), NQINT)

CD    CALL SXGTIDLE
CD    Print *,'New values of plot box:'
CD    Print *,'X: ',XLIM
CD    Print *,'Y: ',YLIM
CD    CALL SXGTTGRAPH

C  Reset the axis lengths: If AX2LEN = 0 then we had automatic selection of
C  Y-axis size to keep 1:1 aspect ratio. Do not change this unless specifically
C  requested.
C  (Later mod: preset aspect ratio for AX1LEN = 0. also)

      CALL SET_DISPLAY_SIZE (A(1), A(2),
     &                       ABS(XLIM(2)-XLIM(1)), ABS(YLIM(2)-YLIM(1)),
     &                       AXLENX, AXLENY,
     &                       PLOTLIMS(1)+5., PLOTLIMS(3)+8.)

CD    CALL SXGTIDLE
CD    PRINT *, ' -- new_scales --'
CD    PRINT *, '    Returned from SET-DISPLAY-SIZE'
CD    PRINT *, '                    a(1),   a(2)   = ', a(1),   a(2)
CD    PRINT *, '                    dxlim,  dylim  = ',
CD   &                      abs(xlim(2)-xlim(1)), abs(ylim(2)-ylim(1))
CD    PRINT *, 'Axis lengths reset: axlenx, axleny = ', axlenx, axleny
CD    CALL SXGTTGRAPH

      CHANGE_PLOT = .TRUE.

      RETURN
      END


