C-----------------------------------------------------------------------

      SUBROUTINE MARK_SAMPLES (MAP)

C   Routine to make spots at sample points where data is available.

      LOGICAL*4 GOODPT
      REAL*4    MAP(NAXX,NAXY)

      INCLUDE  'FLAGCOMM'
      INCLUDE  'PLOT2D'
      INCLUDE  'CNF_PAR'

      IX = LINK(1)
      IY = LINK(2)
      IZ = LINK(3)

      CALL SXGEXPAND (CHARHT/3.)
      IF (TERMINAL .AND. IDEV.LT.10) THEN
        CALL SXGLTYPE  (7)
      ELSE
        CALL SXGLTYPE  (0)
      END IF

      DX = (CEND(IX)-CBEG(IX))/(NAX(LINK(1))-1)
      DY = (CEND(IY)-CBEG(IY))/(NAX(LINK(2))-1)

      DO I = 1,NAX(LINK(1))
        DO J = 1,NAX(LINK(2))
          IF (GOODPT(%VAL(CNF_PVAL(INDEX_PTR)), I, J, 0)) THEN
            X = CBEG(IX) + DX*(I-1)
            Y = CBEG(IY) + DY*(J-1)
            CALL SXGPOINTS (1, X, Y, 17)
          END IF
        END DO
      END DO

      RETURN
      END

C-----------------------------------------------------------------------

