C-----------------------------------------------------------------------

      SUBROUTINE INVSPC

C   Routine to invert a complete spectrum, so that both data and
C   quadrant header variables need to be reversed.

C   History:
C      20-SEP-2000 (AJC):
C         Unused JTEMP
C-

      INCLUDE 'STACKCOMM'

      CALL SWAP_ARR (4, DATA,   NTOT(NQUAD))
      CALL SWAP_ARR (4, JFCEN,  NQUAD)
      CALL SWAP_ARR (4, JFREST, NQUAD)
      CALL SWAP_ARR (4, JFINC,  NQUAD)

      DO NQ = 1,NQUAD
       JFINC(NQ) = -JFINC(NQ)
      END DO

      IF (IQCEN.NE.0)   IQCEN = NQUAD+1-IQCEN

      RETURN
      END


