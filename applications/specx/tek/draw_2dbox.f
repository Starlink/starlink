*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------------

      SUBROUTINE DRAW_2DBOX (P,Q)

C   Routine to draw a box on an open NCAR plot with vertices described by
C   arrays P (X) and Q (Y)

      REAL*4    P(2),Q(2)

*     CALL SXGTIDLE
*     Print *,'Box coordinates:'
*     Print *,'P: ',P
*     Print *,'Q: ',Q
*     CALL SXGTTGRAPH

      CALL SXGLTYPE    (0)
      CALL SXGRELOCATE (P(1),Q(1))
      CALL SXGDRAW     (P(1),Q(2))
      CALL SXGDRAW     (P(2),Q(2))
      CALL SXGDRAW     (P(2),Q(1))
      CALL SXGDRAW     (P(1),Q(1))

      RETURN
      END


