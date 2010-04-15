* History:
*    20-SEP-2000 (AJC):
*       Declare P, IPTS dimension 2, not 1
*       Declare XSCALE assumed size
C--------------------------------------------------------------------------

      SUBROUTINE PPAIR(P,IPTS,NCH,XSCALE,IFAIL)

C   Routine to get ordered pair of data points in P corresponding to
C   points in IPTS(1) and IPTS(2)

      REAL*4  P(2),XSCALE(*)
      INTEGER IPTS(2)

      IFAIL=0

      P(1)=XTRANS(XSCALE,FLOAT(IPTS(1)),NCH,IFAIL)
      IF (IFAIL.NE.0)   RETURN
      P(2)=XTRANS(XSCALE,FLOAT(IPTS(2)),NCH,IFAIL)
      IF (IFAIL.NE.0)   RETURN

      IF(P(1).GT.P(2)) THEN
        TEMP=P(1)
        P(1)=P(2)
        P(2)=TEMP
      END IF

      RETURN
      END


