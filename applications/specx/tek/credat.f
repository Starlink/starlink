C------------------------------------------------------------------------------

      SUBROUTINE CREDAT(P,Q,XSCALE,BUF,N1,N2)

C  This routine manufactures a spectrum in BUF to approximate a line
C  between (P(1),Q(1)) and (P(2),Q(2)) at points N1 and N2
C  respectively.

      REAL*4  P(2),Q(2),XSCALE(1),BUF(1)

      SLOPE = (Q(2)-Q(1))/(P(2)-P(1))
      YOFF  = Q(1)-SLOPE*P(1)

      DO I=N1,N2
        BUF(I)=SLOPE*XSCALE(I)+YOFF
      END DO

      RETURN
      END


