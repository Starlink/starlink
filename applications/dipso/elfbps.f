
      SUBROUTINE ELFBPS(BTS,PTS,B,P)
      DIMENSION B(80),BTS(80),P(10),PTS(10)
      DO JJ=1,80
       BTS(JJ)=B(JJ)
      ENDDO
      DO JJ=1,10
       PTS(JJ)=P(JJ)
      ENDDO
      RETURN
      END
