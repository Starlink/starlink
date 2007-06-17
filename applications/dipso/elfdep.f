      SUBROUTINE ELFDEP(BTS,JJ)
      COMMON/DAT2/A,B,P,S,FLX,INDX,IREL,LP,NL,IAB
      COMMON/DAT5/NDIM,NDI2,NPAR,NUMFIT,NCUR,MAXFC
      DIMENSION A(80),B(80),P(10),FLX(20),INDX(80),IREL(80),LP(20),
     .          BTS(80),PTS(10)
      ND1=JJ/NDIM
      ND=ND1*NDIM
      J=JJ-ND
      DO JP=1,NL
       IF(INDX(JP+ND).EQ.3) THEN
        KK=IREL(JP+ND)
        IF(KK.EQ.J) THEN
           IF(ND1.NE.2) THEN
        BTS(JP+ND)=BTS(KK+ND)+A(JP+ND)
         ELSE
          BTS(JP+ND)=BTS(KK+ND)*A(JP+ND)
         ENDIF
        ENDIF
       ENDIF
      ENDDO
      RETURN
      END
