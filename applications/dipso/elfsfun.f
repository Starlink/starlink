      FUNCTION ELFSFUN(PS,BS,NL)
      COMMON/DAT1/FL,W,WS,WV,NF
      COMMON/DAT5/NDIM,NDI2,NPAR,NUMFIT,NCUR,MAXFC
      DIMENSION FL(1000),W(1000),PS(10),BS(80)
      NDI3=3*NDIM
      S=0.
      DO IF=1,NF
       FLF=0.
       IF(NPAR.NE.0) THEN
        XX=1.
        DO J=1,NPAR
         FLF=FLF+XX*PS(J)
         XX=XX*(W(IF)-WS)
        ENDDO
       ENDIF
       IF(NL.NE.0) THEN
        DO J=1,NL
         FLF=FLF+BS(NDI2+J)*ELFG(W(IF),BS(J),BS(NDIM+J),BS(NDI3+J))
        ENDDO
       ENDIF
       S=S+(FL(IF)-FLF)**2
      ENDDO
      ELFSFUN=S
      RETURN
      END
