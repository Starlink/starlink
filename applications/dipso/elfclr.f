      SUBROUTINE ELFCLR
      COMMON/DAT2/A,B,P,S,FLX,INDX,IREL,LP,NL,IAB
      COMMON/DAT5/NDIM,NDI2,NPAR,NUMFIT,NCUR,MAXFC
      DIMENSION A(80),B(80),P(10),INDX(80),IREL(80),LP(20)
      DIMENSION FLX(20)
      NDI3=3*NDIM
      NDI4=4*NDIM
C
C  CLEAR ARRAYS FOR NEW FIT
        DO I=1,NDI4
          A(I)=0.
          B(I)=0.
          INDX(I)=0
          IREL(I)=0
        ENDDO
        NN=NDI3+1
        DO I=NN,NDI4
          A(I)=1.
        ENDDO
        NL=0
        NPAR=0
        RETURN
        END
