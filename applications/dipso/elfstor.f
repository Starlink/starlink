      SUBROUTINE ELFSTOR
      COMMON/DAT1/FL,W,WS,WV,NF
      COMMON/DAT2/A,B,P,S,FLX,INDX,IREL,LP,NL,IAB
      COMMON/DAT5/NDIM,NDI2,NPAR,NUMFIT,NCUR,MAXFC
      COMMON/KEEP/FUNS,AS,PS,FLXS,FMX,WSS,INDXS,IRELS,NLS,NPARS,IABST
      COMMON/ERRS/DELB,DELP,DS
      COMMON/KERRS/DELBS,DELPS,DSS
      COMMON/DAT3/FLMAX
      COMMON/DEBUG/NY
      DIMENSION A(80),B(80),P(10),INDX(80),IREL(80)
      DIMENSION FUNS(20),AS(80,10,2),INDXS(80,10,2),IRELS(80,10,2)
      DIMENSION PS(10,10),LP(20),NLS(20),IABST(20),NPARS(10)
      DIMENSION FLX(20),FLXS(20,10),FMX(10),WSS(10)
      DIMENSION DELB(80),DELP(10),DS(20),DELBS(80,10),DELPS(10,10),
     .          DSS(20,10)
      DIMENSION FL(1000),W(1000)
      SAVE /ERRS/,/KERRS/
      NDI3=3*NDIM
      NDI4=4*NDIM
C      IF(NL.EQ.0) THEN
C        WRITE (*,'(''   Current fit storage is empty'')')
C        RETURN
C      ENDIF
      NUMFIT=NUMFIT+1
      IF(NUMFIT.GT.10) THEN
        WRITE (*,
     : '(''   Storage exceeded in fit coefficient stack'')')
        RETURN
      ENDIF
      N=NUMFIT
      FUNS(N)=S
      FMX(N)=FLMAX
      NLS(N)=NL
      IABST(N)=IAB
      WSS(N)=WS
      DO I=1,NDI4
      AS(I,N,1)=A(I)
      AS(I,N,2)=B(I)
      INDXS(I,N,1)=INDX(I)
      IRELS(I,N,1)=IREL(I)
      DELBS(I,N)=DELB(I)
      ENDDO
      IF(NL.NE.0) THEN
       DO I=1,NL
         FLXS(I,N)=FLX(I)
         DSS(I,N)=DS(I)
       ENDDO
      ENDIF
      IF(NPAR.NE.0) THEN
        NPARS(N)=NPAR
        DO I=1,NPAR
          PS(I,N)=P(I)
          DELPS(I,N)=DELP(I)
        ENDDO
      ENDIF
        PRINT *, '  ELFCPUSH:  filling fit stack entry ',N
      RETURN
      END
