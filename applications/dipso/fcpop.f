      SUBROUTINE FCPOP(NFS,SUBCHK)
      COMMON/DAT2/A,B,P,S,FLX,INDX,IREL,LP,NL,IAB
      COMMON/DAT5/NDIM,NDI2,NPAR,NUMFIT,NCUR,MAXFC
      COMMON/KEEP/FUNS,AS,PS,FLXS,FMX,WSS,INDXS,IRELS,NLS,NPARS,IABST
      COMMON/DAT3/FLMAX
      DIMENSION A(80),B(80),INDX(80),IREL(80),K1(3),K2(4),IABST(20)
      DIMENSION AS(80,10,2),INDXS(80,10,2),IRELS(80,10,2),NLS(20)
      DIMENSION FUNS(20),P(10),FLX(20),PS(10,10),LP(20),NPARS(10)
      DIMENSION FLXS(20,10),FMX(10),WSS(10)
      CHARACTER*9 K1,K2
       LOGICAL SUBCHK
       SUBCHK = .TRUE.

      IF(NUMFIT.EQ.0) THEN
        WRITE (*,'(''   ELFPOPC:  coefficient stack is empty'')')
        SUBCHK = .FALSE.
        RETURN
      ENDIF
      IF(NFS.GT.NUMFIT) THEN
        SUBCHK = .FALSE.
        WRITE (*,
     :  '(''   ELFPOPC:  only'',I3,'' fits stored'')')
     :  NUMFIT
        RETURN
      ENDIF
      NL=NLS(NFS)
      IAB=IABST(NFS)
      FLMAX=FMX(NFS)
      DO K=1,4
        DO I=1,NDIM
          IP=(K-1)*NDIM+I
          A(IP)=AS(IP,NFS,1)
          B(IP)=AS(IP,NFS,2)
          INDX(IP)=INDXS(IP,NFS,1)
          IREL(IP)=IRELS(IP,NFS,1)
        ENDDO
      NPAR=NPARS(NFS)
      ENDDO
      IF(NPAR.NE.0) THEN
        DO I=1,NPAR
          P(I)=PS(I,NFS)
        ENDDO
      ENDIF
      RETURN
      END
