      SUBROUTINE FCREST(SUBCHK)
      LOGICAL SUBCHK
      CHARACTER*40 ITXT
      COMMON/KEEP/FUNS,AS,PS,FLXS,FMX,WSS,INDXS,IRELS,NLS,NPARS,IABST
      COMMON/DAT4/XV,FXV,ARP,WVF,CFLX,IXV,NXV,NPROF
      COMMON/DAT4A/ITXT
      COMMON/DAT5/NDIM,NDI2,NPAR,NUMFIT,NCUR,MAXFC
      COMMON/KERRS/DELBS,DELPS,DSS
      COMMON/DEBUG/NY
      DIMENSION FUNS(20),PS(10,10),FLXS(20,10),FMX(10),WSS(10)
      DIMENSION AS(80,10,2),INDXS(80,10,2),IRELS(80,10,2),NLS(20)
      DIMENSION IABST(20),NPARS(10)
      DIMENSION DELBS(80,10),DELPS(10,10),DSS(20,10)
      DIMENSION XV(5,1000),FXV(5,1000),ARP(5),IXV(5),NXV(5),ITXT(5)
      DIMENSION WVF(5),CFLX(5)
      SAVE /KERRS/
      SUBCHK=.TRUE.
      READ(34,ERR=100) NFITST
      NFIT=NUMFIT+NFITST
      IF(NFIT.GT.MAXFC) THEN
        NFIT=MAXFC
        WRITE(*, '(''   ELFRESTC:   fit stack overflows storage'')')
        WRITE(*, '(''             :   some fits not restored'')')
      ENDIF
      NMFIT=NUMFIT+1
      DO J=NMFIT,NFIT
        READ(34,ERR=100) ((AS(I,J,K),INDXS(I,J,K),IRELS(I,J,K),
     :                     K=1,2),I=1,80)
        READ(34,ERR=100) (FLXS(I,J),I=1,20)
        READ(34,ERR=100)  (PS(I,J),I=1,10)
        READ(34,ERR=100) FUNS(J),FMX(J),IABST(J),NPARS(J),NLS(J)
        READ(34,ERR=100) (DELBS(I,J),I=1,80)
        READ(34,ERR=100) (DELPS(I,J),I=1,10)
        READ(34,ERR=100) (DSS(I,J),I=1,20)
      ENDDO
      NUMFIT=NFIT
      READ(34,ERR=100) NPF
      IF(NY.GT.0) PRINT *,' FCREST, NPROF, NPF',NPROF,NPF
      IF(NPF.NE.0) THEN
        IF(NPROF.NE.0) THEN
          WRITE(*,
     :    '(''    ELFRESTC:  numerical profiles not restored'',
     :     /''             due to conflict with existing profiles''
     :      )')
          RETURN
        ENDIF
        DO J=1,NPF
          READ(34,ERR=100) ARP(J),WVF(J),CFLX(J),
     :                      IXV(J),NXV(J),ITXT(J)
          READ(34,ERR=100) (XV(J,K),FXV(J,K),K=1,1000)
        ENDDO
        NPROF=NPF
      ENDIF
      DO J=NMFIT,NFIT
        READ(34,ERR=101) WSS(J)
      ENDDO
      RETURN
  100 SUBCHK=.FALSE.
      RETURN
  101 WRITE(6,102)
  102 FORMAT(1X,'Restoring old-style save set (pre 25/3/88)'/
     :       1X,'Information is incomplete for any polynomial fit'/
     :       1X,'until an ELFOPT command is issued')
      RETURN
      END
