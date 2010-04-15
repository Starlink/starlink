      SUBROUTINE ELFVIEW(IV,NFS,FCNAME,SUBCHK)

      INCLUDE 'KUSE_COM'   ! Declares common block KUSE holding K1 and K2

      COMMON/DAT2/A,B,P,S,FLX,INDX,IREL,LP,NL,IAB
      COMMON/DAT5/NDIM,NDI2,NPAR,NUMFIT,NCUR,MAXFC
      COMMON/KEEP/FUNS,AS,PS,FLXS,FMX,WSS,INDXS,IRELS,NLS,NPARS,IABST
      COMMON/ERRS/DELB,DELP,DS
      COMMON/KERRS/DELBS,DELPS,DSS
      COMMON/DEBUG/NY
      COMMON/VIEWALL/IVIEW
      DIMENSION A(80),B(80),INDX(80),IREL(80),IABST(20)
      DIMENSION AS(80,10,2),INDXS(80,10,2),IRELS(80,10,2),NLS(20)
      DIMENSION FUNS(20),P(10),FLX(20),PS(10,10),LP(20),NPARS(10)
      DIMENSION FLXS(20,10),FMX(10),WSS(10)
      DIMENSION DELBS(80,10),DELPS(10,10),DSS(20,10),DELB(80),
     .          DELP(10),DS(20)
      CHARACTER*(*) FCNAME
       CHARACTER*4 SUBCMD
       INTEGER SUBCHK
       INTEGER SLEN

       SAVE /ERRS/,/KERRS/
       IF (IV.EQ.1) THEN
          SUBCMD='ELFWRC'
       ELSE
          SUBCMD='ELFVUC'
       ENDIF
       SUBCHK = 0
      NDI3=3*NDIM
      IF(NUMFIT.EQ.0) THEN
        SUBCHK = 1
        RETURN
      ENDIF
      N1=1
      N2=NUMFIT
      IF(IV.EQ.0) THEN
        IF(NFS.GT.NUMFIT) THEN
          SUBCHK = 2
          WRITE (*,
     :    '(''   '',A,'':  only'',I3,'' fits stored'')')
     :    SUBCMD(1:SLEN(SUBCMD)), NUMFIT
          RETURN
        ENDIF
        N1=NFS
        N2=NFS
        IUN=6
      ELSE
        IUN=35
      ENDIF
      DO N=N1,N2
      IF (IVIEW.GT.0) THEN
          IF (N.GT.N1 .AND. SUBCMD.EQ.'ELFWRC') THEN
             WRITE (IUN,'(''1''//)')
          ELSEIF (SUBCMD.EQ.'ELFWRC') THEN
             WRITE (IUN,'('' ''//)')
          ENDIF
       WRITE (IUN,203) SUBCMD, N
  203 FORMAT ('   ',A4,':',
     :       '  Fit number',I3/
     C       1X,'Line   Parameter   Option    IREL    Initial'
     C         ,'       Final'/
     C       1X,'                                      value '
     C         ,'       value'/)
      DO K=1,4
        DO I=1,NDIM
          IP=(K-1)*NDIM+I
          J=INDXS(IP,N,1)
          IF(J.NE.0)
     C    WRITE(IUN,202) I,K2(K),K1(J),IRELS(IP,N,1),AS(IP,N,1),
     C                   AS(IP,N,2)
  202 FORMAT(1X,I3,5X,A9,2X,A9,I4,2F12.2)
        ENDDO
      ENDDO
      WRITE (IUN,'(''0Sum of (residual)**2: '',1PE13.5,
     :             ''    Intensities normalised to 1='',1PE10.3)')
     :                FUNS(N),FMX(N)*0.01
      ENDIF
      JL=NLS(N)
      IF(JL.NE.0) THEN
        WRITE(IUN,108)
  108   FORMAT(/' Line Profile    Centre   Width       Peak Flux',
     :    '   Line Flux'/)
        DO J=1,JL
         IP=AS(J+NDI3,N,2)
         CJ=AS(J,N,2)
         WJ=AS(J+NDIM,N,2)
         PJ=AS(J+NDI2,N,2) * FMX(N) * 0.01
         SS=FLXS(J,N)
         WRITE(iun,107) J,IP,CJ,WJ,PJ,SS
         FLMAX=FMX(N)
         WRITE(iun,210) DELBS(J,N),DELBS(J+NDIM,N),
     :                DELBS(J+NDI2,N)*FLMAX*0.01,DSS(J,N)
        ENDDO
      ENDIF
      NP=NPARS(N)
      IF(NP.NE.0) THEN
        WRITE(IUN,105) WSS(N), (PS(I,N)*FMX(N)*0.01,I=1,NP)
  105   FORMAT(/3X,'Coefficients of background polynomial:',
     :             '  ordinate origin at',F12.3
     :          /(11X,1P4E14.4))
       WRITE(iun,208) (DELPS(J,N),J=1,NP)
      ENDIF
!     WRITE(iun,209)
  209 FORMAT(//2X,'Error estimates    (68% confidence limit)'/
     .         2X,'---------------'//
     . ' Line      Centre       Width       Peak Flux       Line Flux')
  210 FORMAT(7X,'+/-',F14.3,F9.3,1PE14.3,E12.3)
! 208 FORMAT(/'   Errors in coefficients of background polynomial:'
!    .       /(1X,1P4E14.4))
  208 FORMAT(8X,'+/-',1P4E14.4)
      ENDDO
  107 FORMAT(2X,I2,I6,F14.3,F9.3,1PE14.3,E12.3)
      WRITE (IUN,'('' '')')
      IF(IV.NE.0) CLOSE(35)
      RETURN
      END
