      SUBROUTINE ELFIT(IS,PROCEED)
      COMMON/DAT1/FL,W,WS,WV,NF
      COMMON/DAT2/A,B,P,S0,FLX,INDX,IREL,LP,NL,IAB
      COMMON/DAT5/NDIM,NDI2,NPAR,NUMFIT,NCUR,MAXFC
      COMMON/DAT3/FLMAX
      COMMON/KEEP/FUNS,AS,PS,FLXS,FMX,WSS,INDXS,IRELS,NLS,NPARS,IABST
      COMMON/ERRS/DELB,DELP,DS
      COMMON/DEBUG/NY
      COMMON/NOISE/FLN(1000)
      DIMENSION FL(1000),W(1000),A(80),B(80),X(40),E(40),P(10)
      DIMENSION INDX(80),IREL(80),LP(20),IABST(20),NPARS(10)
      DIMENSION FUNS(20),PS(10,10),FLR(400),FLX(20),FLXS(20,10)
      DIMENSION AS(80,10,2),INDXS(80,10,2),IRELS(80,10,2),NLS(20)
      DIMENSION FMX(10),WSS(10)
      DIMENSION DS(20),SS(2),DELB(80),BTS(80),PTS(10),DELP(10)
      DIMENSION ALPHA(70,70),LV(70),LVINV(60)
       LOGICAL PROCEED,PROC, WARNED
       SAVE /ERRS/
       REAL IDHTST
C
C  DETERMINE NUMBER OF TRUE VARIABLES AND FILL ARRAYS X AND E,
C  CONTAINING VARIABLES AND ACCEPTABLE ERRORS
C
C      PROC=.TRUE.
      IAB=1
      NDI3=3*NDIM
      NDI4=4*NDIM
      NV=0
      IF(NL.NE.0) THEN
        DO 20 I=1,2
        ND=NDIM*(I-1)
        DO 10 J=1,NL
        IF(INDX(J+ND).NE.1) GO TO 10
        NV=NV+1
        X(NV)=A(J+ND)
   10   CONTINUE
   20   CONTINUE
      ENDIF
      DW=(W(NF)-W(1))/1.E4
      IF(NV.NE.0) THEN
       DO I=1,NV
         E(I)=DW
       ENDDO
      ENDIF
      J1=0
      IF(NL.NE.0) THEN
       DO 91 J=1,NL
       IND1=J+NDIM*2
       IF(INDX(IND1).NE.1) GO TO 91
       J1=J1+1
       LP(J)=J1+NPAR
   91  CONTINUE
      ENDIF
      IF(NV.EQ.0) GO TO 11
      WRITE(6,102) NV
!     WRITE(6,103) (X(I),E(I),I=1,NV)
  102 FORMAT
     :(/' Number of non-linear variables:',I5)
  103 FORMAT(1X,2F12.4)
C
C  DEFINE OTHER PARAMETERS FOR VA04A
C
      ESCALE=500.
      KPRINT=2
      ICON=1
      MAXIT=2*NV
      IF(MAXIT.LT.6) MAXIT=6
       CALL VA04A(X,E,NV,F,ESCALE,KPRINT,ICON,MAXIT,IBUG,PROCEED)
   11 CALL ELFSET(X,NINV)
      CALL ELFBPS(BTS,PTS,B,P)
C       CALL ELFNOYZ(PTS,BTS,NL,AX,BX)
!     IF(NL.NE.0) THEN
!  13  WRITE(6,108)
!      DO 15 J=1,NL
!       IP=B(J+NDI3)+0.1
!       S=ELFPFLX(B(J+NDIM),B(J+NDI2),IP)
!       FLX(J)=S
!      IDHTST = 0.0
!      IF (B(J).NE.0.0) THEN
!         IDHTST = (LOG10(ABS(B(J))))
!      ENDIF
!      IF (B(J+NDIM).NE.0.0) THEN
!         IDHTST = MIN(IDHTST,(LOG10(ABS(B(J+NDIM)))))
!      ENDIF
!      IF (IDHTST.GT.0.0) THEN
!         WRITE (6,107)
!    :    J, IP, B(J), B(J+NDIM), B(J+NDI2)*FLMAX*0.01,S
!      ELSE
!         WRITE (6,
!    :    '(''  '',I2,I6,1PE12.3,E11.3,E14.3,E12.3)')
!    :    J, IP, B(J), B(J+NDIM), B(J+NDI2)*FLMAX*0.01,S
!      ENDIF
!       WRITE (6,107)
!    :  J, IP, B(J), B(J+NDIM), B(J+NDI2)*FLMAX*0.01,S
!  15  CONTINUE
!     ENDIF
!     IF(NPAR.NE.0) THEN
!      DO J=1,NPAR
!       PTS(J)=P(J)*FLMAX*0.01
!      ENDDO
!     WRITE(6,114) WS, (PTS(J),J=1,NPAR)
! 114 FORMAT
!    : (/'   Coefficients of polynomial: ordinate origin at',F12.3/
!    : (1X,1P4E14.4))
!     ENDIF
C
C  CALCULATE FUNCTIONAL AT MINIMUM
      CALL ELFBPS(BTS,PTS,B,P)
      S0=ELFSFUN(PTS,BTS,NL)
      SIG=S0/(NF-NINV)
!     WRITE(6,106) S0
C
C  EVALUATE CURVATURE MATRIX AT MINIMUM.
C
C  For one sigma confidence level, require
C  delta (chi**2) = q * number of independent variables,
C  where q = 1.00 for one variable, and
C        q = 1.17 for more than one.
      NVT=NPAR
      DO J=1,NPAR
       LV(J)=0
      ENDDO
      IF(NL.NE.0) THEN
       DO J=1,NL
        DO I=1,3
         ND=NDIM*(I-1)
         IF(INDX(J+ND).EQ.1) THEN
          NVT=NVT+1
          LV(NVT)=J+ND
         ENDIF
        ENDDO
       ENDDO
       DO JV=1,NDI3
         LVINV(JV)=0
       ENDDO
       NPARP=NPAR+1
       DO JV=NPARP,NVT
         JJ=LV(JV)
         LVINV(JJ)=JV
       ENDDO
      ENDIF
       DCHIS=1.00
C
C  EVALUATE CURVATURE MATRIX, ALPHA
C
      DO JV1=1,NVT
       DO JV2=1,JV1
        JJ1=LV(JV1)
        JJ2=LV(JV2)
        CALL ELFBPS(BTS,PTS,B,P)
        CALL DELA(BTS,PTS,HH1,JV1,JJ1,1)
        S1P=ELFSFUN(PTS,BTS,NL)-S0
        CALL ELFBPS(BTS,PTS,B,P)
        CALL DELA(BTS,PTS,HH1,JV1,JJ1,-1)
        S1M=ELFSFUN(PTS,BTS,NL)-S0
        IF(JV1.EQ.JV2) THEN
         ALPHA(JV1,JV1)=(S1P+S1M)/(2*HH1*HH1)
C        WRITE(6,1247) JV1,JV2,JJ1,JJ2,S1P,S1M,HH1
C1247    FORMAT(1X,'JV1,JV2,JJ1,JJ2',4I3,' S1P,S1M',1P2E12.4,
C    *             'STEP',1PE13.5)
        ELSE
        CALL ELFBPS(BTS,PTS,B,P)
        CALL DELA(BTS,PTS,HH2,JV2,JJ2,1)
        S2P=ELFSFUN(PTS,BTS,NL)-S0
        CALL ELFBPS(BTS,PTS,B,P)
        CALL DELA(BTS,PTS,HH2,JV2,JJ2,-1)
        S2M=ELFSFUN(PTS,BTS,NL)-S0
        CALL ELFBPS(BTS,PTS,B,P)
        CALL DELA(BTS,PTS,HH1,JV1,JJ1,1)
        CALL DELA(BTS,PTS,HH2,JV2,JJ2,1)
        S3P=ELFSFUN(PTS,BTS,NL)-S0
        CALL ELFBPS(BTS,PTS,B,P)
        CALL DELA(BTS,PTS,HH1,JV1,JJ1,-1)
        CALL DELA(BTS,PTS,HH2,JV2,JJ2,-1)
        S3M=ELFSFUN(PTS,BTS,NL)-S0
        ALPHA(JV1,JV2)=(S3P+S3M-S1P-S1M-S2P-S2M)/(4*HH1*HH2)
        ALPHA(JV2,JV1)=ALPHA(JV1,JV2)
        S1=S1P+S1M
        S2=S2P+S2M
        S3=S3P+S3M
        ENDIF
C       WRITE(6,1222) JV1,JV2,JJ1,JJ2,S1,S2,S3,ALPHA(JV1,JV2)
C1222   FORMAT(1X,4I3,1P4E12.4)
       ENDDO
      ENDDO
C     WRITE(6,1003) ((ALPHA(I,J),J=1,NVT),I=1,NVT)
C1003 FORMAT(1X,'CURVATURE MATRIX'/(1P5D12.4))
C
C
C     WRITE(6,1043) SIG,DCHIS
 1043 FORMAT(1X,' SIGMA SQUARED',1PE15.3/
     C          ' DELTA CHI SQUARED FOR 68% CONFIDENCE',0PF10.3)
C
C     WRITE(15,1043) SIG,DCHIS
C  OBTAIN ERROR MATRIX
      CALL ELFINV(ALPHA,NVT)
c      WRITE(6,1033) ((ALPHA(I,J),J=1,NVT),I=1,NVT)
 1033 FORMAT(1X,'ERROR MATRIX'/(1P5D12.4))
C
C     WRITE(15,1033) ((ALPHA(I,J),J=1,NVT),I=1,NVT)

      WARNED = .FALSE.

      DO JV=1,NVT
       JJ=LV(JV)

       ERR= DCHIS*SIG*ALPHA(JV,JV)
       IF( ERR .LT. 0.0 ) THEN
          IF( .NOT. WARNED ) THEN
             WRITE(*,*)
             WRITE(*,*) '  WARNING: Errors may be incorrect!!'
             WARNED = .TRUE.
          END IF
          ERR = 0.0
       ELSE
          ERR=SQRT( ERR )
       END IF

C      WRITE(6,1034) JV,JJ,ERR
C1034  FORMAT(1X,'JV,JJ,ERR',2I3,1PE12.4)
       IF(JV.LE.NPAR) DELP(JV)=ERR*FLMAX*0.01
       IF(JV.GT.NPAR) DELB(JJ)=ERR

      ENDDO
C
C  CALCULATE ERRORS FOR FIXED AND DEPENDENT VARIABLES
      IF(NL.NE.0) THEN
         DO I=1,3
          ND=NDIM*(I-1)
          DO J=1,NL
           IF(INDX(J+ND).EQ.2) DELB(J+ND)=0.
          ENDDO
         ENDDO
C
C  PROPAGATE ERRORS TO DEPENDENT VARIABLES
        DO I=1,3
         ND=NDIM*(I-1)
         DO J=1,NL
          IF(INDX(J+ND).EQ.3) THEN
           K=IREL(J+ND)
           IF(I.NE.3) THEN
            DELB(J+ND)=DELB(K+ND)
           ELSE
            DELB(J+ND)=DELB(K+ND)*B(J+ND)/B(K+ND)
           ENDIF
          ENDIF
         ENDDO
        ENDDO
C
C  PROPAGATE ERRORS TO TOTAL FLUXES
C
        DO J=1,NL
         IP=B(J+NDI3)+0.1
         SSS=ELFPFLX(B(J+NDIM),B(J+NDI2),IP)
         IF(IP.LE.5) THEN
C
C  FIND INDICES FOR WIDTH AND PEAK, ALLOWING FOR DEPENDENCIES
           J1=INDX(J+NDIM)
           IF(J1.EQ.3) THEN
             J1=IREL(J+NDIM)+NDIM
           ELSE
             J1=J+NDIM
           ENDIF
           J2=INDX(J+NDI2)
           IF(J2.EQ.3) THEN
             J2=IREL(J+NDI2)+NDI2
           ELSE
             J2=J+NDI2
           ENDIF
C
C  RECOVER POSITION IN LIST OF VARIABLES
           K1=LVINV(J1)
           K2=LVINV(J2)
C
C  DETERMINE COVARIANCE OF PEAK AND WIDTH
           SIGPW=0.
           IF(K1.GT.0.AND.K2.GT.0) SIGPW=SIG*ALPHA(K1,K2)
C            WRITE(6,882) J1,J2,K1,K2,SIGPW
C 882        FORMAT(1X,'PAR INDEX J',2I3,5X,'  VAR INDEX K',2I3,
C    *                    1PE12.4)
          DS(J)=SSS*SQRT((DELB(J+NDIM)/B(J+NDIM))**2 +
     .                   (DELB(J+NDI2)/B(J+NDI2))**2  +
     .                   2.*SIGPW/(B(J1)*B(J2)))
         ELSE
          DS(J)=SSS*(DELB(J+NDI2)/B(J+NDI2))
         ENDIF
        ENDDO
      ENDIF
C  WRITE OUT ERRORS
C
!     WRITE(6,209)
      IF(NL.NE.0) THEN
!       WRITE(6,211)
        WRITE (6,108)
        DO J=1,NL

        IP=B(J+NDI3)+0.1
        S=ELFPFLX(B(J+NDIM),B(J+NDI2),IP)
        FLX(J)=S
        WRITE (6,107)
     :  J, IP, B(J), B(J+NDIM), B(J+NDI2)*FLMAX*0.01,S

         WRITE(6,210) DELB(J),DELB(J+NDIM),DELB(J+NDI2)*FLMAX*0.01,
     :               DS(J)
        ENDDO
      ENDIF
      IF(NPAR.GT.0) THEN

       DO J=1,NPAR
        PTS(J)=P(J)*FLMAX*0.01
       ENDDO
       WRITE(6,114) WS, (PTS(J),J=1,NPAR)
  114 FORMAT
     : (/' Coefficients of polynomial: ordinate origin at',F12.3/
     : (11X,1P4E14.4))

       WRITE(6,208) (DELP(J),J=1,NPAR)
      ENDIF
       WRITE (6,'(''0(All errors are 68% confidence intervals)''/)')
  106 FORMAT ('0  Value of functional is',1pE15.5)
  108 FORMAT(/' Line Profile    Centre   Width       Peak Flux',
     : '   Line Flux')
  107 FORMAT(2X,I2,I6,F14.3,F9.3,1PE14.3,E12.3)
  209 FORMAT(//2X,'Error estimates    (68% confidence limit)'/
     .         2X,'---------------'/)
  211 FORMAT(
     . ' Line      Centre       Width       Peak Flux       Line Flux')
  210 FORMAT(7X,'+/-',F14.3,F9.3,1PE14.3,E12.3)
! 208 FORMAT('   Errors in coefficients of polynomial:'
!    .       /(1X,1P4E14.4))
  208 FORMAT(8X,'+/-',1P4E14.4)
      RETURN
      END

