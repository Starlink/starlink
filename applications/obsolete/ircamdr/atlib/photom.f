      SUBROUTINE PHOTOM(N,KKK,MODE,DS,DENMAG,SUM,P,B,
     :                  SIG,CF,NP,SRS)
*+
*   PHOTOM
*
*   radial profile fit to photometric image parameters for GAUFIT
*
*   Given      (arguments)
*   N       I   No. of annulus with innermost local minimum
*               beyond one-sigma point
*   MODE    I   no. of parameters being determined
*   DS      R   saturation density (given if MODE.LT.4)
*   SUM     RA  mean densities of annular rings
*   P       R   saturation exponent
*   SIG     R   image width
*   CF      R   coefficients of SIG = FN(DENMAG), given if MODE=2
*   NP      IA  no. of pixels in annular rings
*   SRS     RA  sums of squares of radii to pixels in annular rings
*
*   Returned   (arguments)
*   KKK     I   no. of iterations, = 0 on convergence failure
*   DS      R   saturation density (returned if MODE.GE.4)
*   DENMAG  R   magnitude parameter
*   P       R   saturation exponent (returned if MODE.EQ.5)
*   B       R   sky background
*   SIG     R   image width
*
*   Subroutines called :
*   INVERS,VMUL        : E2DLIB
*
*   B.D.Kelly/ROE/7.5.1982
*-
      INTEGER N,KKK,MODE
      REAL DS,P,SIG,DENMAG,B
      DOUBLE PRECISION C(5,5),V(5),X(5)
      REAL SUM(N),SRS(N),CF(6)
      INTEGER NP(N)

*
*   Set up initial values
*
      RS=-1.0
      RRSS=1.0
      B=SUM(N)
      DCEN=SUM(1)*(1.0-MIN(SUM(1)/DS,0.9)**P)**(-1.0/P)-B
      DENMAG=1.0857*(10.0-ALOG(DCEN*SIG*SIG))
      X(1)=1.0
*
*   this is the big iteration loop
*
      KKK=0
      DO WHILE((KKK.LE.10).AND.(DENMAG.LT.10.0).AND.(SIG.GE.1.0)
     :         .AND.((ABS(X(1)).GE.0.001).OR.(RS/RRSS.LT.0.9)))
         KKK=KKK+1
*
*
*   if image width as a function of magnitude index is known (MODE=2),
*   compute image width and derivative of image width with respect to
*   magnitude index.
*
         IF(MODE.LE.2) THEN
            DX=EXP(-DENMAG)
            SIG=DENMAG*(DENMAG*(DENMAG*CF(6)+CF(5))+CF(4))
            SIG=DENMAG*(SIG+CF(3))+CF(2)
            SIG=SIG*DX
            DSIGDD=DENMAG*(DENMAG*(DENMAG*CF(6)*5.0+CF(5)*4.0)+
     :             CF(4)*3.0)
            DSIGDD=DENMAG*(DSIGDD+CF(3)+CF(3))+CF(2)
            DSIGDD=DSIGDD*DX-SIG
            SIG=SIG+CF(1)
         ENDIF
         SIGSI=1.0/(SIG*SIG)
         RRSS=RS
         RS=0.0
         DO I=1,MODE
            V(I)=0.000
            DO J=1,MODE
               C(I,J)=0.000
            ENDDO
         ENDDO
*
*   set up normal equations
*
         ALP=-1.0-1.0/P
         DO I=1,N
            IF(NP(I).GE.1) THEN
               D1=EXP(10.0-0.921*DENMAG-0.5*SRS(I)*SIGSI)
               D2=SIGSI*D1
               DX=((D2+B)/DS)**P
               DY=1.0+DX
               DFDD=DY**ALP
               IF( MODE .GT. 3 ) THEN
                  X(4)=DFDD*(DX/DS)*(D2+B)
                  IF( MODE .GT. 4 ) THEN
                     X(5)=DFDD*(D2+B)*(DY/P)*
     :                    (ALOG(DY)/P-(DX/DY)*ALOG((D2+B)/DS))
                  ENDIF
               ENDIF
               X(3)=DFDD*D2*(SRS(I)*SIGSI-2.0)/SIG
               X(2)=DFDD
               X(1)=-DFDD*0.921*D2
               IF(MODE.LE.2)X(1)=X(1)+DSIGDD*X(3)
               R=DFDD*(D2+B)*DY-SUM(I)
               DO J=1,MODE
                  V(J)=V(J)+REAL(NP(I))*X(J)*R
                  DO K=J,MODE
                     C(J,K)=C(J,K)+REAL(NP(I))*X(J)*X(K)
                  ENDDO
               ENDDO
               RS=RS+REAL(NP(I))*R**2
            ENDIF
         ENDDO
         DO I=2,MODE
            K=I-1
            DO J=1,K
               C(I,J)=C(J,I)
            ENDDO
         ENDDO
*
*   solve normal equations
*
         CALL INVERS(C,MODE)
         CALL VMUL(C,MODE,V,X)
         RS=RS/REAL(N-MODE)
*
*   improve parameter estimates.
*
         IF( MODE .GT. 2 ) THEN
            SIG=SIG-X(3)/(1.0+2.0*DABS(X(3))/SIG)
            IF( MODE .GT. 3 ) THEN
               DS=DS-X(4)/(1.0+3.0*DABS(X(4))/DS)
               IF( MODE .GT. 4 ) THEN
                  P=P-X(5)/(1.0+2.0*DABS(X(5))/P)
               ENDIF
            ENDIF
         ENDIF
         B=B-X(2)/(1.0+3.0*DABS(X(2))/B)
         DENMAG=DENMAG-X(1)/(1.0+2.0*DABS(X(1)))
      ENDDO
*
*   Check for convergence failure
*
      IF((DENMAG.GT.10.0).OR.(SIG.LT.1.0).OR.(KKK.GT.10)) KKK=0

      END
