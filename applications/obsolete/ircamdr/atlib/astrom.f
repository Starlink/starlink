      SUBROUTINE ASTROM(SMARGE,JX,LX,XI,S1,SX,DCEN,B,KOUNT)
*+
*   ASTROM
*
*   astrometric fit for GAUFIT procedure
*
*   Given      (arguments)
*   SMARGE  RA  marginal vector
*   JX      I   low-side local minimum of SMARGE - 1
*   LX      I   high-side local minimum of SMARGE + 1
*   XI      R   estimate of image centre
*   SX      R   estimate of image width
*
*   Returned   (arguments)
*   XI      R   estimate of image centre.
*               The given value is returned on convergence failure
*   S1      R   estimate of error in XI.
*               S1 = 0.0 on convergence failure
*   SX      R   estimate of image width
*   DCEN    R   estimate of image height
*   B       R   estimate of background density
*   KOUNT   I   number of iterations required
*               KOUNT = 0 on convergence failure
*                     = 8 on convergence incomplete
*   Subroutines called :
*   INVERS,VMUL        : E2DLIB
*
*   B.D.Kelly/ROE/6.5.1982
*-
      DOUBLE PRECISION CTF(5),CTC(5,5),V(5)
      REAL C(3)
      REAL SMARGE(LX)
      INTEGER JX,LX,KOUNT
      REAL XI,SX,S1,DCEN,B

      IF((LX-JX).LE.4) THEN
        KOUNT=0
        S1=0.0
        RETURN
      ENDIF
      XO=XI
*
*   this part of the program uses the estimates of the image centre and
*   width from SEARCH to derive initial estimates of the
*   image height and background density by linear least-squares.
*
      SUM1=0.0
      SUM2=0.0
      DCEN=0.0
      B=0.0
      DO I=JX,LX
         EX=EXP(-0.5*((I-XO)/SX)**2)
         DCEN=DCEN+EX*SMARGE(I)
         B=B+EX*EX
         SUM1=SUM1+SMARGE(I)
         SUM2=SUM2+EX
      ENDDO
      F=LX-JX+1
      DCEN=(DCEN-SUM1*SUM2/F)/(B-SUM2*SUM2/F)
      B=(SUM1-DCEN*SUM2)/F
      IF(DCEN.LT.20.0) DCEN=20.0
      IF(SX.LT.2.0) SX=2.0
      FOLD=1.0E38
*
*   main iteration loop.
*
      DO JKOUNT=1,8
         KOUNT=JKOUNT
         S1=0.0
         S2=0.0
         FF=0.0
         SXSI=1.0/(SX*SX)
         DO I=1,4
            CTF(I)=0.0D0
            DO J=1,4
               CTC(I,J)=0.0D0
            ENDDO
         ENDDO
*
*   set up normal equations.
*
         DO L=JX,LX
            C(2)=EXP(-0.5*SXSI*(L-XO)**2)
            S=DCEN*C(2)
            C(1)=(L-XO)*SXSI*S
            S1=S1+(C(1)*(S+B-SMARGE(L)))**2
            S2=S2+C(1)**2
            FF=FF+(S+B-SMARGE(L))**2
            C(3)=C(1)*(L-XO)/SX
            DO I=1,3
               CTF(I)=CTF(I)+C(I)*(S+B-SMARGE(L))
               CTC(I,4)=CTC(I,4)+C(I)
               DO J=I,3
                  CTC(I,J)=CTC(I,J)+C(I)*C(J)
               ENDDO
            ENDDO
            CTF(4)=CTF(4)+S+B-SMARGE(L)
         ENDDO
         CTC(4,4)=DBLE(LX-JX+1)
         DO I=2,4
            L=I-1
            DO J=1,L
               CTC(I,J)=CTC(J,I)
            ENDDO
         ENDDO
*
*   solve normal equations.
*
         CALL INVERS(CTC,4)
         CALL VMUL(CTC,4,CTF,V)
*
*   if DA is the correction to the estimate of A, then replacing DA by
*   DA/(1+(DA/X))=DA' prevents an ill-determined solution from jumping
*   around too much in subsequent iterations.
*   DA' is less than X, and DA' goes to DA for DA much less than X.
*   if X=A/R for some R greater than one, then an intrinsically
*   positive parameter will never be permitted to pass through zero.
*   this can be helpful for parameters like image width or height.
*
         XO=XO-V(1)/(1.0+ABS(V(1)/5.0))
         DCEN=DCEN-V(2)/(1.0+2.0*ABS(V(2)/DCEN))
         SX=SX-V(3)/(1.0+2.0*ABS(V(3)/SX))
         B=B-V(4)/(1.0+2.0*ABS(V(4)/B))
         IF((DCEN.LT.20.0).OR.(SX.LT.1.0).OR.(SX.GT.0.5*(LX-JX+1))) THEN
           KOUNT=0
           S1=0.0
           RETURN
         ENDIF
         IF((ABS(V(1)).LT.0.01).AND.((FOLD-FF)/FOLD.LT.0.05)) THEN
           XI=XO
           S1=S1*CTC(1,1)*(LX-JX+1)/(S2*(LX-JX-3.0))
           RETURN
         ENDIF
         FOLD=FF
      ENDDO
      S1=0.0
      RETURN
      END
