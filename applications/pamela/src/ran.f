      REAL FUNCTION RAN1(ISEED)
*
* Numerical recipes random number routine
* Better but slower than RAN2
*
      INTEGER IFF, IX1, IX2, IX3, IC1, IC2, IC3, IA1, IA2, IA3
      PARAMETER (M1=259200,IA1=7141,IC1=54773,RM1=1./M1)
      PARAMETER (M2=134456,IA2=8121,IC2=28411,RM2=1./M2)
      PARAMETER (M3=243000,IA3=4561,IC3=51349)
      REAL R(97)
      DATA IFF/0/
      SAVE IFF, IX1, IX2, IX3, R

      IF(ISEED.LT.0 .OR. IFF.EQ.0) THEN
        IFF = 1
        IX1 = MOD(IC1-ISEED,M1)
        IX1 = MOD(IA1*IX1+IC1,M1)
        IX2 = MOD(IX1,M2)
        IX1 = MOD(IA1*IX1+IC1,M1)
        IX3 = MOD(IX1,M3)
        DO 11 J=1, 97
          IX1=MOD(IA1*IX1+IC1,M1)
          IX2=MOD(IA2*IX2+IC2,M2)
          R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11      CONTINUE
        ISEED = 1
      END IF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      J=1+(97*IX3)/M3
      IF(J.GT.97 .OR. J.LT.1) THEN
        WRITE(*,*) 'RAN1 error'
        RAN1 = 0.
        RETURN
      END IF
      RAN1 = R(J)
      R(J) = (FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      RETURN
      END

      REAL*4 FUNCTION RAN2(ISEED)

***** Calculates random number between 0 and 1.  From Numerical Recipes.

      PARAMETER (M=714025,IA=1366,IC=150889,RM=1./M)
      SAVE IFF, IR, IY
      INTEGER IR(97), IY, IFF
      DATA IFF /0/

***** Initialization.

      IF(ISEED.LT.0 .OR. IFF.EQ.0) THEN
        IFF = 1
        ISEED = MOD(IC-ISEED,M)
        DO J = 1, 97
          ISEED = MOD(IA*ISEED+IC,M)
          IR(J) = ISEED
        END DO
        ISEED=MOD(IA*ISEED+IC,M)
        IY=ISEED
      END IF

***** Start here except for initialization

      J = 1+(97*IY)/M
      IY= IR(J)
      RAN2  = IY*RM
      ISEED = MOD(IA*ISEED+IC,M)
      IR(J) = ISEED

      RETURN

      END
