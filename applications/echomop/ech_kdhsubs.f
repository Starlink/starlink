      DOUBLE PRECISION FUNCTION POLY( A, N, X )
C
C               N
C  EVALUATES   SUM A(J) * X**(J-1)
C              J=1
C
      INTEGER N
      DOUBLE PRECISION A( N )
      DOUBLE PRECISION X
C
      IF ( N .LT. 1 ) THEN
         POLY = 0.D0
         RETURN
      END IF
C
      GOTO (1,2,3,4,5,6,7,8,9,10,11) N
C
      POLY=A(N)
      IF(N.LE.1) RETURN
      IC=N
      DO 100 I=2,N
      IC=IC-1
      POLY=POLY*X+A(IC)
  100 CONTINUE
      RETURN
C
    1 POLY=A(1)
      RETURN
C
    2 POLY=X*A(2)+A(1)
      RETURN
C
    3 POLY=X*(X*A(3)+A(2))+A(1)
      RETURN
C
    4 POLY=X*(X*(X*A(4)+A(3))+A(2))+A(1)
      RETURN
C
    5 POLY=X*(X*(X*(X*A(5)+A(4))+A(3))+A(2))+A(1)
      RETURN
C
    6 POLY=X*(X*(X*(X*(X*A(6)+A(5))+A(4))+A(3))+A(2))+A(1)
      RETURN
C
    7 POLY=X*(X*(X*(X*(X*(X*A(7)+A(6))+A(5))+A(4))+A(3))+A(2))+A(1)
      RETURN
C
    8 POLY=X*(X*(X*(X*(X*(X*(X*A(8)+A(7))+A(6))+A(5))+A(4))+A(3))
     #+A(2))+A(1)
      RETURN
C
    9 POLY=X*(X*(X*(X*(X*(X*(X*(X*A(9)+A(8))+A(7))+A(6))+A(5))+A(4))
     #+A(3))+A(2))+A(1)
      RETURN
C
   10 POLY=X*(X*(X*(X*(X*(X*(X*(X*(X*A(10)+A(9))+A(8))+A(7))+A(6))
     #+A(5))+A(4))+A(3))+A(2))+A(1)
      RETURN
C
   11 POLY=X*(X*(X*(X*(X*(X*(X*(X*(X*(X*A(11)+A(10))+A(9))+A(8))
     #+A(7))+A(6))+A(5))+A(4))+A(3))+A(2))+A(1)
      RETURN
C
      END

      DOUBLE PRECISION FUNCTION DPOLY(A,N,X)
C
C               N
C  EVALUATES   SUM  A(J) * (J-1) * X**(J-2)
C              J=2
C
      INTEGER N
      DOUBLE PRECISION A( N )
      DOUBLE PRECISION X
C
      IF(N.LT.1) THEN
         DPOLY=0.D0
         RETURN
      END IF
C
      GOTO(1,2,3,4,5,6,7,8,9,10,11),N
C
      DPOLY=A(N)*(N-1)
      IC=N
      DO 100 I=3,N
      IC=IC-1
      DPOLY=DPOLY*X+A(IC)*(IC-1.)
  100 CONTINUE
      RETURN
C
    1 DPOLY=0.D0
      RETURN
C
    2 DPOLY=A(2)
      RETURN
C
    3 DPOLY=X*2*A(3)+A(2)
      RETURN
C
    4 DPOLY=X*(X*3*A(4)+2*A(3))+A(2)
      RETURN
C
    5 DPOLY=X*(X*(X*4*A(5)+3*A(4))+2*A(3))+A(2)
      RETURN
C
    6 DPOLY=X*(X*(X*(X*5*A(6)+4*A(5))+3*A(4))+2*A(3))+A(2)
      RETURN
C
    7 DPOLY=X*(X*(X*(X*(X*6*A(7)+5*A(6))+4*A(5))+3*A(4))+2*A(3))
     #+A(2)
      RETURN
C
    8 DPOLY=X*(X*(X*(X*(X*(X*7*A(8)+6*A(7))+5*A(6))+4*A(5))
     #+3*A(4))+2*A(3))+A(2)
      RETURN
C
    9 DPOLY=X*(X*(X*(X*(X*(X*(X*8*A(9)+7*A(8))+6*A(7))+5*A(6))
     #+4*A(5))+3*A(4))+2*A(3))+A(2)
      RETURN
C
   10 DPOLY=X*(X*(X*(X*(X*(X*(X*(X*9*A(10)+8*A(9))+7*A(8))
     #+6*A(7))+5*A(6))+4*A(5))+3*A(4))+2*A(3))+A(2)
      RETURN
C
   11 DPOLY=X*(X*(X*(X*(X*(X*(X*(X*(X*10*A(11)+9*A(10))+8*A(9))
     #+7*A(8))+6*A(7))+5*A(6))+4*A(5))+3*A(4))+2*A(3))+A(2)
      RETURN
C
      END

      SUBROUTINE REJECT( NDATA, DATA, SIGMA, FIT,
     :           THRHI, THRLO, RMS, NREJ )
*
*  FLAGS OUTLIERS BY MAKING SIGMA NEGATIVE
*
*  Input:
*       NDATA   = NUMBER OF DATA VALUES
*       DATA    = DATA VALUES
*       SIGMA   = UNCERTAINTIES
*       FIT     = PREDICTED DATA VALUES
*       THRHI   = HIGH REJECT THRESHOLD
*       THRLO   = LOW REJECT THRESHOLD
*  Output:
*       RMS     = RMS RESIDUAL OF POINTS NOT REJECTED
*       NREJ    = NUMBER OF REJECTED DATA POINTS
*
*  JULY 1984 BY KEITH HORNE
* 29/01/87 TRM @ RGO put in restore data section as well as reject
* 12/01/87 TRM @ RGO NREJ = - 1 disables restore option
*
      REAL DATA(1), SIGMA(1), FIT(1)
      DOUBLE PRECISION CALC
      LOGICAL RESTORE
*
      RESTORE = .TRUE.
      IF(NREJ.EQ.-1) RESTORE = .FALSE.
      IF( NDATA.LE.0) THEN
        GOTO 999
      END IF

* Compute scale-up factor for uncertainties
      CALC = 0.D0
      NSUM = 0
      DO I=1,NDATA
      IF(SIGMA(I).GT.0.) THEN
        NSUM = NSUM + 1
        RESIDUAL = ( DATA(I) - FIT(I) ) / SIGMA(I)
        CALC = CALC + RESIDUAL*RESIDUAL
      END IF
      END DO
      RMS = 1.
      IF( NSUM.GT.0 ) RMS = SQRT(CALC/NSUM)

* Reject outlying data
* and restore OK data
      THI = THRHI * RMS
      TLO = THRLO * RMS
      CALC = 0.D0
      NREJ = 0
      NOK  = 0
      DO I=1,NDATA
          IF(SIGMA(I).NE.0.) THEN
            RESIDUAL = (DATA(I)-FIT(I))/ABS(SIGMA(I))
            IF(SIGMA(I).GT.0.) THEN
              IF( RESIDUAL.GT.THI .OR. RESIDUAL.LT.TLO ) THEN
* Reject point
                NREJ = NREJ + 1
                SIGMA(I) = - ABS(SIGMA(I))
              ELSE
                CALC = CALC + RESIDUAL*RESIDUAL
                NOK = NOK + 1
              END IF
            ELSE
              IF(RESTORE.AND.RESIDUAL.LT.THI.AND.RESIDUAL.GT.TLO) THEN
* Restore point
                SIGMA(I) = ABS(SIGMA(I))
              ELSE
                NREJ = NREJ + 1
                SIGMA(I) = - ABS(SIGMA(I))
              END IF
            END IF
         END IF
      END DO
      RMS = 1.
      IF(NREJ.LT.NDATA) RMS = SQRT(CALC/REAL(NOK))

* normal return
      RETURN

* error return
  999 CONTINUE
      RETURN

      END


      SUBROUTINE REJECT2( NDATA, DATA, SIGMA, FIT,
     :           THRHI, THRLO, RMS, NREJ )
*
*  FLAGS OUTLIERS BY MAKING SIGMA NEGATIVE
*
*  Input:
*       NDATA   = NUMBER OF DATA VALUES
*       DATA    = DATA VALUES
*       SIGMA   = UNCERTAINTIES
*       FIT     = PREDICTED DATA VALUES
*       THRHI   = HIGH REJECT THRESHOLD
*       THRLO   = LOW REJECT THRESHOLD
*  Output:
*       RMS     = RMS RESIDUAL OF POINTS NOT REJECTED
*       NREJ    = NUMBER OF REJECTED DATA POINTS
*
*  JULY 1984 BY KEITH HORNE
* 29/01/87 TRM @ RGO put in restore data section as well as reject
* 12/01/87 TRM @ RGO NREJ = - 1 disables restore option
*
      DOUBLE PRECISION DATA(1), FIT(1)
      DOUBLE PRECISION CALC
      REAL SIGMA(1)
      LOGICAL RESTORE
*
      RESTORE = .TRUE.
      IF(NREJ.EQ.-1) RESTORE = .FALSE.
      IF( NDATA.LE.0) THEN
        GOTO 999
      END IF

* Compute scale-up factor for uncertainties
      CALC = 0.D0
      NSUM = 0
      DO I=1,NDATA
      IF(SIGMA(I).GT.0.) THEN
        NSUM = NSUM + 1
        RESIDUAL = ( DATA(I) - FIT(I) ) / SIGMA(I)
        CALC = CALC + RESIDUAL*RESIDUAL
      END IF
      END DO
      RMS = 1.
      IF( NSUM.GT.0 ) RMS = SQRT(CALC/NSUM)

* Reject outlying data
* and restore OK data
      THI = THRHI * RMS
      TLO = THRLO * RMS
      CALC = 0.D0
      NREJ = 0
      NOK  = 0
      DO I=1,NDATA
          IF(SIGMA(I).NE.0.) THEN
            RESIDUAL = (DATA(I)-FIT(I))/ABS(SIGMA(I))
            IF(SIGMA(I).GT.0.) THEN
              IF( RESIDUAL.GT.THI .OR. RESIDUAL.LT.TLO ) THEN
* Reject point
                NREJ = NREJ + 1
                SIGMA(I) = - ABS(SIGMA(I))
              ELSE
                CALC = CALC + RESIDUAL*RESIDUAL
                NOK = NOK + 1
              END IF
            ELSE
              IF(RESTORE.AND.RESIDUAL.LT.THI.AND.RESIDUAL.GT.TLO) THEN
* Restore point
                SIGMA(I) = ABS(SIGMA(I))
              ELSE
                NREJ = NREJ + 1
                SIGMA(I) = - ABS(SIGMA(I))
              END IF
            END IF
         END IF
      END DO
      RMS = 1.
      IF(NREJ.LT.NDATA) RMS = SQRT(CALC/REAL(NOK))

* normal return
      RETURN

* error return
  999 CONTINUE
      RETURN

      END
