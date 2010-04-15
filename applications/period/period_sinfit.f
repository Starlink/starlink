
      SUBROUTINE PERIOD_SINFIT(XDATA,YDATA,YERR,NUM,PERIOD,GAM,
     :                         KVEL,PHI0,COV,NP,F,IFAIL)

C============================================================================
C Subroutine to fit " Y = GAM + KVEL*SIN( 2*PI*(X/PERIOD-PHI0) ) "
C Returning GAM, KVEL, PHI0.
C
C Method fits " Y = A + B*SIN(2*PI/PERIOD*X) + C*COS(2*PI/PERIOD*X) "
c using linear least squares, then converts A, B, C TO GAM, KVEL, PHI0.
C Formal least squares error matrix for A, B, C transformed to the
C matrix for GAM, KVEL, PHI0 using a linear approximation.
C To ignore any point, YERR should be set to less than or equal 0.
C
C PASSED:
C
C DOUBLE PRECISION XDATA(NUM) -- ARRAY OF X VALUES
C DOUBLE PRECISION YDATA(NUM) -- ARRAY OF Y VALUES
C DOUBLE PRECISION YERR(NUM)  -- ARRAY OF ERRORS ON Y VALUES 1-SIGMA
C DOUBLE PRECISION PERIOD     -- ( X VALUE/PERIOD ) REPRESENT A PHASE
C                                OF THE SINE CURVE
C
C INTEGER NUM     -- NUMBER OF VALUES
C
C RETURNED:
C
C DOUBLE PRECISION GAM      -- "GAMMA VELOCITY"
C DOUBLE PRECISION KVEL     -- "K VELOCITY"
C DOUBLE PRECISION PHI0     -- "PHASE OFFSET" 0 IF RED STAR VELOCITY
C DOUBLE PRECISION COV(6)   -- VARIANCE MATRIX OF THE ABOVE. Y ERROR
C                              ARRAY MUST BE CORRECT
C		     THESE ARE CALCULATED BY TRANSFORMING THE COVARIANCE MATRIX
C		     FOR A, B, C USING A LINEAR APPROXIMATION
C DOUBLE PRECISION    F     -- VALUE OF CHI-SQUARED, CORRECT IF YERR
C                              ARE SCALED CORRECTLY
C INTEGER NP    -- ACTUAL NUMBER OF POINTS USED
C INTEGER IFAIL -- 0 IF NO ERROR, 1 OTHERWISE
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 1-July-1992.
C
C GJP March 1997
C
C Corrected the SIN2D, COSD and ATAN2D expressions. Removed
C REAL*4 and INTEGER *4 expressions. Removed IJK - did not seem
C to do anything.
C
C Converted to Double Precision (KPD), August 2001

C Should QTWOPI be identical to TWOPI, rather than PI? KPD noted a
C possible "naming" inconsistency, whereby the original use of TWOPI has
C been highlighted (by KPD) by a renaming of that variable from TWOPI to
C QTWOPI, subject to it being verfied (or otherwise) by Vik Dhillon that
C the original intention was indeed for the TWOPI variable to be assigned
C a calculated value equivalent to "pi" !! (KPD) August 2001

C Corrected calculation of XX to Use TWOPI not QTWOPI (=PI) (AJC) April 2003
C============================================================================

      IMPLICIT NONE

      INCLUDE "PIVARS"

      INTEGER IFAIL,NP,I,NUM
      DOUBLE PRECISION XDATA(NUM),YDATA(NUM),YERR(NUM)
      DOUBLE PRECISION A,B,C,PERIOD,COV(6)
      DOUBLE PRECISION KVEL,GAM,PHI0, QTWOPI
      DOUBLE PRECISION XX,SN,CN,WW,W1,C1,C2,C3,C4,C5,C6,DET,F,SQ
      DOUBLE PRECISION SW,SY,SY2,SS,SS2,SYS,SYC,SC,SSC,SC2

C----------------------------------------------------------------------------
C Set dummy variables to 0.0D0, to accumulate sums.
C----------------------------------------------------------------------------

      QTWOPI = 4.0D0*DATAN(1.0D0)
      IF ( PERIOD.LE.0.0D0 ) THEN
         IFAIL = 1
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: PERIOD_SINFIT failed.'
         WRITE (*, *) '** ERROR: Period is less than or equal to zero.'
         RETURN
      END IF
      IFAIL = 0
      SW = 0.0D0
      SY = 0.0D0
      SY2 = 0.0D0
      SS = 0.0D0
      SS2 = 0.0D0
      SYS = 0.0D0
      SYC = 0.0D0
      SC = 0.0D0
      SSC = 0.0D0
      SC2 = 0.0D0
      NP = 0
      DO 100 I = 1, NUM
         IF ( YERR(I).GT.0.0D0 ) THEN
            NP = NP + 1
            XX = TWOPI*XDATA(I)/PERIOD
            SN = DSIN(XX)
            CN = DCOS(XX)
            WW = 1.0D0/YERR(I)/YERR(I)
            W1 = WW*SN

C----------------------------------------------------------------------------
C Accumulate sums.
C----------------------------------------------------------------------------

            SW = SW + WW
            SY = SY + WW*YDATA(I)
            SY2 = SY2 + WW*YDATA(I)*YDATA(I)
            SS = SS + W1
            SS2 = SS2 + W1*SN
            SYS = SYS + W1*YDATA(I)
            SYC = SYC + WW*CN*YDATA(I)
            SC = SC + WW*CN
            SSC = SSC + W1*CN
            SC2 = SC2 + WW*CN*CN
         END IF
 100  CONTINUE
      IF ( NP.LE.3 ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: PERIOD_SINFIT failed.'
         WRITE (*, *)
     :              '** ERROR: <= 3 valid points for a 3 parameter fit.'
         IFAIL = 1
      END IF

C----------------------------------------------------------------------------
C Linear least squares to find A, B and C. Need to invert 3x3 (symmetric)
C matrix. Calculate co-factors (only 6).
C----------------------------------------------------------------------------

      C1 = SS2*SC2 - SSC*SSC
      C2 = SC*SSC - SS*SC2
      C3 = SS*SSC - SC*SS2
      C4 = SW*SC2 - SC*SC
      C5 = SC*SS - SW*SSC
      C6 = SW*SS2 - SS*SS

C----------------------------------------------------------------------------
C Calculate determinant.
C----------------------------------------------------------------------------
      DET = SW*C1 + SS*C2 + SC*C3
      IF ( DABS(DET).LT.1.0D-24 ) THEN
         IFAIL = 1
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: PERIOD_SINFIT failed.'
         WRITE (*, *) '** ERROR: ABS(DETERMINANT) < 1.D-24.'
         RETURN
      END IF

C----------------------------------------------------------------------------
C Calculate A, B and C.
C----------------------------------------------------------------------------

      A = (C1*SY+C2*SYS+C3*SYC)/DET
      B = (C2*SY+C4*SYS+C5*SYC)/DET
      C = (C3*SY+C5*SYS+C6*SYC)/DET

C----------------------------------------------------------------------------
C Calculate chi-squared.
C----------------------------------------------------------------------------

      F = SY2 + A*A*SW + B*B*SS2 + C*C*SC2 + 2.0D0*B*C*SSC +
     :    2.0D0*A*B*SS + 2.0D0*A*C*SC - 2.0D0*A*SY -
     :    2.0D0*B*SYS - 2.0D0*C*SYC

C----------------------------------------------------------------------------
C Calculate GAM, KVEL and PHI0, and their errors.
C----------------------------------------------------------------------------

      SQ = B*B + C*C
      KVEL = DSQRT(SQ)
      IF ( KVEL.EQ.0.0D0 ) THEN
         IFAIL = 1
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: PERIOD_SINFIT failed.'
         WRITE (*, *) '** ERROR: KVEL = 0. Windowed data?'
         RETURN
      END IF
      COV(1) = C1/DET
      COV(2) = (C2*B+C3*C)/KVEL/DET
      COV(3) = (C2*C-C3*B)/SQ/QTWOPI/DET
      COV(4) = (C4*B*B+2.0D0*C5*B*C+C6*C*C)/SQ/DET
      COV(5) = ((C4-C6)*B*C+C5*(C*C-B*B))/SQ/KVEL/QTWOPI/DET
      COV(6) = (C4*C*C-2.0D0*C5*B*C+C6*B*B)/DET/SQ/SQ/QTWOPI/QTWOPI
      GAM = A
      PHI0 = DATAN2(-C, B)/2.0D0/PI
      RETURN
      END
