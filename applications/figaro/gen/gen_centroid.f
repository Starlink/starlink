      SUBROUTINE GEN_CENTROID(FARRAY,NO_EL,LSIG,LCENT,
     :                                       LSTRENGTH,STATUS)
C
C       G E N _ C E N T R O I D
C
C       Locates a peak in a data array by convolution with the
C       derivative of a Gaussian.
C
C       Parameters -    (">" input, "<" output, "!" modified)
C
C       (>) FARRAY    (Real array FARRAY(NO_EL)) Array holding
C                     the data to be searched for a peak.
C       (>) NO_EL     (Integer) The number of elements in FARRAY.
C       (>) LSIG      (Real) Measure of the expected line width -
C                     in elements - center will be taken over a
C                     block of data 6*LSIG elements wide.
C       (!) LCENT     (Real) Passed as a first guess as to the
C                     position of the line center, returned as the
C                     determined position.  Note that the first
C                     element of FARRAY is #1, not 0.
C                     Note: Assumes that the integer value of the
C                     pixel number refers to the center of the
C                     pixel.  ie if data is all zeros except for
C                     elements 9,10,11 which contain say, 25,50,25
C                     counts, LCENT will return as 10.0
C       (<) LSTRENGTH (Real) Returned as a measure of the line
C                     strength.  If an error occurs, LSTRENGTH
C                     will be set to some negative number.
C       (<) STATUS    (Integer) Returns a status code.
C                     0 => OK
C                     1 => Line too close to end of array.
C                     2 => Measurement does not converge.
C                     3 => Measurement is running away.
C                     4 => LSIG passed with a daft value.
C
C       History -
C
C       Orginally obtained from TYB's FORTH package.
C       Modified by KS: Error messages removed, replaced by
C       STATUS parameter; Integer array option removed.
C
C       This version -             KS / CIT   17th April 1983
C
C       Modified:
C
C       28th Sep 1992  HME / UoE, Starlink.  TABs removed.
C+
        INTEGER STATUS
        REAL FARRAY(NO_EL)
        REAL LCENT,LSIG,LSTRENGTH,LAST_LCENT
        IF(LSIG.EQ.0.)GO TO 3999
        NO_ITER=0
        PRECISION=0.001
        DEN=2.*LSIG**2
C ******************************************************
C   EACH ITERATION STARTS HERE
C ******************************************************
  100   LAST_LCENT = LCENT
        NO_ITER = NO_ITER + 1
C ******************************************************
C   SET 3 SIGMA LIMITS AROUND CENTER
C ******************************************************
        LLLIM = LCENT - 3. * LSIG
        LULIM = LCENT + 3. * LSIG + 0.5      ! TO ROUND
C ******************************************************
C   CHECK TO MAKE SURE ENTIRE REGION IS WITHIN ARRAY
C ******************************************************
        IF (LLLIM.LT.1) GO TO 999
        IF (LULIM.GT.NO_EL) GO TO 999
C ******************************************************
C   NOW CALCULATE G(X) AND G'(X)
C ******************************************************
        GOFX = 0.
        GPOFX = 0.
        DO 202 I = LLLIM,LULIM
        T1 = FLOAT(I) - LCENT - 0.5
        T2 = T1 + 1.
        T3 = FARRAY(I)
        TERM1 = EXP (-(T1**2)/DEN)
        TERM2 = EXP (-(T2**2)/DEN)
        GOFX = GOFX + T3 * (TERM2-TERM1)/2.
        GPOFX = GPOFX + T3 * (T2*2./DEN*TERM2 - T1* 2./DEN*TERM1)/2.
  202   CONTINUE
C ******************************************************
C   CHECK TO SEE IF IT IS RUNNING AWAY
C ******************************************************
        IF(GPOFX.EQ.0.)GO TO 2999
        IF(ABS(GOFX/GPOFX).GT.1.5*LSIG) GO TO 2999
C ******************************************************
C   OR BOUNCING BACK AND FORTH
C ******************************************************
        IF (NO_ITER.GE.10) GO TO 1999
C ******************************************************
C   CALCULATE NEW LCENT
C ******************************************************
        LCENT = LCENT - GOFX/GPOFX
C ******************************************************
C   HAS IT CONVERGED?
C ******************************************************
        IF(ABS(LCENT-LAST_LCENT).LE.PRECISION) GO TO 300
        GO TO 100
C ******************************************************
C   END OF ITERATION
C ******************************************************
  300   LSTRENGTH = GPOFX * DEN
        STATUS=0
        RETURN
  999   CONTINUE
        STATUS=1
        LSTRENGTH = 0.
        RETURN
 1999   CONTINUE
        STATUS=2
        LSTRENGTH = -1.
        RETURN
 2999   CONTINUE
        STATUS=3
        LSTRENGTH = -2
        RETURN
 3999   CONTINUE
        STATUS=4
        LSTRENGTH = -3.
        RETURN
        END
