C
C This file contains subroutines that are not I/O related, but rather
C involve arithmetic or character operations that my be somewhat
C machine-specific-- it may be necessary or desirable to make some
C changes to optimize the code to run on your computer.
C
C              OFFICIAL DAO VERSION:  1991 April 18
C
C***********************************************************************
C
C Current contents:
C
C   INVERS  inverts a square matrix.
C
C     VMUL  multiplies a square matrix (on the left) by a column vector
C           (on the right) yielding a new column vector.
C
C    QUICK  does a quicksort on a vector of data.
C
C     SHOW  produces what is effectively an eleven-level gray scale plot
C           of a rectangular data array on the terminal.
C
C***********************************************************************
C
      SUBROUTINE  INVERS (A, MAX, N, IFLAG)
C
C Although it seems counter-intuitive, the tests that I have run
C so far suggest that the 180 x 180 matrices that NSTAR needs can
C be inverted with sufficient accuracy if the elements are REAL
C rather than DOUBLE PRECISION
C
C Arguments
C
C     A (INPUT/OUTPUT) is a square matrix of dimension N.  The inverse
C       of the input matrix A is returned in A.
C
C   MAX (INPUT) is the size assigned to the matrix A in the calling
C       routine.  It's needed for the dimension statement below.
C
C IFLAG (OUTPUT) is an error flag.  IFLAG = 1 if the matrix could not
C       be inverted; IFLAG = 0 if it could.
C
      IMPLICIT NONE
      INTEGER MAX
      REAL A(MAX,MAX)
C
      INTEGER N, IFLAG, I, J, K
C
C-----------------------------------------------------------------------
C
      IFLAG=0
      I=1
  300 IF(A(I,I).EQ.0.0E0)GO TO 9100
      A(I,I)=1.0E0/A(I,I)
      J=1
  301 IF(J.EQ.I)GO TO 304
      A(J,I)=-A(J,I)*A(I,I)
      K=1
  302 IF(K.EQ.I)GO TO 303
      A(J,K)=A(J,K)+A(J,I)*A(I,K)
  303 IF(K.EQ.N)GO TO 304
      K=K+1
      GO TO 302
  304 IF(J.EQ.N)GO TO 305
      J=J+1
      GO TO 301
  305 K=1
  306 IF(K.EQ.I)GO TO 307
      A(I,K)=A(I,K)*A(I,I)
  307 IF(K.EQ.N)GO TO 308
      K=K+1
      GO TO 306
  308 IF(I.EQ.N)RETURN                                   ! Normal return
      I=I+1
      GO TO 300
C
C-----------------------------------------------------------------------
C
C Error:  zero on the diagonal.
C
 9100 IFLAG=I
      RETURN
C
      END!
C
C#######################################################################
C
      SUBROUTINE  VMUL (A, MAX, N, V, X)
C
C Multiply a matrix by a vector:
C
C                    A * V = X
C
C Arguments
C
C    A(column,row)  (INPUT) is a square matrix of dimension N.
C
C              MAX  (INPUT) is the size assigned to the array in the
C                           calling routine.
C
C           V(row)  (INPUT) is a column vector of dimension N.
C
C           X(row) (OUTPUT) is a column vector of dimension N.
C
      IMPLICIT NONE
      INTEGER MAX
      REAL A(MAX,MAX), V(MAX)
      REAL X(MAX)
C
      DOUBLE PRECISION DBLE
      REAL SNGL
C
      DOUBLE PRECISION SUM
      INTEGER N, I, J
C
C-----------------------------------------------------------------------
C
      I=1
  200 SUM=0.0D0
      J=1
  201 SUM=SUM+DBLE(A(J,I))*DBLE(V(J))
      IF (J .EQ. N) GO TO 203
      J=J+1
      GO TO 201
  203 X(I)=SNGL(SUM)
      IF (I .EQ. N) RETURN                               ! Normal return
      I=I+1
      GO TO 200
      END!
C
C#######################################################################
C
      SUBROUTINE  DINVRS (A, MAX, N, IFLAG)
      IMPLICIT NONE
      INTEGER MAX
      DOUBLE PRECISION A(MAX,MAX)
C
      INTEGER I, J, K, N, IFLAG
C
C-----------------------------------------------------------------------
C
      IFLAG=0
      I=1
  300 IF(A(I,I).EQ.0.0D0)GO TO 9100
      A(I,I)=1.0D0/A(I,I)
      J=1
  301 IF(J.EQ.I)GO TO 304
      A(J,I)=-A(J,I)*A(I,I)
      K=1
  302 IF(K.EQ.I)GO TO 303
      A(J,K)=A(J,K)+A(J,I)*A(I,K)
  303 IF(K.EQ.N)GO TO 304
      K=K+1
      GO TO 302
  304 IF(J.EQ.N)GO TO 305
      J=J+1
      GO TO 301
  305 K=1
  306 IF(K.EQ.I)GO TO 307
      A(I,K)=A(I,K)*A(I,I)
  307 IF(K.EQ.N)GO TO 308
      K=K+1
      GO TO 306
  308 IF(I.EQ.N)RETURN                                   ! Normal return
      I=I+1
      GO TO 300
C
C-----------------------------------------------------------------------
C
C Error:  zero on the diagonal.
C
 9100 IFLAG=I
      RETURN
C
      END!
C
C#######################################################################
C
      SUBROUTINE  DVMUL (A, MAX, N, V, X)
      IMPLICIT NONE
      INTEGER MAX
      DOUBLE PRECISION A(MAX,MAX), V(MAX), X(MAX)
C
      DOUBLE PRECISION SUM
      INTEGER I, J, N
C
C-----------------------------------------------------------------------
C
      I=1
  200 SUM=0.0D0
      J=1
  201 SUM=SUM+A(J,I)*V(J)
      IF (J .EQ. N) GO TO 203
      J=J+1
      GO TO 201
  203 X(I)=SUM
      IF (I .EQ. N) RETURN                               ! Normal return
      I=I+1
      GO TO 200
      END!
C
C#######################################################################
C
      SUBROUTINE  QUICK (DATUM, N, INDEX)
C
C=======================================================================
C
C A quick-sorting algorithm suggested by the discussion on pages 114-119
C of THE ART OF COMPUTER PROGRAMMING, Vol. 3, SORTING AND SEARCHING, by
C D.E. Knuth, which was referenced in Don Wells' subroutine QUIK.  This
C is my own attempt at encoding a quicksort-- PBS.
C
C Arguments
C
C DATUM (INPUT/OUTPUT) is a vector of dimension N containing randomly
C        ordered real data upon input.  Upon output the elements of
C        DATUM will be in order of increasing value.
C
C
C INDEX (OUTPUT) is an integer vector of dimension N.  Upon return to
C       the calling program the i-th element of INDEX will tell where
C       the i-th element of the sorted vector DATUM had been BEFORE
C       DATUM was sorted.
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER MAXSTK, N
      PARAMETER (MAXSTK=28)
C
C Parameter
C
C MAXSTK is the maximum number of entries the stack can contain.
C         A limiting stack length of 14 restricts this quicksort
C         subroutine to vectors of maximum length of order 32,768
C         (= 2**15).

      REAL DATUM(N)
      INTEGER INDEX(N), STKLO(MAXSTK), STKHI(MAXSTK)
C
      REAL DKEY
      INTEGER I, HI, LO, NSTAK, LIMLO, LIMHI, IKEY
C
C Initialize INDEX.
C
      DO 50 I=1,N
   50 INDEX(I)=I
C
C Initialize the pointers.
C
      NSTAK=0
      LIMLO=1
      LIMHI=N
C
  100 DKEY=DATUM(LIMLO)
      IKEY=INDEX(LIMLO)
CD     TYPE *, 'LO =', LIMLO, '   HI =', LIMHI
C
C Compare all elements in the sub-vector between LIMLO and LIMHI with
C the current key datum.
C
      LO=LIMLO
      HI=LIMHI
  101 CONTINUE
C
      IF (LO .EQ. HI)GO TO 200
C
      IF (DATUM(HI) .LE. DKEY) GO TO 109
      HI=HI-1
C
C The pointer HI is to be left pointing at a datum SMALLER than the
C key, which is intended to be overwritten.
C
      GO TO 101
C
  109 DATUM(LO)=DATUM(HI)
      INDEX(LO)=INDEX(HI)
      LO=LO+1
  110 CONTINUE
C
      IF (LO .EQ. HI) GO TO 200
C
      IF (DATUM(LO) .GE. DKEY) GO TO 119
C
      LO=LO+1
      GO TO 110
C
  119 DATUM(HI)=DATUM(LO)
      INDEX(HI)=INDEX(LO)
      HI=HI-1
C
C The pointer LO is to be left pointing at a datum LARGER than the
C key, which is intended to be overwritten.
C
      GO TO 101
C
  200 CONTINUE
C
C LO and HI are equal, and point at a value which is intended to
C be overwritten.  Since all values below this point are less than
C the key and all values above this point are greater than the key,
C this is where we stick the key back into the vector.
C
      DATUM(LO)=DKEY
      INDEX(LO)=IKEY
CD     DO 1666 I=LIMLO,LO-1
CD1666 TYPE *, DATUM(I)
CD     TYPE *, DATUM(LO), ' KEY'
CD     DO 2666 I=LO+1,LIMHI
CD2666 TYPE *, DATUM(I)
C
C At this point in the subroutine, all data between LIMLO and LO-1,
C inclusive, are less than DATUM(LO), and all data between LO+1 and
C LIMHI are larger than DATUM(LO).
C
C If both subarrays contain no more than one element, then take the most
C recent interval from the stack (if the stack is empty, we're done).
C If the larger of the two subarrays contains more than one element, and
C if the shorter subarray contains one or no elements, then forget the
C shorter one and reduce the other subarray.  If the shorter subarray
C contains two or more elements, then place the larger subarray on the
C stack and process the subarray.
C
      IF (LIMHI-LO .GT. LO-LIMLO) GO TO 300
C
C Case 1:  the lower subarray is longer.  If it contains one or no
C elements then take the most recent interval from the stack and go
C back and operate on it.
C
      IF (LO-LIMLO .LE. 1) GO TO 400
C
C If the upper (shorter) subinterval contains one or no elements, then
C process the lower (longer) one, but if the upper subinterval contains
C more than one element, then place the lower (longer) subinterval on
C the stack and process the upper one.
C
      IF (LIMHI-LO .GE. 2) GO TO 250
C
C Case 1a:  the upper (shorter) subinterval contains no or one elements,
C so we go back and operate on the lower (longer) subinterval.
C
      LIMHI=LO-1
      GO TO 100
C
  250 CONTINUE
C
C Case 1b:  the upper (shorter) subinterval contains at least two
C elements, so we place the lower (longer) subinterval on the stack and
C then go back and operate on the upper subinterval.
C
      NSTAK=NSTAK+1
      STKLO(NSTAK)=LIMLO
      STKHI(NSTAK)=LO-1
      LIMLO=LO+1
CD     DO 3666 I=1,NSTAK
CD3666 TYPE *, 'STACK: ', I, STKLO(I), STKHI(I)
      GO TO 100
C
  300 CONTINUE
C
C Case 2:  the upper subarray is longer.  If it contains one or no
C elements then take the most recent interval from the stack and
C operate on it.
C
      IF (LIMHI-LO .LE. 1) GO TO 400
C
C If the lower (shorter) subinterval contains one or no elements, then
C process the upper (longer) one, but if the lower subinterval contains
C more than one element, then place the upper (longer) subinterval on
C the stack and process the lower one.
C
      IF (LO-LIMLO .GE. 2) GO TO 350
C
C Case 2a:  the lower (shorter) subinterval contains no or one elements,
C so we go back and operate on the upper (longer) subinterval.
C
      LIMLO=LO+1
      GO TO 100
C
  350 CONTINUE
C
C Case 2b:  the lower (shorter) subinterval contains at least two
C elements, so we place the upper (longer) subinterval on the stack and
C then go back and operate on the lower subinterval.
C
      NSTAK=NSTAK+1
      STKLO(NSTAK)=LO+1
      STKHI(NSTAK)=LIMHI
      LIMHI=LO-1
CD     DO 4666 I=1,NSTAK
CD4666 TYPE *, 'STACK: ', I, STKLO(I), STKHI(I)
      GO TO 100
C
  400 CONTINUE
C
C Take the most recent interval from the stack.  If the stack happens
C to be empty, we are done.
C
      IF (NSTAK .LE. 0) RETURN                           ! Normal return
CD     TYPE *, 'POP: ', NSTAK, STKLO(NSTAK), STKHI(NSTAK)
      LIMLO=STKLO(NSTAK)
      LIMHI=STKHI(NSTAK)
      NSTAK=NSTAK-1
      GO TO 100
C
      END!
C
C#######################################################################
C
      SUBROUTINE RECTFY (X, NSTAR, INDEX, HOLD)
      IMPLICIT NONE
C
      REAL X(*), HOLD(*)
      INTEGER INDEX(*)
C
      INTEGER I, NSTAR
C
      DO I=1,NSTAR
         HOLD(I)=X(I)
      END DO
      DO I=1,NSTAR
         X(I)=HOLD(INDEX(I))
      END DO
      RETURN
      END!
C
C#######################################################################
C
      REAL FUNCTION PCTILE(DATUM,N,NPCT)
C
C=======================================================================
C
C This is a modification of a quick-sorting algorithm, which is intended
C to take in a vector of numbers, and return the value of the PCT-th
C percentile in that vector:
C
C    DATUM (input real vector)     containing real data.
C    N     (input integer)         number of elements in DATUM.
C    NPCT  (input integer)         element of sorted vector whose value
C                                  is desired.
C    PCTILE (output real)          the value of the NPCT-th element
C                                  in the sorted vector DATUM.
C
C-----------------------------------------------------------------------
C
C The quick-sorting algorithm was suggested by the discussion on pages
C 114-119 of THE ART OF COMPUTER PROGRAMMING, Vol. 3, SORTING AND
C SEARCHING, by D.E. Knuth, which was referenced in Don Wells'
C subroutine QUIK.  This is my own attempt at encoding a quicksort--
C                                                             PBS.
C
C The array DATUM contains randomly ordered data.
C
      IMPLICIT NONE
      INTEGER MAXSTK
      PARAMETER (MAXSTK=25)
      REAL DATUM(*)
C
      INTEGER MIN0, MAX0
C
      REAL DKEY
      INTEGER LO, HI, N, NPCT, LIMLO, LIMHI
C
C Which element of the sorted array will we be interested in?
C
      NPCT=MAX0(1,MIN0(N,NPCT))
C
C Initialize the pointers.
C
      LIMLO=1
      LIMHI=N
C
  100 DKEY=DATUM(LIMLO)
CD     TYPE *,'LOW=',LIMLO,' HIGH=',LIMHI,' KEY=',DKEY
C
C Compare all elements in the sub-vector between LIMLO and LIMHI with
C the current key datum.
C
      LO=LIMLO
      HI=LIMHI
  101 CONTINUE
C
C If LO equals HI, we have tested all the elements in the current search
C interval.
C
      IF(LO.EQ.HI)GO TO 200
      IF(DATUM(HI).LE.DKEY)GO TO 109
      HI=HI-1
C
C The pointer HI is to be left pointing at a datum SMALLER than the
C key, which is intended to be overwritten.
C
      GO TO 101
C
  109 DATUM(LO)=DATUM(HI)
      LO=LO+1
  110 CONTINUE
      IF(LO.EQ.HI)GO TO 200
      IF(DATUM(LO).GE.DKEY)GO TO 119
      LO=LO+1
      GO TO 110
C
  119 DATUM(HI)=DATUM(LO)
      HI=HI-1
C
C The pointer LO is to be left pointing at a datum LARGER than the
C key, which is intended to be overwritten.
C
      GO TO 101
C
  200 CONTINUE
C
C LO and HI are equal, and point at a value which is intended to
C be overwritten.  Since all values below this point are less than
C the key and all values above this point are greater than the key,
C this is where we stick the key back into the vector.
C
      DATUM(LO)=DKEY
CD     DO 1666 I=LIMLO,LO-1
CD1666 TYPE *,DATUM(I)
CD     TYPE *,DATUM(LO),' KEY'
CD     DO 2666 I=LO+1,LIMHI
CD2666 TYPE *,DATUM(I)
C
C At this point in the subroutine, all data between LIMLO and LO-1,
C inclusive, are less than DATUM(LO), and all data between LO+1 and
C LIMHI are larger than DATUM(LO).  If LO = NPCT, then DATUM(LO) is
C the value we are looking for.  If NPCT < LO, then we want to sort the
C values of DATUM from LIMLO to LO-1, inclusive, whereas if NPCT > LO,
C then we want to sort the values of DATUM from LO+1 to LIMHI,
C inclusive.
C
CD     TYPE *,'NPCT=',NPCT,' LO=',LO
      IF(NPCT-LO)300,900,400
  300 LIMHI=LO-1
      GO TO 100
  400 LIMLO=LO+1
      GO TO 100
  900 PCTILE=DATUM(LO)
      RETURN
      END!
C
C#######################################################################
C
      REAL FUNCTION SMLLST (DATUM, N, M)
      IMPLICIT NONE
      REAL DATUM(*)
      INTEGER I, J, N, M, LEAST
      DO J=1,M
         SMLLST = DATUM(J)
         LEAST = J
         DO I=J,N
            IF (DATUM(I) .LT. SMLLST) THEN
               SMLLST = DATUM(I)
               LEAST = I
            END IF
         END DO
         DATUM(LEAST) = DATUM(J)
         DATUM(J) = SMLLST
      END DO
      RETURN
      END!
C
C#######################################################################
C
      REAL FUNCTION BIGGST (DATUM, N, M)
      IMPLICIT NONE
      REAL DATUM(*)
      INTEGER I, J, N, M, MOST
      DO J=N,N-M+1,-1
         MOST = J
         BIGGST = DATUM(MOST)
         DO I=1,J
            IF (DATUM(I) .GT. BIGGST) THEN
               BIGGST = DATUM(I)
               MOST = I
            END IF
         END DO
         DATUM(MOST) = DATUM(J)
         DATUM(J) = BIGGST
      END DO
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  MMM (SKY, NSKY, HIBAD, SKYMN, SKYMED,
     .     SKYMOD, SIGMA, SKEW)
C
C=======================================================================
C
C               Official DAO version:  1988 July 1
C
C This version of MMM (modified by PBS 1984.IV.10ff) assumes that
C the sky brightnesses in the one-dimensional array SKY are already
C sorted on entering this routine, and that pixels outside the "bad"
C limits have already been eliminated.
C
C This particular version of MMM also takes cognizance of the fact that,
C pixels falling below the LOBAD threshold already having been
C eliminated, the contaminated sky pixels values overwhelmingly display
C POSITIVE departures from the true value.
C
C If for some reason it is impossible to obtain the mode of the sky
C distribution, this will be flagged by setting SIGMA = -1.0.
C
C Arguments
C
C     SKY (INPUT) is a real vector containing actual sorted sky values.
C    NSKY (INPUT) is the number of defined elements in SKY.
C  SKYMOD (OUTPUT) is the estimated mode of the sky values.
C   SIGMA (OUTPUT) is the computed standard deviation of the peak in
C         the sky histogram.
C    SKEW (OUTPUT) is the computed skewness of the peak in the sky
C         histogram.
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER NSKY
      REAL SKY(NSKY)
C
      DOUBLE PRECISION DSQRT, DBLE
      REAL ALOG10, AMIN1, AMAX1
C
      DOUBLE PRECISION SUM,SUMSQ
      REAL CUT, CUT1, CUT2, DELTA, SKYMID, SKYMED, SKYMN, SKYMOD
      REAL SIGMA, SKEW, R, SIGN, X, HIBAD, CENTER, SIDE
      INTEGER I, MINIMM, MAXIMM, NITER, ISTEP, MAXIT, MINSKY, JSTEP
      LOGICAL REDO
      DATA MAXIT / 30 /, MINSKY / 20 /
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
      IF (NSKY .LE. 0) GO TO 9900
      SKYMID=0.5*(SKY((NSKY+1)/2)+SKY(NSKY/2+1))
C
C SKYMID is the median value for the whole ensemble of sky pixels.
C Notice that if NSKY is odd, then (NSKY+1)/2 and (NSKY/2)+1 will be the
C same number, whereas if NSKY is even, they will be different numbers.
C This same trick will be used again later.
C
C Initialize the variables for accumulating the mean and standard
C deviation, and initialize the rejection limits.
C
      SUM=0.D0
      SUMSQ=0.D0
      CUT1=AMIN1(SKYMID-SKY(1), SKY(NSKY)-SKYMID, HIBAD-SKYMID)
C
C For the first pass we will consider only pixels in a symmetric
C interval of brightness values about the median value.  This exploits
C the assumption that all the bad pixels are already rejected from the
C lower end of the brightness range.
C
      CUT2=SKYMID + CUT1
      CUT1=SKYMID - CUT1
C
      MINIMM=0
      DO 1010 I=1,NSKY
         IF (SKY(I) .LT. CUT1) THEN
            MINIMM=I
            GO TO 1010
         END IF
         IF (SKY(I) .GT. CUT2) GO TO 1020
         DELTA=SKY(I)-SKYMID
         SUM=SUM+DELTA
         SUMSQ=SUMSQ+DELTA**2
         MAXIMM=I
 1010 CONTINUE
C
C Henceforth in this subroutine, MINIMM will point to the highest value
C rejected at the lower end of the vector, and MAXIMM will point to the
C highest value accepted at the upper end of the vector.
C MAXIMM-MINIMM is the number of pixels within the acceptance range.
C
C Compute mean and sigma (from the first pass).
C
 1020 CONTINUE
      SKYMED=0.5*(SKY((MINIMM+MAXIMM+1)/2)+SKY((MINIMM+MAXIMM)/2+1))
      SKYMN=SUM/DBLE(MAXIMM-MINIMM)
      SIGMA=DSQRT(SUMSQ/DBLE(MAXIMM-MINIMM)-SKYMN**2)
      SKYMN=SKYMN+SKYMID
C
C The middle sky value, SKYMID, was subtracted off up above and added
C back in down here to reduce the truncation error in the computation
C of SIGMA.
C Note that this definition of SIGMA is incorrect by a factor of
C SQRT [NSKY/(NSKY-1.)], but for all but pathological cases (where none
C of this can be trusted anyway), it's close enough.
C
      SKYMOD=SKYMN
      IF (SKYMED .LT. SKYMN) SKYMOD=3.*SKYMED-2.*SKYMN
C
C If the mean is less than the mode, that means the contamination is
C slight, and the mean value is what we really want.  Note that this
C introduces a slight bias toward underestimating the sky when
C the scatter in the sky is caused by random fluctuations rather than
C by contamination, but I think this bias is negligible compared to the
C problem of contamination.
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Rejection and recomputation loop:
C
      NITER=0
 2000 NITER=NITER+1
      IF ((NITER .GT. MAXIT) .OR. (MAXIMM-MINIMM .LT. MINSKY))
     .     GO TO 9900
C
C Compute Chauvenet rejection criterion.
C
      R=ALOG10(FLOAT(MAXIMM-MINIMM))
      R=AMAX1(2., (-.1042*R+1.1695)*R+.8895)
C
C Compute rejection limits (symmetric about the current mode).
C
      CUT=R*SIGMA+0.5*ABS(SKYMN-SKYMOD)
      CUT=AMAX1(1.5,CUT)
      CUT1=SKYMOD-CUT
      CUT2=SKYMOD+CUT
C
C Recompute mean and sigma by adding and/or subtracting sky values
C at both ends of the interval of acceptable values.
C
C At each end of the interval, ISTEP will show the direction we have to
C step through the vector to go from the old partition to the new one.
C Pixels are added or subtracted depending upon whether the limit is
C moving toward or away from the mode.
C
      REDO=.FALSE.
C
C Is CUT1 above or below the minimum currently-accepted value?
C
      ISTEP=INT(SIGN(1.0001, CUT1-SKY(MINIMM+1)))
      JSTEP=(ISTEP+1)/2
C
C If ISTEP = +1, JSTEP = 1.  If ISTEP = -1, JSTEP=0.  If ISTEP = +1,
C then we know that at least one pixel must be deleted at the low end.
C
      IF (ISTEP .GT. 0) GO TO 2120
 2100 IF ((ISTEP .LT. 0) .AND. (MINIMM .LE. 0)) GO TO 2150
C
C Quit when SKY(MINIMM) < CUT1 <= SKY(MINIMM+1)
C
      IF ((SKY(MINIMM) .LE. CUT1) .AND. (SKY(MINIMM+1) .GE. CUT1))
     .     GO TO 2150
C
C If ISTEP is positive, subtract out the sky value at MINIMM+1; if
C ISTEP is negative, add in the sky value at MINIMM.
C
 2120 CONTINUE
      DELTA=SKY(MINIMM+JSTEP)-SKYMID
      SUM=SUM-REAL(ISTEP)*DELTA
      SUMSQ=SUMSQ-REAL(ISTEP)*DELTA**2
      MINIMM=MINIMM+ISTEP
      REDO=.TRUE.                                 ! A change has occured
      GO TO 2100
C
 2150 CONTINUE
C
C Is CUT2 above or below the current maximum?
C
      ISTEP=INT(SIGN(1.0001, CUT2-SKY(MAXIMM)))
      JSTEP=(ISTEP+1)/2
C
C If ISTEP = +1, JSTEP = 1.  If ISTEP = -1, JSTEP=0.  If ISTEP = -1,
C then we know that we must subtract at least one pixel from the high
C end.
C
      IF (ISTEP .LT. 0) GO TO 2220
 2200 IF ((ISTEP .GT. 0) .AND. (MAXIMM .GE. NSKY)) GO TO 2250
C
C Quit when SKY(MAXIMM) <= CUT2 < SKY(MAXIMM+1)
C
      IF ((SKY(MAXIMM) .LE. CUT2) .AND. (SKY(MAXIMM+1) .GE. CUT2))
     .     GO TO 2250
C
C If ISTEP is positive, add in the sky value at MAXIMM+1; if ISTEP is
C negative, subtract off the sky value at MAXIMM.
C
 2220 DELTA=SKY(MAXIMM+JSTEP)-SKYMID
      SUM=SUM+REAL(ISTEP)*DELTA
      SUMSQ=SUMSQ+REAL(ISTEP)*DELTA**2
      MAXIMM=MAXIMM+ISTEP
      REDO=.TRUE.                                 ! A change has occured
      GO TO 2200
C
 2250 CONTINUE
C
C Compute mean and sigma (from this pass).
C
      SKYMN=SUM/DBLE(MAXIMM-MINIMM)
      SIGMA=DSQRT(SUMSQ/DBLE(MAXIMM-MINIMM)-SKYMN**2)
      SKYMN=SKYMN+SKYMID
C
C Obtain the median.  To first approximation, the median would be the
C value of the sky in the middle pixel in the sorted data (if the
C total number is odd) or the mean of the two pixels straddling
C the middle (if the total number of pixels is even).
C
C     SKYMED=0.5*(SKY((MINIMM+MAXIMM+1)/2)+SKY((MINIMM+MAXIMM)/2+1))
C
C However, this is not good enough.  If you look at the estimator for
C the mode, you will note that a tiny change in the list of sky pixels,
C just sufficient to alter the median value of the sky brightness by
C one unit, will change the estimator of the mode by three units.  We
C really want something more robust than this.  As a first attempt
C at a more robust median estimator, I propose to estimate the median
C of the distribution by the mean of the central five percent of sky
C values.  This involves considerable care to make sure you get
C a perfectly symmetric sample of pixels about the median, whether
C there is an even or an odd number of pixels within the acceptance
C interval.
C
      SKYMED=0.0
      X=0.0
      CENTER = REAL(MINIMM+1 + MAXIMM)/2.
      SIDE = REAL(NINT(0.05*REAL(MAXIMM-MINIMM)))/2. + 0.25
C
      DO 2310 I=NINT(CENTER-SIDE),NINT(CENTER+SIDE)
      SKYMED=SKYMED+SKY(I)
 2310 X=X+1.
C
      SKYMED=SKYMED/X
      SKYMOD=SKYMN
      IF (SKYMED .LT. SKYMN) SKYMOD=3.*SKYMED-2.*SKYMN
C
C If the mean is less than the mode, that means the contamination is
C slight, and the mean value is what we really want.  Note that this
C introduces a slight bias toward underestimating the sky when
C the scatter in the sky is caused by random fluctuations rather than
C by contamination, but I think this bias is negligible compared to the
C problem of contamination.
C
C If the limits have not yet stopped moving, try again.
C
      IF (REDO) GO TO 2000
C
C-----------------------------------------------------------------------
C
C Normal return.
C
      SKEW=(SKYMN-SKYMOD)/AMAX1(1., SIGMA)
      NSKY=MAXIMM-MINIMM
      RETURN
C
C-----------------------------------------------------------------------
C
C An error condition has been detected.
C
 9900 SIGMA=-1.0
      SKEW=0.0
      RETURN
C
      END!
C
C#######################################################################
C
 	SUBROUTINE  SHOW (F, FMAX, FZERO, NX, NY, NCOL)
C
C=======================================================================
C
C A simple subroutine to use alphanumeric characters to produce a 2-D
C gray-scale plot on an alphanumeric terminal.
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER NCHAR, MAXPLT, NCOL
      PARAMETER  (NCHAR=11, MAXPLT=36)
C
C Parameters
C
C   NCHAR is the number of discrete gray levels we wish to produce.
C
C MAXPLT is the widest plot that can be produced on the terminal
C         screen.  Since two characters will be typed out per pixel (to
C         make the overall plot more nearly square) MAXPLT should be
C         equal to (N-2)/2 where N is the number of character positions
C         per line on the screen; the extra two characters will be used
C         for vertical bars ('|') to delimit the picture.  Arrays
C         which are more than MAXPLT pixels on a side will be
C         rebinned before display.
C
      REAL F(NCOL,*), FF(MAXPLT)
C
      REAL SQRT
      INTEGER MIN0
C
      CHARACTER BLANK*80, DASH*2
      CHARACTER*2 CHAR(NCHAR)
      REAL SUM, PIXELS, FMAX, FZERO, S
      INTEGER I, ISTEP, NX, MX, IX, IY, KX, JX, JY, NY, LOW
      DATA BLANK /' '/, DASH/'--'/
      DATA CHAR / '  ', '- ', '--', '::', '==', 'll',
     .     'II', '%%', '00', 'HH', '##' /
C
C-----------------------------------------------------------------------
C
      ISTEP=((NX-1)/MAXPLT)+1
C
C ISTEP is the number of pixels in each row of the input array which
C will have to be averaged for each pixel of the display.
C
      MX=(NX+ISTEP-1)/ISTEP
C
C MX is the number of pixels per row which will be produced on the
C output display.
C
      LOW=MAXPLT-MX+1
      S=SQRT(AMAX1(FLOAT(NCHAR), FMAX-FZERO))
      WRITE (6,610) BLANK(1:LOW), (DASH, I=1,MX), '+ '
  610 FORMAT (A, '+', 80A2)
C
      DO 1010 IY=1,NY,ISTEP
      KX=0
      DO 1007 IX=1,NX,ISTEP
      KX=KX+1
      PIXELS=0.0
      SUM=0.0
      DO 1005 JY=IY,MIN0(NY,IY+ISTEP-1)
      DO 1003 JX=IX,MIN0(NX,IX+ISTEP-1)
      PIXELS=PIXELS+1.0
 1003 SUM=SUM+F(JX,JY)
 1005 CONTINUE
 1007 FF(KX)=SUM/PIXELS
 1010 WRITE (6,602) BLANK(1:LOW), '|', (CHAR(MIN0(
     .     NCHAR,
     .     IFIX( NCHAR*SQRT(AMAX1(0., FF(IX)-FZERO)  )/S )+1)),
     .     IX=1,MX), '|'
  602 FORMAT ( 80A )
      WRITE (6,610) BLANK(1:LOW), (DASH, I=1,MX), '+ '
      RETURN                                             ! Normal return
C
      END!
C
C#######################################################################
C
      INTEGER FUNCTION  RDPSF  (PSFFIL, IPSTYP, PAR, MAXPAR, NPAR,
     .     PSF, MAXPSF, MAXEXP, NPSF, NEXP, NFRAC,
     .     PSFMAG, BRIGHT, XPSF, YPSF)
C
C Read in the point-spread function
C
      IMPLICIT NONE
      INTEGER MAXPSF, MAXTYP, MAXPAR, MAXEXP
      PARAMETER (MAXTYP=6)
C
      REAL PAR(MAXPAR), PSF(MAXPSF,MAXPSF,MAXEXP)
C
      CHARACTER*30 PSFFIL
      CHARACTER*8 LABEL, CHECK
      REAL PSFMAG, BRIGHT, XPSF, YPSF
      INTEGER I, J, K, IPSTYP, NPSF, NPAR, NEXP, NFRAC, ISTAT
      INTEGER NTERM, NPARAM
C
      CALL INFILE (3, PSFFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening PSF file '//PSFFIL)
         RDPSF = -1
         RETURN
      END IF
C
      READ (3,302,IOSTAT=ISTAT) LABEL, NPSF, NPAR, NEXP, NFRAC, PSFMAG,
     .     BRIGHT, XPSF, YPSF
  302 FORMAT (1X, A8, 4I5, F9.3, F15.3, 2F9.1)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error reading PSF.')
         CALL CLFILE (3)
         RDPSF = -1
         RETURN
      END IF
      DO IPSTYP=1,MAXTYP
         I = NPARAM(IPSTYP, 1., CHECK, PAR, MAXPAR)
         IF ((LABEL .EQ. CHECK) .AND. (I .EQ. NPAR)) GO TO 1100
      END DO
      CALL STUPID ('Inappropriate PSF: '//LABEL)
      CALL CLFILE (3)
      RDPSF = -1
      RETURN
C
 1100 READ (3,301,IOSTAT=ISTAT) (PAR(I), I=1,NPAR)
  301 FORMAT (1X, 6E13.5)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error reading PSF.')
         RDPSF = -1
         CALL CLFILE (3)
         RETURN
      END IF
      NTERM = NEXP+NFRAC
      IF (NTERM .GE. 1) THEN
         DO K=1,NTERM
            READ (3,311,IOSTAT=ISTAT) ((PSF(I,J,K), I=1,NPSF), J=1,NPSF)
  311       FORMAT (1X, 6E13.6)
            IF (ISTAT .NE. 0) THEN
               CALL CLFILE (3)
               RDPSF = -1
               RETURN
            END IF
         END DO
      END IF
      CALL CLFILE (3)
      RDPSF = 0
      RETURN
      END!
C
C#######################################################################
C
      REAL  FUNCTION  DAOERF (XIN, XO, BETA, DFDXO, DFDBET)
C
C Numerically integrate a Gaussian function
C
C          F = EXP {-0.5*[(x-XO)/SIGMA]**2 },
C
C from XIN-0.5 to XIN+0.5 using Gauss-Legendre integration.  BETA
C is the half-width at half-maximum, which is equal to 1.17741 * SIGMA.
C Thus,
C
C          F = EXP {-0.6931472*[(x-XO)/BETA]**2 }.
C
C Also: provide the first derivative of the integral with respect to
C Xo and BETA.  Use Gauss-Legendre integration.
C
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER MAXPT
      PARAMETER (MAXPT=4)
C
      REAL DX(MAXPT,MAXPT), WT(MAXPT,MAXPT)
C
      REAL EXP
C
      REAL X, XSQ
      REAL XIN, XO, BETA, DFDXO, DFDBET, BETASQ, DELTAX, F, WF
      INTEGER NPT, I
C
      DATA DX / 0.00000000,  0.0,        0.0       , 0.0       ,
     .         -0.28867513,  0.28867513, 0.0       , 0.0       ,
     .         -0.38729833,  0.00000000, 0.38729833, 0.0       ,
     .         -0.43056816, -0.16999052, 0.16999052, 0.43056816/
      DATA WT / 1.00000000,  0.0       , 0.0       , 0.0       ,
     .          0.50000000,  0.50000000, 0.0       , 0.0       ,
     .          0.27777778,  0.44444444, 0.27777778, 0.0       ,
     .          0.17392742,  0.32607258, 0.32607258, 0.17392742/
      DAOERF = 0.
      DFDXO = 0.
      DFDBET = 0.
      BETASQ=BETA**2
      DELTAX = XIN-XO
C
      XSQ = DELTAX**2
      F = XSQ/BETASQ
      IF (F .GT. 34.) RETURN
      F = EXP(-0.6931472*F)
      IF (F .GE. 0.046) THEN
         NPT = 4
      ELSE IF (F .GE. 0.0022) THEN
         NPT = 3
      ELSE IF (F .GE. 0.0001) THEN
         NPT = 2
      ELSE IF (F .GE. 1.E-10) THEN
         DAOERF = F
         DFDXO = 1.3862944 * DELTAX * F / BETASQ
         DFDBET = 1.3862944 * XSQ * F / (BETASQ*BETA)
         RETURN
      ELSE
         RETURN
      END IF
C
      DO I=1,NPT
         X = DELTAX + DX(I,NPT)
         XSQ = X**2
         F = EXP(-0.6931472*XSQ/BETASQ)
         WF = WT(I,NPT)*F
         DAOERF = DAOERF+WF
         DFDXO = DFDXO + X*WF
         DFDBET = DFDBET + XSQ*WF
      END DO
      DFDXO = 1.3862944*DFDXO/BETASQ
      DFDBET = 1.3862944*DFDBET/(BETASQ*BETA)
C
      RETURN
      END
C
C#######################################################################
C
      REAL  FUNCTION  USEPSF (IPSTYP, DX, DY, BRIGHT, PAR, PSF,
     .     NPSF, NPAR, NEXP, NFRAC, DELTAX, DELTAY, DVDXC, DVDYC)
C
C Evaluate the PSF for a point distant DX, DY from the center of a
C star located at relative frame coordinates DELTAX, DELTAY.
C
      IMPLICIT NONE
      INTEGER MAXPSF, MAXPAR, MAXEXP
      PARAMETER (MAXPSF=145, MAXPAR=6, MAXEXP=11)
C
      REAL PAR(*), PSF(MAXPSF,MAXPSF,*), JUNK(MAXEXP)
C
      REAL PROFIL, BICUBC
C
      REAL MIDDLE, BRIGHT, DVDXC, DVDYC, DELTAX, DELTAY, XX, YY
      REAL DX, DY, CORR, DFDX, DFDY
      INTEGER J, K, LX, LY, IPSTYP
      INTEGER NFRAC, NTERM, NPSF, NEXP, NPAR
C
      NTERM = NEXP + NFRAC
      USEPSF = BRIGHT*PROFIL(IPSTYP, DX, DY, PAR, DVDXC, DVDYC,
     .     JUNK, 0)
CD     TYPE *, USEPSF, ' ANALYTIC '
      DVDXC = BRIGHT*DVDXC
      DVDYC = BRIGHT*DVDYC
      IF (NTERM .LT. 0) RETURN
      MIDDLE = (NPSF+1)/2
C
C The PSF look-up tables are centered at (MIDDLE, MIDDLE).
C
      IF (NEXP .GE. 0) THEN
         JUNK(1) = 1.
         IF (NEXP .GE. 1) THEN
            JUNK(2) = DELTAX
            JUNK(3) = DELTAY
            IF (NEXP .GE. 2) THEN
               JUNK(4) = 1.5*DELTAX**2-0.5
               JUNK(5) = DELTAX*DELTAY
               JUNK(6) = 1.5*DELTAY**2-0.5
            END IF
         END IF
      END IF
C
      IF (NFRAC .GT. 0) THEN
         J = NEXP+1
         JUNK(J) = -2.*(DX - REAL(NINT(DX)))
         J = J+1
         JUNK(J) = -2.*(DY - REAL(NINT(DY)))
         J = J+1
         JUNK(J) = 1.5*JUNK(J-2)**2 - 0.5
         J = J+1
         JUNK(J) = JUNK(J-3)*JUNK(J-2)
         J = J+1
         JUNK(J) = 1.5*JUNK(J-3)**2 - 0.5
      END IF
      XX = 2.*DX+MIDDLE
      LX = INT(XX)
      YY = 2.*DY+MIDDLE
      LY = INT(YY)
C
C This point in the stellar profile lies between columns LX and LX+1,
C and between rows LY and LY+1 in the look-up tables.
C
      DO K=1,NTERM
         CORR = BICUBC(PSF(LX-1,LY-1,K), MAXPSF,
     .        XX-REAL(LX), YY-REAL(LY), DFDX, DFDY)
         USEPSF = USEPSF + JUNK(K)*CORR
CD        TYPE *, USEPSF, ' CORRECTED ', JUNK(K), CORR
         DVDXC = DVDXC-JUNK(K)*DFDX
         DVDYC = DVDYC-JUNK(K)*DFDY
      END DO
      RETURN
      END!
C
C#######################################################################
C
      REAL  FUNCTION  BICUBC  (F, NBOX, DX, DY, DFDX, DFDY)
C
C Perform a type of bicubic interpolation in a grid of values.
C For a point located DX, DY distant from the corner of the grid
C (defined to be 1,1), return both the interpolated value and
C its first derivatives with respect to x and y.
C
      IMPLICIT NONE
      INTEGER NBOX
      REAL F(NBOX,NBOX), TEMP(4), DFDXT(4)
C
      REAL DX, DY, DFDX, DFDY, C1, C2, C3
      INTEGER JY
C
C By construction, the point at which we want to estimate the function
C will lie between the second and third columns, and between the second
C and third rows of F, at a distance of (DX,DY) from the (2,2) element
C of F.
C
      DO JY=1,4
         C1 = 0.5*(F(3,JY)-F(1,JY))
         C2 = 3.*(F(3,JY)-F(2,JY)-C1) - 0.5*(F(4,JY)-F(2,JY)) + C1
         C3 = F(3,JY) - F(2,JY) - C1 - C2
         TEMP(JY) = DX*(DX*(DX*C3+C2)+C1)+F(2,JY)
         DFDXT(JY)= DX*(DX*C3*3.+2.*C2)+C1
      END DO
      C1 = 0.5*(TEMP(3)-TEMP(1))
      C2 = 3.*(TEMP(3)-TEMP(2)-C1) - 0.5*(TEMP(4)-TEMP(2)) + C1
      C3 = TEMP(3) - TEMP(2) - C1 - C2
      BICUBC = DY*(DY*(DY*C3+C2)+C1)+TEMP(2)
      DFDY = DY*(DY*C3*3.+2.*C2)+C1
      C1 = 0.5*(DFDXT(3)-DFDXT(1))
      C2 = 3.*(DFDXT(3)-DFDXT(2)-C1) - 0.5*(DFDXT(4)-DFDXT(2)) + C1
      C3 = DFDXT(3) - DFDXT(2) - C1 - C2
      DFDX = DY*(DY*(DY*C3+C2)+C1)+DFDXT(2)
      RETURN
      END!
C
C#######################################################################
C
      REAL  FUNCTION  PROFIL  (IPSTYP, DX, DY, PAR, DHDXC, DHDYC,
     .     TERM, IDERIV)
C
C Compute the value of an ANALYTIC prfile for a point DX,DY distant
C from the centroid.  Return both the computed value and its
C first derivatives with respect to x and y.  If IDERIV .NE. 0,
C return also the first derivatives with respect to all the parameters
C defining the profile.
C
      IMPLICIT NONE
      INTEGER MAXPAR, MAXPT
      PARAMETER (MAXPAR=6, MAXPT=4)
C
      REAL PAR(MAXPAR), TERM(MAXPAR)
      REAL D(MAXPT,MAXPT), W(MAXPT,MAXPT)
      REAL X(MAXPT), XSQ(MAXPT), P1XSQ(MAXPT)
C
      REAL EXP, DAOERF
C
      REAL DX, DY, DHDXC, DHDYC, WFSQ, Y, WT, WF, ONEMP3
      REAL RSQ, E, TALPHA, P1SQ, P2SQ, XY, DENOM
      REAL FUNC, YSQ, WP4FOD, P4FOD, F, P1P2, ERFX, DHDSX, ERFY
      REAL DEBY, DFBY, DBYX0, DBYY0
      REAL DHDSY, ALPHA, P2YSQ
      INTEGER I, IPSTYP, IDERIV, IX, IY, NPT
C
      DATA D / 0.00000000,  0.0,        0.0       , 0.0       ,
     .        -0.28867513,  0.28867513, 0.0       , 0.0       ,
     .        -0.38729833,  0.00000000, 0.38729833, 0.0       ,
     .        -0.43056816, -0.16999052, 0.16999052, 0.43056816/
      DATA W / 1.00000000,  0.0       , 0.0       , 0.0       ,
     .         0.50000000,  0.50000000, 0.0       , 0.0       ,
     .         0.27777778,  0.44444444, 0.27777778, 0.0       ,
     .         0.17392742,  0.32607258, 0.32607258, 0.17392742/
C
      PROFIL = 0.
      DHDXC = 0.
      DHDYC = 0.
C
      IF (IDERIV .GT. 0) THEN
         DO I=1,MAXPAR
            TERM(I) = 0.
         END DO
      END IF
C
      IF (IPSTYP .EQ. 1) THEN
C
C GAUSSIAN
C
C     F = ERFX * ERFY / (PAR(1) * PAR(2))
C
C PAR(1) is the HWHM in X; sigma(x) = 0.8493218 * HWHM
C PAR(2) is the HWHM in Y; ditto
C
         P1P2 = PAR(1)*PAR(2)
         ERFX = DAOERF(DX, 0., PAR(1), DHDXC, DHDSX)
         ERFY = DAOERF(DY, 0., PAR(2), DHDYC, DHDSY)
         PROFIL = ERFX*ERFY/P1P2
         DHDXC = DHDXC*ERFY/P1P2
         DHDYC = DHDYC*ERFX/P1P2
         IF (IDERIV .GT. 0) THEN
            TERM(1) = (DHDSX-ERFX/PAR(1))*ERFY/P1P2
            TERM(2) = (DHDSY-ERFY/PAR(2))*ERFX/P1P2
         END IF
      ELSE IF (IPSTYP .EQ. 4) THEN
C
C MOFFAT FUNCTION  BETA = 2.5
C                            BETA-1
C F = --------------------------------------------------------
C      Ax * Ay * [1 + (X/Ax)**2 + (Y/Ay)**2 + (XY*Axy)]**BETA
C
C PAR(1) is the HWHM in x at y = 0:
C
C             1/2 = 1/[1 + (PAR(1)/Ax)**2]**BETA
C so
C             2**(1/BETA) - 1 = (PAR(1)/Ax)**2
C
C             Ax**2 = PAR(1)**2/[2**(1/BETA) - 1]
C
C When BETA = 2.5, Ax**2 = 3.129813 * PAR(1)**2
C
C Hence, let us use
C
C                                  1
C F = ---------------------------------------------------------------
C     P(1)*P(2)*{1+0.3195079*[(X/P(1))**2+(Y/P(2))**2+(XY*P(3))]**2.5
C
C neglecting a constant of proportionality.
C
         ALPHA = 0.3195079
         TALPHA = 0.6390158                 ! 2.*ALPHA
         P1SQ = PAR(1)**2
         P2SQ = PAR(2)**2
         P1P2 = PAR(1)*PAR(2)
         XY = DX*DY
C
         DENOM = 1. + ALPHA*(DX**2/P1SQ + DY**2/P2SQ + XY*PAR(3))
         IF (DENOM .GT. 1.E4) RETURN
         FUNC = 1. / (P1P2 * DENOM**PAR(4))
         IF (FUNC .GE. 0.046) THEN
            NPT = 4
         ELSE IF (FUNC .GE. 0.0022) THEN
            NPT = 3
         ELSE IF (FUNC .GE. 0.0001) THEN
            NPT = 2
         ELSE IF (FUNC .GE. 1.E-10) THEN
            PROFIL = (PAR(4) - 1.) * FUNC
            P4FOD = PAR(4)*ALPHA*PROFIL/DENOM
            DHDXC = P4FOD*(2.*DX/P1SQ + DY*PAR(3))
            DHDYC = P4FOD*(2.*DY/P2SQ + DX*PAR(3))
            IF (IDERIV .GT. 0) THEN
               TERM(1) = (2.*P4FOD*DX**2/P1SQ-PROFIL)/PAR(1)
               TERM(2) = (2.*P4FOD*DY**2/P2SQ-PROFIL)/PAR(2)
               TERM(3) = - P4FOD*XY
C              TERM(4) = PROFIL*(1./(PAR(4)-1.)-ALOG(DENOM))
            END IF
            RETURN
         ELSE
            RETURN
         END IF
C
         DO IX=1,NPT
            X(IX) = DX+D(IX,NPT)
            XSQ(IX) = X(IX)**2
            P1XSQ(IX) = XSQ(IX)/P1SQ
         END DO
C
         DO IY=1,NPT
            Y = DY+D(IY,NPT)
            YSQ = Y**2
            P2YSQ = YSQ/P2SQ
            DO IX=1,NPT
               WT = W(IY,NPT)*W(IX,NPT)
               XY=X(IX)*Y
               DENOM = 1. + ALPHA*(P1XSQ(IX) + P2YSQ + XY*PAR(3))
               FUNC = (PAR(4) - 1.) / (P1P2 * DENOM**PAR(4))
               P4FOD = PAR(4)*ALPHA*FUNC/DENOM
               WP4FOD = WT*P4FOD
               WF = WT*FUNC
               PROFIL = PROFIL + WF
               DHDXC = DHDXC + WP4FOD*(2.*X(IX)/P1SQ + Y*PAR(3))
               DHDYC = DHDYC + WP4FOD*(2.*Y/P2SQ + X(IX)*PAR(3))
               IF (IDERIV .GT. 0) THEN
                  TERM(1) = TERM(1) +
     .                     (2.*WP4FOD*P1XSQ(IX)-WF)/PAR(1)
                  TERM(2) = TERM(2) +
     .                     (2.*WP4FOD*P2YSQ-WF)/PAR(2)
                  TERM(3) = TERM(3) - WP4FOD*XY
C                 TERM(4) = TERM(4) + WF*(1./(PAR(4)-1.)-ALOG(DENOM))
               END IF
            END DO
         END DO
      ELSE IF (IPSTYP .EQ. 5) THEN
C
C Penny function --- Gaussian core plus Lorentzian wings.  The Lorentzian
C is elongated along the x or y axis, the Gaussian may be tilted.
C
         P1SQ = PAR(1)**2
         P2SQ = PAR(2)**2
         ONEMP3 = 1.-PAR(3)
         XY = DX*DY
C
         RSQ = DX**2/P1SQ + DY**2/P2SQ
         IF (RSQ .GT. 1.E10) RETURN
C
         F = 1./(1.+RSQ)
         RSQ = RSQ + XY*PAR(4)
         IF (RSQ .LT. 34.) THEN
            E = EXP(-0.6931472*RSQ)
            FUNC = PAR(3)*E + ONEMP3*F
         ELSE
            E = 0.
            FUNC = ONEMP3*F
         END IF
C
         IF (FUNC .GE. 0.046) THEN
            NPT = 4
         ELSE IF (FUNC .GE. 0.0022) THEN
            NPT = 3
         ELSE IF (FUNC .GE. 0.0001) THEN
            NPT = 2
         ELSE IF (FUNC .GE. 1.E-10) THEN
            PROFIL = FUNC
            DFBY = ONEMP3*F**2
            DEBY = 0.6931472*PAR(3)*E
            DBYX0 = 2.*DX/P1SQ
            DBYY0 = 2.*DY/P2SQ
            DHDXC = DEBY*(DBYX0 + DY*PAR(4)) + DFBY*DBYX0
            DHDYC = DEBY*(DBYY0 + DX*PAR(4)) + DFBY*DBYY0
            IF (IDERIV .GT. 0) THEN
               DBYX0 = DBYX0*DX/PAR(1)
               DBYY0 = DBYY0*DY/PAR(2)
               DFBY = DFBY + DEBY
               TERM(1) = DFBY * DBYX0
               TERM(2) = DFBY * DBYY0
               TERM(3) = E - F
               TERM(4) = - DEBY * XY
            END IF
            RETURN
         ELSE
            RETURN
         END IF
C
         DO IX=1,NPT
            X(IX) = DX+D(IX,NPT)
            P1XSQ(IX) = X(IX)/P1SQ
         END DO
C
         DO IY=1,NPT
            Y = DY+D(IY,NPT)
            P2YSQ = Y/P2SQ
            DO IX=1,NPT
               WT = W(IY,NPT)*W(IX,NPT)
               XY = X(IX)*Y
               RSQ = P1XSQ(IX)*X(IX) + P2YSQ*Y
               F = 1./(1.+RSQ)
               RSQ = RSQ + XY*PAR(4)
               IF (RSQ .LT. 34.) THEN
                  E = EXP(-0.6931472*RSQ)
                  FUNC = PAR(3)*E + ONEMP3*F
                  DEBY = 0.6931472*WT*PAR(3)*E
               ELSE
                  E = 0.
                  FUNC = ONEMP3*F
                  DEBY = 0.
               END IF
               PROFIL = PROFIL + WT*FUNC
               DFBY = WT*ONEMP3*F**2
               DBYX0 = 2.*P1XSQ(IX)
               DBYY0 = 2.*P2YSQ
               DHDXC = DHDXC +
     .              DEBY*(DBYX0 + DY*PAR(4)) + DFBY*DBYX0
               DHDYC = DHDYC +
     .              DEBY*(DBYY0 + DX*PAR(4)) + DFBY*DBYY0
               IF (IDERIV .GT. 0) THEN
                  DBYX0 = DBYX0*DX/PAR(1)
                  DBYY0 = DBYY0*DY/PAR(2)
                  TERM(1) = TERM(1) + (DFBY+DEBY)*DBYX0
                  TERM(2) = TERM(2) + (DFBY+DEBY)*DBYY0
                  TERM(3) = TERM(3) + WT*(E-F)
                  TERM(4) = TERM(4) - DEBY * XY
               END IF
            END DO
         END DO
      ELSE IF (IPSTYP .EQ. 6) THEN
C
C Penny function --- Gaussian core plus Lorentzian wings.
C The Lorentzian and Gaussian may be tilted in different
C directions.
C
         P1SQ = PAR(1)**2
         P2SQ = PAR(2)**2
         ONEMP3 = 1.-PAR(3)
         XY = DX*DY
C
         RSQ = DX**2/P1SQ + DY**2/P2SQ
         DFBY = RSQ + PAR(5)*XY
         IF (DFBY .GT. 1.E10) RETURN
         F = 1./(1.+DFBY)
C
         DEBY = RSQ + PAR(4)*XY
         IF (DEBY .LT. 34.) THEN
            E = EXP(-0.6931472*DEBY)
         ELSE
            E = 0.
         END IF
C
         FUNC = PAR(3)*E + ONEMP3*F
         IF (FUNC .GE. 0.046) THEN
            NPT = 4
         ELSE IF (FUNC .GE. 0.0022) THEN
            NPT = 3
         ELSE IF (FUNC .GE. 0.0001) THEN
            NPT = 2
         ELSE IF (FUNC .GE. 1.E-10) THEN
            PROFIL = FUNC
            DFBY = ONEMP3*F**2
            DEBY = 0.6931472*PAR(3)*E
            DBYX0 = 2.*DX/P1SQ
            DBYY0 = 2.*DY/P2SQ
            DHDXC = DEBY*(DBYX0 + DY*PAR(4)) +
     .              DFBY*(DBYX0 + DY*PAR(5))
            DHDYC = DEBY*(DBYY0 + DX*PAR(4)) +
     .              DFBY*(DBYY0 + DX*PAR(5))
            IF (IDERIV .GT. 0) THEN
               DBYX0 = DBYX0*DX/PAR(1)
               DBYY0 = DBYY0*DY/PAR(2)
               TERM(5) = -DFBY * XY
               DFBY = DFBY + DEBY
               TERM(1) = DFBY * DBYX0
               TERM(2) = DFBY * DBYY0
               TERM(3) = E - F
               TERM(4) = - DEBY * XY
            END IF
            RETURN
         ELSE
            RETURN
         END IF
C
         DO IX=1,NPT
            X(IX) = DX+D(IX,NPT)
            P1XSQ(IX) = X(IX)/P1SQ
         END DO
C
         DO IY=1,NPT
            Y = DY+D(IY,NPT)
            P2YSQ = Y/P2SQ
            DO IX=1,NPT
               WT = W(IY,NPT)*W(IX,NPT)
               XY = X(IX)*Y
               RSQ = P1XSQ(IX)*X(IX) + P2YSQ*Y
               F = 1./(1.+RSQ+PAR(5)*XY)
               DEBY = RSQ + PAR(4)*XY
               IF (DEBY .LT. 34.) THEN
                  E = EXP(-0.6931472*DEBY)
                  FUNC = PAR(3)*E + ONEMP3*F
                  DEBY = 0.6931472*WT*PAR(3)*E
               ELSE
                  E = 0.
                  FUNC = ONEMP3*F
                  DEBY = 0.
               END IF
               PROFIL = PROFIL + WT*FUNC
               DFBY = WT*ONEMP3*F**2
               DBYX0 = 2.*P1XSQ(IX)
               DBYY0 = 2.*P2YSQ
               DHDXC = DHDXC +
     .              DEBY*(DBYX0 + DY*PAR(4)) +
     .              DFBY*(DBYX0 + DY*PAR(5))
               DHDYC = DHDYC +
     .              DEBY*(DBYY0 + DX*PAR(4)) +
     .              DFBY*(DBYY0 + DX*PAR(5))
               IF (IDERIV .GT. 0) THEN
                  DBYX0 = DBYX0*DX/PAR(1)
                  DBYY0 = DBYY0*DY/PAR(2)
                  TERM(1) = TERM(1) + (DFBY+DEBY)*DBYX0
                  TERM(2) = TERM(2) + (DFBY+DEBY)*DBYY0
                  TERM(3) = TERM(3) + WT*(E-F)
                  TERM(4) = TERM(4) - DEBY * XY
                  TERM(5) = TERM(5) - DFBY * XY
               END IF
            END DO
         END DO
      ELSE IF (IPSTYP .EQ. 3) THEN
C
C MOFFAT FUNCTION   BETA = 1.5
C
C                            BETA-1
C F = --------------------------------------------------------
C      Ax * Ay * [1 + (X/Ax)**2 + (Y/Ay)**2 + (XY*Axy)]**BETA
C
C PAR(1) is the HWHM in x at y = 0:
C
C             1/2 = 1/[1 + (PAR(1)/Ax)**2]**BETA
C so
C             2**(1/BETA) - 1 = (PAR(1)/Ax)**2
C
C             Ax**2 = PAR(1)**2/[2**(1/BETA) - 1]
C
C When BETA = 1.5, Ax**2 = 1.7024144 * PAR(1)**2
C
C Hence, let us use
C
C                                  1
C F = ---------------------------------------------------------------
C     P(1)*P(2)*{1+0.5874011*[(X/P(1))**2+(Y/P(2))**2+(XY*P(3))]**1.5
C
C neglecting a constant of proportionality.
C
         ALPHA = 0.5874011
         TALPHA = 1.1748021  ! 2.*ALPHA
         P1SQ = PAR(1)**2
         P2SQ = PAR(2)**2
         P1P2 = PAR(1)*PAR(2)
         XY = DX*DY
C
         DENOM = 1. + ALPHA*(DX**2/P1SQ + DY**2/P2SQ + XY*PAR(3))
         IF (DENOM .GT. 5.E6) RETURN
         FUNC = 1. / (P1P2 * DENOM**PAR(4))
         IF (FUNC .GE. 0.046) THEN
            NPT = 4
         ELSE IF (FUNC .GE. 0.0022) THEN
            NPT = 3
         ELSE IF (FUNC .GE. 0.0001) THEN
            NPT = 2
         ELSE IF (FUNC .GE. 1.E-10) THEN
            PROFIL = (PAR(4) - 1.) * FUNC
            P4FOD = PAR(4)*ALPHA*PROFIL/DENOM
            DHDXC = P4FOD*(2.*DX/P1SQ + DY*PAR(3))
            DHDYC = P4FOD*(2.*DY/P2SQ + DX*PAR(3))
            IF (IDERIV .GT. 0) THEN
               TERM(1) = (2.*P4FOD*DX**2/P1SQ-PROFIL)/PAR(1)
               TERM(2) = (2.*P4FOD*DY**2/P2SQ-PROFIL)/PAR(2)
               TERM(3) = - P4FOD*XY
C              TERM(4) = PROFIL*(1./(PAR(4)-1.)-ALOG(DENOM))
            END IF
            RETURN
         ELSE
            RETURN
         END IF
C
         DO IX=1,NPT
            X(IX) = DX+D(IX,NPT)
            XSQ(IX) = X(IX)**2
            P1XSQ(IX) = XSQ(IX)/P1SQ
         END DO
C
         DO IY=1,NPT
            Y = DY+D(IY,NPT)
            YSQ = Y**2
            P2YSQ = YSQ/P2SQ
            DO IX=1,NPT
               WT = W(IY,NPT)*W(IX,NPT)
               XY=X(IX)*Y
               DENOM = 1. + ALPHA*(P1XSQ(IX) + P2YSQ + XY*PAR(3))
               FUNC = (PAR(4) - 1.)/ (P1P2 * DENOM**PAR(4))
               P4FOD = PAR(4)*ALPHA*FUNC/DENOM
               WP4FOD = WT*P4FOD
               WF = WT*FUNC
               PROFIL = PROFIL + WF
               DHDXC = DHDXC + WP4FOD*(2.*X(IX)/P1SQ + Y*PAR(3))
               DHDYC = DHDYC + WP4FOD*(2.*Y/P2SQ + X(IX)*PAR(3))
               IF (IDERIV .GT. 0) THEN
                  TERM(1) = TERM(1) +
     .                     (2.*WP4FOD*P1XSQ(IX)-WF)/PAR(1)
                  TERM(2) = TERM(2) +
     .                     (2.*WP4FOD*P2YSQ-WF)/PAR(2)
                  TERM(3) = TERM(3) - WP4FOD*XY
C                 TERM(4) = TERM(4) + WF*(1./(PAR(4)-1.)-ALOG(DENOM))
               END IF
            END DO
         END DO
      ELSE IF (IPSTYP .EQ. 2) THEN
C
C LORENTZ FUNCTION
C                      1
C F = --------------------------------------
C     [1 + (X/Ax)**2 + (Y/Ay)**2 + (XY*Axy)]
C
C PAR(1) is the HWHM in x at y = 0.
C
         P1SQ = PAR(1)**2
         P2SQ = PAR(2)**2
         P1P2 = PAR(1)*PAR(2)
         XY = DX*DY
C
         DENOM = 1. + P1XSQ(IX) + P2YSQ + XY*PAR(3)
         IF (DENOM .GT. 1.E10) RETURN
         FUNC = 1. / DENOM
         IF (FUNC .GE. 0.046) THEN
            NPT = 4
         ELSE IF (FUNC .GE. 0.0022) THEN
            NPT = 3
         ELSE IF (FUNC .GE. 0.0001) THEN
            NPT = 2
         ELSE IF (FUNC .GE. 1.E-10) THEN
            PROFIL = FUNC
            WFSQ = FUNC**2
            DHDXC = WFSQ*(2.*DX/P1SQ + DY*PAR(3))
            DHDYC = WFSQ*(2.*DY/P2SQ + DX*PAR(3))
            IF (IDERIV .GT. 0) THEN
               TERM(1) = WFSQ*(2.*DX**2/P1SQ)/PAR(1)
               TERM(2) = WFSQ*(2.*DY**2/P2SQ)/PAR(2)
               TERM(3) = - WFSQ*XY
            END IF
            RETURN
         ELSE
            RETURN
         END IF
C
         DO IX=1,NPT
            X(IX) = DX+D(IX,NPT)
            XSQ(IX) = X(IX)**2
            P1XSQ(IX) = XSQ(IX)/P1SQ
         END DO
C
         DO IY=1,NPT
            Y = DY+D(IY,NPT)
            YSQ = Y**2
            P2YSQ = YSQ/P2SQ
            DO IX=1,NPT
               WT = W(IY,NPT)*W(IX,NPT)
               XY=X(IX)*Y
               DENOM = 1. + P1XSQ(IX) + P2YSQ + XY*PAR(3)
               FUNC = 1. / DENOM
               WF = WT*FUNC
               WFSQ = WF*FUNC
               PROFIL = PROFIL + WF
               DHDXC = DHDXC + WFSQ*(2.*X(IX)/P1SQ + Y*PAR(3))
               DHDYC = DHDYC + WFSQ*(2.*Y/P2SQ + X(IX)*PAR(3))
               IF (IDERIV .GT. 0) THEN
                  TERM(1) = TERM(1) + WFSQ*(2.*P1XSQ(IX))/PAR(1)
                  TERM(2) = TERM(2) + WFSQ*(2.*P2YSQ)/PAR(2)
                  TERM(3) = TERM(3) - WFSQ*XY
               END IF
            END DO
         END DO
      ELSE
         CALL STUPID ('Invalid PSF type.')
         CALL BYEBYE
      END IF
      RETURN
      END!
C
C=======================================================================
C
      FUNCTION NPARAM  (IPSTYP, FWHM, LABEL, PAR, MAXPAR)
      CHARACTER*8 LABEL
      INTEGER MAXPAR
      REAL PAR(MAXPAR)
      PAR(1) = FWHM/2.
      PAR(2) = PAR(1)
      IF (IPSTYP .EQ. 1) THEN
         NPARAM = 2
         LABEL = 'GAUSSIAN'
      ELSE IF (IPSTYP .EQ. 4) THEN
         NPARAM = 3
         PAR(3) = 0.
         PAR(4) = 2.5
         LABEL = 'MOFFAT25'
      ELSE IF (IPSTYP .EQ. 5) THEN
         NPARAM = 4
         PAR(3) = 0.75
         PAR(4) = 0.0
         LABEL = 'PENNY1  '
      ELSE IF (IPSTYP .EQ. 6) THEN
         NPARAM = 5
         PAR(3) = 0.75
         PAR(4) = 0.0
         PAR(5) = 0.0
         LABEL = 'PENNY2  '
      ELSE IF (IPSTYP .EQ. 3) THEN
         NPARAM = 3
         PAR(3) = 0.
         PAR(4) = 1.5
         LABEL = 'MOFFAT15'
      ELSE IF (IPSTYP .EQ. 2) THEN
         NPARAM = 3
         PAR(3) = 0.
         LABEL = 'LORENTZ '
      ELSE
         CALL STUPID ('Invalid PSF type: '//CHAR(IPSTYP+48))
      END IF
      RETURN
      END!
