C*PGRND -- find the smallest `round' number greater than x
C%float cpgrnd(float x, int *nsub);
C+
      REAL FUNCTION PGRND (X, NSUB)
      REAL X
      INTEGER NSUB
C
C Routine to find the smallest "round" number larger than x, a
C "round" number being 1, 2 or 5 times a power of 10. If X is negative,
C PGRND(X) = -PGRND(ABS(X)). eg PGRND(8.7) = 10.0,
C PGRND(-0.4) = -0.5.  If X is zero, the value returned is zero.
C This routine is used by PGBOX for choosing  tick intervals.
C
C Returns:
C  PGRND         : the "round" number.
C Arguments:
C  X      (input)  : the number to be rounded.
C  NSUB   (output) : a suitable number of subdivisions for
C                    subdividing the "nice" number: 2 or 5.
C--
C  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
C  2-Dec-1991 - Fix for bug found on Fujitsu [TJP].
C-----------------------------------------------------------------------
      INTEGER  I,ILOG
      REAL     FRAC,NICE(3),PWR,XLOG,XX
      INTRINSIC ABS, LOG10, SIGN
      DATA     NICE/2.0,5.0,10.0/
C
      IF (X.EQ.0.0) THEN
          PGRND = 0.0
          NSUB = 2
          RETURN
      END IF
      XX   = ABS(X)
      XLOG = LOG10(XX)
      ILOG = XLOG
      IF (XLOG.LT.0) ILOG=ILOG-1
      PWR  = 10.0**ILOG
      FRAC = XX/PWR
      I = 3
      IF (FRAC.LE.NICE(2)) I = 2
      IF (FRAC.LE.NICE(1)) I = 1
      PGRND = SIGN(PWR*NICE(I),X)
      NSUB = 5
      IF (I.EQ.1) NSUB = 2
      END
