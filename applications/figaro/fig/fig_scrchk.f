C+
      LOGICAL FUNCTION FIG_SCRCHK (NX,XDATA)
C
C     F I G _ S C R C H K
C
C     Figaro subroutine that checks whether a set of X values
C     represent a 'scrunched' - ie a linear - wavelength scale.
C
C     Parameters - (">" input, "<" output)
C
C     (>) NX     (Integer) Number of elements in the X array.
C     (>) XDATA  (Real array XDATA(NX)) X data elements.
C
C     Returns -
C
C     FIG_SCRCHK (Logical) True if data is on a linear scale
C                - defined for these purposes as the difference
C                between successive elements being constant to
C                some limit calculated roughly to allow for the
C                real number precision of the machine.
C
C     Common variables used - None
C
C     Functions / subroutines used - None
C
C                                      KS / CIT 16th July 1984
C     Modified:
C
C     11th Aug 1986.  KS / AAO.  Algorithm modified to work when an
C                     array value is zero.
C     14th April 1987 KS / AAO.  Algorithm further modified to work
C                     better when an array starts with very small values.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX
      REAL    XDATA(NX)
C
C     Local variables
C
      INTEGER IX
      REAL DIFF1, DIFLIM, XIS, XWAS
C
      FIG_SCRCHK=.FALSE.
      DIFF1=XDATA(2)-XDATA(1)
      XWAS=XDATA(2)
      DO IX=3,NX
         XIS=XDATA(IX)
         DIFLIM=0.00001*MAX(ABS(XIS),ABS(XWAS))
         IF (ABS(XIS-XWAS-DIFF1).GT.DIFLIM) GO TO 300
         XWAS=XIS
      END DO
      FIG_SCRCHK=.TRUE.
  300 CONTINUE
C
      END
