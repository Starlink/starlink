C+
      SUBROUTINE FIG_ABROWS (DATA,NX,NY,FACTOR,PASSES,MAXROWS,ARRAYS,
     :                                                    NBROW,BROWS)
C
C     F I G _ A B R O W S
C
C     Automatic search for Bad Rows in a CCD image.
C
C     This algorithm works by examining the 1D array that results from
C     collapsing the data along the rows.  Each row in the image then
C     becomes a single pixel in this array, and a bad row should show
C     up as a pixel significantly lower than its neighboure.  The
C     criteria used by the program are that the differences between a
C     'bad' pixel and both its neighbours will be both a) positive (ie
C     the bad pixel will be low) and b) greater by a specified factor
C     than the median difference between successive pixels over a range
C     surrounding the test pixel.   To make the test more stringent,
C     the test is repeated a number of times, each time on an array
C     formed by taking every nth column of each row, rather than the
C     whole row.  A pixel has to show up as significantly low each
C     time to be considered to represent a bad row.
C
C     Parameters -  (">" input, "<" output, "W" workspace)
C
C     (>) DATA       (Real array DATA(NX,NY)) The image data
C     (>) NX         (Integer) The number of columns in DATA
C     (>) NY         (Integer) The number of rows in DATA.
C     (>) FACTOR     (Real) The factor by which the median difference
C                    must be exceeded for a bad pixel.
C     (>) PASSES     (Integer) The number of times the test is to
C                    be repeated.  (The test generally seems to work
C                    best with a low-ish value for FACTOR, say 1.5, and
C                    a high-ish value for PASSES, say 4).
C     (>) MAXROWS    (Integer) The number of bad rows that can be found.
C     (W) ARRAYS     (Real array ARRAYS(NY,PASSES)) Workspace.
C     (<) NBROW      (Integer) The number of bad rows found.  If NBROW
C                    exceeds MAXROWS, then too many bad rows were found.
C     (<) BROWS      (Integer array BROWS(MAXROWS)) The row numbers
C                    of the bad rows found.
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     FIG_QBROW      (FIG_ package) Test a candidate low pixel in an array
C
C
C                                           KS / CIT 26th Feb 1984
C
C     Modifications:
C     27-04-1999: Tim Ash (Starlink, RAL): Corrected mistake in
C     initialisation of ARRAYS, and initialised NBROW to zero, to
C     fix bugs encountered when running under Linux.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,PASSES,MAXROWS,NBROW,BROWS(MAXROWS)
      REAL DATA(NX,NY),ARRAYS(NY,PASSES),FACTOR
C
C     Functions
C
      LOGICAL FIG_QBROW
      EXTERNAL FIG_QBROW
C
C     Local variables
C
      LOGICAL CANDID
      INTEGER IP,IX,IY
C
C     Fill up the pass arrays
C
      DO IP=1,PASSES
         DO IY=1,NY
            ARRAYS(IY,IP)=0.
         END DO
      END DO
      DO IY=1,NY
         IP=1
         DO IX=1,NX
            ARRAYS(IY,IP)=ARRAYS(IY,IP)+DATA(IX,IY)
            IP=IP+1
            IF (IP.GT.PASSES) IP=1
         END DO
      END DO
C
C     Look for any candidates that appear in all the pass arrays
C
      NBROW=0
      DO IY=2,NY-1
         IP=1
         CANDID=.TRUE.
         DO WHILE (CANDID.AND.(IP.LE.PASSES))
            CANDID=FIG_QBROW(ARRAYS(1,IP),NY,IY,FACTOR)
            IP=IP+1
         END DO
         IF (CANDID) THEN
            NBROW=NBROW+1
            IF (NBROW.GT.MAXROWS) GO TO 600
            BROWS(NBROW)=IY
         END IF
      END DO
C
  600 CONTINUE
      END





