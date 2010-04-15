C+
      SUBROUTINE FIT_BSET (BLOCK)
C
C     F I T _ B S E T
C
C     If data is being written to tape, this can be called after
C     FIT_INIT - but before any other FIT_ routine - to set the
C     tape blocking factor to be used.  This can be any integer
C     from 1 to 10.  Higher values provide more efficient use
C     of high density tapes, particularly 6250 bpi, but note that
C     the original FITS standard only allowed a blocking factor of
C     1, so larger numbers may trouble older reading programs.
C     If an invalid number is supplied, the closest valid value
C     will be used.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) BLOCK    (Integer) The blocking factor to be used.  This
C                  should be in the range 1 to 10 inclusive.
C
C     Common variables used -
C
C     (>) MAXTBL   (Integer) Is the maximum allowed value for TBLOCK
C                  (fixed parameter).
C     (<) TBLOCK   (Integer) Is the current blocking factor - number of
C                  logical records in each actual tape record.
C
C     Both defined in the file COMF.INC.
C
C     Subroutines / functions used - None.
C
C                                               KS / AAO  30th Jan 1990
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER BLOCK
C
C     Common blocks
C
      INCLUDE 'COMF'
C
C     Set TBLOCK to the nearest legal value.
C
      TBLOCK=MIN(MAXTBL,MAX(1,BLOCK))
C
      END
