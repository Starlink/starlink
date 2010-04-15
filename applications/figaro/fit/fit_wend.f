C+
      SUBROUTINE FIT_WEND (STATUS)
C
C     F I T _ W E N D
C
C     Terminates a FITS header, by writing an END record and filling
C     up the rest of the FITS buffer with blanks.
C
C     Parameters -    (">" input, "!" modified, "<" output)
C
C     (<) STATUS     (Integer) Return code.  0 => OK.  Non-zero
C                    values will be error codes caused by a tape
C                    I/O error.  These can be decoded by FIT_ERROR.
C
C     Common variables used -
C
C     (<) BUFF       (Character) Buffer for one header line
C     (>) FPTR       (Integer) Pointer to the main FITS buffer FBUFF.
C
C     BUFF is defined in COMB.INC, FPTR in COMF.INC
C
C     Subroutines / functions used -
C
C     FIT_BUFFO      (FIT_ package) Outputs BUFF to main I/O buffer
C
C                                      KS / CIT 9th Oct 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
C
C     Common blocks
C
      INCLUDE 'COMB'
      INCLUDE 'COMF'
C
C     Write the end line into the header
C
      BUFF='END'
      CALL FIT_BUFFO(STATUS)
C
C     Keep writing blank lines into the header until the pointer
C     is reset - this indicates that the buffer has been output.
C
      BUFF=' '
      DO WHILE ((FPTR.GT.1).AND.(STATUS.EQ.0))
         CALL FIT_BUFFO(STATUS)
      END DO
C
      END
