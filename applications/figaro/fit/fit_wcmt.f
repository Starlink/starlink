C+
      SUBROUTINE FIT_WCMT (NAME,COMMENT,STATUS)
C
C     F I T _ W C M T
C
C     Writes a comment into a FITS header.  Assumes
C     FIT_INIT has been called to initialise the header routines.
C     The FITS standard allows comments to be given the keywords
C     COMMENT, or HISTORY, or blank.  NAME should be passed as one
C     of these, but this is not tested by this routine.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NAME      (Character) The name of the comment (see above).
C     (>) COMMENT   (Character) Any comment to be associated with the
C                   keyword.  Should be no longer than 70 characters.
C     (>) STATUS    (Integer) Return status code. 0 => OK, non-zero
C                   values will be error codes caused by a tape
C                   I/O error.  These can be decoded by FIT_ERROR.
C
C     Note: The FITS standard says keyword names and values must be
C     output in upper case.  NAME can be passed in lower case, but
C     it will be output in upper.  COMMENT will be output as passed.
C
C     Common variables used -
C
C     (<) BUFF      (Character) Buffer for one header line
C
C     Defined in the file COMB.INC
C
C     Functions / subroutines used -
C
C     FIT_BUFFO     (FIT_ package) Outputs BUFF to main I/O buffer.
C     ICH_FOLD      (ICH_    "   ) Convert string to upper case.
C
C                                                KS / CIT  9th OCT 1983
C     Modified:
C
C     28th Jul 1993.  HME / UoE, Starlink.  Disuse STR$UPCASE.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER STATUS
      CHARACTER*(*) NAME,COMMENT
C
C     Common blocks
C
      INCLUDE 'COMB'
C
C     Functions and local variables
C
      INTEGER ICH_FOLD
      INTEGER INVOKE
C
      BUFF=' '
      BUFF(1:8)=NAME
      INVOKE=ICH_FOLD(BUFF(1:8))
      BUFF(11:)=COMMENT
      CALL FIT_BUFFO(STATUS)
C
      END
