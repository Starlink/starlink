C+
      SUBROUTINE FIT_WLOG (NAME,VALUE,COMMENT,STATUS)
C
C     F I T _ W L O G
C
C     Writes a logical keyword into a FITS header.  Assumes
C     FIT_INIT has been called to initialise the header routines.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NAME      (Character) The name of the keyword.
C     (>) VALUE     (Logical) The value of the parameter.
C     (>) COMMENT   (Character) Any comment to be associated with the
C                   keyword.  Should be no longer than 46 characters.
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
C     FIT_BUFP      ( "     "    ) Copies NAME and COMMENTS into BUFF.
C
C                                                KS / CIT  9th OCT 1983
C+
      IMPLICIT NONE
C
C     Parameters -
C
      LOGICAL VALUE
      INTEGER STATUS
      CHARACTER*(*) NAME,COMMENT
C
C     Common blocks
C
      INCLUDE 'COMB'
C
      CALL FIT_BUFP(NAME,COMMENT)
      IF (VALUE) THEN
         BUFF(30:30)='T'
      ELSE
         BUFF(30:30)='F'
      END IF
      CALL FIT_BUFFO(STATUS)
C
      END
