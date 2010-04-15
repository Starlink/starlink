C+
      SUBROUTINE FIT_WSTR (NAME,STRING,COMMENT,STATUS)
C
C     F I T _ W S T R
C
C     Writes a string keyword into a FITS header.  Assumes
C     FIT_INIT has been called to initialise the header routines.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NAME      (Character) The name of the keyword.
C     (>) STRING    (Character) The value of the keyword - this routine
C                   appends the quote marks required by the standard, so
C                   these should not be included in STRING.
C     (>) COMMENT   (Character) Any comment to be associated with the
C                   keyword.  Normally the comment is started at col 34
C                   and so should be limited to 46 characters.  If the
C                   keyword itself runs over past col 30, the comment
C                   will have to be shorter.
C     (>) STATUS    (Integer) Return status code. 0 => OK, non-zero
C                   values will be error codes caused by a tape
C                   I/O error.  These can be decoded by FIT_ERROR.
C
C     Note: The FITS standard says keyword names and values must be
C     output in upper case.  NAME & STRING can be passed in lower case,
C     but will be output in upper.  COMMENT will be output as passed.
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
C                                                KS / CIT  15th Nov 1983
C     Modified:
C
C     28th Jul 1993.  HME / UoE, Starlink.  Disuse STR$UPCASE.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER STATUS
      CHARACTER*(*) NAME,COMMENT,STRING
C
C     Functions
C
      INTEGER ICH_FOLD
C
C     Local variables
C
      INTEGER INVOKE
      INTEGER IBUF,ISTR,LPTR
      CHARACTER CHR
C
C     Common blocks
C
      INCLUDE 'COMB'
C
      BUFF=' '
      BUFF(1:8)=NAME
      INVOKE=ICH_FOLD(BUFF(1:8))
      BUFF(9:11)='= '''
      ISTR=1
      IBUF=12
      LPTR=12
      DO WHILE ((ISTR.LE.LEN(STRING)).AND.(IBUF.LT.LEN(BUFF)))
         CHR=STRING(ISTR:ISTR)
         BUFF(IBUF:IBUF)=CHR
         INVOKE=ICH_FOLD(BUFF(IBUF:IBUF))
         IF (CHR.NE.' ') LPTR=IBUF
         IBUF=IBUF+1
         ISTR=ISTR+1
      END DO
      BUFF(MAX(30,LPTR+1):)=''' / '//COMMENT
      CALL FIT_BUFFO(STATUS)
C
      END
