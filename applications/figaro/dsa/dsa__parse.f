C+
C                  D S A _ _ P A R S E _ F I T S _ I T E M
C
C  Routine name:
C     DSA__PARSE_FITS_ITEM
C
C  Function:
C     Gets value string and comment from a FITS header line.
C
C  Description:
C     This routine takes a FITS header line from the common array
C     FITS_ARRAY and extracts the value and comment strings.  A logical
C     value is returned as just its first character - which should be
C     'T' or 'F' but this routine doesn't check that.  A numeric value
C     is returned as the formatted number, a character string is returned
C     as the string with no enclosing quotes, and a comment string is
C     just returned as found in the header line.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__PARSE_FITS_ITEM (NSTR,STRING,VALUE,COMMENT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NSTR          (Integer,ref) The number of the string in common.
C     (<) STRING        (Logical,ref) True if the value was a quoted string.
C     (<) VALUE         (Fixed string,descr) The value string.
C     (<) COMMENT       (Fixed string,descr) The comment string.
C     (!) STATUS        (Integer,ref) Status code. If bad status is passed
C                       to it, this routine returns immediately.
C
C  External variables used:
C     Only common variable sinternal to the DSA routines.
C
C  External subroutines / functions used:
C     [{name}...]
C
C  Prior requirements:
C     The string in question must have been initialised by one of the
C     DSA NDF FITS routines, such as DSA__READ_NDF_FITS.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C
C  Common variable details:
C
C  History:
C     9th  Feb 1990.  Original version.  KS / AAO.
C     14th Feb 1991.  Now tests for a `optional' blank between the slash
C                     and the comment.  KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA__PARSE_FITS_ITEM (NSTR,STRING,VALUE,COMMENT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL STRING
      INTEGER NSTR, STATUS
      CHARACTER*(*) VALUE, COMMENT
C
C     Functions used
C
      INTEGER ICH_VERIF, ICH_DELIM
C
C     DSA common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      CHARACTER CHAR1*1     ! First character of line
      LOGICAL   CMMT        ! True if line is for a comment-type keyword
      INTEGER   IEND        ! Position of last character in value part
      INTEGER   IPTR        ! Pointer into character string
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     HISTORY, COMMENT and blank keywords have values that start
C     in column 9, other keywords have an = sign in that column.
C     Note that test on CHAR1 assumes that a single character test is
C     efficient - it is on the VAX - and is used here to save CPU.
C
      STRING=.FALSE.
      CMMT=.FALSE.
      CHAR1=FITS_ARRAY(NSTR)(1:1)
      IF (CHAR1.EQ.'H') THEN
         CMMT=FITS_ARRAY(NSTR)(1:8).EQ.'HISTORY'
      ELSE IF (CHAR1.EQ.'C') THEN
         CMMT=FITS_ARRAY(NSTR)(1:8).EQ.'COMMENT'
      ELSE IF (CHAR1.EQ.' ') THEN
         CMMT=FITS_ARRAY(NSTR)(1:8).EQ.' '
      END IF
      IF (CMMT) THEN
         COMMENT=' '
         VALUE=FITS_ARRAY(NSTR)(9:)
      ELSE
C
C        Not a comment-type keyword, so look for a character string -
C        ie look for a quotation mark as the first non-blank character.
C
         IPTR=ICH_VERIF (FITS_ARRAY(NSTR),10,' ')
         IF (IPTR.EQ.0) THEN
            COMMENT=' '
            VALUE=' '
         ELSE
            IF (FITS_ARRAY(NSTR)(IPTR:IPTR).EQ.'''') THEN
C
C              It is a quoted string.  Look for the terminating quote.
C
               STRING=.TRUE.
               IF (IPTR.GE.80) THEN
                  VALUE=' '
                  COMMENT=' '
               ELSE
                  IEND=ICH_DELIM(FITS_ARRAY(NSTR),IPTR+1,'''')
                  IF (IEND.LE.0) IEND=81
                  VALUE=FITS_ARRAY(NSTR)(IPTR+1:IEND-1)
                  IF (IEND.GE.80) THEN
                     COMMENT=' '
                  ELSE
                     IPTR=ICH_DELIM(FITS_ARRAY(NSTR),IEND+1,'/')
                     IF ((IPTR.EQ.0).OR.(IPTR.GE.80)) THEN
                        COMMENT=' '
                     ELSE
                        IF (FITS_ARRAY(NSTR)(IPTR+1:IPTR+1).EQ.' ')
     :                                                           THEN
                           COMMENT=FITS_ARRAY(NSTR)(IPTR+2:)
                        ELSE
                           COMMENT=FITS_ARRAY(NSTR)(IPTR+1:)
                        END IF
                     END IF
                  END IF
               END IF
            ELSE
C
C              It is not a quoted string.  Look for a comment separator.
C              Anything between the first non-blank character and either
C              the comment separator or the end of the string is the value.
C              (Somewhat arbitrarily, we ignore a single leading blank in
C              the comment.  It is usually put there - see the FITS papers -
C              but doesn't appear to be obligatory.  Doing this at least
C              makes us match the way DSA__WRITE_FITS_x writes comments.)
C
               IEND=ICH_DELIM(FITS_ARRAY(NSTR),IPTR,'/')
               IF (IEND.EQ.0) THEN
                  COMMENT=' '
                  VALUE=FITS_ARRAY(NSTR)(IPTR:)
               ELSE
                  VALUE=FITS_ARRAY(NSTR)(IPTR:IEND-1)
                  IF (IEND.GE.80) THEN
                     COMMENT=' '
                  ELSE
                     IF (FITS_ARRAY(NSTR)(IEND+1:IEND+1).EQ.' ') THEN
                        COMMENT=FITS_ARRAY(NSTR)(IEND+2:)
                     ELSE
                        COMMENT=FITS_ARRAY(NSTR)(IEND+1:)
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF
C
      END
