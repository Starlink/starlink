C+
C                      D S A _ _ W R I T E _ F I T S _ {x}
C
C  Routine name:
C     DSA__WRITE_FITS_{x}
C
C  Function:
C     Writes a FITS item into a string.
C
C  Routines:
C     This covers a set of routines, one for each of a set of data
C     types: DSA__WRITE_FITS_C (character string), DSA__WRITE_FITS_D (double
C     precision), DSA__WRITE_FITS_F (single precision floating point),
C     DSA__WRITE_FITS_L (logical), DSA__WRITE_FITS_S (short integer), and
C     DSA__WRITE_FITS_I (integer).
C
C  Description:
C     This is a set of utility routines for use with DSA_PUT_FITS_{x}.
C     Once DSA__PRE_PUT_FITS had been called, this routine uses the
C     information it returns to get write the value of the FITS item
C     specified.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__WRITE_FITS_{x} (KEYWORD,CODE,STRING,NAME,VALUE,
C                                                      COMMENT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) KEYWORD         (Fixed string,descr) The name of the item
C                         in question - (this is a FITS keyword).
C                         Must be in upper case.
C     (>) CODE            (Integer, ref) A code indicating the type of
C                         data structure holding the FITS items.
C                         CODE, STRING and NAME should be as returned by
C                         DSA__PRE_PUT_FITS.
C     (>) STRING          (Integer,ref) For NDF format data, this should
C                         be the number of the string in the common array
C                         FITS_ARRAY to be used for the value.  For
C                         original Figaro format data, this will normally
C                         be ignored, except for the case of DSA__WRITE_FITS_C
C                         where if non-zero it will be taken as the number of
C                         the string in the common array FITS_STRINGS where
C                         the string is to be buffered (this will only apply
C                         to multi-valued comment-type keywords).
C     (>) NAME            (Fixed string,descr) Ignored for NDF format data.
C                         For original Figaro format data, this should be the
C                         DTA name of the data object to which the value is
C                         to be written.
C     (>) COMMENT         (Fixed string,descr) Ignored for original Figaro
C                         format data, for which the comment will already
C                         have been written by DSA__PRE_PUT_FITS.  For NDF
C                         format data, this should be the comment to be
C                         associated with the FITS keyword.
C     (>) VALUE           (Any, ref/descr) The value of the FITS item
C                         in question.   DSA__WRITE_FITS_C expects a string
C                         passed by descriptor, all others expect a logical
C                         or numeric quantity passed by reference.
C     (!) STATUS          (Integer,ref) Status value.  If non-zero
C                         status is passed to it, this routine will
C                         return immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_POST_PUT_FITS, DTA_SZVAR, DTA_WRVARB, DTA_WRVARD, DTA_WRVARF,
C     DTA_WRVARI, DTA_WRVARS, DTA_WRVARC, ICH_LEN, ICH_CI, ICH_CD, ICH_CF
C
C  Prior requirements:
C     DSA__PRE_PUT_FITS should have been called to determin the values
C     of CODE, STRING and NAME.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_POST_PUT_FITS     Test DTA_ status after write to FITS structure
C     DTA_SZVAR             Get size of a data object
C     DTA_WRVAR{x}          Write an item to a data object
C     ICH_LEN               Position o flast non-blank char in string
C     ICH_C{x}              Format a number into a character stirng
C
C  History:
C     14th Feb 1990  Original version.  KS / AAO.
C     21st Feb 1990  Position of comments for NDF char strings improved. KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA__WRITE_FITS_L (KEYWORD,CODE,STRING,NAME,
     :                                         VALUE,COMMENT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL VALUE
      INTEGER CODE, STRING, STATUS
      CHARACTER*(*) NAME, KEYWORD, COMMENT
C
C     DSA common and parameter definitions
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      BYTE      BVAL            ! Byte value used in structure for logicals
      INTEGER   DTA_STATUS      ! Status returned by DTA routine
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
      IF (CODE.EQ.NDF_CODE) THEN
         FITS_ARRAY(STRING)=KEYWORD
         FITS_ARRAY(STRING)(9:9)='='
         IF (VALUE) THEN
            FITS_ARRAY(STRING)(30:32)='T /'
         ELSE
            FITS_ARRAY(STRING)(30:32)='F /'
         END IF
         FITS_ARRAY(STRING)(34:)=COMMENT
      ELSE
         IF (VALUE) THEN
            BVAL=1
         ELSE
            BVAL=0
         END IF
         CALL DTA_WRVARB (NAME,1,BVAL,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_POST_PUT_FITS(DTA_STATUS,NAME,STATUS)
         END IF
      END IF
C
      END

      SUBROUTINE DSA__WRITE_FITS_F (KEYWORD,CODE,STRING,NAME,
     :                                         VALUE,COMMENT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER CODE, STRING, STATUS
      REAL VALUE
      CHARACTER*(*) NAME, KEYWORD, COMMENT
C
C     DSA common and parameter definitions
C
      INCLUDE 'DSA_COMMON'
C
C     Functions
C
      INTEGER ICH_LEN
      CHARACTER*13 ICH_CF
C
C     Local variables
C
      INTEGER   DTA_STATUS      ! Status returned by DTA routine
      INTEGER   NCHAR           ! Number of characters needed to format number
      CHARACTER NUMBER*13       ! Used to format number
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
      IF (CODE.EQ.NDF_CODE) THEN
         FITS_ARRAY(STRING)=KEYWORD
         FITS_ARRAY(STRING)(9:9)='='
         NUMBER=ICH_CF(VALUE)
         NCHAR=ICH_LEN(NUMBER)
         FITS_ARRAY(STRING)(31-NCHAR:30)=NUMBER
         FITS_ARRAY(STRING)(32:32)='/'
         FITS_ARRAY(STRING)(34:)=COMMENT
      ELSE
         CALL DTA_WRVARF (NAME,1,VALUE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_POST_PUT_FITS(DTA_STATUS,NAME,STATUS)
         END IF
      END IF
C
      END

      SUBROUTINE DSA__WRITE_FITS_D (KEYWORD,CODE,STRING,NAME,
     :                                         VALUE,COMMENT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER CODE, STRING, STATUS
      DOUBLE PRECISION VALUE
      CHARACTER*(*) NAME, KEYWORD, COMMENT
C
C     DSA common and parameter definitions
C
      INCLUDE 'DSA_COMMON'
C
C     Functions
C
      INTEGER ICH_LEN
      CHARACTER*22 ICH_CD
C
C     Local variables
C
      INTEGER   CMTPTR          ! Start character position for '/ comment'
      INTEGER   DTA_STATUS      ! Status returned by DTA routine
      INTEGER   NCHAR           ! Number of characters needed to format number
      CHARACTER NUMBER*22       ! Used to format number
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
      IF (CODE.EQ.NDF_CODE) THEN
         FITS_ARRAY(STRING)=KEYWORD
         FITS_ARRAY(STRING)(9:9)='='
         NUMBER=ICH_CD(VALUE)
         NCHAR=ICH_LEN(NUMBER)
         IF (NCHAR.LE.20) THEN
            FITS_ARRAY(STRING)(31-NCHAR:30)=NUMBER
            CMTPTR=32
         ELSE
            FITS_ARRAY(STRING)(11:10+NCHAR)=NUMBER
            CMTPTR=12+NCHAR
         END IF
         FITS_ARRAY(STRING)(CMTPTR:CMTPTR)='/'
         FITS_ARRAY(STRING)(CMTPTR+2:)=COMMENT
      ELSE
         CALL DTA_WRVARD (NAME,1,VALUE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_POST_PUT_FITS(DTA_STATUS,NAME,STATUS)
         END IF
      END IF
C
      END

      SUBROUTINE DSA__WRITE_FITS_I (KEYWORD,CODE,STRING,NAME,
     :                                         VALUE,COMMENT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER CODE, STRING, VALUE, STATUS
      CHARACTER*(*) NAME, KEYWORD, COMMENT
C
C     DSA common and parameter definitions
C
      INCLUDE 'DSA_COMMON'
C
C     Functions
C
      INTEGER ICH_LEN
      CHARACTER*11 ICH_CI
C
C     Local variables
C
      INTEGER   DTA_STATUS      ! Status returned by DTA routine
      INTEGER   NCHAR           ! Number of characters needed to format number
      CHARACTER NUMBER*11       ! Used to format number
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
      IF (CODE.EQ.NDF_CODE) THEN
         FITS_ARRAY(STRING)=KEYWORD
         FITS_ARRAY(STRING)(9:9)='='
         NUMBER=ICH_CI(VALUE)
         NCHAR=ICH_LEN(NUMBER)
         FITS_ARRAY(STRING)(31-NCHAR:30)=NUMBER
         FITS_ARRAY(STRING)(32:32)='/'
         FITS_ARRAY(STRING)(34:)=COMMENT
      ELSE
         CALL DTA_WRVARI (NAME,1,VALUE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_POST_PUT_FITS(DTA_STATUS,NAME,STATUS)
         END IF
      END IF
C
      END

      SUBROUTINE DSA__WRITE_FITS_S (KEYWORD,CODE,STRING,NAME,
     :                                         VALUE,COMMENT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER CODE, STRING, STATUS
      INTEGER*2 VALUE
      CHARACTER*(*) NAME, KEYWORD, COMMENT
C
C     DSA common and parameter definitions
C
      INCLUDE 'DSA_COMMON'
C
C     Functions
C
      INTEGER ICH_LEN
      CHARACTER*11 ICH_CI
C
C     Local variables
C
      INTEGER   DTA_STATUS      ! Status returned by DTA routine
      INTEGER   IVAL            ! Temporary full-size integer
      INTEGER   NCHAR           ! Number of characters needed to format number
      CHARACTER NUMBER*11       ! Used to format number
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
      IF (CODE.EQ.NDF_CODE) THEN
         FITS_ARRAY(STRING)=KEYWORD
         FITS_ARRAY(STRING)(9:9)='='
         IVAL=VALUE
         NUMBER=ICH_CI(IVAL)
         NCHAR=ICH_LEN(NUMBER)
         FITS_ARRAY(STRING)(31-NCHAR:30)=NUMBER
         FITS_ARRAY(STRING)(32:32)='/'
         FITS_ARRAY(STRING)(34:)=COMMENT
      ELSE
         CALL DTA_WRVARS (NAME,1,VALUE,DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_POST_PUT_FITS(DTA_STATUS,NAME,STATUS)
         END IF
      END IF
C
      END

      SUBROUTINE DSA__WRITE_FITS_C (KEYWORD,CODE,STRING,NAME,
     :                                         VALUE,COMMENT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER CODE, STRING, STATUS
      CHARACTER*(*) NAME, KEYWORD, VALUE, COMMENT
C
C     DSA common and parameter definitions
C
      INCLUDE 'DSA_COMMON'
C
C     Functions
C
      INTEGER ICH_LEN
C
C     Local variables
C
      CHARACTER CHAR1           ! First character of KEYWORD.
      INTEGER   CPTR            ! Position of comment delimiter ('/')
      LOGICAL   CTYPE           ! True if keyword is a comment-type keyword
      INTEGER   DTA_STATUS      ! Status returned by DTA routine
      INTEGER   NCHAR           ! Number of characters needed to format number
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
      IF (CODE.EQ.NDF_CODE) THEN
C
C        NDF format.  We have to check for a comment-type keyword, since
C        this does not have an '=' or quotes around the value.
C
         CTYPE=.FALSE.
         CHAR1=KEYWORD(1:1)
         IF (CHAR1.EQ.'C') THEN
            CTYPE=KEYWORD.EQ.'COMMENT'
         ELSE IF (CHAR1.EQ.'H') THEN
            CTYPE=KEYWORD.EQ.'HISTORY'
         ELSE IF (CHAR1.EQ.' ') THEN
            CTYPE=KEYWORD.EQ.' '
         END IF
         FITS_ARRAY(STRING)=KEYWORD
         IF (CTYPE) THEN
            FITS_ARRAY(STRING)(9:)=VALUE
            IF (COMMENT.NE.' ') THEN
               NCHAR=ICH_LEN(VALUE)
               IF (NCHAR+12.LE.80) THEN
                  FITS_ARRAY(STRING)(NCHAR+10:NCHAR+10)='/'
                  FITS_ARRAY(STRING)(NCHAR+12:)=COMMENT
               END IF
            END IF
         ELSE
            FITS_ARRAY(STRING)(9:11)='= '''
            FITS_ARRAY(STRING)(12:)=VALUE
            NCHAR=ICH_LEN(VALUE)
            IF (NCHAR.LT.8) NCHAR=8
            IF (NCHAR.GT.68) NCHAR=68
            FITS_ARRAY(STRING)(NCHAR+12:NCHAR+12)=''''
            CPTR=MAX(32,NCHAR+14)
            IF (CPTR+2.LE.80) THEN
               FITS_ARRAY(STRING)(CPTR:CPTR)='/'
               FITS_ARRAY(STRING)(CPTR+2:)=COMMENT
            END IF
         END IF
      ELSE
C
C        Original Figaro format.  If no buffering required, write out data
C        value and check status. Otherwise put into buffer slot entry - will
C        be written out when structure is closed.  If this is a buffered
C        string, we append any comment (we don't really expect one) to the
C        value string.
C
         IF (STRING.EQ.0) THEN
            CALL DTA_WRVARC (NAME,LEN(VALUE),VALUE,DTA_STATUS)
            CALL DSA_POST_PUT_FITS (DTA_STATUS,NAME,STATUS)
         ELSE
            NCHAR=ICH_LEN(VALUE)
            IF (COMMENT.EQ.' ') THEN
               FITS_STRINGS(STRING)=VALUE
            ELSE
               FITS_STRINGS(STRING)=VALUE
               NCHAR=MAX(31,NCHAR)
               FITS_STRINGS(STRING)(NCHAR+1:)='/ '//COMMENT
            END IF
         END IF
      END IF
C
      END

