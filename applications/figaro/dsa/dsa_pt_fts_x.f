C+
C                      D S A _ P U T _ F I T S _ {x}
C
C  Routine name:
C     DSA_PUT_FITS_{x}
C
C  Function:
C     Puts an item into the special FITS structure of a file.
C
C  Routines:
C     This covers a set of routines, one for each of a set of data
C     types: DSA_PUT_FITS_C (character string), DSA_PUT_FITS_D (double
C     precision), DSA_PUT_FITS_F (single precision floating point),
C     DSA_PUT_FITS_L (logical), DSA_PUT_FITS_S (short integer), and
C     DSA_PUT_FITS_I (integer).
C
C  Description:
C     Each file handled by DSA may contain a special structure - the
C     `FITS' structure - which contains elements that can be written to
C     a FITS tape header.  These must generally be simple items: single
C     character strings, or single numeric values.  Each item is named
C     (the name must be no more than eight characters long) and can
C     have a comment associated with it.  Normally, there can only be
C     one item with a given name, but there can be multiple instances
C     of items called `COMMENT', `HISTORY' or with no name (where the
C     name is blank).  These are all treated as comments.  This routine
C     is passed an item in three parts: the name of the item, its value
C     (whose type depends on the routine being called), and any
C     associated comment.  If the name is `COMMENT', `HISTORY' or
C     blank, the value is always a string to be used as the comment,
C     and the comment argument will normally be left blank (although it
C     does not have to be - the two will be concatenated).  This
C     routine does not guarantee that the order in which the items
C     appear in the file will match the order in which they are passed
C     to this routine.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_PUT_FITS_{x} (REF_NAME,ITEM,VALUE,COMMENT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME         (Fixed string,descr) The reference name
C                          used to identify the structure.
C     (>) ITEM             (Fixed string,descr) The name of the item
C                          in question - (this is a FITS keyword).
C                          Must be no more than 8 characters, and
C                          is case-insensitive.
C     (>) VALUE            (Any, ref/descr) The value of the item
C                          in question.  If the item is a string, then
C                          trailing blanks are significant.  The case of
C                          any string is maintained. DSA_PUT_FITS_C
C                          expects a string passed by descriptor, all
C                          others expect a logical or numeric quantity
C                          passed by reference.
C     (>) COMMENT          (Fixed string,descr) A comment to be
C                          associated with the item.  The case of the
C                          comment will be maintained.
C     (!) STATUS           (Integer,ref) Status value.  If non-zero
C                          status is passed to it, this routine will
C                          return immediately.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     DSA__PRE_PUT_FITS, DSA__WRITE_FITS_{x}
C
C  Prior requirements:
C     DSA_OPEN should have been used to initialise the system, and the
C     structure in question should have been opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (<) FITS_STRINGS    (String array) Buffered FITS strings
C
C  Subroutine / function details:
C     DSA__PRE_PUT_FITS    Preliminary processing for FITS items
C     DSA__WRITE_FITS_{x}  Write to a FITS item.
C
C  History:
C     25th Nov 1988  Original version.  KS / AAO.
C     13th Feb 1990  Modified to support different data formats, through
C                    use of DSA__ routines.  KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_PUT_FITS_L (REF_NAME,ITEM,VALUE,COMMENT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL VALUE
      INTEGER STATUS
      CHARACTER*(*) REF_NAME, ITEM, COMMENT
C
C     Local variables
C
      INTEGER   CODE                   ! Indicates type of data structure
      CHARACTER KEYWORD*16             ! Keyword (for folding to upper case)
      CHARACTER NAME*80                ! DTA name of value data object
      INTEGER   STRING                 ! Number of string to use
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Pre-process data and write out.
C
      KEYWORD=ITEM
      CALL DSA__PRE_PUT_FITS (REF_NAME,KEYWORD,'BYTE',0,COMMENT,CODE,
     :                                              NAME,STRING,STATUS)
      CALL DSA__WRITE_FITS_L (KEYWORD,CODE,STRING,NAME,VALUE,COMMENT,
     :                                                          STATUS)
C
      END

      SUBROUTINE DSA_PUT_FITS_D (REF_NAME,ITEM,VALUE,COMMENT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      DOUBLE PRECISION VALUE
      CHARACTER*(*) REF_NAME, ITEM, COMMENT
C
C     Local variables
C
      INTEGER   CODE                   ! Indicates type of data structure
      CHARACTER KEYWORD*16             ! Keyword (for folding to upper case)
      CHARACTER NAME*80                ! DTA name of value data object
      INTEGER   STRING                 ! Number of string to use
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Pre-process data and write out
C
      KEYWORD=ITEM
      CALL DSA__PRE_PUT_FITS (REF_NAME,KEYWORD,'DOUBLE',0,COMMENT,CODE,
     :                                           NAME,STRING,STATUS)
      CALL DSA__WRITE_FITS_D (KEYWORD,CODE,STRING,NAME,VALUE,COMMENT,
     :                                                          STATUS)
C
      END

      SUBROUTINE DSA_PUT_FITS_I (REF_NAME,ITEM,VALUE,COMMENT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER VALUE, STATUS
      CHARACTER*(*) REF_NAME, ITEM, COMMENT
C
C     Local variables
C
      INTEGER   CODE                   ! Indicates type of data structure
      CHARACTER KEYWORD*16             ! Keyword (for folding to upper case)
      CHARACTER NAME*80                ! DTA name of value data object
      INTEGER   STRING                 ! Number of string to use
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Pre-process data and write out
C
      KEYWORD=ITEM
      CALL DSA__PRE_PUT_FITS (REF_NAME,KEYWORD,'INT',0,COMMENT,CODE,
     :                                           NAME,STRING,STATUS)
      CALL DSA__WRITE_FITS_I (KEYWORD,CODE,STRING,NAME,VALUE,COMMENT,
     :                                                        STATUS)
C
      END

      SUBROUTINE DSA_PUT_FITS_S (REF_NAME,ITEM,VALUE,COMMENT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      INTEGER*2 VALUE
      CHARACTER*(*) REF_NAME, ITEM, COMMENT
C
C     Local variables
C
      INTEGER   CODE                   ! Indicates type of data structure
      CHARACTER KEYWORD*16             ! Keyword (for folding to upper case)
      CHARACTER NAME*80                ! DTA name of value data object
      INTEGER   STRING                 ! Number of string to use
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Pre-process data and write out
C
      KEYWORD=ITEM
      CALL DSA__PRE_PUT_FITS (REF_NAME,KEYWORD,'SHORT',0,COMMENT,CODE,
     :                                           NAME,STRING,STATUS)
      CALL DSA__WRITE_FITS_S (KEYWORD,CODE,STRING,NAME,VALUE,COMMENT,
     :                                                         STATUS)
C
      END

      SUBROUTINE DSA_PUT_FITS_F (REF_NAME,ITEM,VALUE,COMMENT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER VALUE, STATUS
      CHARACTER*(*) REF_NAME, ITEM, COMMENT
C
C     Local variables
C
      INTEGER   CODE                   ! Indicates type of data structure
      CHARACTER KEYWORD*16             ! Keyword (for folding to upper case)
      CHARACTER NAME*80                ! DTA name of value data object
      INTEGER   STRING                 ! Number of string to use
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Pre-process data and write out
C
      KEYWORD=ITEM
      CALL DSA__PRE_PUT_FITS (REF_NAME,KEYWORD,'FLOAT',0,COMMENT,CODE,
     :                                           NAME,STRING,STATUS)
      CALL DSA__WRITE_FITS_F (KEYWORD,CODE,STRING,NAME,VALUE,COMMENT,
     :                                                         STATUS)
C
      END

      SUBROUTINE DSA_PUT_FITS_C (REF_NAME,ITEM,VALUE,COMMENT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) REF_NAME, ITEM, VALUE, COMMENT
C
C     Local variables
C
      INTEGER   CODE                   ! Indicates type of data structure
      CHARACTER KEYWORD*16             ! Keyword (for folding to upper case)
      CHARACTER NAME*80                ! DTA name of value data object
      INTEGER   STRING                 ! Number of string to use
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Pre-process data and write out
C
      KEYWORD=ITEM
      CALL DSA__PRE_PUT_FITS (REF_NAME,KEYWORD,'CHAR',LEN(VALUE),
     :                              COMMENT,CODE,NAME,STRING,STATUS)
      CALL DSA__WRITE_FITS_C (KEYWORD,CODE,STRING,NAME,VALUE,COMMENT,
     :                                                          STATUS)
C
      END
