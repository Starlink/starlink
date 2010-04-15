C+
C                      D S A _ G E T _ F I T S _ {x}
C
C  Routine name:
C     DSA_GET_FITS_{x}
C
C  Function:
C     Gets an item from the special FITS structure of a file.
C
C  Routines:
C     This covers a set of routines, one for each of a set of data
C     types: DSA_GET_FITS_C (character string), DSA_GET_FITS_D (double
C     precision), DSA_GET_FITS_F (single precision floating point),
C     DSA_GET_FITS_L (logical), DSA_GET_FITS_S (short integer), and
C     DSA_GET_FITS_I (integer).
C
C  Description:
C     Each file handled by DSA may contain a special structure - the
C     `FITS' structure - which contains elements that can be written to
C     a FITS tape header.  These must generally be simple items: single
C     character strings, or single numeric values.  Each item is named
C     and can have a comment associated with it.  Normally, each item
C     is single valued, but there are some items such as `HISTORY' and
C     `COMMENT' where the structure may contain a number of items all
C     with the same associated FITS keyword.  These are treated as
C     being multiple elements of the same named item.  These routines
C     will return the value of an element of a named item.  If the
C     routine used is not the one that matches the actual type of the
C     item, it will be converted to the type requested, but it is
C     usually best to use the correct routine.  These routines assume
C     that the specified item (and element) exist, and will generate
C     error messages if this is not the case.  DSA_SEEK_FITS can be
C     used to see if an element does exist, and to find out its type.
C     DSA_NTH_FITS_ITEM can be used to discover one by one all the
C     items in a FITS structure.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_GET_FITS_{x} (REF_NAME,ITEM,ELEMENT,VALUE,COMMENT,
C                                                           STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME         (Fixed string,descr) The reference name
C                          used to identify the structure.
C     (>) ITEM             (Fixed string,descr) The name of the item
C                          in question - (this is a FITS keyword).
C                          Case-insensitive.
C     (>) ELEMENT          (Integer,ref) The element number to be read.
C                          This is only needed for multivalued objects,
C                          and should be 1 or 0 (either will do) for
C                          single valued objects.
C     (<) VALUE            (Any, ref/descr) The value of the item
C                          in question.   DSA_GET_FITS_C expects a
C                          string passed by descriptor, all others
C                          expect a logical or numeric quantity passed
C                          by reference.
C     (<) COMMENT          (Fixed string,descr) The comment that was
C                          associated with the item.
C     (!) STATUS           (Integer,ref) Status value.  If non-zero
C                          status is passed to it, this routine will
C                          return immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA__PRE_GET_FITS, DSA__READ_FITS_{x}
C
C  Prior requirements:
C     DSA_OPEN should have been used to initialise the system, and the
C     structure in question should have been opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA__PRE_GET_FITS    Preliminary processing for FITS items
C     DSA__READ_FITS_{x}   Read value of FITS item
C
C  History:
C     2nd Dec 1988  Original version.  KS / AAO.
C     7th Feb 1990  Now uses DSA__ routines to handle different data
C                   structures.  KS / AAO.
C     21st Aug 1992 Automatic portability modifications
C                   ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992 "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_GET_FITS_L (REF_NAME,ITEM,ELEMENT,VALUE,
     :                                               COMMENT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL VALUE
      INTEGER ELEMENT, STATUS
      CHARACTER*(*) REF_NAME, ITEM, COMMENT
C
C     Local variables
C
      INTEGER   CODE                   ! Code indicating structure type
      CHARACTER STRING*80              ! Access string for data value
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Pre-process data
C
      CALL DSA__PRE_GET_FITS (REF_NAME,ITEM,ELEMENT,CODE,COMMENT,
     :                                                  STRING,STATUS)
C
C     Read data value and check status
C
      CALL DSA__READ_FITS_L (STRING,CODE,REF_NAME,ITEM,VALUE,STATUS)
C
      END

      SUBROUTINE DSA_GET_FITS_D (REF_NAME,ITEM,ELEMENT,VALUE,COMMENT,
     :                                                           STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ELEMENT, STATUS
      DOUBLE PRECISION VALUE
      CHARACTER*(*) REF_NAME, ITEM, COMMENT
C
C     Local variables
C
      INTEGER   CODE                   ! Code indicating structure type
      CHARACTER STRING*80              ! Access string for data value
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Pre-process data
C
      CALL DSA__PRE_GET_FITS (REF_NAME,ITEM,ELEMENT,CODE,COMMENT,
     :                                                  STRING,STATUS)
C
C     Read data value and check status
C
      CALL DSA__READ_FITS_D (STRING,CODE,REF_NAME,ITEM,VALUE,STATUS)
C
      END

      SUBROUTINE DSA_GET_FITS_I (REF_NAME,ITEM,ELEMENT,VALUE,COMMENT,
     :                                                          STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ELEMENT, VALUE, STATUS
      CHARACTER*(*) REF_NAME, ITEM, COMMENT
C
C     Local variables
C
      INTEGER   CODE                   ! Code indicating structure type
      CHARACTER STRING*80              ! Access string for data value
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Pre-process data
C
      CALL DSA__PRE_GET_FITS (REF_NAME,ITEM,ELEMENT,CODE,COMMENT,
     :                                                  STRING,STATUS)
C
C     Read data value and check status
C
      CALL DSA__READ_FITS_I (STRING,CODE,REF_NAME,ITEM,VALUE,STATUS)
C
      END

      SUBROUTINE DSA_GET_FITS_S (REF_NAME,ITEM,ELEMENT,VALUE,COMMENT,
     :                                                          STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ELEMENT, STATUS
      INTEGER*2 VALUE
      CHARACTER*(*) REF_NAME, ITEM, COMMENT
C
C     Local variables
C
      INTEGER   CODE                   ! Code indicating structure type
      CHARACTER STRING*80              ! Access string for data value
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Pre-process data
C
      CALL DSA__PRE_GET_FITS (REF_NAME,ITEM,ELEMENT,CODE,COMMENT,
     :                                                  STRING,STATUS)
C
C     Read data value and check status
C
      CALL DSA__READ_FITS_S (STRING,CODE,REF_NAME,ITEM,VALUE,STATUS)
C
      END

      SUBROUTINE DSA_GET_FITS_F (REF_NAME,ITEM,ELEMENT,VALUE,COMMENT,
     :                                                          STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ELEMENT, VALUE, STATUS
      CHARACTER*(*) REF_NAME, ITEM, COMMENT
C
C     Local variables
C
      INTEGER   CODE                   ! Code indicating structure type
      CHARACTER STRING*80              ! Access string for data value
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Pre-process data
C
      CALL DSA__PRE_GET_FITS (REF_NAME,ITEM,ELEMENT,CODE,COMMENT,
     :                                                  STRING,STATUS)
C
C     Read data value and check status
C
      CALL DSA__READ_FITS_F (STRING,CODE,REF_NAME,ITEM,VALUE,STATUS)
C
      END

      SUBROUTINE DSA_GET_FITS_C (REF_NAME,ITEM,ELEMENT,VALUE,COMMENT,
     :                                                          STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ELEMENT, STATUS
      CHARACTER*(*) REF_NAME, ITEM, VALUE, COMMENT
C
C     Local variables
C
      INTEGER   CODE                   ! Code indicating structure type
      CHARACTER STRING*80              ! Access string for data value
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Pre-process data
C
      CALL DSA__PRE_GET_FITS (REF_NAME,ITEM,ELEMENT,CODE,COMMENT,
     :                                                  STRING,STATUS)
C
C     Read data value and check status
C
      CALL DSA__READ_FITS_C (STRING,CODE,REF_NAME,ITEM,VALUE,STATUS)
C
      END
