C+
C                       D S A _ S E E K _ F I T S
C
C  Routine name:
C     DSA_SEEK_FITS
C
C  Function:
C     Checks for the existence of a specified item in a FITS structure
C
C  Description:
C     This routine looks to see if the FITS sub-structure associated with
C     a specified structure contains an item of given name.  If so, it
C     returns the type and size of the item.  Most FITS items are scalar
C     and show as having one element.  Only those items that can appear
C     a number of times, such as the comments `COMMENT', `HISTORY' and
C     blank items, will show as having a number of elements.  If the
C     item is not present, this routine returns indicating that but does
C     not treat as an error and will not generate an error message.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SEEK_FITS (REF_NAME,ITEM,EXIST,ACCESS,ELEMENTS,
C                                                       STRLEN,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME         (Fixed string,descr) The reference name
C                          associated with the structure in question.
C     (>) ITEM             (Fixed string,descr) The name of the item in
C                          question.  Case insensitive.  This should
C                          normally be a FITS keyword, or blank.
C     (<) EXIST            (Logical,ref) Returned true if such an item
C                          exists, false otherwise.
C     (<) ACCESS           (Fixed string,descr) A single character that
C                          indicates the routine to be used to access
C                          the item in its `natural' form.  This will be
C                          one of `L',`I',`S',`C',`F',`D', corresponding
C                          to DSA_GET_FITS_L, DSA_GET_FITS_I, etc.  If
C                          the item exists but is of a non-standard type
C                          that cannot be accessed by these routines,
C                          ACCESS will be set to blank.
C     (<) ELEMENTS         (Integer,ref) The number of elements in the
C                          FITS structure for this item.  This will be
C                          one for all bar the comment items that may
C                          occur multiply.
C     (<) STRLEN           (Integer,ref) If the item is a character string,
C                          this returns the number of characters in it.
C     (<) STATUS           (Integer,ref) Status code.  If bad status is
C                          passed to this routine, it returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DSA_FIND_REF, DSA_FITS_DETAILS, ICH_FOLD
C
C  Prior requirements:
C     The structure in question must have been opened by a routine such
C     as DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_REF_SLOT      Look up reference name in common tables
C     DSA__FITS_DETAILS Get size and access details for a FITS item
C     ICH_FOLD          Convert string to upper case
C
C  History:
C     1st Dec 1988.   Original version.  KS / AAO.
C     12th Feb 1990.  Now uses the now-renamed DSA__FITS_DETAILS which
C                     supports NDF formats.  KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_SEEK_FITS (REF_NAME,ITEM,EXIST,ACCESS,ELEMENTS,
     :                                                  STRLEN,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL EXIST
      INTEGER ELEMENTS, STRLEN, STATUS
      CHARACTER*(*) REF_NAME, ITEM, ACCESS
C
C     Functions
C
      INTEGER ICH_FOLD
C
C     Local variables
C
      INTEGER   INVOKE            ! Dummy function value
      CHARACTER ITEM_UC*16        ! Upper case verison of ITEM
      INTEGER   REF_SLOT          ! Common slot number for ref name
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Look up REF_NAME in tables, convert ITEM to upper case, and let
C     DSA__FITS_DETAILS do the rest.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      ITEM_UC=ITEM
      INVOKE=ICH_FOLD(ITEM_UC)
      CALL DSA__FITS_DETAILS (REF_SLOT,0,ITEM_UC,EXIST,ACCESS,ELEMENTS,
     :                                                   STRLEN,STATUS)
C
      END
