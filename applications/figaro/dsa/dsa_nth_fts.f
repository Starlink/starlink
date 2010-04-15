C+
C                       D S A _ N T H _ F I T S _ I T E M
C
C  Routine name:
C     DSA_NTH_FITS_ITEM
C
C  Function:
C     Returns details about the Nth FITS item in a structure
C
C  Description:
C     This routine looks at the FITS sub-structure associated with a
C     specified structure, and in particluar at the Nth item in that
C     sub-structure.  If there is such an item, this routine returns
C     the type and size of the item.  The type is returned as a single
C     character indicating which of the various DSA_GET_FITS_{x}
C     routines should be used to access the item.  Most FITS items are
C     scalar and show as having one element.  Only those items that can
C     appear a number of times, such as the comments `COMMENT',
C     `HISTORY' and blank items, will show as having a number of
C     elements.  If the item is not present, this routine returns
C     indicating that but does not treat it as an error and will not
C     generate an error message.  This routine may be used to search
C     completely through the FITS items in a structure, by starting
C     at NTH=1 and continuing until EXIST is returned as false.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_NTH_FITS_ITEM (REF_NAME,NTH,EXIST,ITEM,ACCESS,ELEMENTS,
C                                                       STRLEN,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME         (Fixed string,descr) The reference name
C                          associated with the structure in question.
C     (>) NTH              (Integer,ref) The number of the item in
C                          question.
C     (<) EXIST            (Logical,ref) Returned true if such an item
C                          exists, false otherwise.
C     (<) ITEM             (Fixed string,descr) The name of the item in
C                          question.  This should normally be a FITS
C                          keyword, or blank.
C     (<) ACCESS           (Fixed string,descr) A single character that
C                          indicates the routine to be used to access
C                          the item in its `natural' form.  This will be
C                          one of `L',`I',`S',`C',`F',`D', corresponding
C                          to DSA_GET_FITS_L, DSA_GET_FITS_I, etc.  If
C                          the item exists but cannot be accessed by any
C                          of these routines (for example, because it is
C                          a structure), ACCESS will be set to blank.
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
C     DSA_REF_SLOT, DSA__FITS_DETAILS, ICH_FOLD
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
C     ICH_FOLD          Convert a string to upper case
C
C  History:
C     1st Dec 1988.   Original version.  KS / AAO.
C     12th Feb 1990.  Most work now relegated to DSA__FITS_DETAILS, to
C                     support different data formats.  KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_NTH_FITS_ITEM (REF_NAME,NTH,EXIST,ITEM,ACCESS,
     :                                           ELEMENTS,STRLEN,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL EXIST
      INTEGER NTH, ELEMENTS, STRLEN, STATUS
      CHARACTER*(*) REF_NAME, ITEM, ACCESS
C
C     Functions used
C
      INTEGER ICH_FOLD
C
C     Local variables
C
      INTEGER   INVOKE            ! Dummy function value
      INTEGER   REF_SLOT          ! Common slot number for ref name
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Look up REF_NAME in tables and let DSA__FITS_DETAILS do most
C     of the work.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
C
      CALL DSA__FITS_DETAILS (REF_SLOT,NTH,ITEM,EXIST,ACCESS,
     :                                       ELEMENTS,STRLEN,STATUS)
      INVOKE=ICH_FOLD(ITEM)
C
      END
