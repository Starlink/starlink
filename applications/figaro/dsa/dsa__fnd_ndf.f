C+
C                     D S A _ _ F I N D _ N D F _ F I T S
C
C  Routine name:
C     DSA__FIND_NDF_FITS
C
C  Function:
C     Finds a named FITS item in the NDF array.
C
C  Description:
C     This routine looks through the NDF FITS array in common, looking
C     for a named FITS item.  If it finds it, it returns the string number
C     for it.  If the item cannot be found, then if the caller has specified
C     that it must exist, an error message will be generated and bad status
C     returned.  If the caller has not specified that it must exist, failure
C     to find it will not be treated as an error - although the string
C     number for it will be returned as zero.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__FIND_NDF_FITS (REF_SLOT,ITEM,ELEMENT,REQ,ISTR,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT        (Integer,ref) The common slot number for the
C                         reference name for the structure.
C     (>) ITEM            (Fixed string,descr) The name of the item
C                         in question.  This should be a FITS keyword.
C     (>) ELEMENT         (Integer,ref) The element of the FITS item
C                         to be accessed.  Normally, a FITS item has
C                         only one element, in which case ELEMENT can be
C                         passed as either 0 or 1.  Items that can be
C                         multiple, such as `HISTORY' will need to
C                         specify the required element.
C     (>) REQ             (Logical,ref) True if the keyword has to exist.
C     (>) ISTR            (Integer,ref) The slot number for the item in
C                         question.
C     (!) STATUS          (Integer,ref) Status code.  If bad status is
C                         passed to it, this routine returns immedaitely.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:
C     DSA_WRUSER, ICH_FOLD, ICH_LEN, ICH_CI, GEN_NTH
C
C  Prior requirements:
C     The FITS items (if any) for the structure must have been read in
C     by DSA__READ_NDF_FITS.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_WRUSER       Outputs message to user
C     ICH_FOLD         Converts string to upper case
C     ICH_LEN          Position of last non-blank char in string
C     ICH_CI           Formats an integer into a string
C     GEN_NTH          Returns 'st', 'nd', 'rd', 'th' etc for a number
C
C  Common variable details:
C     (>) MAX_NDF_FITS  (Integer parameter) Maximum number of NDF FITS strings.
C     (>) FITS_ARRAY    (String array) Used for all strings read from NDF
C                       format FITS structures.
C     (>) FITS_REFS     (Integer array) Reference slot numbers associated with
C                       each array entry.  0 implies unused.
C     (>) ACTUAL_NAMES  (String array) The fully extended name for the structure
C
C  History:
C     9th  Feb 1990.   Original version.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA__FIND_NDF_FITS (REF_SLOT,ITEM,ELEMENT,REQ,
     :                                                    ISTR,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL REQ
      INTEGER REF_SLOT, ELEMENT, ISTR, STATUS
      CHARACTER*(*) ITEM
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
      CHARACTER ICH_CI*8, GEN_NTH*2
C
C     DSA common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      CHARACTER CHAR1*1        ! First character in ITEM
      INTEGER   COUNT          ! Number of matches to keyword found so far
      INTEGER   INDX           ! Index into FITS common arrays
      INTEGER   INVOKE         ! Function value we ignore
      LOGICAL   MORE           ! Controls loop through FITS arrays
      CHARACTER NUMBER*8       ! Used to format element number
      CHARACTER ITEM_UC*8      ! Upper case version of ITEM
C
C     Return immediately if bad status passed.
C
      IF (STATUS.NE.0) RETURN
C
C     Start looking through the items in the FITS array.  Note that a
C     number of things are tested (ref_slot matches, item non-zero,
C     name matches) and the order of the tests makes some assumptions
C     about the most likely non-matching conditions so that they can
C     be tested first.  The code also assumes that it is much more
C     efficient to test for a single character match than for a string
C     match - this is the case for the VMS compiler - hence the test on
C     the first character (CHAR1).
C
      COUNT=0
      ISTR=0
      INDX=1
      MORE=.TRUE.
      ITEM_UC=ITEM
      INVOKE=ICH_FOLD(ITEM_UC)
      CHAR1=ITEM_UC(1:1)
      DO WHILE (MORE)
         IF (FITS_REFS(INDX).EQ.REF_SLOT) THEN
            IF (FITS_ARRAY(INDX)(1:1).EQ.CHAR1) THEN
               IF (FITS_ARRAY(INDX)(1:8).EQ.ITEM_UC) THEN
                  COUNT=COUNT+1
                  IF (COUNT.GE.ELEMENT) THEN
                     MORE=.FALSE.
                     ISTR=INDX
                  END IF
               END IF
            END IF
         END IF
         INDX=INDX+1
         IF (INDX.GT.MAX_NDF_FITS) MORE=.FALSE.
      END DO
C
      IF ((ISTR.EQ.0).AND.REQ) THEN
         CALL DSA_WRUSER ('Unable to locate ')
         IF (ELEMENT.GT.1) THEN
            NUMBER=ICH_CI(ELEMENT)
            CALL DSA_WRUSER ('the '//NUMBER(:ICH_LEN(NUMBER))//
     :                           GEN_NTH(ELEMENT)//' occurrence of ')
         END IF
         CALL DSA_WRUSER ('the FITS keyword "')
         CALL DSA_WRUSER (ITEM(:ICH_LEN(ITEM)))
         CALL DSA_WRUSER ('" in the structure ')
         CALL DSA_WRUSER (ACTUAL_NAMES(REF_SLOT)
     :                            (:ICH_LEN(ACTUAL_NAMES(REF_SLOT))))
         CALL DSA_WRUSER ('.')
         CALL DSA_WRFLUSH
         STATUS=DSA__FITSNF
      END IF
C
      END
