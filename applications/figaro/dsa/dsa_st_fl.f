C+
C               D S A _ S E T _ F L A G G E D _ V A L U E S
C
C  Routine name:
C     DSA_SET_FLAGGED_VALUES
C
C  Function:
C     Indicates whether or not a data array may contain flagged values.
C
C  Description:
C     If an application needs to explicitly indicate either that a newly
C     created data array contains flagged data values, or needs to
C     indicate to the system that a data array that did contain flagged
C     values no longer does so, it can call this routine to set the
C     appropriate flags in the data structure.  This routine should be
C     used with care.  If used with EXIST set TRUE on a structure that
C     actually has quality information (which can be determined using
C     DSA_SEEK_QUALITY) it will generate an invalid structure (since
C     DSA assumes that quality and flagged values are mutually exclusive).
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SET_FLAGGED_VALUES (REF_NAME,EXIST,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name associated
C                      with the structure.
C     (>) EXIST        (Logical,ref) True if the data array may contain
C                      flagged values.
C     (!) STATUS       (Integer,ref) Status return code.  If a bad status
C                      value is passed to it, this routine returns
C                      immediately.
C
C  External variables used:
C     Only common variables internal to the DSA package.
C
C  External subroutines / functions used:
C     DTA_ERROR, DSA_FIND_REF, DSA__SET_FLAGGED, DSA_WRUSER,
C     ICH_FOLD, ICH_LEN
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     data structure should have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_FIND_REF     Look up reference name in common tables.
C     DSA_WRUSER       Output string to user.
C     DSA__SET_FLAGGED Sets or clears the `data flagged' flag for a structure
C     DTA_ERROR        Get error text describing DTA status code.
C     ICH_FOLD         Convert string to upper case.
C     ICH_LEN          Position of last non-blank char in string
C
C  Common variable details:
C     (>) ACTUAL_NAMES (String array) The fully extended name for the structure.
C     (!) DATA_FLAGGED (Integer array) State of knowledge about data flagging.
C                      Indicates unknown (0), known not to exist (-1),
C                      exists (1).
C     (<) DTA_CODE     (Integer) Last DTA system error code.
C
C  History:
C     14th July 1988.   Original version.  KS / AAO.
C     13th Dec  1989.   Now uses DSA__SET_FLAGGED.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_SET_FLAGGED_VALUES (REF_NAME,EXIST,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL EXIST
      CHARACTER*(*) REF_NAME
      INTEGER STATUS
C
C     Functions
C
      INTEGER ICH_FOLD, ICH_LEN
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! Status from DTA routine
      CHARACTER ERROR*64                    ! DTA error text
      INTEGER   INVOKE                      ! Dummy function value
      INTEGER   LENGTH                      ! Object name length - ignored
      CHARACTER OBJ_NAME*32                 ! DTA_ name of data object - ignored
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot #
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     DSA system errors
C
      INCLUDE 'DSA_ERRORS'
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables.
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
C
      IF (EXIST) THEN
         DATA_FLAGGED(REF_SLOT)=1
      ELSE
         DATA_FLAGGED(REF_SLOT)=-1
      END IF
      CALL DSA__SET_FLAGGED (REF_SLOT,EXIST,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
         CALL DSA_WRUSER('Unable to ')
         IF (EXIST) THEN
            CALL DSA_WRUSER('set ')
         ELSE
            CALL DSA_WRUSER('clear ')
         END IF
         CALL DSA_WRUSER('"data flagged" flag in the structure ')
         CALL DSA_WRUSER(ACTUAL_NAMES(REF_SLOT)
     :                            (:ICH_LEN(ACTUAL_NAMES(REF_SLOT))))
         CALL DSA_WRUSER('. ')
         CALL DTA_ERROR(DTA_STATUS,ERROR)
         CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRUSER('.')
         CALL DSA_WRFLUSH
         DTA_CODE=DTA_STATUS
         STATUS=DSA__DTAERR
      END IF
C
C     Exit
C
      END
