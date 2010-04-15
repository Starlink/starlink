C+
C                     D S A _ S E T _ E X P O S U R E
C
C  Routine name:
C     DSA_SET_EXPOSURE
C
C  Function:
C     Sets the exposure time associated with a data structure.
C
C  Description:
C     A number of Figaro routines make use of an exposure time
C     associated with a structure.  This routine allows such an
C     exposure time to be set.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_SET_EXPOSURE (REF_NAME,TIME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name used to
C                      identify the structure.
C     (>) TIME         (Float,ref) The exposure time, in seconds, to be
C                      associated with the structure.
C     (!) STATUS       (Integer,ref) Status code.  If bad status is
C                      passed, this routine returns immediately.
C
C  External variables used:
C     Only common variables internal to the DSA system.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DSA_WRUSER, DSA_GET_ACTUAL_NAME, DSA__EXPOSURE_NAME,
C     DSA__CREATE_OBS_EXTRA, ICH_LEN, DTA_CRVAR, DTA_WRVARF
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     structure in question should have been already opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (<) DTA_CODE    (Integer) Last DTA system error code.
C
C  Subroutine / function details:
C     DSA_REF_SLOT     Look up reference name in common tables.
C     DSA_WRUSER       Write string to user.
C     DSA_GET_ACTUAL_NAME  Get full name for structure.
C     DSA__EXPOSURE_NAME   Get name of object containing expsoure time.
C     DSA__CREATE_OBS_EXTRA Create substructure containing observation info.
C     DTA_WRVARF       Read from a floating point data object.
C     DTA_CRVAR        Create a data object.
C     ICH_LEN          Position of last non-blank char in string.
C
C  History:
C     5th Sept 1988.   Original version.  KS / AAO.
C     12th Mar 1990.   Now uses DSA__ routines rather than just assuming the
C                      original Figaro data structure.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992    Remove unused variable declarations. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_SET_EXPOSURE (REF_NAME,TIME,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      REAL TIME
      CHARACTER*(*) REF_NAME
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     DSA error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     DSA system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! DTA_ routine status codes
      CHARACTER ERROR*64                    ! DTA error text
      INTEGER   IGNORE                      ! Dummy status value
      INTEGER   LENGTH                      ! Length of structure name
      CHARACTER NAME*80                     ! DTA_ name for exposure time
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE*80                ! Full name of structure
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look up the reference name in the tables.
C
      CALL DSA_REF_SLOT (REF_NAME,REF_SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500          ! Error exit
C
C     Generate the name of the exposure time data object and try to set it.
C
      CALL DSA__CREATE_OBS_EXTRA (REF_SLOT,DTA_STATUS)
      CALL DSA__EXPOSURE_NAME (REF_SLOT,NAME,LENGTH)
      CALL DTA_CRVAR(NAME,'FLOAT',DTA_STATUS)
      CALL DTA_WRVARF(NAME,1,TIME,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
C
C        Unable to set it.
C
         CALL DSA_WRUSER('Unable to set an exposure time in ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,IGNORE)
         CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER('. ')
         CALL DTA_ERROR(DTA_STATUS,ERROR)
         CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRFLUSH
         DTA_CODE=DTA_STATUS
         STATUS=DSA__DTAERR
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
