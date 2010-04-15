C+
C                     D S A _ D A T A _ L O C A T I O N
C
C  Routine name:
C     DSA_DATA_LOCATION
C
C  Function:
C     Returns the file name and block number offset for a main data array.
C
C  Description:
C     Returns the disk location of the main data array in a structure.
C     This allows direct access to the array using either direct disk I/O calls,
C     or direct mapping of the file.  Note that if either of these
C     is attempted, other access to the same data through DSA or DTA should
C     not be attempted at the same time.  If the object is sufficiently
C     small that other objects are held in the same disk blocks as it,
C     this sort of access is very unsafe, and an error code is returned.
C     All non-standard I/O to the object should be closed down before
C     accessing it with reading routines such as DSA_MAP_DATA.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_DATA_LOCATION (REF_NAME,FILE_NAME,BLOCK,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME     (Fixed string,descr) The reference name used to
C                      identify the structure.
C     (<) FILE_NAME    (Character) Returns with the full file name of
C                      the file containing the data array.  Note that
C                      this full file name can be quite long.
C     (<) BLOCK        (Integer) Returns the block number in the file
C                      at which the data starts.  The first block is
C                      number one, not zero.
C     (!) STATUS       (Integer,ref) Status code.  If bad status is
C                      passed, this routine returns immediately.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_REF_SLOT, DSA_WRUSER, DSA_GET_ACTUAL_NAME, DTA_DWVAR,
C     DSA__DATA_NAME, ICH_LEN
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and the
C     structure in question should have been already opened.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_REF_SLOT     Look up reference name in common tables.
C     DSA_WRUSER       Write string to user.
C     DSA_GET_ACTUAL_NAME  Get full name for structure.
C     DSA__DATA_NAME   Get DTA name of main data array.
C     DTA_DWVAR        Get file name and block offset for named data item.
C     ICH_LEN          Position of last non-blank char in string.
C
C  History:
C     10th Aug 1987    Original version.  KS / AAO.
C     20th Feb 1990    Modified to use DSA__DATA_NAME to get details of
C                      file structure.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_DATA_LOCATION (REF_NAME,FILE_NAME,BLOCK,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER BLOCK, STATUS
      CHARACTER*(*) REF_NAME, FILE_NAME
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! DTA_ routine status codes
      CHARACTER ERROR*64                    ! Used to format DTA error text
      INTEGER   IGNORE                      ! Dummy status value
      INTEGER   LENGTH                      ! Length of structure name
      CHARACTER NAME*80                     ! DTA_ name for exposure time
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE*80                ! Full name of structure
C
C     DSA system error codes
C
      INCLUDE 'DSA_ERRORS'
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
C     Generate the name of the data array and see if we can find where
C     it is.   Note that this will not work for contracted arrays - which
C     is probably correct.
C
      CALL DSA__DATA_NAME (REF_SLOT,NAME,LENGTH)
      CALL DTA_DWVAR(NAME,FILE_NAME,BLOCK,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
C
C        Couldn't locate it.  Output an error message.
C
         CALL DSA_WRUSER(
     :        'Unable to get the location of the main data array in ')
         IGNORE=0
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,IGNORE)
         CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER('. ')
         CALL DTA_ERROR (DTA_STATUS,ERROR)
         CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR)))
         CALL DSA_WRUSER ('.')
         CALL DSA_WRFLUSH
         STATUS=DSA__DTAERR
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
