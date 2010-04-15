C+
C                   D S A _ C L O S E _ S T R U C T U R E
C
C  Routine name:
C     DSA_CLOSE_STRUCTURE
C
C  Function:
C     Closes down a structure opened by a routine such as DSA_INPUT
C
C  Description:
C     DSA_CLOSE closes down all structures opened by the DSA_ system,
C     but sometimes there may be a need to close one down explicitly.
C     DSA_CLOSE_STRUCTURE will close down the structure associated with
C     a specific reference name.  The structure must have been opened
C     by a routine like DSA_INPUT or DSA_OUTPUT.  This routine does not
C     automatically unmap any arrays in the structure, so they must have
C     been explicitly unmapped by calls to DSA_UNMAP.  Nor does it
C     automatically perform the post-processing required for data
C     quality arrays and flagged data values.  So whichever (if any)
C     of DSA_POST_PROCESS_FLAGGED_VALUES or DSA_POST_PROCESS_QUALITY
C     is required must also be called explicitly.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_CLOSE_STRUCTURE (REF_NAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME       (Fixed string,descr) The reference name associated
C                        with the structure to be closed.
C     (>) STATUS         (Integer,ref) Status code.  If bad status is passed
C                        to it, this routine will return immediately.
C
C  External variables used:
C     Only common variables internal to the DSA_ routines.
C
C  External subroutines / functions used:
C     ICH_FOLD, ICH_LEN, DSA_FIND_REF, DSA_WRUSER, DSA_GET_ACTUAL_NAME,
C     DSA_CHECK_STRUCTURE, DSA__FLUSH_FITS, DSA_FREE_WORKSPACE, DSA_WRNAME,
C     DSA_RENAME_TEMP, DTA_FRVAR, DTA_FCLOSE, DTA_ERROR
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.  The
C     structure in question must have been opened and no array in it
C     should be currently mapped.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 17th Dec 1992.
C-
C  Common variable details:
C    (<)  DTA_CODE     (Integer) Last DTA_ system failure status code.
C    (!)  FILE_COUNT   (Integer array) Number of reference names associated
C                      with each file.
C    (>)  FILE_NAMES   (String array) Full file specification for each file.
C    (>)  FILE_TOP_NAMES (String array) Top-level name associated with
C                      each file.
C    (!)  FILE_USED    (Logical array) Indicates file table slot in use.
C    (>)  MAX_MAPS     (Integer parameter) Maximum number of mapped arrays.
C    (!)  MAP_USED     (Logical array) Indicates map slot in use.
C    (!)  MAP_WORK     (Integer array) Workspace slot associated with array.
C    (>)  MAP_NAMES    (String array) Names of requested mapped arrays.
C    (>)  MAP_COUNT    (Integer array) Reference count for this array.
C    (<)  REF_USED     (Logical array) Indicates reference slot in use.
C    (>)  PRE_QUAL     (Logical array) Indicates quality pre-processing done.
C    (>)  PRE_FLAG     (Logical array) Indicates flag values pre-processed.
C    (>)  FITS_OPEN    (Logical array) Indicates FITS structure opened.
C
C  Subroutine / function details:
C    ICH_FOLD       Fold string to upper case
C    ICH_LEN        Position of last non-blank char in string
C    DSA_FIND_REF   Look up reference name in common tables
C    DSA_WRUSER     Output message to user
C    DSA_CHECK_STRUCTURE  Tidy up structure prior to closing
C    DSA__FLUSH_FITS Flush out any buffered FITS keywords
C    DSA_FREE_WORKSPACE   Release a workspace array
C    DSA_GET_ACTUAL_NAME  Get full structure name from reference name
C    DSA_RENAME_TEMP Rename any temporary files created by the system
C    DSA_WRNAME     Output data object name to user
C    DTA_FRVAR      Unmap a data object
C    DTA_ERROR      Get error description from DTA_ status code
C    DTA_FCLOSE     Close a data file
C
C  History:
C     14th Aug 1987  Original version.  KS / AAO.
C     5th July 1988  Call to DSA_CHECK_STRUCTURE added.  KS/AAO.
C     22nd July 1988 Test for post processing added.  KS/AAO.
C     29th Nov 1988  Call to DSA_FLUSH_FITS added.  KS/AAO.
C     11th Aug 1989  Now releases associated work arrays.  KS/AAO.
C     13th Feb 1990  DSA_FLUSH_FITS renamed to DSA__FLUSH_FITS.  KS/AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992  Remove unused variable declarations. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     17th Dec 1992  Call to DSA_RENAME_TEMP added. KS/AAO.
C+
      SUBROUTINE DSA_CLOSE_STRUCTURE (REF_NAME,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) REF_NAME
C
C     Functions used
C
      INTEGER ICH_FOLD, ICH_LEN
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     DSA_ system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   DTA_STATUS                  ! DTA_ status code.
      CHARACTER ERROR*64                    ! DTA_ error description
      INTEGER   FILE_SLOT                   ! File slot number in common
      INTEGER   I                           ! Loop variable
      INTEGER   INVOKE                      ! Dummy function return value
      INTEGER   LENGTH                      ! Object name length
      CHARACTER OBJ_NAME*128                ! DTA_ name of data object
      CHARACTER REF_NAME_UC*32              ! Upper case version of REF_NAME
      INTEGER   REF_SLOT                    ! Reference table slot #
      CHARACTER STRUCTURE*128               ! Full structure name from ref_name
      INTEGER   WORK_SLOT                   ! Work array associated with data
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     We need an upper case version of REF_NAME
C
      REF_NAME_UC=REF_NAME
      INVOKE=ICH_FOLD(REF_NAME_UC)
C
C     Look up the reference name in the tables and get the data
C     array dimensions.
C
      CALL DSA_FIND_REF (REF_NAME_UC,REF_SLOT,OBJ_NAME,LENGTH,STATUS)
      IF (STATUS.NE.0) GO TO 500         ! Error exit
C
C     Now that we have the top level object name, see if any of the
C     mapped arrays match it.  If so, it may be that they are in fact
C     no longer in use but have not been physically unmapped yet.  If
C     this is the case, their reference counts will be zero and we can
C     actually unmap them now.  Otherwise we have an error condition.
C
      DO I=1,MAX_MAPS
         IF (MAP_USED(I)) THEN
            IF (MAP_NAMES(I)(:LENGTH+1).EQ.(OBJ_NAME(:LENGTH)//'.'))
     :                                                            THEN
               IF (MAP_COUNT(I).GT.0) THEN
                  CALL DSA_WRUSER('Attempt to close the structure ')
                  CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,STATUS)
                  CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
                  CALL DSA_WRUSER(' while it still has arrays mapped.')
                  CALL DSA_WRUSER(' Probable programming error.')
                  CALL DSA_WRFLUSH
                  STATUS=DSA__OBJMAP
                  GO TO 500      ! Error exit
               ELSE
                  CALL DTA_FRVAR(MAP_ACTNAM(I),DTA_STATUS)
                  IF (DTA_STATUS.NE.0) THEN
                     CALL DSA_WRUSER('Error unmapping the array ')
                     CALL DSA_WRNAME(MAP_ACTNAM(I))
                     CALL DTA_ERROR(DTA_STATUS,ERROR)
                     CALL DSA_WRUSER('. ')
                     CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
                     CALL DSA_WRUSER('.')
                     CALL DSA_WRFLUSH
                     DTA_CODE=DTA_STATUS
                     STATUS=DSA__DTAERR
                     GO TO 500    ! Error exit
                  END IF
C
C                 If there were any workspace arrays associated with this
C                 mapped array, free them now.  Note that DSA_FREE_WORKSPACE
C                 mapipulates the MAP_WORK values in common, which is what
C                 makes this while loop work.
C
                  DO WHILE (MAP_WORK(I).NE.0)
                     WORK_SLOT=MAP_WORK(I)
                     CALL DSA_FREE_WORKSPACE (WORK_SLOT,STATUS)
                     IF (STATUS.NE.0) GO TO 500    ! Error exit
                  END DO
               END IF
               MAP_USED(I)=.FALSE.
            END IF
         END IF
      END DO
C
C     If there is an opened FITS structure, flush out any buffered
C     keywords.
C
      IF (FITS_OPEN(REF_SLOT)) THEN
         CALL DSA__FLUSH_FITS(REF_SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500   ! Error exit
      END IF
C
C     See if any post processing has been omitted - note that we
C     can't do it here, because some of the arrays may have been
C     unmapped.
C
      IF (PRE_QUAL(REF_SLOT).OR.PRE_FLAG(REF_SLOT)) THEN
         CALL DSA_WRUSER('Attempt to close the structure ')
         CALL DSA_GET_ACTUAL_NAME(REF_NAME,STRUCTURE,STATUS)
         CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
         CALL DSA_WRUSER(' which still requires post processing to')
         CALL DSA_WRUSER(' be performed on its ')
         IF (PRE_QUAL(REF_SLOT)) THEN
            CALL DSA_WRUSER('quality data')
         ELSE
            CALL DSA_WRUSER('flagged data values')
         END IF
         CALL DSA_WRUSER('. A call to DSA_POST_PROCESS')
         IF (PRE_QUAL(REF_SLOT)) THEN
            CALL DSA_WRUSER('QUALITY')
         ELSE
            CALL DSA_WRUSER('FLAGGED_VALUES')
         END IF
         CALL DSA_WRUSER(
     :          ' has been omitted. Probable programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__SEQERR
         GO TO 500      ! Error exit
      END IF
C
C     If we've got this far, there are no mapped arrays in the
C     structure, so we can close it.  First, check that the structure
C     is internally consistent, and fix it up if not.
C
      CALL DSA_CHECK_STRUCTURE(REF_SLOT,STATUS)
C
C     Now we can actually do the close. First we flag it as closed, then
C     go on to close it at the file level.  We decrement the file
C     reference count and if it goes to zero we can actually close
C     the file.
C
      REF_USED(REF_SLOT)=.FALSE.
      FILE_SLOT=REF_FILE(REF_SLOT)
      FILE_COUNT(FILE_SLOT)=FILE_COUNT(FILE_SLOT)-1
      IF (FILE_COUNT(FILE_SLOT).EQ.0) THEN
         CALL DTA_FCLOSE(FILE_TOP_NAMES(FILE_SLOT),DTA_STATUS)
         IF (DTA_STATUS.NE.0) THEN
            CALL DSA_WRUSER('Error closing the file ')
            CALL DSA_WRUSER(
     :         FILE_NAMES(FILE_SLOT)(:ICH_LEN(FILE_NAMES(FILE_SLOT))))
            CALL DTA_ERROR(DTA_STATUS,ERROR)
            CALL DSA_WRUSER('. ')
            CALL DSA_WRUSER(ERROR(:ICH_LEN(ERROR)))
            CALL DSA_WRUSER('.')
            CALL DSA_WRFLUSH
            DTA_CODE=DTA_STATUS
            STATUS=DSA__DTAERR
            GO TO 500    ! Error exit
         END IF
         FILE_USED(FILE_SLOT)=.FALSE.
      END IF
C
C     Now, just in case any temporary file was waiting for this file to be
C     finished with before being renamed to the same name, check the
C     various temporary files.
C
      CALL DSA_RENAME_TEMP(STATUS)
C
C     Exit
C
  500 CONTINUE
C
      END
