C+
C                           D S A _ C L O S E
C
C  Routine name:
C     DSA_CLOSE
C
C  Function:
C     Closes down the DSA_ routines.
C
C  Description:
C     DSA_CLOSE should be called to close down the DSA_ routines at
C     the end of a Figaro program.   Once this routine has been called,
C     it is then possible to call DSA_OPEN to reinitialise the system.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_CLOSE (STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (!) STATUS       (Integer,ref) Status return code.  Note that
C                      this routine, unlike almost all the other DSA_
C                      routines, although it does not ignore the value
C                      of STATUS passed to it, does not return immediately
C                      if passed bad status, so will always shutdown the
C                      DSA_ system.
C
C  External variables used -
C     Only common variables internal to the DSA_ system.
C
C  External subroutines / functions used:
C     DSA_UNMAP, DSA_FREE_WORKSPACE, DSA_WRUSER, DSA_WRNAME
C     DTA_FRVAR, DTA_FCLOSE, DTA_ERROR, DTA_DLVAR, DTA_WRVARI, ICH_LEN
C     DSA_FREE_LU, DSA_POST_PROCESS_FLAGGED_VALUES, DSA_RENAME_TEMP,
C     DSA_POST_PROCESS_QUALITY, DSA__FLUSH_FITS
C
C  Prior requirements:
C     DSA_OPEN should have been called to initialise the DSA_ system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 17th Dec 1992.
C-
C  Common variable details:
C    (<) OPEN_FLAG    (Logical) Indicates system has been initialised.
C    (<) SYS_STATUS   (Integer) Overall DSA system status.
C    (>) MAX_FILES     (Integer parameter) Maximum number of file entries.
C    (>) FILE_NAMES    (String array) Full file specification for each file.
C    (>) FILE_TOP_NAMES (String array) Top-level name associated with
C                      each file.
C    (!) FILE_USED     (Logical array) Indicates file table slot in use.
C    (>) MAX_MAPS      (Integer parameter) Maximum number of mapped arrays.
C    (!) MAP_USED      (Logical array) Indicates map slot in use.
C    (>) MAP_NAMES     (String array) Names of mapped arrays.
C    (>) MAX_MAP_CALLS (Integer parameter) Maximum number of map calls.
C    (!) MAP_CALL_USED (Logical array) Indicates table entry in use.
C    (>) MAX_REFS      (Integer parameter) Maximum number of reference names.
C    (!) REF_USED      (Logical array) Indicates reference slot in use.
C    (>) REF_NAMES     (String array) Reference names in use.
C    (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                      object corresponding to reference name.
C    (>) MAX_WORK      (Integer parameter) Number of workspace slots available.
C    (>) WORK_USED     (Logical array) Indicates workspace slot in use.
C    (>) MAX_LUS       (Integer parameter) Number of logical units available
C    (!) LU_USED       (Logical array) Indicates logical unit slot in use
C    (>) LU_NUMBER     (Integer array) Logical unit numbers
C    (>) PRE_QUAL      (Logical array) Indicates quality pre-processing done.
C    (>) PRE_FLAG      (Logical array) Indicates flag values pre-processed.
C    (>) FITS_OPEN     (Logical array) Indicates FITS structure opened.
C
C  Subroutine / function details:
C     DTA_ERROR    Return error message given DTA error code
C     DTA_FRVAR    Unmap a mapped data object
C     DTA_FCLOSE   Close down a file
C     DSA_CHECK_STRUCTURE  Tidy up structure prior to closing
C     DSA_UNMAP    Unmap a mapped data array
C     DSA_WRUSER   Write string to user
C     DSA_WRNAME   Write name of data object to user
C     DSA__FLUSH_FITS  Flush out FITS strings from buffer
C     DSA_FREE_WORKSPACE  Release workspace
C     DSA_POST_PROCESS_FLAGGED_VALUES  Perform flag values post-processing
C     DSA_POST_PROCESS_QUALITY  Perform quality data post-processing
C     DSA_RENAME_TEMP Rename any temporary file that were created
C     ICH_LEN      Position of last non-blank char in string
C     DSA_FREE_LU  Release logical unit number
C
C  History:
C     08 Jul 1987  Original version.  KS / AAO.
C     01 Mar 1988  Code to close down Fortran logical units added. KS/AAO.
C     05 Jul 1988  Modified to use DSA_CHECK_STRUCTURE. KS/AAO.
C     22 Jul 1988  Calls to post processing routines added.  Also call
C                  to FIG_SETERR added.  KS/AAO.
C     30 Nov 1988  Flushing of FITS structure added. KS/AAO.
C     13 Feb 1990  DSA_FLUSH_FITS changed to DSA__FLUSH_FITS.  KS/AAO.
C     21 Aug 1992  Automatic portability modifications
C                  ("INCLUDE" syntax etc) made. KS/AAO
C     29 Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     03 Sep 1992  Don't call FIG_SETERR. (That is the only call from
C                  DSA to FIG (apart from _ENCDIM).) HME/UoE
C     17 Dec 1992  Added call to DSA_RENAME_TEMP. And added setting of
C                  SYS_STATUS as replacement for FIG_SETERR. KS/AAO.
C     21 Jul 1993  Use DSA_FREE_LU. HME/UoE, Starlink.
C     19 Jul 1995  Use DSA_FREE_LU. HME/UoE, Starlink.
C+
      SUBROUTINE DSA_CLOSE (STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
C
C     Functions used
C
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER   CALL_STATUS                ! Status from DSA_ routines
      INTEGER   DTA_STATUS                 ! Status from DTA_ routines
      CHARACTER ERROR*64                   ! DTA_ system error message
      INTEGER   I                          ! Loop index
      INTEGER   IGNORE                     ! Close status - ignored
      INTEGER   LU                         ! Local copy of unit number
C
C     DSA_ system common
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     First, make sure any outstanding post-processing has been
C     performed, and flush out any FITS strings still in buffers.
C     (Perhaps this should be done through a call to DSA_CLOSE_STRUCTURE?)
C
      DO I=1,MAX_REFS
         IF (REF_USED(I)) THEN
            IF (FITS_OPEN(I)) THEN
               CALL_STATUS=0
               CALL DSA__FLUSH_FITS(I,CALL_STATUS)
            END IF
            IF (PRE_QUAL(I)) THEN
               CALL_STATUS=0
               CALL DSA_POST_PROCESS_QUALITY(REF_NAMES(I),CALL_STATUS)
               IF (CALL_STATUS.NE.0) STATUS=DSA__CLSERR
            ELSE IF (PRE_FLAG(I)) THEN
               CALL_STATUS=0
               CALL DSA_POST_PROCESS_FLAGGED_VALUES (
     :                                       REF_NAMES(I),CALL_STATUS)
               IF (CALL_STATUS.NE.0) STATUS=DSA__CLSERR
            END IF
         END IF
      END DO
C
C     Second, clear up all map calls.
C
      DO I=1,MAX_MAP_CALLS
         IF (MAP_CALL_USED(I)) THEN
            CALL_STATUS=0
            CALL DSA_UNMAP (I,CALL_STATUS)
            IF (CALL_STATUS.NE.0) STATUS=DSA__CLSERR
         END IF
      END DO
C
C     Next, close all workspace allocations (most will probably
C     have been closed by the map call clean up - the only ones
C     left will have been allocated by direct user calls.)
C
      DO I=1,MAX_WORK
         IF (WORK_USED(I)) THEN
            CALL_STATUS=0
            CALL DSA_FREE_WORKSPACE (I,CALL_STATUS)
            IF (CALL_STATUS.NE.0) STATUS=DSA__CLSERR
         END IF
      END DO
C
C     Now unmap all the still mapped data objects.
C
      DO I=1,MAX_MAPS
         IF (MAP_USED(I)) THEN
            CALL DTA_FRVAR (MAP_ACTNAM(I),DTA_STATUS)
            IF (DTA_STATUS.NE.0) THEN
               CALL DTA_ERROR (DTA_STATUS,ERROR)
               CALL DSA_WRUSER ('Error unmapping ')
               CALL DSA_WRNAME (MAP_ACTNAM(I))
               CALL DSA_WRUSER ('. ')
               CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR)))
               CALL DSA_WRUSER ('.')
               CALL DSA_WRFLUSH
               STATUS=DSA__CLSERR
            END IF
            MAP_USED(I)=.FALSE.
         END IF
      END DO
C
C     Go through the reference slots checking the structures for
C     internal errors and fixing them up.
C
      DO I=1,MAX_REFS
         IF (REF_USED(I)) THEN
            CALL_STATUS=0
            CALL DSA_CHECK_STRUCTURE(I,CALL_STATUS)
            IF (CALL_STATUS.NE.0) STATUS=DSA__CLSERR
         END IF
      END DO
C
C     Close down all the open data files.
C
      DO I=1,MAX_FILES
         IF (FILE_USED(I)) THEN
            CALL DTA_FCLOSE (FILE_TOP_NAMES(I),DTA_STATUS)
            IF (DTA_STATUS.NE.0) THEN
               CALL DTA_ERROR (DTA_STATUS,ERROR)
               CALL DSA_WRUSER ('Error closing down the file ')
               CALL DSA_WRUSER (FILE_NAMES(I)(:ICH_LEN(FILE_NAMES(I))))
               CALL DSA_WRUSER ('. ')
               CALL DSA_WRUSER (ERROR(:ICH_LEN(ERROR)))
               CALL DSA_WRUSER ('.')
               CALL DSA_WRFLUSH
               STATUS=DSA__CLSERR
            END IF
            FILE_USED(I)=.FALSE.
         END IF
      END DO
C
C     Close down all the reference slots (this isn't strictly necessary,
C     but will block further -illegal- attempts to access the system.)
C
      DO I=1,MAX_REFS
         REF_USED(I)=.FALSE.
      END DO
C
C     Run through the Fortran logical unit numbers, closing down
C     any open files and freeing the logical units.
C
      DO I=1,MAX_LUS
         IF (LU_USED(I)) THEN
            LU=LU_NUMBER(I)
            IGNORE=0
            CALL DSA_FREE_LU(LU,IGNORE)
         END IF
      END DO
C
C     Rename any files that were given temporary names.
C
      CALL DSA_RENAME_TEMP(STATUS)
C
C     Flag the system as uninitialised
C
      OPEN_FLAG=.FALSE.
C
C     If anything was wrong when this routine was called, or if anything
C     went wrong during it, indicate an error to the overall system.
C
      IF (STATUS.NE.0) SYS_STATUS=STATUS
C
      END
