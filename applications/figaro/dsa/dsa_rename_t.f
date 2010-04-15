C+
C                 D S A _ R E N A M E _ T E M P
C
C  Routine:
C     DSA_RENAME_TEMP
C
C  Function:
C     Renames any temporary files created to the names that they should have.
C
C  Description:
C     On systems without version numbers for files, a new file is sometimes
C     requested with the same name as an existing file. If this existing file
C     is already opened by DSA a new file of the same name cannot be created
C     without confusing the system. In this case the system (the code is in
C     DSA_FNAME) creates a temporary file with a name that it stores in the
C     common blocks maintained by the DSA system. Each time a file is closed,
C     this routine should be called to see if there are files that can now
C     safely be renamed, in which case it does so.
C
C  Language:
C     Fortran
C
C  Call:
C     CALL DSA_RENAME_TEMP (STATUS)
C
C  Parameters:
C     (!) STATUS      (Integer) Inherited status. If passed as non-zero,
C                     routine returns immediately.
C
C  Prior requirements:
C     This routine should be called from any DSA routine that closes a file,
C     once it has removed the reference to that file from the file open
C     common blocks.
C
C  Support: K. Shortridge, AAO
C
C  Version date: 17th Dec 1992.
C
C  Subroutines / functions used:
C      GEN_ERRMSG, GEN_SYSERR, GEN_RENAME, ICH_LEN
C-
C  Common variables used:
C     (>) MAX_FILES     (Integer parameter) Maximum number of file entries.
C     (>) FILE_NAMES    (String array) Full file specification for each file.
C     (>) FILE_USED     (Logical array) Indicates file table slot in use.
C     (>) MAX_TEMP_NAMES (Integer parameter) Maximum number of temporary
C                       file names.
C     (>) FILE_TEMP_NAMES(String array) Temporary names used for files.
C     (>) FILE_REAL_NAMES(String array) Name file really ought to have.
C     (!) NAME_USED     (Logical array) This name pair already in use.
C
C  History:
C     17th Dec 1992.  Original version, KS/AAO.
C     19th Jul 1995.  Disuse GEN_SYSERR and GEN_ERRMSG, HME/UoE, Starlink.
C
C+
      SUBROUTINE DSA_RENAME_TEMP (STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
C
C     DSA common blocks
C
      INCLUDE 'DSA_COMMON'
      INCLUDE 'DSA_ERRORS'
C
C     Functions used
C
      INTEGER GEN_RENAME, ICH_LEN
C
C     Local variables
C
      INTEGER   IFILE             ! Loop index through open files
      INTEGER   ITEMP             ! Loop index through temp name tables
      LOGICAL   MATCH             ! True if temp file matches open file
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look one by one at the list of files to be renamed. For each one
C     see if the file whose name it is supposed to have is still open.
C     If it is not, rename it.
C
      DO ITEMP=1,MAX_TEMP_NAMES
         IF (NAME_USED(ITEMP)) THEN
            MATCH=.FALSE.
            DO IFILE=1,MAX_FILES
               IF (FILE_USED(IFILE)) THEN
                  IF (FILE_REAL_NAMES(ITEMP).EQ.FILE_NAMES(IFILE)) THEN
                     MATCH=.TRUE.
                     GO TO 340       ! Break from IFILE loop
                  END IF
               END IF
            END DO
  340       CONTINUE
            IF (.NOT.MATCH) THEN
               STATUS=GEN_RENAME(FILE_TEMP_NAMES(ITEMP),
     :                                  FILE_REAL_NAMES(ITEMP))
               IF (STATUS.NE.0) THEN
                  CALL DSA_WRUSER(
     :                  'Unable to rename the temporary file ')
                  CALL DSA_WRUSER(FILE_TEMP_NAMES(ITEMP)
     :                           (:ICH_LEN(FILE_TEMP_NAMES(ITEMP))))
                  CALL DSA_WRUSER(' as ')
                  CALL DSA_WRUSER(FILE_REAL_NAMES(ITEMP)
     :                           (:ICH_LEN(FILE_REAL_NAMES(ITEMP))))
                  CALL DSA_WRUSER('.')
                  CALL DSA_WRFLUSH
                  STATUS=DSA__VMSERR
                  GO TO 500      ! Error exit
               END IF
               STATUS=0
               NAME_USED(ITEMP)=.FALSE.
            END IF
         END IF
      END DO
C
C     Exit
C
  500 CONTINUE
C
      END


