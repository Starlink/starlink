C+
C                 D S A _ R E A L _ N A M E
C
C  Routine:
C     DSA_REAL_NAME
C
C  Function:
C     Returns the real file name corresponding to a temporary file.
C
C  Description:
C     On systems without version numbers for files, a new file is sometimes
C     requested with the same name as an existing file. If this existing file
C     is already opened by DSA a new file of the same name cannot be created
C     without confusing the system. In this case the system (the code is in
C     DSA_FNAME) creates a temporary file with a name that it stores in the
C     common blocks maintained by the DSA system.  This routine can be called
C     to determine the real file name corresponding to such a temporary file
C     name.
C
C  Language:
C     Fortran
C
C  Call:
C     CALL DSA_REAL_NAME (FILE_NAME,REAL_NAME)
C
C  Parameters:
C     (>) FILE_NAME   (Character string) The name of the file in question - ie
C                     the name of an actual file.
C     (<) REAL_NAME   (Character string) The name by which the file will be
C                     known after it is closed down. If a temporary name is
C                     being used, this will be the name to which the file will
C                     eventually be renamed. Otherwise this will be the same
C                     as FILE_NAME. If FILE_NAME is not known to the DSA system
C                     at all, REAL_NAME is returned set equaal to FILE_NAME.
C
C  Prior requirements:
C     The DSA system should have been initialised.
C
C  Support: K. Shortridge, AAO
C
C  Version date: 28th June 1993.
C
C  Subroutines / functions used: None.
C-
C  Common variables used:
C     (>) MAX_TEMP_NAMES (Integer parameter) Maximum number of temporary
C                       file names.
C     (>) FILE_TEMP_NAMES(String array) Temporary names used for files.
C     (>) FILE_REAL_NAMES(String array) Name file really ought to have.
C     (!) NAME_USED     (Logical array) This name pair already in use.
C
C  History:
C     28th Jun 1993.  Original version, KS/AAO.
C
C+
      SUBROUTINE DSA_REAL_NAME (FILE_NAME,REAL_NAME)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) FILE_NAME, REAL_NAME
C
C     DSA common blocks
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   ITEMP             ! Loop index through temp name tables
C
C     Look one by one at the list of files to be renamed. For each one
C     see if the name matches the one we've been passed.
C
      REAL_NAME=FILE_NAME
      DO ITEMP=1,MAX_TEMP_NAMES
         IF (NAME_USED(ITEMP)) THEN
            IF (FILE_TEMP_NAMES(ITEMP).EQ.FILE_NAME) THEN
               REAL_NAME=FILE_REAL_NAMES(ITEMP)
               GO TO 340             ! Break from ITEMP loop
            END IF
         END IF
      END DO
  340 CONTINUE
C
      END


