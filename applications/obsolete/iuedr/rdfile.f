      SUBROUTINE RDFILE( FILE, DEFEXT, FD, STATUS )
*+
*  Name:
*     SUBROUTINE RDFILE

*  Description:
*     Opens a file for FORTRAN READ.
*     A file descriptor is returned.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDFILE( FILE, DEFEXT, FD, STATUS )

*  Arguments:
*     FILE = BYTE( * ) (Given)
*        Name of the file to open.
*     DEFEXT = BYTE( * ) (Given)
*        Extension to append to file name if none is present.
*     FD = INTEGER (Returned)
*        Unit number of the opened file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     MBT: Mark Taylor (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       AT4 version
*     06-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     02-SEP-94 (MJC):
*       IUEDR Vn. 3.1-3
*       Removed PSX_UNAME call.
*     03-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*       FIO I/O unit numbers.
*     25-NOV-94 (MJC):
*       Modified Logical Name/Environment Variable support:
*
*       o Either Logical Names or Environemt Variables syntaxes
*         may be used on VMS/unix platforms.
*       o If the $IUEDR_DATA directory is selected the file name is
*         forced to lower case.
*         The translation of $IUEDR_DATA retains its case.
*       o Other file names are left as-is.
*     25-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     09-JUL-99 (MBT):
*       Removed nonstandard OPEN keywords.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMPSX'

*  External References:
      INTEGER CHR_LEN

*  Local Constants:
      INTEGER FNAMLEN          ! Maximum file name length.
      INTEGER ERR              ! Error code.
      PARAMETER ( FNAMLEN = 81, ERR = -3 )

*  Arguments Given:
      BYTE FILE( FNAMLEN )     ! File name.
      BYTE DEFEXT( FNAMLEN )   ! File default extension.

*  Arguments Returned:
      INTEGER FD               ! File descriptor.

*  Status:
      INTEGER STATUS           ! Global status.

*  Local Variables:
      CHARACTER*( FNAMLEN ) FN ! CHARACTER version of file name.
      CHARACTER*( FNAMLEN ) LFN ! CHARACTER version environment variable.
      CHARACTER*( FNAMLEN ) EXT ! CHARACTER version of file extension.
      CHARACTER*( FNAMLEN ) ENV_PATH

      INTEGER ENCHR            ! Character count in extension.
      INTEGER NCHAR            ! Character count in file name.
      INTEGER I
      INTEGER LINDEX
      INTEGER IOSTAT           ! Local status.

      LOGICAL FLOG             ! Is a logical name is used?
      LOGICAL ISEXT            ! Does the file name have an extension?
*.

*  CHARACTER version of the filename.
      CALL GEN_STOC( FILE, FNAMLEN, FN, NCHAR )

*  Translate logical name or environment variable.
      FLOG = .FALSE.
      DO I = 1, NCHAR
         IF ( FN( I : I ) .EQ. ':' ) THEN
            FLOG = .TRUE.
            LINDEX = I
         END IF
      END DO
      IF ( FLOG ) THEN
         LFN = FN( : LINDEX - 1 )

      ELSE
         IF ( FN( 1 : 1 ) .EQ. '$' ) THEN
            FLOG = .TRUE.
            LINDEX = NCHAR
            DO I = 2, NCHAR
               IF ( FN( I : I ) .EQ. '/' ) THEN
                  LINDEX = I - 1
               END IF
            END DO
         END IF
         LFN = FN( 2 : LINDEX )
      END IF

      IF ( FLOG ) THEN
         IOSTAT = SAI__OK
         CALL PSX_GETENV( LFN, ENV_PATH, IOSTAT )
         IF ( IOSTAT .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( IOSTAT )
            CALL ERROUT( '\\', STATUS )
            GO TO 999
         END IF

         IF ( STATUS .EQ. SAI__OK ) THEN
            I = FNAMLEN
            DO WHILE ( ENV_PATH( I : I ) .EQ. ' ' )
               I = I - 1
            END DO

            IF ( SYSNAME .NE. 'VMS' ) THEN
               IF ( LINDEX .NE. NCHAR ) THEN
                  FN = ENV_PATH( : I ) // '/' // FN( LINDEX+1 : )

               ELSE
                  FN = ENV_PATH( : I )
               END IF
               NCHAR = NCHAR - LINDEX + I + 1
               IF ( LFN.EQ.'IUEDR_DATA' .OR. LFN.EQ.'iuedr_data' ) THEN
                  CALL CHR_LCASE( FN( I + 1 : ) )
               END IF

            ELSE
               FN = ENV_PATH( : I ) // FN( LINDEX + 1 : )
               NCHAR = NCHAR - LINDEX + I + 1
            END IF

         ELSE
            NCHAR = CHR_LEN( FN )
         END IF
      END IF

*  Fetch I/O unit number.
      CALL FIO_GUNIT( FD, STATUS )

*  See if the file name has an extension present already.
      ISEXT = .FALSE.
      DO I = 1, NCHAR
          IF ( FN( I : I ) .EQ. '.' ) THEN
             ISEXT = .TRUE.
          END IF
      END DO

*  Add the default extension if none is provided.
      IF ( .NOT. ISEXT ) THEN
         CALL GEN_STOC( DEFEXT, FNAMLEN, EXT, ENCHR )
         CALL CHR_APPND( EXT, FN, NCHAR )
      END IF

*  Try to open the file.
      OPEN( UNIT = FD, NAME = FN( : NCHAR ), ACCESS = 'SEQUENTIAL',
     :      STATUS = 'OLD', IOSTAT = IOSTAT )

*  Release I/O unit on error.
      IF ( IOSTAT .NE. 0 ) THEN
         IOSTAT = SAI__OK
         CALL FIO_PUNIT( FD, IOSTAT )
         CALL ERROUT( '\\', STATUS )
      END IF

 999  CONTINUE

      END
