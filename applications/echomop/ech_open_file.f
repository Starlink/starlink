      SUBROUTINE ECH_OPEN_FILE(
     :           FILE_NAME,
     :           TYPE,
     :           EXIST,
     :           WRITABLE,
     :           LUN,
     :           OPENED_NAME,
     :           STATUS
     :          )
*+
*  Name:
*     ECHMOP - ECH_OPEN_FILE

*  Purpose:
*     Open or close a file for FORTRAN I/O access.

*  Description:
*     Despite its name, this subroutine opens or closes a file.
*
*     On opening, the file specification given is tried first.
*     If this fails an open on the same file in the $ADAM_USER directory
*     is attempted, followed by a final attempt to open the file in the
*     $EXCOMOP_EXEC directory.

*  Invocation:
*     CALL ECH_OPEN_FILE(
*    :     FILE_NAME,
*    :     TYPE,
*    :     EXIST,
*    :     WRITABLE,
*    :     LUN,
*    :     OPENED_NAME,
*    :     STATUS
*    :    )

*  Arguments:
*     FILE_NAME = CHARACTER*( * ) (Given)
*        Name of the file to be opened.
*     TYPE = CHARACTER*( * ) (Given)
*        Type of file to be opened.
*     EXIST = CHARACTER*( * ) (Given)
*        Whether the file should exist or not.
*     WRITABLE = LOGICAL (Given)
*        Whether the file should be writable or not.
*     LUN = INTEGER (Given or Returned)
*        I/O unit associated with the file.
*     OPENED_NAME = CHARACTER*( * ) (Returned)
*        Full name of the file opened.
*     STATUS = INTEGER (Given and Returned)
*        Global inerited status.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     ??-???-???? (DMILLS):
*       Initial release.
*     22-APR-1996 (MJC):
*       Prologue added - simplified file open, flush FIO errors.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*( * ) FILE_NAME
      CHARACTER*( * ) TYPE
      CHARACTER*( * ) EXIST
      LOGICAL WRITABLE

*  Arguments Given or Returned:
      INTEGER LUN

*  Arguments Returned:
      CHARACTER*( * ) OPENED_NAME

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER FFDN( 300 )
      INTEGER FDN
      INTEGER LLEN
      INTEGER ISTAT

      LOGICAL FTYP( 300 )

      CHARACTER*255 LFILE_NAME
      CHARACTER*16 FORM
      CHARACTER*6 ACCESS
*.

      STATUS = SAI__OK
      LFILE_NAME = FILE_NAME
      CALL ECH_PARSE_ENV( LFILE_NAME, LLEN )

*  Open a file.
      IF ( TYPE .EQ. 'TEXT' .OR. TYPE .EQ. 'UNFORMATTED' ) THEN

*     Determine required access format.
         IF ( TYPE .EQ. 'TEXT' ) THEN
            FORM = 'NONE'

         ELSE
            FORM = 'UNFORMATTED'
         END IF

*     Determine required access mode.
         IF ( EXIST .EQ. 'NEW' ) THEN
            ACCESS = 'WRITE'

         ELSE IF ( EXIST .EQ. 'OLD' .AND. WRITABLE ) THEN
            ACCESS = 'UPDATE'

         ELSE
            ACCESS = 'READ'
         END IF

*     Try to open the file as specified.
         IF ( ACCESS .EQ. 'WRITE' .AND. TYPE .EQ. 'TEXT' ) THEN
            CALL FIO_OPEN( LFILE_NAME, ACCESS, 'LIST', 0, FDN, STATUS )

         ELSE
            CALL FIO_OPEN( LFILE_NAME, ACCESS, FORM, 0, FDN, STATUS )
         END IF

*     Now try to open the file in the $ADAM_USER directory.
         FORM = 'NONE'
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            LFILE_NAME = 'ADAM_USER:' // FILE_NAME
            CALL ECH_PARSE_ENV( LFILE_NAME, LLEN )
            CALL FIO_OPEN( LFILE_NAME, ACCESS, FORM, 0, FDN, STATUS )
         END IF

*     Lastly, try to open the file in the $ECHOMOP_EXEC directory.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            LFILE_NAME = 'ECHOMOP_EXEC:' // FILE_NAME
            CALL ECH_PARSE_ENV( LFILE_NAME, LLEN )
            CALL FIO_OPEN( LFILE_NAME, ACCESS, FORM, 0, FDN, STATUS )
         END IF

*     If the file has been opened setup and save I/O unit.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL FIO_FNAME( FDN, OPENED_NAME, STATUS )
            CALL FIO_UNIT( FDN, LUN, STATUS )
            FFDN( LUN ) = FDN
            FTYP( LUN ) = .TRUE.

*     Warn that the file could not be opened.
         ELSE
            ISTAT = STATUS
            CALL ERR_FLUSH( STATUS )
            STATUS = ISTAT
         END IF

*  Close a file.
      ELSE IF ( TYPE .EQ. 'CLOSE' ) THEN
         IF ( FTYP( LUN ) ) THEN
            CALL FIO_CLOSE( FFDN( LUN ), STATUS )

         ELSE
            CALL FIO_CLOSE( FFDN( LUN ), STATUS )
         END IF
         FFDN( LUN ) = 0
      END IF

      END
