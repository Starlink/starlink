      SUBROUTINE USR_CMDREAD( STATUS )
*+
*  Name:
*     SUBROUTINE USR_CMDREAD

*  Purpose:
*     Reads commands from specified text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_CMDREAD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     26-SEP-94 (MJC):
*       IUEDR Vn. 3.1-5
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     Not yet implemented.
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMPSX'

*  Local Constants:
      INTEGER FILENAMESIZE       ! Maximum length of file name.
      PARAMETER ( FILENAMESIZE = 81 )

*  Status:
      INTEGER STATUS             ! Global status.

*  Local Variables:
      CHARACTER CFILE( FILENAMESIZE ) ! File name for open.
      BYTE FILE( FILENAMESIZE )  ! File name for parameter get.

      INTEGER FD                 ! File descriptor.
      INTEGER FIOSTAT
      INTEGER ACTVAL             ! Parameter value count.
      INTEGER NCHAR
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Not implemented message and Return.
      CALL line_WCONT( '%p CMDREAD is not yet implemented.\\' )
      CALL PRTBUF( STATUS )
      IF ( .TRUE. ) GO TO 999

*   Get SCRIPT parameter.
*      CALL RDPARC( 'SCRIPT\\', .FALSE., FILENAMESIZE, FILE,
*     :             ACTVAL, STATUS )
*      IF ( STATUS .NE. SAI__OK) THEN
*         CALL PARFER( 'SCRIPT\\', STATUS )
*         GO TO 999
*      ELSE
*         CALL CNPAR( 'SCRIPT\\', STATUS )
*         IF ( STATUS .NE. SAI__OK) THEN
*            CALL PCANER( 'SCRIPT\\', STATUS )
*            GO TO 999
*         END IF
*      END IF
*      CALL GEN_STOC( FILE, FILENAMESIZE, CFILE, NCHAR )
*
**   Get an I/O unit number.
*      CALL FIO_GUNIT( FD, STATUS )
*
**   Open the file.
*      OPEN( UNIT = FD, FILE = CFILE( :NCHAR ), ACCESS = 'SEQUENTIAL',
*     :      STATUS = 'OLD', IOSTAT = STATUS )
*      IF ( STATUS .NE. SAI__OK ) THEN
*          CALL ERROUT( 'Command script file: open error\\',
*     :                 STATUS )
*          STATUS = SAI__OK
*          GO TO 999
*      END IF
*
**   Read file line-by-line, break on an error.
*      DO WHILE ( .FALSE. )
*      END DO
*
**   Close the file and free the I/O unit.
*      CLOSE( FD )
*      FIOSTAT = SAI__OK
*      CALL FIO_PUNIT( FD, FIOSTAT )
*

 999  CONTINUE

      END
