************************************************************************

      SUBROUTINE AGI_1ODB ( STATUS )

*+
*  Name:
*     AGI_1ODB

*  Purpose:
*     Open the database.

*  Language:
*     VAX Fortran

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AGI_1ODB( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Open the databas

*  Algorithm:
*     Check status on entry.
*     Open the database file if present.
*     If file is not found then
*        Create a new database file.
*     Endif.

*  Authors:
*     Nick Eaton  ( DUVAD::NE )
*     {enter_new_authors_here}

*  History:
*     July 1988
*     July 1989  Read database locator from common block
*     July 1990  Call AGI_1FNAME and HDS_TUNE
*     Dec  1991  Removed call to HDS_TUNE
*     Mar  1992  Define file extension using AGI__ENAME parameter
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'agi_nam'


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'agi_locs'


*  Local Variables:
      LOGICAL FOUND

      CHARACTER FNAME * 64

      INTEGER LEXT, LNAME

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

*   Check if database is already there
         CALL AGI_1FDB( FOUND, STATUS )

*   If there is no database then create it
         IF ( .NOT. FOUND ) THEN

*   Inquire the database file name
            CALL AGI_1FNAME( FNAME, LNAME, STATUS )

*   Append the file extension
            LEXT = LEN( AGI__ENAME )
            FNAME( LNAME+1 : LNAME+LEXT ) = AGI__ENAME

*   Set up the HDS tuning parameter to wait if the file is being
*   accessed by another task
C            CALL HDS_TUNE( 'WAIT', .TRUE., STATUS )

*   Create the new file
            DABLOC = ' '
            CALL HDS_NEW( FNAME( :LNAME+LEXT ), AGI__DBNAM, AGI__DBTYP,
     :                    0, 0, DABLOC, STATUS )

*   Set the database locator valid flag and the flush flag
            LOCVAL = .TRUE.
            FLUSH = .TRUE.

         ENDIF

      ENDIF

*      print*, '+++++ AGI_1ODB +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

