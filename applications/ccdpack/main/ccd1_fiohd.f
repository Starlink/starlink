      SUBROUTINE CCD1_FIOHD( FD, TITLE, STATUS )
*+
*  Name:
*     CCD1_FIOHD

*  Purpose:
*     Adds a descriptive header to (CCDPACK) position lists.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_FIOHD( FD, TITLE, STATUS )

*  Description:
*     This routine writes a descriptive header to an FIO file
*     which is being used as a position list in CCDPACK. The header
*     written is of the form.
*
*        #
*        #   TITLE         (i.e something like 'Output from IDICURS')
*        #
*        #   Written by USER on DATE.
*        #
*        #   Identifier   X-position   Y-position
*        #
*
*     The USER and DATE are replaced by values obtained from PSX.

*  Arguments:
*     FD = INTEGER (Given)
*        The FIO file descriptor.
*     TITLE = CHARACTER * ( * ) (Given)
*        The header title
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-JUL-1993 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Message system parameters

*  Arguments Given:
      INTEGER FD
      CHARACTER * ( * ) TITLE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( MSG__SZMSG ) MESS ! Output message string
      INTEGER MESLEN             ! Length of message string
      INTEGER NTICKS             ! Number of time ticks

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Add the title part to the file.
      CALL FIO_WRITE( FD, '#', STATUS )
      CALL MSG_SETC( 'TITLE', TITLE )
      MESS = ' '
      CALL MSG_LOAD( ' ', '#   ^TITLE', MESS, MESLEN, STATUS )
      CALL FIO_WRITE( FD, MESS( :MESLEN ), STATUS )
      CALL FIO_WRITE( FD, '#', STATUS )

*  Get the username from PSX.
      MESS = ' '
      CALL PSX_CUSERID( MESS, STATUS )
      CALL MSG_SETC( 'USER', MESS )

*  Get the date from PSX convert this to a string.
      CALL PSX_TIME( NTICKS, STATUS )
      MESS = ' '
      CALL PSX_CTIME( NTICKS, MESS, STATUS )
      CALL MSG_SETC( 'DATE', MESS )

*  Construct user time and date string
      CALL MSG_LOAD( ' ', '#   Written by ^USER on ^DATE.', MESS,
     :               MESLEN, STATUS )
      CALL FIO_WRITE( FD, MESS( : MESLEN ), STATUS )
      CALL FIO_WRITE( FD, '#', STATUS )

*  Add description of contents.
      CALL FIO_WRITE( FD, '#   Identifier   X-position   Y-position',
     :                STATUS )
      CALL FIO_WRITE( FD, '#', STATUS )

      END
* $Id$
