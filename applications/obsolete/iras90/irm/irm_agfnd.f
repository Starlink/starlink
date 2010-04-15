      SUBROUTINE IRM_AGFND( NAME, PICID, STATUS )
*+
*  Name:
*     IRM_AGFND

*  Purpose:
*     Selects the highest picture of a given name within the current AGI
*     picture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_AGFND( NAME, PICID, STATUS )

*  Description:
*     This routine searches forwards through the AGI database for a
*     picture of a given name that lies within the current picture,
*     including the current picture itself.  If one is found it
*     becomes the new current picture.  If it could not be found a
*     bad status will be returned, and the current picture is unchanged.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the picture to be searched for in the graphics
*        database.
*     PICID = INTEGER (Returned)
*        The picture identifier of the most-recent picture named NAME.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 7 (MJC):
*        Original version.
*     18-JAN-1993 (DSB):
*        Name changed frm KPG1_AGFND to IRM_AGFND.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants.

*  Arguments Given:
      CHARACTER NAME*(*)

*  Arguments Returned:
      INTEGER PICID              ! Input picture identifier

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER PNAME*(DAT__SZNAM)! Name of the current picture.

      INTEGER PICIDC             ! Data image picture identifier
      INTEGER PICIDT             ! Work picture identifier

      LOGICAL LOBS               ! Value returned by AGI_IPOBS

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  At the moment, AGI sometimes uses cached information stored in
*  internal storage, rather than reading it from the database file.
*  This causes problems when two monoliths try to use the same
*  database file. What happens is that one monolith updates the
*  database, but the second monolith doesn't see the new state of
*  the database until the database file is read again for any reason.
*  The following call to AGI_IPOBS forces the IRAS90 monolith to update
*  its cached information by reading the database file.
      CALL AGI_IPOBS( -1, LOBS, STATUS )

*  Initialise the temporary and image picture identifications.
      PICIDT = -1
      PICID = -1

*  It could be the input picture.
      CALL AGI_INAME( PNAME, STATUS )
      CALL CHR_UCASE( PNAME )
      IF ( PNAME .NE. NAME ) THEN

*  Try to get the last related named picture from the database.
         CALL AGI_RCL( NAME, PICIDT, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  There is no picture of the chosen name in the current frame so
*  report the error context.
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'IRM_AGFND_ERR1',
     :   'IRM_AGFND: The current database picture is not of name '//
     :   '^NAME, or it does not contain a ^NAME picture within itself.',
     :        STATUS )
         ELSE
            PICID = PICIDT
         END IF

      ELSE

*  Obtain the current picture identifier.
         CALL AGI_ICURP( PICIDC, STATUS )

*  Use the current picture.
         PICID = PICIDC
      END IF

*  Select the picture of the chosen name to be the current database
*  picture.
      CALL AGI_SELP( PICID, STATUS )

      END
