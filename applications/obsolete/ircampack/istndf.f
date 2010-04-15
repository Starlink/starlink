      SUBROUTINE ISTNDF( PARAM, LOG, FD, STATUS )
*+
*  Name:
*     ISTNDF

*  Purpose:
*     Display the name of an NDF associated with an IRCAM global
*     parameter

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ISTNDF( PARAM, LOG, FD, STATUS )

*  Description:
*     An attempt is made to obtain the name of an existing NDF using the
*     supplied parameter (which should be associated with an IRCAMPACK
*     global parameter). If an NDF is succesfully obtained, its name is
*     displayed, and optionally logged to a file. If no existing NDF was
*     obtained, the word "undefined" is used instead.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter.
*     LOG = LOGICAL (Given)
*        If .TRUE. then log the NDF name.
*     FD = INTEGER (Given)
*        The FIO file descriptor for the log file. Ignored if LOG is
*        .FALSE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error messages

*  Arguments Given:
      CHARACTER PARAM*(*)
      LOGICAL LOG
      INTEGER FD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        NDF*255            ! Name of NDF

      INTEGER
     :        INDF,              ! NDF identifier
     :        NDFLEN             ! No. of characters in NDF name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the NDF.
      CALL NDF_EXIST( PARAM, 'READ', INDF, STATUS )

*  If an error occurrred (other than a parameter abort), annul the error.
      IF( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If no NDF has been obtained, report an undefined NDF.
      IF( INDF .EQ. NDF__NOID ) THEN
         CALL MSG_OUT( 'ISTNDF_MSG1', 'undefined', STATUS )
         IF( LOG ) CALL FIO_WRITE( FD, 'undefined', STATUS )

*  Otherwise, get the name of the NDF, display it and optionally log it.
      ELSE
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_LOAD( ' ', '^NDF', NDF, NDFLEN, STATUS )
         CALL MSG_OUT( 'ISTNDF_MSG2', NDF( : NDFLEN ), STATUS )
         IF( LOG ) CALL FIO_WRITE( FD, NDF( : NDFLEN ), STATUS )

*  Annul the NDF identifier.
         CALL NDF_ANNUL( INDF, STATUS )

      END IF

      END
