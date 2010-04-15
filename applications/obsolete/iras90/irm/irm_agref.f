      SUBROUTINE IRM_AGREF( PICID, ACCESS, THERE, LOC, STATUS )
*+
*  Name:
*     IRM_AGREF

*  Purpose:
*     Obtains a locator to an object referenced in the graphics
*     database.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_AGREF( PICID, ACCESS, THERE, LOC, STATUS )

*  Description:
*     This routine determines whether a given picture in the AGI
*     graphics database has an object associated with it by reference.
*     If it has, a locator to the object is returned with the desired
*     access mode.

*  Arguments:
*     PICID = INTEGER (Given)
*        The identifier of the picture with which a data object may
*        be associated by reference.
*     ACCESS = CHARACTER * ( * ) (Given)
*        Access mode to the object: 'READ', 'WRITE' or 'UPDATE'.
*     THERE = LOGICAL (Returned)
*        If true the picture has an associated object and the returned
*        locator is meaningful.
*     LOC = CHARACTER * ( DAT__SZLOC ) (Returned)
*        The locator to the data object referenced by picture PICID.
*        It should be ignored if THERE is false.  This locator should
*        be annulled by REF_ANNUL.
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
*        Name changed from KPG1_AGREF to IRM_AGREF.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants

*  Arguments Given:
      INTEGER PICID

      CHARACTER ACCESS*(*)

*  Arguments Returned:
      LOGICAL THERE

      CHARACTER LOC*(DAT__SZLOC)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new error context.
      CALL ERR_MARK

*  Initialise the flag.
      THERE = .FALSE.

*  Get a locator to the NDF associated with the DATA picture.
      CALL AGI_GTREF( PICID, ACCESS, LOC, STATUS )

*  Check the status to determine whether or not a locator was found.
      IF ( STATUS .NE. SAI__OK ) THEN

*  No, but since we can ask for an input NDF the error can be handled
*  transparently.
         CALL ERR_ANNUL( STATUS )

      ELSE

*  Record that the locator was found by reference.  It may be needed to
*  avoid obtaining the object again, and will be required to annul the
*  locator via REF.
         THERE = .TRUE.

      END IF

*  Release the error context.
      CALL ERR_RLSE

      END
