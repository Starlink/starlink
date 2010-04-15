      SUBROUTINE IRA_DROPS( ITEM, VALUE, STATUS )
*+
*  Name:
*     IRA_DROPS

*  Purpose:
*     Get the current value of a graphics option.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_DROPS( ITEM, VALUE, STATUS )

*  Description:
*     This routine returns the current value of a graphics option. See
*     routine IRA_DROPT for a description of these options.

*  Arguments:
*     ITEM = CHARACTER * ( * ) Given)
*        The name of the option (see the "Notes:" section below). An
*        unambiguous abbreviation may be supplied. Case is ignored.
*     VALUE = DOUBLE PRECISION (Returned)
*        The current value of the option.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-FEB-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_DROPT( IRA__NOPT ) = DOUBLE PRECISION (Read)
*           The graphics options values.

*  Arguments Given:
      CHARACTER ITEM*(*)

*  Arguments Returned:
      DOUBLE PRECISION VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IRA1_INIT         ! IRA common block initialisation.

*  Local Variables:
      INTEGER INDX               ! Index into the common options array.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the index at which the option is stored in the common array
*  ACM_DROPT.
      CALL IRA1_OPTID( ITEM, INDX, STATUS )

*  If all was OK, return the stored value.
      IF( STATUS .EQ. SAI__OK ) THEN
         VALUE = ACM_DROPT( INDX )

*  Otherwise, add a context message.
      ELSE
         CALL ERR_REP( 'IRA_DROPS_ERR1',
     : 'IRA_DROPS: Unable to get the current value of an astrometric '//
     : 'graphics option', STATUS )
      END IF

      END
