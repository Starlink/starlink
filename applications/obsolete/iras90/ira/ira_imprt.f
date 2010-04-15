      SUBROUTINE IRA_IMPRT( INDF, IDA, STATUS )
*+
*  Name:
*     IRA_IMPRT

*  Purpose:
*     Get an identifier for astrometry information stored in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_IMPRT( INDF, IDA, STATUS )

*  Description:
*     A search is made for an astrometry structure through all the
*     extensions in the supplied NDF. If one is found, the astrometry
*     information is copied into internal common blocks and an "IRA
*     identifier" is returned which can be passed to other IRA routines
*     to refer to the stored astrometry information.  This identifier
*     should be annulled when it is no longer required by calling
*     IRA_ANNUL. An error is reported if no astrometry structure is
*     found in the NDF.

*  Arguments:
*     INDF = INTEGER (Given)
*        The identifier for the NDF containing the astrometry
*        information
*     IDA = INTEGER (Returned)
*        The IRA identifier which is used by other IRA routines to
*        access the astrometry information copied from the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JAN-1991 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA version 2.
*     20-AUG-1992 (DSB):
*        Modified to call IRA_READ to do most of the work.
*     11-FEB-1993 (DSB):
*        Changed to remove storage of locators in common, and to search
*        through all extensions for an astrometry structure.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants.
      INCLUDE 'IRA_ERR'          ! IRA_ error constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_STATE = CHARACTER (Read)
*           Set to the value of symbolic constant IRA__GOING if IRA has
*           been initialised.

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      INTEGER IDA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :          ASNAME*(DAT__SZNAM),! Name of the AS
     :          LOC*(DAT__SZLOC), ! HDS locator for the AS.
     :          XLOC*(DAT__SZLOC),! HDS locator to the NDF extension.
     :          XNAME*(DAT__SZNAM)! Name of the extension

      LOGICAL
     :          THERE             ! True if an AS was found.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that IRA has been initialised.
      IF( ACM_STATE .NE. IRA__GOING ) THEN
         STATUS = IRA__INIT
         CALL ERR_REP( 'IRA_IMPRT_ERR1',
     :             'IRA_IMPRT: The IRAS90 astrometry system has not '//
     :             'been initialised', STATUS )
      END IF

*  Obtain an HDS locator to the NDF extension holding the astrometry
*  structure.
      CALL IRA_FIND( INDF, THERE, XNAME, ASNAME, XLOC, STATUS )

*  If no astrometry structure was found, report an error.
      IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRA__NOAS
         CALL ERR_REP( 'IRA_IMPRT_ERR2',
     :'IRA_IMPRT: Unable to find any astrometry information in the NDF',
     :                 STATUS )
      END IF

*  Get a locator to the astrometry structure.
      CALL DAT_FIND( XLOC, ASNAME, LOC, STATUS )

*  Call IRA_READ to copy the astrometry information from the astrometry
*  structure into internal common arrays.
      CALL IRA_READ( LOC, IDA, STATUS )

*  Annul the locators.
      CALL DAT_ANNUL( LOC, STATUS )
      CALL DAT_ANNUL( XLOC, STATUS )

*  If an error occurred, give the context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'IRA_IMPRT_ERR3',
     :     'IRA_IMPRT: Unable to import astrometry information from '//
     :     '^NDF', STATUS )
      END IF

      END
