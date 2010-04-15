      SUBROUTINE SPD_EABA( NDF, XTHERE, AXIS, STATUS )
*+
*  Name:
*     SPD_EABA

*  Purpose:
*     Get number of spectroscopic axis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EABA( NDF, XTHERE, AXIS, STATUS )

*  Description:
*     This routine returns for a given main NDF the number of the
*     spectroscopic axis. This is either stored in the Extension, or
*     defaults to 1.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XTHERE = LOCIGAL (Given)
*        True if the Extension exists.
*     AXIS = INTEGER (Returned)
*        The number of the spectroscopic axis.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set if the given NDF identifier is
*        invalid.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     07 May 1992 (hme):
*        Original version.
*     30 Jan 1995 (hme):
*        Used to set XTHERE false if the given NDF identifier was
*        invalid. That was a bug, since XTHERE is a given argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'           ! Specdre Extension constants

*  Arguments Given:
      INTEGER NDF
      LOGICAL XTHERE

*  Arguments Returned:
      INTEGER AXIS

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check given parameters.
      IF ( NDF .EQ. NDF__NOID ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INVNDF',
     :       'SPD_EABA: Error: Invalid NDF identifier.', STATUS )
         GO TO 500
      END IF

*  Default value.
      AXIS = 1

*  If Extension exists, look in there for the actual value.
      IF ( XTHERE ) CALL NDF_XGT0I( NDF, XNAME, XCMP1, AXIS, STATUS )

*  Return.
 500  CONTINUE
      END
