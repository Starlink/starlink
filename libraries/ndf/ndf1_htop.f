      SUBROUTINE NDF1_HTOP( LOC1, MODE, LOC2, STATUS )
*+
* Name:
*    NDF1_HTOP

*  Purpose:
*     Return a top-level locator for an HDS container file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HTOP( LOC1, MODE, LOC2, STATUS )

*  Description:
*     The routine returns a top-level (secondary) locator for a
*     container file, given a locator for one of the objects within the
*     file.

*  Arguments:
*     LOC1 = CHARACTER * ( * ) (Given)
*        Locator to an object in the container file.
*     MODE = CHARACTER * ( * ) (Given)
*        Required mode of access: 'READ', 'UPDATE' or 'WRITE'.
*     LOC2 = CHARACTER * ( * ) (Returned)
*        Top-level (secondary) locator for the container file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     A value of DAT__NOLOC will be returned for the LOC2 argument if
*     this routine is called with STATUS set. The same value will also
*     be returned if it should fail for any reason.

*  Copyright:
*     Copyright (C) 1994 Particle Physics and Astronomy Research Council

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     28-APR-1994 (RFWS):
*        Original version.
*     4-MAY-1994 (RFWS):
*        Annul the returned locator on error.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ error codes
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      
*  Arguments Given:
      CHARACTER * ( * ) LOC1
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      CHARACTER * ( * ) LOC2
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZFIL ) FILE ! HDS container file name
      CHARACTER * ( NDF__SZPTH ) PATH ! HDS object path
      INTEGER NLEV               ! Object nesting level
      
*.

*  Set an initial null value for the returned locator.
      LOC2 = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      
*  Obtain the file and path names of the HDS object.
      CALL HDS_TRACE( LOC1, NLEV, PATH, FILE, STATUS )

*  Re-open the HDS container file to obtain a top-level locator.
      CALL HDS_OPEN( FILE, MODE, LOC2, STATUS )

*  Demote it to be a secondary locator.
      CALL DAT_PRMRY( .TRUE., LOC2, .FALSE., STATUS )

*  If an error occurred, then annul any locator which may have been
*  obtained.
      IF ( STATUS .NE. SAI__OK ) CALL DAT_ANNUL( LOC2, STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HTOP', STATUS )

      END
