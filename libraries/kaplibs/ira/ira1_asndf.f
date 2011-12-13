      SUBROUTINE IRA1_ASNDF( INDF, LOC, STATUS )
*+
*  Name:
*     IRA1_ASNDF

*  Purpose:
*     Create an empty astrometry structure within an NDF extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_ASNDF( INDF, LOC, STATUS )

*  Description:
*     This routine returns an HDS locator to an empty astrometry
*     structure (AS) stored in a given NDF. The AS is stored in an NDF
*     extension (identified by global variable ACM_XNAME) which must
*     already exist. An error is returned if the extension does not
*     exist. The name of the AS itself is held in global variable
*     ACM_ASNAME. Both extension name and AS name can be set up by
*     calling IRA_LOCAT. The default values (set up by IRA_INIT) are
*     an extension name of "IRAS", and an AS name of "ASTROMETRY".
*
*     If the NDF already contains an astrometry structure (at any
*     location) it is deleted before creating the new one.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     LOC = CHARACTER * ( * ) (Returned)
*        An HDS locator to the astrometry structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-FEB-1993 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_ASNAME = CHARACTER (Read)
*           Name of HDS object holding the astrometry structure.
*        ACM_XNAME = CHARACTER (Read)
*           Name of NDF extension holding the astrometry structure.

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ASNAME*(DAT__SZNAM)! HDS name of the astrometry structure
      LOGICAL EXISTS             ! True if an object exists.
      LOGICAL STRUC              ! True if an object is a structure
      CHARACTER XLOC*(DAT__SZLOC)! HDS locator to the NDF extension.
      CHARACTER XNAME*(DAT__SZNAM)! HDS name of the NDF extension.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the NDF already contains an astrometry structure.
      CALL IRA_FIND( INDF, EXISTS, XNAME, ASNAME, XLOC, STATUS )

*  If so, delete it.
      IF( EXISTS ) THEN
         CALL DAT_ERASE( XLOC, ASNAME, STATUS )
         CALL DAT_ANNUL( XLOC, STATUS )
      END IF

*  See if the NDF extension specified by IRA_LOCAT exists. If not,
*  report an error.
      CALL NDF_XSTAT( INDF, ACM_XNAME, EXISTS, STATUS )
      IF( .NOT. EXISTS .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRA__NOEXT
         CALL MSG_SETC( 'X', ACM_XNAME )
         CALL ERR_REP( 'IRA1_ASNDF_ERR1',
     :         'IRA1_ASNDF: NDF extension "^X" does not exist', STATUS )
         GO TO 999
      END IF

*  If it does, get a locator to the NDF extension.
      CALL NDF_XLOC( INDF, ACM_XNAME, 'UPDATE', XLOC, STATUS )

*  See if the extension is a structure. If not, report an error.
      CALL DAT_STRUC( XLOC, STRUC, STATUS )
      IF( .NOT. STRUC .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRA__BADAS
         CALL MSG_SETC( 'X', ACM_XNAME )
         CALL ERR_REP( 'IRA1_ASNDF_ERR2',
     : 'IRA1_ASNDF: NDF extension "^X" is a primative object and '//
     : 'therefore cannot hold an astrometry structure', STATUS )
         GO TO 999
      END IF

*  See if the named component exists within the extension.
      CALL DAT_THERE( XLOC, ACM_ASNAME, EXISTS, STATUS )

*  If so, delete it.
      IF( EXISTS ) CALL DAT_ERASE( XLOC, ACM_ASNAME, STATUS )

*  Create a new empty structure.
      CALL DAT_NEW( XLOC, ACM_ASNAME, IRA__HDSTY, 0, 0, STATUS )

*  Return a locator to it.
      CALL DAT_FIND( XLOC, ACM_ASNAME, LOC, STATUS )

*  Annull the locator to the extension.
      CALL DAT_ANNUL( XLOC, STATUS )

 999  CONTINUE

      END
