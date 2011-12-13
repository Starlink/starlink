      SUBROUTINE IRQ_CHKQN( LOCS, QNAME, THERE, STATUS )
*+
*  Name:
*     IRQ_CHKQN

*  Purpose:
*     Check a specified quality name to see if it is defined.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_CHKQN( LOCS, QNAME, THERE, STATUS )

*  Description:
*     This routine searches for a specified quality name in the quality
*     name specified by LOCS. If it is found, THERE is returned true.
*     Otherwise THERE is returned false.

*  Arguments:
*     LOCS(5) = CHARACTER * ( * ) (Given)
*        An array of five HDS locators. These locators identify the NDF
*        and the associated quality name information.  They should have
*        been obtained using routine IRQ_FIND or routine IRQ_NEW.
*     QNAME = CHARACTER * ( * ) (Given)
*        The quality name to search for. Leading blanks are ignored and
*        the search is case-insensitive. The maximum allowed length for
*        quality names is given by symbolic constant IRQ__SZQNM which
*        currently has the value of 15.
*     THERE = LOGICAL (Returned)
*        If true, then the quality name is defined within the NDF
*        specified by LOCS. If false, then the quality name is
*        undefined.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     25-JUL-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.

*  Arguments Given:
      CHARACTER LOCS(5)*(*)
      CHARACTER QNAME*(*)

*  Arguments returned:
      LOGICAL THERE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FIRST              ! Position of first non-blank character
      INTEGER INDF               ! Identifier for the NDF containing the
                                 ! quality names information.
      INTEGER LAST               ! Position of last non-blank character.
      CHARACTER LQNAME*(IRQ__SZQNM) ! Upper case copy of quality name.
      INTEGER SLOT               ! Slot at which the name was stored.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF identifier from LOCS, and check it is still valid.
      CALL IRQ1_INDF( LOCS, INDF, STATUS )

*  Produce an uppercase copy of the supplied quality name, exluding
*  leading blanks.
      CALL CHR_FANDL( QNAME, FIRST, LAST )
      LQNAME = QNAME( FIRST : LAST )
      CALL CHR_UCASE( LQNAME )

*  Check the requested quality name.
      CALL IRQ1_CHECK( LOCS, LQNAME( : LAST - FIRST + 1 ), THERE,
     :                 SLOT, STATUS )

*  If an error occur, give context information.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_SETC( 'QN', QNAME )
         CALL ERR_REP( 'IRQ_CHKQN_ERR1',
     :'IRQ_CHKQN: Unable to check for quality name ^QN in NDF ^NDF',
     :       STATUS )
      END IF

      END
