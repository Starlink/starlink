      SUBROUTINE IRQ1_SEARC( LOCS, QNAME, FIXED, VALUE, BIT, COMMNT,
     :                       RDONLY, FIXBIT, SLOT, STATUS )
*+
*  Name:
*     IRQ1_SEARC

*  Purpose:
*     Search for a given quality name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_SEARC( LOCS, QNAME, FIXED, VALUE, BIT, COMMNT,
*                      RDONLY, FIXBIT, SLOT, STATUS )

*  Description:
*     The supplied name is compared with each name stored in the QUAL
*     array until a match is found or the last used slot is reached.
*     If the name is not found, an error is reported and STATUS returned
*     equal to IRQ__NOQNM.

*  Arguments:
*     LOCS( 5 ) = CHARACTER * ( * ) (Given)
*        A set of 5 HDS locators. LOCS( 1 ) locates a temporary
*        structure holding a cloned NDF identifier. LOCS(2) locates the
*        QUAL array. LOCS(3) locates the LAST_USED value, holding the
*        index of the last used slot in the QUAL array. LOCS(4) locates
*        the NFREE value, holding the number of free slots in the QUAL
*        array. LOCS(5) locates the FREE array, which contains a stack
*        of the NFREE slot indices corresponding to free slots. This
*        stack is accessed in a "First-In-Last-Out" method.
*     QNAME = CHARACTER * ( * ) (Given)
*        The quality name to search for.
*     FIXED = LOGICAL (Returned)
*        True if all or none of the pixels hold the quality. False is
*        some do and some don't.
*     VALUE = LOGICAL (Returned)
*        If FIXED is true and VALUE is true, then the quality is held
*        by all pixels by default.  If FIXED is true and VALUE is
*        false, then the quality is held by no pixels. If FIXED is
*        false, then VALUE is indeterminate.
*     BIT = INTEGER (Returned)
*        The bit number within the QUALITY component which corresponds
*        to the quality name. If the quality name has no corresponding
*        quality bit, then zero is returned.
*     COMMNT = CHARACTER * ( * ) (Returned)
*        A descriptive comment stored with the name.
*     RDONLY = LOGICAL (Returned)
*        The read-only flag for the quality name.
*     FIXBIT = LOGICAL (Returned)
*        A flag indicating if the bit number associated with the quality
*        name should never be changed (even if all pixels hold, or do not
*        hold, the quality).
*     SLOT = INTEGER (Returned)
*        The index at which the name is stored in the QUAL array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     26-JUL-1991 (DSB):
*        Original version.
*     15-FEB-2008 (DSB):
*        Added argument RDONLY.
*     4-MAR-2008 (DSB):
*        Cater for fixed-bit qualities.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Arguments Given:
      CHARACTER LOCS(5)*(*)
      CHARACTER QNAME*(*)

*  Arguments Returned:
      LOGICAL FIXED
      LOGICAL VALUE
      INTEGER BIT
      CHARACTER COMMNT*(*)
      LOGICAL RDONLY
      LOGICAL FIXBIT
      INTEGER SLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NAME*(IRQ__SZQNM)! The name stored in the current slot.
      LOGICAL THERE              ! True if the name is defined.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the slot containing the specified name.
      CALL IRQ1_CHECK( LOCS, QNAME, THERE, SLOT, STATUS )

*  If the name was found, get the other associated information.
      IF( THERE ) THEN
         CALL IRQ1_GET( LOCS, SLOT, NAME, FIXED, VALUE, BIT, COMMNT,
     :                  RDONLY, FIXBIT, STATUS )

*  If the name was not found, report an error.
      ELSE
         STATUS = IRQ__NOQNM
         CALL MSG_SETC( 'QN', QNAME )
         CALL ERR_REP( 'IRQ1_SEARC_ERR1',
     :                 'IRQ1_SEARC: Quality name ^QN not found.',
     :                 STATUS )
      END IF

      END
