      SUBROUTINE IRQ_RESQM( LOCS, BAD, QNAME, SIZE, MASK, SET, STATUS )
*+
*  Name:
*     IRQ_RESQM

*  Purpose:
*     Remove a quality from pixels selected using a mask image, leaving
*     unselected pixels unchanged.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_RESQM( LOCS, BAD, QNAME, SIZE, MASK, SET, STATUS )

*  Description:
*     The quality specified by QNAME is removed from all NDF pixels
*     which either do (or, if BAD is false, do not) correspond to `bad'
*     pixels in the input mask array.  The quality of all other pixels
*     is left unchanged.  The quality name must be defined in the NDF
*     specified by LOCS (LOCS should be obtained either by calling
*     IRQ_FIND or IRQ_NEW). An error is reported if the quality name is
*     undefined.
*
*     Note, write or update access must be available for the NDF (as
*     set up by routine LPG_ASSOC for instance), and the QUALITY
*     component of the NDF must not be mapped on entry to this routine.
*
*  Arguments:
*     LOCS(5) = CHARACTER * ( * ) (Given)
*        An array of five HDS locators. These locators identify the NDF
*        and the associated quality name information.  They should have
*        been obtained using routine IRQ_FIND or routine IRQ_NEW.
*     BAD = LOGICAL (Given)
*        If true, then the quality is removed from all NDF pixels
*        corresponding to `bad' pixels in the mask.  If false, then the
*        quality is removed from all NDF pixels corresponding to pixels
*        which are not `bad' in the mask.
*     QNAME = CHARACTER * ( * ) (Given)
*        The quality name to be removed from the selected pixels. This
*        quality name must be defined in the NDF specified by LOC. Name
*        definitions can be added to the NDF using routine IRQ_ADDQN.
*     SIZE = INTEGER (Given)
*        The total number of pixels in the MASK array.
*     MASK( SIZE ) = REAL (Given)
*        A vector which defines the pixels from which the quality
*        specified by QNAME is to be removed.  It is assumed that this
*        vector corresponds pixel-for-pixel with the vectorised NDF as
*        supplied to routine IRQ_FIND or IRQ_NEW.
*     SET = INTEGER (Returned)
*        The number of pixels in the NDF which hold the quality.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-JUL-1991 (DSB):
*        Original version.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     15-FEB-2008 (DSB):
*        Added RDONLY to IRQ1_SEARC and IRQ1_MOD call.
*     4-MAR-2008 (DSB):
*        Cater for fixed bit quality names.
*     24-OCT-2019 (DSB):
*        This routine is now a wrapper around IRQ_RESQM8.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Arguments Given:
      CHARACTER LOCS*(*)
      LOGICAL BAD
      CHARACTER QNAME*(*)
      INTEGER SIZE
      REAL MASK( SIZE )

*  Arguments Returned:
      INTEGER SET

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*8 SIZE8
      INTEGER*8 SET8
*.

      SIZE8 = SIZE
      CALL IRQ_RESQM8( LOCS, BAD, QNAME, SIZE8, MASK, SET8, STATUS )

      SET = SET8
      IF( SET .NE. SET8 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRQ__OVFLW
         CALL ERR_REP( ' ', 'IRQ_RESQM: Return value (SET) too large '//
     :                 'for 4-byte integer.', STATUS )
      END IF

      END
