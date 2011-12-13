      SUBROUTINE ATL_FSPEC( FRM, SPAX, SPFRM, STATUS )
*+
*  Name:
*     ATL_FSPEC

*  Purpose:
*     Locate a SpecFrame within a CmpFrame.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_FSPEC( FRM, SPAX, SPFRM, STATUS )

*  Description:
*     This routine searches the supplied CmpFrame for an axis that is a
*     SpecFrame. It returns the axis index of the SpecFrame within the
*     CmpFrame, and also returns a pointer to the SpecFrame itself.
*
*     No error is reported if the CmpFrame does not contain a SpecFrame.
*     If the CmpFrame contains more than one SpecFrame, the first (i.e.
*     the lowest index) is returned.

*  Arguments:
*     FRM= INTEGER (Given)
*        The CmpFrame pointer.
*     SPAX = INTEGER (Returned)
*        The index of the spectral axis within the CmpFrame. Returned
*        equal to zero if no spectral axis is found.
*     SPFRM = INTEGER (Returned)
*        A pointer to the SpecFrame. Returned equal to AST__NULL if no
*        spectral axis is found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     1-JUN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER FRM

*  Arguments Returned:
      INTEGER SPAX
      INTEGER SPFRM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER NAX
      INTEGER MAP
*.

*  Initialise.
      SPAX = 0
      SPFRM = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

* Loop round all axes in the CmpFrame
      NAX = AST_GETI( FRM, 'Naxes', STATUS )
      DO I = 1, NAX

*  Skip if we have already found a SpecFrame.
         IF( SPAX .EQ. 0 ) THEN

*  Get a Frame containing just this one axis.
            SPFRM = AST_PICKAXES( FRM, 1, I, MAP, STATUS )

*  If it is SpecFrame, note its index. Otherwise, annnul the Frame
*  pointer.
            IF( AST_ISASPECFRAME( SPFRM, STATUS ) ) THEN
               SPAX = I
            ELSE
               CALL AST_ANNUL( SPFRM, STATUS )
            END IF

*  Annul the unneeded Mapping.
            CALL AST_ANNUL( MAP, STATUS )

         END IF

      END DO

      END
