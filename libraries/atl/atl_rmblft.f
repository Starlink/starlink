      SUBROUTINE ATL_RMBLFT( FC, STATUS )
*+
*  Name:
*     ATL_RMBLFT

*  Purpose:
*     Remove contiguous blanks from FITS header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_RMBLFT( FC, STATUS )

*  Description:
*     This routine removes contiguous blank lines from the FITS header.

*  Arguments:
*     FC1 = INTEGER (Given)
*        Pointer to the FitsChan to clean.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
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
*     6-JUL-2009 (TIMJ):
*        Original version moved from part of ATL_MGFTS
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
      INTEGER FC

*  Arguments Returned:

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CARD*80
      LOGICAL FLAG
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make sure we have a FITSCHAN
      IF ( FC .EQ. AST__NULL ) RETURN

*  Replace each contiguous group of
*  blank headers with a single blank header.
      FLAG = .FALSE.
      CALL AST_CLEAR( FC, 'CARD', STATUS )
      DO WHILE( AST_FINDFITS( FC, '%f', CARD, .TRUE., STATUS ) )
         IF( CARD .EQ. ' ' ) THEN
            IF( FLAG ) THEN
               FLAG = .FALSE.
            ELSE
*  The card we have just retrieved is BLANK and the one before it
*  was also blank. Since AST_FINDFITS increments the Card we have
*  to move it back one to delete that card.
               CALL AST_SETI( FC, 'CARD',
     :              AST_GETI( FC, 'CARD', STATUS ) - 1,
     :              STATUS )
               CALL AST_DELFITS( FC, STATUS )
            END IF
         ELSE
            FLAG = .TRUE.
         END IF
      END DO

      END
