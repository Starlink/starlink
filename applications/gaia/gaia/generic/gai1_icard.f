      SUBROUTINE GAI1_ICARD( CARD, NCARD, ICARD, BLOCK, STATUS )
*+
* Name:
*    GAI1_ICARD

*  Purpose:
*     Inserts a FITS card without checking.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAI1_ICARD( CARD, NCARD, ICARD, BLOCK, STATUS )

*  Description:
*     This routine writes a FITS card  into the "FITS block" by
*     inserting at the given position. No checking is made.

*  Arguments:
*     CARD = CHARACTER * ( * ) (Given)
*        The complete CARD to copy into the FITS block.
*     NCARD = INTEGER (Given)
*        The number of elements (cards) in BLOCK.
*     ICARD = INTEGER (Given)
*        The position to insert the card.
*     BLOCK( NCARD ) = CHARACTER * ( * ) (Given and Returned)
*        The FITS block (note this is passed at this point so that it is
*        before the other *(*) characters which allows this array to be
*        mapped -- see SUN/92).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  Authors:
*     PWD: Peter Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     05-DEC-2007 (PWD):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NCARD
      CHARACTER * ( * ) CARD
      INTEGER ICARD

*  Arguments Given and Returned:
      CHARACTER * ( * ) BLOCK( NCARD )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Insert the CARD.
      BLOCK( ICARD ) = CARD
      END
