      SUBROUTINE CON_PCARD( CARD, LINENO, EL, CARDAR, STATUS )
*+
*  Name:
*     CON_PCARD

*  Purpose:
*     Puts a card image into an array of card images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_PCARD( CARD, LINENO, EL, CARDAR, STATUS )

*  Description:
*     This copies an 80-character `card image' into an array of card
*     images at a specified element.  It is most likely use is to
*     write FITS headers.

*  Arguments:
*     CARD = CHARACTER * ( 80 ) (Given)
*        The CARD to be copied.
*     LINENO = INTEGER (Given)
*        The index that this line will have in the CARDAR array.  It
*        must have a value between 1 and EL.
*     EL = INTEGER (Given)
*        The size of CARDAR.
*     CARDAR( EL ) = CHARACTER * ( 80 ) (Given and Returned)
*        The array of card images. On exit it will contain CARD at
*        element LINENO provided LINENO is within its bounds.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-NOV-1992 (RAHM):
*        Original version.
*     1993 July 22 (MJC):
*        Tidied, reordered the arguments, validated the input line
*        number, and renamed from PUTLIN.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER LINENO
      CHARACTER * 80 CARD
      INTEGER EL

*  Arguments Given and Returned:
      CHARACTER * 80 CARDAR( EL )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the line number.
      IF ( LINENO .GE. 1 .AND. LINENO .LE. EL ) THEN

*  Initialise CARD.
         CARDAR( LINENO ) = ' '
         CALL CHR_MOVE( CARD, CARDAR( LINENO ) )

*  Make an error report.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'L', LINENO )
         CALL MSG_SETI( 'EL', EL )
         CALL ERR_REP( 'CON_PCARD_BOUNDS',
     :    'The chosen record (^L) lies outside the bounds (1 to ^EL) '/
     :    /'of the card-image array.', STATUS )
      END IF

      END
