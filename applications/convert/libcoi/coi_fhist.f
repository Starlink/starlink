      SUBROUTINE COI_FHIST( IMDESC, NHEAD, KEYNO, THERE, STATUS )
*+
*  Name:
*     COI_FHIST

*  Purpose:
*     Locates the next HISTORY card in an IRAF header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_FHIST( IMDESC, NHEAD, KEYNO, THERE, STATUS )

*  Description:
*     This routine searches forward through an IRAF OIF header looking
*     for the next HISTORY card, starting from the KEYNOth card.  It
*     returns the index number of the HISTORY card, and a flag to
*     indicate whether or not a HISTORY card was found.

*  Arguments:
*     IMDESC = INTEGER (Given)
*        The IMFORT file descriptor.
*     NHEAD = INTEGER (Given)
*        Number of IRAF headers.
*     KEYNO = INTEGER (Given and Returned)
*        On input, it is the index number within the IRAF header of the
*        record where the search is to begin.  On exit it is the
*        index number of the next HISTORY record, unless THERE is
*        .FALSE..  KEYNO=1 is the first IRAF header card.
*     THERE = LOGICAL (Returned)
*        If .TRUE., a HISTORY record was located.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The IRAF file must be open.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 July 23 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER IMDESC
      INTEGER NHEAD

*  Arguments Given and Returned:
      INTEGER KEYNO

*  Arguments Returned:
      LOGICAL THERE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) CARD    ! IRAF header card
      INTEGER KEY                ! Keyword index
      LOGICAL NOTHIS             ! HISTORY not found?

*.

*  Initialise the THERE flag.
      THERE = .FALSE.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the the chosen key number is within range.
      KEY = MIN( NHEAD, MAX( 1, KEYNO ) )

*  Read each keyword in turn until HISTORY is found or the header is
*  exhausted of cards.
      NOTHIS = .TRUE.
  100 CONTINUE     ! Start of DO WHILE loop
      IF ( NOTHIS .AND. KEY .LE. NHEAD ) THEN

*  Obtain the current card.
         CALL GETLIN( IMDESC, KEY, CARD )

*  Is this a HISTORY card.
         IF ( CARD( 1:8 ) .EQ. 'HISTORY' )  THEN
            NOTHIS = .FALSE.

*  Assign returned values.
            THERE = .TRUE.
            KEYNO = KEY

*  Skip to the next key.
         ELSE
            KEY = KEY + 1
         END IF

*  End of DO WHILE loop.
         GOTO 100
      END IF

      END
