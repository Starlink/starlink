      SUBROUTINE COF_FHIST( FUNIT, KEYNO, THERE, STATUS )
*+
*  Name:
*     COF_FHIST

*  Purpose:
*     Locates the next HISTORY card in a FITS header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_FHIST( FUNIT, KEYNO, THERE, STATUS )

*  Description:
*     This routine searches forward through a FITS header looking for
*     the next HISTORY card, starting from the KEYNOth card.  It
*     returns the index number of the HISTORY card, and a flag to
*     indicate whether or not a HISTORY card was found.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     KEYNO = INTEGER (Given and Returned)
*        On input, it is the index number within the FITS header of the
*        record where the search is to begin.  On exit it is the
*        index number of the next HISTORY record, unless THERE is
*        .FALSE..  KEYNO=1 is the first FITS card.
*     THERE = LOGICAL (Returned)
*        If .TRUE., a HISTORY record was located.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The FITS file must be open.

*  Copyright:
*     Copyright (C) 1997, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1997 March 6 (MJC):
*        Original version.
*     2004 September 10 (TIMJ):
*        Fix valgrind warning with uninitialised CARD on entry
*        to fitsio routine.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FUNIT

*  Arguments Given and Returned:
      INTEGER KEYNO

*  Arguments Returned:
      LOGICAL THERE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FITSOK             ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      CHARACTER * ( 80 ) CARD    ! FITS header card
      INTEGER FSTAT              ! FITSIO status
      INTEGER KEY                ! Keyword index
      INTEGER MAXKEY             ! Number of keywords in the header
      LOGICAL NOTHIS             ! HISTORY not found?

*.

*  Initialise the THERE flag.
      THERE = .FALSE.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Obtain the number of keywords in the header.
      CALL FTGHPS( FUNIT, MAXKEY, KEY, FSTAT )

*  Check for a FITSIO error.  Handle a bad status.  Negative values are
*  reserved for non-fatal warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_FHIST_ERR1', 'FTGHPS',
     :                   'Error finding number of FITS keywords.',
     :                   STATUS )
         GOTO 999
      END IF

*  Check that the the chosen key number is within range.
      KEY = MAX( 1, KEYNO )

*  Read each keyword in turn until HISTORY is found or the header is
*  exhausted of cards.
      NOTHIS = .TRUE.
  100 CONTINUE     ! Start of DO WHILE loop
      IF ( NOTHIS .AND. KEY .LE. MAXKEY ) THEN

*  Obtain the current card.
         CARD = ' '
         CALL FTGREC( FUNIT, KEY, CARD, FSTAT )

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

*  Check for a FITSIO error.  Handle a bad status.  Negative values are
*  reserved for non-fatal warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_FHIST_ERR', 'FTGREC',
     :                   'Error searching for the next HISTORY card '/
     :                   /'in the FITS headers', STATUS )
      END IF

  999 CONTINUE

      END
