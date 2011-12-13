      SUBROUTINE FTS1_COMNT( NCARD, BUFFER, STCARD, THERE, VALUE, CARD,
     :                       STATUS )
*+
*  Name:
*     FTS1_COMNT

*  Purpose:
*     Gets the value of a FITS COMMENT card from a buffer of cards.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_COMNT( NCARD, BUFFER, STCARD, THERE, VALUE, CARD,
*    :                 STATUS )

*  Description:
*     This routine searches a buffer containing the header card images
*     from a FITS file for the next card with keyword COMMENT.  The
*     search begins at a defined card image; and ends when the next end
*     of a header block, marked by the END keyword, is encountered or
*     the buffer is exhausted.  The routine returns the comment string,
*     and the number of the card image within the buffer array that
*     contains the comment.  If the keyword is is present %THERE is
*     true, otherwise it is false.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the buffer.
*     BUFFER( NCARD ) = CHARACTER * ( * ) (Given)
*        The buffer containing the header card images.
*     STCARD = INTEGER (Given)
*        The number of the card from which to search for the next
*        comment card
*     THERE = LOGICAL (Returned)
*        If true the parameter %NAME is present.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The comment string of the first COMMENT keyword found at or
*        after %STCARD.  The length should be at least 72 characters.
*     CARD = INTEGER(Returned)
*        The number of the card containing the first COMMENT card.  If
*        no COMMENT card could be found this is returned with a value of
*        zero.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Initialise counter and flag
*     For all cards until the last card, or the requested card
*        If card contains the required parameter then
*           Set flag to say parameter has been found
*           Define search limits for trailing quote
*           Extract character string from between the quotes in the
*             buffer
*        Else
*           Increment number of header records
*        Endif
*     Endfor
*     Reset card number to zero if the keyword is not present
*     End

*  Copyright:
*     Copyright (C) 1989, 1990, 1991 Science & Engineering Research
*                   Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1989 Jul 31 (MJC):
*        Original (RAL::CUR).
*     1990 November 19 (MJC):
*        Renamed from FITSCM and converted to the SST style prologue.
*     1991 February 28 (MJC):
*        Converted BUFFER from an assumed-size to an adjustable array
*        via the NCARD argument in case the END keyword is missing.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE           ! no default typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'       ! SSE global definitions

*  Arguments Given:
      INTEGER
     :  NCARD,                 ! Number of FITS card images to search
     :  STCARD                 ! Number of the card from which to search
                               ! for the next comment card

      CHARACTER * ( * )
     :  BUFFER( NCARD )        ! FITS tape buffer

*  Arguments Returned:
      LOGICAL                  ! True if:
     :  THERE                  ! Card containing the parameter is
                               ! present

      CHARACTER * ( * )
     :  VALUE                  ! Value of the parameter

      INTEGER
     :  CARD                   ! Number of the card image containing
                               ! keyword "COMMENT"

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER
     :  NC                     ! Number of characters in NAME

*.


*    Check for error on entry.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise some variables.

      CARD = MAX( 1, STCARD )
      THERE = .FALSE.
      NC = 7
      VALUE = ' '

*    Now loop through the cards ('END' terminates header)

      DO WHILE ( .NOT. THERE .AND. CARD .LE. NCARD .AND.
     :            BUFFER( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' )

*       Is current card the required parameter?

         IF ( BUFFER( CARD )( :NC ) .EQ. 'COMMENT' ) THEN

*          Parameter is present.

            THERE = .TRUE.

*          Extract string from buffer removing leading blanks.

            VALUE = BUFFER( CARD )( 9:80 )
            CALL CHR_LDBLK( VALUE )

         ELSE

*          Onto the next card in the buffer.

            CARD = CARD + 1
         END IF
      END DO

      IF ( .NOT. THERE ) CARD = 0

      END
