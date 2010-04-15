      SUBROUTINE IRM_COMNT( NCARD, BUFFER, STCARD, THERE, VALUE, CARD,
     :                      STATUS )
*+
*  Name:
*     IRM_COMNT

*  Purpose:
*     Get the value of a FITS COMMENT card from a buffer of cards.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_COMNT( NCARD, BUFFER, STCARD, THERE, VALUE, CARD,
*                     STATUS )

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
*        The number of the card containing the first COMMENT card. If
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

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-DEC-1992 (DSB):
*        Copied from KAPPA routine FTS1_COMNT, written by MJC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE           ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'       ! SSE global definitions

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

*  Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise some variables.
      CARD = MAX( 1, STCARD )
      THERE = .FALSE.
      NC = 7
      VALUE = ' '

*  Now loop through the cards ('END' terminates header)
      DO WHILE ( .NOT. THERE .AND. CARD .LE. NCARD .AND.
     :            BUFFER( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' )

*  Is current card the required parameter?
         IF ( BUFFER( CARD )( :NC ) .EQ. 'COMMENT' ) THEN

*  Parameter is present.
            THERE = .TRUE.

*  Extract string from buffer removing leading blanks.
            VALUE = BUFFER( CARD )( 9:80 )
            CALL CHR_LDBLK( VALUE )

         ELSE

*  Onto the next card in the buffer.
            CARD = CARD + 1
         END IF
      END DO

      IF ( .NOT. THERE ) CARD = 0

      END
