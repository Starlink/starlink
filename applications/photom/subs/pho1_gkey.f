      SUBROUTINE PHO1_GKEY( NCARD, BUFFER, NAME, THERE, VALUE, STATUS )
*+
*  Name:
*     PHO1_GKEY

*  Purpose:
*     Gets the value of a floating point FITS keyword.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PHO1_GKEY( NCARD, BUFFER, NAME, THERE, VALUE, STATUS )

*  Description:
*     This routine searches a buffer containing the header card images
*     from a FITS extension for a keyword NAME; and returns its value,
*     and the number of the card image within the buffer array that
*     contains the named keyword.  The search ends when the next end
*     of a header block, marked by the END keyword, is encountered
*     or the buffer is exhausted.  If the keyword is present THERE
*     is true, otherwise it is false. An error status will be returned
*     if the conversion to floating point fails. If a keyword is not
*     found then no error results and the argument VALUE remains
*     unmodified.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the buffer.
*     BUFFER( NCARD ) = CHARACTER * 80 (Given)
*        The buffer containing the header card images.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the keyword whose value is required.
*     THERE = LOGICAL (Returned)
*        If true the keyword NAME is present (regardless of exit
*        status).
*     VALUE = REAL (Returned)
*        The value of the keyword.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Authors:
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     16-MAY-1998 (PWD)
*        Original version. Based on IMG_ routine.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE           ! no default typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'       ! SSE global definitions

*  Arguments Given:
      INTEGER NCARD
      CHARACTER * ( * ) BUFFER( NCARD )
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      LOGICAL THERE
      REAL VALUE

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Number of characters in a string
                                ! ignoring trailing blanks

*  Local Variables:
      CHARACTER * ( 60 ) CHRVAL ! The value in characters
      CHARACTER * ( 80 ) KEYWRD ! The keyword in upper case
      INTEGER NF                ! Number of occurrences found
      INTEGER NC                ! Number of characters
      INTEGER NCHV              ! Number of characters in the value
      INTEGER CARD              ! Number of CARDS checked

*.


*  Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise some variables.
      CARD = 1
      THERE = .FALSE.
      NF = 0

*  Remove blanks from the keyword to be searched for, and make it
*  uppercase for comparisons.  Find its effective length.
      KEYWRD = NAME
      CALL CHR_UCASE( KEYWRD )
      CALL CHR_RMBLK( KEYWRD )
      NC = CHR_LEN( KEYWRD )

*  Name is limited to 8 characters.
      NC = MIN( NC, 8 )

*  Now loop through the cards.  Compare the keyword on each word with
*  the given keyword, until the required card is found, or the 'END'
*  card is met, or there are no cards remaining.
 1    CONTINUE                  ! Start of 'DO WHILE' loop
      IF ( ( .NOT. THERE ) .AND.
     :     ( CARD .LE. NCARD ) .AND.
     :     ( BUFFER( MIN( NCARD, CARD ) )( :3 ) .NE. 'END' ) ) THEN

*  Does the current card contain the required keyword?
         IF ( BUFFER( CARD )( :8 ) .EQ. KEYWRD( :NC ) ) THEN

*  The keyword is present.
            THERE = .TRUE.
            CHRVAL = BUFFER( CARD )( 11:30 )

*  Remove leading blanks and get number of characters comprising the
*  value.
            CALL CHR_LDBLK( CHRVAL )
            NCHV = CHR_LEN( CHRVAL )

*  Convert the string to the correct type. Trap the occasion when the
*  value is a blank string.
            IF ( NCHV .GT. 0 ) THEN
               CALL CHR_CTOR( CHRVAL( :NCHV ), VALUE, STATUS )
            ELSE
               STATUS = SAI__ERROR
            END IF

*  Beef up the error context.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'NAME', NAME( :NC ) )
               CALL MSG_SETC( 'VALUE', CHRVAL( :NCHV ) )
               CALL ERR_REP( 'PHO1_GKEY_TYPCNV',
     :              'Error converting FITS item ^NAME (with ' //
     :              'value ^VALUE) to type REAL.', STATUS )
            END IF
         ELSE

*  Onto the next card in the buffer.
            CARD = CARD + 1
         END IF

*  Next 'WHILE'.
         GO TO 1
      END IF
      END

* $Id:
