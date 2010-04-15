      SUBROUTINE PON_PARSE( STRING, DELIM, MAXPAR, PARS, NPAR, STATUS )
*+
*  Name:
*     PON_PARSE

*  Purpose:
*     Parses a string into components.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_PARSE( STRING, DELIM, MAXPAR, PARS, NPAR, STATUS )

*  Description:
*     This routine takes a string and parses it into a list of words
*     which are separated by the given delimeters (each delimeter is
*     considered as a single character). The return is an array whose
*     elements are the words. Multiple occurrences of any of the
*     delimeters are skipped. The number of words which may be returned
*     is limited to MAXPAR.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string containing the list of words.
*     DELIM = CHARACTER * ( * ) (Given)
*        The word delimeters. Multiple occurences of any of the
*        characters in this string are considered as a word boundary.
*     MAXPAR = INTEGER (Given)
*        The maximum number of words which can be extracted.
*     PARS( MAXPAR ) = CHARACTER * ( * ) (Returned)
*        The extracted words.
*     NPAR = INTEGER (Returned)
*        The number of words extracted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     3-JUN-1994 (PDRAPER):
*        Original version based on the PONGO parse subroutine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) STRING
      CHARACTER * ( * ) DELIM
      INTEGER MAXPAR

*  Arguments Returned:
      CHARACTER * ( * ) PARS( MAXPAR )
      INTEGER NPAR

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      INTEGER IDX                ! Current word size
      INTEGER IAT                ! Current position in string
      INTEGER LENDEL             ! Number of delimeters
      INTEGER PARLEN             ! Maximum length of extracted word
      INTEGER STRLEN             ! Length of input string
      INTEGER I                  ! Loop variable
      INTEGER POS                ! Position of delimeter in current
                                 ! sub-string
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find lenght of input strings and set maximum length of word.
      LENDEL = CHR_LEN( DELIM )
      LENDEL = MAX( 1, LENDEL )
      STRLEN = CHR_LEN( STRING )
      PARLEN = LEN( PARS( 1 ) )

*  Initialise position in string, number of words and word size.
      NPAR = 0
      IAT = 1
      IDX = 0

*  Loop until we can extract no more words or we have exceeded the
*  maximum number of words allowed.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( NPAR .LT. MAXPAR  .AND.  IAT .LE. STRLEN ) THEN

*  Set end of word to after the end of string (+1). (Note this is offset to the
*  system starting at the position iat.)
         IDX = STRLEN - IAT + 2

*  Look over all delimeters locating the position of the one nearest to
*  the current position.
         DO 2 I = 1, LENDEL
            POS = INDEX( STRING( IAT: ), DELIM( I: I ) )
            IF ( POS .NE. 0 ) IDX = MIN( IDX, POS )
 2       CONTINUE

*  If delimeter was next to the current position then move on.
         IF ( IDX .EQ. 1 ) THEN
            IAT = IAT + 1
         ELSE

*  Word is between this position and the previous position.
            NPAR = NPAR + 1
            PARS( NPAR ) = STRING( IAT: IAT + MIN( PARLEN, IDX-2 ) )
            IAT = IAT + IDX
         ENDIF
         GO TO 1
      END IF
      END
* $Id$
