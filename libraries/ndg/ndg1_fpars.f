      SUBROUTINE NDG1_FPARS( SPEC, DIR, BN, SUF, SEC, STATUS )
*+
*  Name:
*     NDG1_FPARS

*  Purpose:
*     Extracts fields from a full NDF specification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_FPARS( SPEC, DIR, BN, SUF, SEC, STATUS )

*  Description:
*     This routine extracts and returned fields from the supplied NDF
*     specification.

*  Arguments:
*     SPEC = CHARACTER * ( * ) (Given)
*        The full NDF file spec.
*     DIR = CHARACTER * ( * ) (Returned)
*        The directory path (ending at the final "\" in the spec).
*     BN = CHARACTER * ( * ) (Returned)
*        The file base name. Ends with the character preceeding the first
*        "." or "(" or "[" following the directory path.
*     SUF = CHARACTER * ( * ) (Returned)
*        Any string following the file base name, and preceeding any 
*        opening parenthesis. SUF will either be blank, or begin with a
*        dot or a "[".
*     SEC = CHARACTER * ( * ) (Returned)
*        Any parenthesised string following the suffix.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-SEP-1999 (DSB):
*        Original version.
*     18-JUL-2000 (DSB):
*        Use "[" as an additional delimiter for the basename, in order to
*        allow for foreign extension specifiers.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER SPEC*(*)

*  Arguments Returned:
      CHARACTER DIR*(*)
      CHARACTER BN*(*)
      CHARACTER SUF*(*)
      CHARACTER SEC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      INTEGER BNBEG              ! Index of first character in base name
      INTEGER BNEND              ! Index of last character in base name
      INTEGER DIRBEG             ! Index of first character in directory
      INTEGER DIREND             ! Index of last character in directory
      INTEGER DOT                ! Index of dot
      INTEGER FXS                ! Index of opening square bracket 
      INTEGER LSPEC              ! Length of spec
      INTEGER PAR                ! Index of opening parenthesis
      INTEGER SUFBEG             ! Index of first character in suffix
      INTEGER SUFEND             ! Index of last character in suffix
*.

*  Initialise
      DIR = ' '
      BN = ' '
      SUF = ' '
      SEC = ' '

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the used length of the supplied spec.
      LSPEC = CHR_LEN( SPEC )

*  The directory path starts at the first character.
      DIRBEG = 1

*  Find the end of the directory path. This is the last "/" in the string.
*  DIREND will be zero if there is no "/".
      CALL NDG1_LASTO( SPEC, '/', DIREND, STATUS )

*  The first character in the file basename follows the last "/".
      BNBEG = DIREND + 1

*  Find the first dot following the start of the basename.
      DOT = INDEX( SPEC( BNBEG : ), '.' )
      IF( DOT .EQ. 0 ) DOT = 100000

*  Find the first "(" following the start of the basename (a potential 
*  NDF slice or HDS cell specification).
      PAR = INDEX( SPEC( BNBEG : ), '(' )
      IF( PAR .EQ. 0 ) PAR = 100000

*  Find the first "[" following the start of the basename (a potential 
*  foreign extension specifier).
      FXS = INDEX( SPEC( BNBEG : ), '[' )
      IF( FXS .EQ. 0 ) FXS = 100000

*  The end of the current file basename is marked by the earlier of the
*  three.
      BNEND = MIN( DOT, MIN( PAR, FXS ) ) - 1

*  If no end marker was found use the whole string. 
      IF( BNEND .EQ. -1 ) THEN
         BNEND = LSPEC 

*  Otherwise, correct for the start of the string.
      ELSE
         BNEND = BNEND + BNBEG - 1
      END IF

*  The file suffix is any string following the basename, extending
*  to the first "]" or "(" or the end of the string (which ever is first).
      SUFBEG = BNEND + 1
      FXS = INDEX( SPEC( SUFBEG : ), ']' )
      IF( FXS .EQ. 0 ) FXS = 100000
      SUFEND = MIN( LSPEC, MIN( PAR + BNBEG, FXS + SUFBEG ) 
     :                     + BNBEG - 2 )

*  Extract the fields into separate variables.
      IF( DIRBEG .LE. DIREND ) DIR = SPEC( DIRBEG : DIREND )
      IF( BNBEG .LE. BNEND ) BN = SPEC( BNBEG : BNEND )
      IF( SUFBEG .LE. SUFEND ) SUF = SPEC( SUFBEG : SUFEND )
      IF( SUFEND .LT. LSPEC ) SEC = SPEC( SUFEND + 1 : LSPEC )

      END
