      SUBROUTINE CCD1_GASTD( PREFIX, EXPRES, STATUS )
*+
*  Name:
*     CCD1_GAST
 
*  Purpose:
*     Access and substitutes (TRANSFORM) numeric tokens into a string
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL CCD1_GAST( PREFIX, EXPRES, STATUS )
 
*  Description:
*     This routine parses the expression in EXPRES looking for
*     tokens of the name PREFIX//[A-Z]. If one is located an attempt
*     to access a value for this tokens if made using the ADAM
*     parameter PREFIX//[A-Z]. If a value is obtained then it is
*     substituted into the string EXPRES.
 
*  Arguments:
*     PREFIX = CHARACTER * ( * ) (Given)
*        The prefix of the tokens. Valid tokens are ones with any
*        trailing single alphabetic character.
*     EXPRES = CHARACTER * ( * ) (Given and Returned)
*        On entry this contains a TRANSFORM algebraic-like expression
*        which may contain tokens which need to be substituted either
*        for values (constants).
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     9-JUL-1993 (PDRAPER):
*        Original version.
*     12-JUL-1993 (PDRAPER):
*        Double precision version.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations
 
*  Arguments Given:
      CHARACTER * ( * ) PREFIX
 
*  Arguments Given and Returned:
      CHARACTER * ( * ) EXPRES
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      DOUBLE PRECISION VALUE     ! Value of token
      CHARACTER * ( CCD1__SZTRN ) DUMMY ! Local copy of expression
      CHARACTER * ( 1 ) ALPHA( 26 ) ! The alphabet
      CHARACTER * ( 5 ) TOKEN    ! Current token
      INTEGER I                  ! Loop variable
      INTEGER NSUBS              ! Number of tokens substituted
 
*  Local Data:
      DATA ALPHA / 'A','B','C','D','E','F','G','H','I','J', 'K','L',
     :             'M','N','O','P','Q','R','S','T','U','V','W','X',
     :             'Y','Z' /
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Construct each of the possible tokens in turn. Look for it in the
*  expression. When a token is located get a value.
      DO 2 I = 1, 26
         TOKEN = PREFIX//ALPHA( I )
 
*  Look for token.
         DUMMY = EXPRES
         CALL TRN_STOKD( TOKEN, 1.0D0, DUMMY, NSUBS, STATUS )
 
*  Check the number of substitutions.
         IF ( NSUBS .GT. 0 ) THEN
 
*  Token present in expression get a value.
            CALL PAR_GET0D( TOKEN, VALUE, STATUS )
 
*  And substitute it.
            CALL TRN_STOKD( TOKEN, VALUE, EXPRES, NSUBS, STATUS )
         END IF
 2    CONTINUE
      END
* $Id$
