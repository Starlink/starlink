      SUBROUTINE KPG1_SATKD( PREFIX, EXPRES, STATUS )
*+
*  Name:
*     KPG1_SATKD

*  Purpose:
*     Substitutes alphabetic (TRANSFORM) numeric tokens into a string

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SATKD( PREFIX, EXPRES, STATUS )

*  Description:
*     This routine parses the expression in EXPRES looking for
*     tokens of the name PREFIX//[A-Z].  If one is located an attempt
*     to access a value for this tokens if made using the ADAM
*     parameter PREFIX//[A-Z].  If a value is obtained then it is
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

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 February 22 (MJC):
*        Original KAPPA version derived closely from PDRAPER's
*        CCD1_GASTD.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PREFIX

*  Arguments Given and Returned:
      CHARACTER * ( * ) EXPRES

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 1 ) ALPHA( 26 ) ! The alphabet
      CHARACTER * ( 512 ) DUMMY  ! Local copy of expression
      INTEGER I                  ! Loop variable
      INTEGER NSUBS              ! Number of tokens substituted
      CHARACTER * ( 5 ) TOKEN    ! Current token
      DOUBLE PRECISION VALUE     ! Value of token

*  Local Data:
      DATA ALPHA / 'A','B','C','D','E','F','G','H','I','J','K','L',
     :             'M','N','O','P','Q','R','S','T','U','V','W','X',
     :             'Y','Z' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct each of the possible tokens in turn.  Look for it in the
*  expression.  When a token is located get a value.
      DO 100 I = 1, 26
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
  100 CONTINUE

      END
