      SUBROUTINE IRQ_GETQX( PARAM, QEXP, STATUS )
*+
*  Name:
*     IRQ_GETQX

*  Purpose:
*     Get a quality expression from the user and check for syntax
*     errors.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_GETQX( PARAM, QEXP, STATUS )

*  Description:
*     A string is obtained from the environment using the supplied ADAM
*     parameter. This string is check to see if it has correct syntax
*     for a quality expression (no checks are made to ensure that the
*     quality names referenced within the expression are defined within
*     any specific NDF). If a syntax error is detected, the quality
*     expression is displayed with an exclamation mark under the
*     position at which the syntax error was detected. If any problem is
*     found with the supplied expression, a new value is obtained from
*     the environment. The returned string is converted to upper case
*     and leading blanks are removed.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter of type LITERAL.
*     QEXP = CHARACTER * ( * ) (Returned)
*        The returned quality expression.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-JAN-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Returned:
      CHARACTER QEXP*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ARROW*254        ! String holding pointer to syntax error
      INTEGER ERRPNT             ! Offset of syntax error
      INTEGER TSTAT              ! Local status value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a quality expression from the environment.
 10   CONTINUE

      CALL PAR_GET0C( PARAM, QEXP, STATUS )

*  Check the syntax is correct. This operation also converts the quality
*  expression to upper case and removes leading blanks. Note, even if
*  the quality expression passes this syntax check, there is still the
*  possibility that it may not compile because of undefined quality
*  names.
      ERRPNT = 0
      CALL IRQ_SYNTX( QEXP, ERRPNT, STATUS )

*  If a pointer to a syntax error was returned, indicate to the user
*  where the syntax error occured.
      IF( ERRPNT .GT. 0 ) THEN
         TSTAT = SAI__OK
         CALL MSG_BLANK( TSTAT )
         CALL MSG_OUT( ' ', QEXP, TSTAT )
         ARROW = ' '
         ARROW( ERRPNT : ERRPNT ) = '!'
         CALL MSG_OUT( ' ', ARROW, TSTAT )
         CALL MSG_BLANK( TSTAT )
      END IF

*  If there was anything wrong with the supplied quality expression,
*  cancel the parameter value and obtain a new value.
      IF( STATUS .EQ. IRQ__BADSY .OR.
     :    STATUS .EQ. IRQ__CMPLX .OR.
     :    STATUS .EQ. IRQ__MSDOT .OR.
     :    STATUS .EQ. IRQ__MSOPD .OR.
     :    STATUS .EQ. IRQ__MSOPT .OR.
     :    STATUS .EQ. IRQ__MSPAR .OR.
     :    STATUS .EQ. IRQ__NOOPS .OR.
     :    STATUS .EQ. IRQ__QEXPL .OR.
     :    STATUS .EQ. IRQ__QREFS ) THEN

         CALL ERR_FLUSH( STATUS )
         CALL PAR_CANCL( PARAM, STATUS )
         GO TO 10

      END IF

      END
