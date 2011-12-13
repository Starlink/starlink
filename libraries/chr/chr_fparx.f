      SUBROUTINE CHR_FPARX( STR, OPPAR, CLPAR, F, L )
*+
*  Name:
*     CHR_FPARX

*  Purpose:
*     Find a parenthesised expression in a character string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_FPARX( STR, OPPAR, CLPAR, F, L )

*  Description:
*     The routine searches the string STR to identify a sub-string
*     containing a parenthesised expression and returns the character
*     positions of the opening and closing parentheses in the F and L
*     arguments. Allowance is made for nested parentheses. If a
*     parenthesised expression was not found, then the returned value
*     of F will be greater than the returned value of L.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String to be searched.
*     OPPAR = CHARACTER * ( 1 ) (Given)
*        The opening parenthesis character.
*     CLPAR = CHARACTER * ( 1 ) (Given)
*        The closing parenthesis character.
*     F = INTEGER (Returned)
*        Character position of the opening parenthesis.
*     L = INTEGER (Returned)
*        Character position of the closing parenthesis.

*  Copyright:
*     Copyright (C) 1994 STARLINK
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David Berry (UCLan, Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     9-OCT-1989 (RFWS):
*        Original version.
*     15-APR-1994 (RFWS):
*        Added the OPPAR and CLPAR arguments.
*     15-FEB-1998 (DSB):
*        Brought into NDG from NDF.
*     23-DEC-2005 (TIMJ):
*        Brought into CHR from NDG and NDF since this is a generic
*        string handling function.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR
      CHARACTER * ( 1 ) OPPAR
      CHARACTER * ( 1 ) CLPAR

*  Arguments Returned:
      INTEGER F
      INTEGER L

*  Local Variables:
      INTEGER I                  ! Loop counter for characters
      INTEGER IPAR               ! Count of nested parentheses
      LOGICAL FOUND              ! Whether first character found

*.

*  Initialise.
      F = 1
      L = 0
      FOUND = .FALSE.

*  Inspect each character in the string, looking for an opening
*  parenthesis.
      DO 1 I = 1, LEN( STR )
         IF ( STR( I : I ) .EQ. OPPAR ) THEN

*  If found, then note its position.
            FOUND = .TRUE.
            F = I
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

*  If the start of an expression has been found, then search for the
*  end.
      IF ( FOUND ) THEN

* Initialise the count of nested parentheses.
         IPAR = 1

*  Loop to inspect subsequent characters in the string.
         DO 3 I = F + 1, LEN( STR )

*  Count opening parentheses.
            IF ( STR( I : I ) .EQ. OPPAR ) THEN
               IPAR = IPAR + 1

*  Count closing parentheses.
            ELSE IF ( STR( I : I ) .EQ. CLPAR ) THEN
               IPAR = IPAR - 1

*  If the number of nested parentheses falls to zero, then the final
*  character of the expression has been found. Note its position.
               IF ( IPAR .EQ. 0 ) THEN
                  L = I
                  GO TO 4
               END IF
            END IF
3        CONTINUE
4        CONTINUE
      END IF

      END
