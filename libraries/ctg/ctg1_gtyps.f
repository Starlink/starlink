      SUBROUTINE CTG1_GTYPS( MXTYP, STRING, NTYP, TYP, STATUS )
*+
*  Name:
*     CTG1_GTYPS

*  Purpose:
*     Get file types from a catalogue file types list.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG1_GTYPS( MXTYP, STRING, NTYP, TYP, STATUS )

*  Description:
*     This routine extracts the file types (including leading dots) from
*     a string representing a set of cataologue types, in the same format
*     as the NDF_FORMATS_IN or NDF_FORMATS_OUT environment variable.

*  Arguments:
*     MXTYP = INTEGER (Given)
*        The maximum number of types allowed.
*     STRING = CHARACTER * ( * ) (Given)
*        The value of "CAT_FORMATS_IN" or "CAT_FORMATS_OUT".
*     NTYP = INTEGER (Returned)
*        The number of file types returned in TYP.
*     TYP( MXTYP ) = CHARACTER * ( * ) (Returned)
*        The file types extracted from STRING, in elements 1 to NTYP.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     13-SEP-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER MXTYP
      CHARACTER STRING*(*)

*  Arguments Returned:
      INTEGER NTYP
      CHARACTER TYP( MXTYP )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      INTEGER CL                 ! Index of next closing parenthesis
      INTEGER COMMA              ! Index of next comma
      INTEGER IAT                ! Index of start of current field
      INTEGER IEND               ! Index of end of current field
      INTEGER LSTR               ! Used length of STRING
      INTEGER OP                 ! Index of next opening parenthesis
      LOGICAL MORE               ! Was another opening parenthesis found?
*.

*  Initialise the number of file types to zero.
      NTYP = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the used length.
      LSTR = CHR_LEN( STRING )

*  Find the first comma.
      IAT = 1
      COMMA = INDEX( STRING, ',' )

*  Use the characater following the end of the string if no comma was
*  found.
      IF( COMMA .EQ. 0 ) COMMA = LSTR + 1

*  Loop round until no more fields can be found.
      MORE = .TRUE.
      DO WHILE( MORE )

*  Find the last character before the next comma.
         IEND = COMMA - 1

*  Ignore null fields.
         IF( IAT .LE. IEND ) THEN

*  If the field is just a dot, or an asterisk, store it.
            IF( STRING( IAT : IEND ) .EQ. '.' .OR.
     :          STRING( IAT : IEND ) .EQ. '*' ) THEN
               NTYP = NTYP + 1
               TYP( NTYP ) = STRING( IAT : IEND )
               IF( NTYP .EQ. MXTYP ) MORE = .FALSE.

*  Otherwise,
            ELSE

*  Find the first opening parenthesis within this field.
               OP = INDEX( STRING( IAT : IEND ), '(' )

*  If found, look for the last closing parenthesis, following the opening
*  parenthesis.
               IF( OP .GT. 0 ) THEN
                  OP = OP + IAT - 1
                  IF( OP .LT. IEND ) THEN
                     CL = INDEX( STRING( OP + 1 : IEND ), ')' )

*  If found, store the string between the parentheses.
                     IF( CL .GT. 0 ) THEN
                        CL = CL + OP
                        IF( OP .LT. CL - 1 ) THEN
                           NTYP = NTYP + 1
                           TYP( NTYP ) = STRING( OP + 1 : CL - 1 )
                           IF( NTYP .EQ. MXTYP ) MORE = .FALSE.
                        END IF
                     END IF

                  END IF
               END IF

            END IF
         END IF

*  The next field starts at the character following the comma.
         IAT = COMMA + 1

*  Leave the loop if we are beyond the end of the string.
         IF( IAT .GT. LSTR ) THEN
            MORE = .FALSE.

*  Otherwise, find the next comma.
         ELSE
            COMMA = INDEX( STRING( IAT : ), ',' )
            IF( COMMA .EQ. 0 ) THEN
               COMMA = LSTR + 1
            ELSE
               COMMA = COMMA + IAT - 1
            END IF
         END IF

      END DO

      END
