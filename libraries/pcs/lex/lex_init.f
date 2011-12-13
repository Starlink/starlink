      SUBROUTINE LEX_INIT(NSTATE,TABLE)
*+
*  Name:
*     LEX_INIT

*  Purpose:
*     Initialise a state table for the LEX parser.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL LEX_INIT(NSTATE,TABLE)

*  Arguments:
*     NSTATE = INTEGER (given)
*           The number of states in the state table
*     TABLE(4,0:127,NSTATE) = BYTE (returned)
*           The state table

*  Algorithm:
*     The state table is filled with entries which cause the
*     parser to signal an error. Valid state transitions will
*     be subsequently overwritten by calls to the LEX_SET
*     routine.

*  Copyright:
*     Copyright (C) 1987 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     Jeremy Bailey (AAOEPP::JAB)
*     {enter_new_authors_here}

*  History:
*     08-JAN-1987 (JAB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
*     <any INCLUDE files containing global constant definitions>
*  Arguments Given:
      INTEGER NSTATE
*  Arguments Returned:
      BYTE TABLE(4,0:127,NSTATE)
*  Local Variables:
      INTEGER I,C
*.

      DO I=1,NSTATE
         DO C=0,127
            TABLE(1,C,I)=0
            TABLE(2,C,I)=-1
            TABLE(3,C,I)=0
            TABLE(4,C,I)=0
         ENDDO
      ENDDO

      END
