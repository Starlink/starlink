      SUBROUTINE PARSECON_TABSTART ( STATUS )
*+
*  Name:
*     PARSECON_TABSTART

*  Purpose:
*     Fill the parse state-tables with error states.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_TABSTART ( STATUS )

*  Description:
*     The two parse-state tables are set so that all their values
*     correspond to invalid parse-states. This is the first stage in
*     initialising the tables.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     The parse-state is stored in two BYTE arrays -
*       ACTTAB - action code
*       STATETAB - new parse state to be entered.
*     All the elements of these are initialised to
*       ACTTAB - ERROR
*       STATETAB - FACEGOT
*     Positions in the tables corresponding to valid parse states will
*     be subsequently overwritten.

*  Copyright:
*     Copyright (C) 1984 Science & Engineering Research Council.
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
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     18.09.1984:  Original (REVAD::BDK)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'PARSECON_CMN'


*  Local Variables:
      INTEGER I
      INTEGER J


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      DO J = 1, PARSE__NUMTOK
         DO I = 1, PARSE__NUMSTATE
            ACTTAB(I,J) = ERROR
            STATETAB(I,J) = FACEGOT
         ENDDO
      ENDDO

      END
