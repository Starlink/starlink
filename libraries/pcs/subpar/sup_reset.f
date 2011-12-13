      SUBROUTINE SUBPAR_RESET ( STATUS )
*+
*  Name:
*     SUBPAR_RESET

*  Purpose:
*     To set parameters into an appropriate RESET state

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_RESET ( STATUS )

*  Description:
*     Set each parameter into a relevant RESET state if its current
*     state permits. In a RESET state, VPATH and PPATH searches will
*     operate as though there is no 'current' value for the parameter.
*
*     At present, the RESET state may only be set by the special keyword
*     RESET on the command line - i.e. the parameter will be in GROUND
*     or a 'pseudo-GROUND' state.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     For each parameter relevant to the currently active program, check
*     its current state. Change the state where relevant.

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
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
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     28-MAY-1987 (BDK):
*        Original
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     16-MAR-1993 (AJC):
*        Revise for separate RESACC and RESACCPR
*        Remove unnecessary ELSE IF clauses
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      INTEGER NAMECODE       ! index to parameters

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   PROGNUM selects the relevant parameters if this is a monolith.
*   Otherwise, it has the effect of selecting all parameters.
*
      DO NAMECODE = PROGADD(1,PROGNUM), PROGADD(2,PROGNUM)

         IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__GROUND ) THEN
            PARSTATE(NAMECODE) = SUBPAR__RESET
         ELSE IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACCEPT ) THEN
            PARSTATE(NAMECODE) = SUBPAR__RESACC
         ELSE IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__FPROMPT ) THEN
            PARSTATE(NAMECODE) = SUBPAR__RESPROM
         ELSE IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACCPR ) THEN
            PARSTATE(NAMECODE) = SUBPAR__RESACCPR
         ENDIF

      ENDDO

      END
