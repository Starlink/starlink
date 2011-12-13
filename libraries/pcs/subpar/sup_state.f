      SUBROUTINE SUBPAR_STATE ( NAMECODE, STATE, STATUS )
*+
*  Name:
*     SUBPAR_STATE

*  Purpose:
*     return the state of a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_STATE ( NAMECODE, STATE, STATUS )

*  Description:
*     Return the current state of the indicated parameter.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to parameter
*     STATE=INTEGER (returned)
*        current state of parameter, one of
*        SUBPAR__GROUND
*        SUBPAR__ACTIVE
*        SUBPAR__CANCEL
*        SUBPAR__NULL
*        SUBPAR__EOL
*        SUBPAR__RESET
*        SUBPAR__ACCEPT
*        SUBPAR__RESACC
*        SUBPAR__FPROMPT
*        SUBPAR__RESPROM
*        SUBPAR__ACCPR
*        SUBPAR__RESACCPR
*     STATUS=INTEGER
*        Global status

*  Algorithm:
*     Return the parameter state from the SUBPAR common blocks.

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
*     AJC: A J Chipoperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-JUL-1987 (BDK):
*        Original
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     16-MAR-1993 (AJC):
*        Add new states
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER NAMECODE     ! pointer to parameter

*  Arguments Returned:
      INTEGER STATE        ! current state of parameter

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.
      IF ( STATUS .NE. SAI__OK ) RETURN

      STATE = PARSTATE(NAMECODE)

      END
