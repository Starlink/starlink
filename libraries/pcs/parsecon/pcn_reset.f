      SUBROUTINE PARSECON_RESET( STATE, STATUS )












*+
*  Name:
*     {routine_name}

*  Purpose:
*     {routine_purpose}

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_RESET( STATE, STATUS )

*  Description:
*     To reset the parsing STATE to one suitable for recovery
*     after an error.

*  Arguments:
*     STATE = INTEGER (Given and Returned)
*        Given the state in which the error occurred,
*        returns the recovery state.
*     STATUS = INTEGER (Given)
*        The global status.

*  Implementation Deficiencies:
*     Actually STATUS is not used but it is included as an argument
*     because it could be helpful in deciding what state to return
*     to and a more complicated routine could want to set it.

*  Name:
*     PARSECON_RESET

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.

*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.

*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}
*     {enter_new_authors_here}

*  History:
*     25-SEP-1990 (AJC):
*        Original version.
*     {enter_changes_here}
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*     {note_new_bugs_here}

*-

*.


*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'PARSECON_CMN'     ! Actually for declaration of states

*  Arguments Given and Returned:
      INTEGER STATE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER STATES( PARSE__NUMSTATE )  ! Table of recovery states

*  Local Data:
      DATA STATES ( START    )/ START /,
     :STATES ( MONGOT   )/ MONGOT /,
     :STATES ( MHLPLB   )/ MONGOT /
      DATA STATES ( FACESTART)/ FACEGOT /,
     :STATES ( FACEGOT  )/ FACEGOT /,
     :STATES ( FHLPLB   )/ FACEGOT /
     :STATES ( PARSTART )/ FACEGOT /,
     :STATES ( ACTSTART )/ FACEGOT /,
     :STATES ( PROGSTART)/ FACEGOT /,
     :STATES ( EPSTART  )/ FACEGOT /,
     :STATES ( MESTART  )/ FACEGOT /,
     :STATES ( MESGOT   )/ FACEGOT /,
     :STATES ( TEXTGOT  )/ FACEGOT /
      DATA STATES ( PARGOT   )/ PARGOT /,
     :STATES ( PARANGE  )/ PARGOT /,
     :STATES ( PARIN    )/ PARGOT /,
     :STATES ( PARDEF   )/ PARGOT /,
     :STATES ( PARTYPE  )/ PARGOT /,
     :STATES ( PARKEY   )/ PARGOT /,
     :STATES ( PARPOS   )/ PARGOT /,
     :STATES ( PARACC   )/ PARGOT /,
     :STATES ( PARVP    )/ PARGOT /,
     :STATES ( PARHEL   )/ PARGOT /,
     :STATES ( PARPTY   )/ PARGOT /,
     :STATES ( PARASS   )/ PARGOT /,
     :STATES ( PARPROM  )/ PARGOT /,
     :STATES ( PARMENU  )/ PARGOT /,
     :STATES ( PARCOORDS)/ PARGOT /,
     :STATES ( PARPP    )/ PARGOT /,
     :STATES ( PARHKY   )/ PARGOT /
      DATA STATES ( ACTGOT   )/ ACTGOT /,
     :STATES ( OGOT     )/ ACTGOT /,
     :STATES ( CGOT     )/ ACTGOT /,
     :STATES ( ONEEDST  )/ ACTGOT /,
     :STATES ( ONEED    )/ ACTGOT /,
     :STATES ( CNEEDST  )/ ACTGOT /,
     :STATES ( CNEED    )/ ACTGOT /,
     :STATES ( ORANGE   )/ ACTGOT /,
     :STATES ( CRANGE   )/ ACTGOT /,
     :STATES ( OBIN     )/ ACTGOT /,
     :STATES ( CANCIN   )/ ACTGOT /,
     :STATES ( ACTHEL   )/ ACTGOT /,
     :STATES ( ACTKEY   )/ ACTGOT /,
     :STATES ( ACTMENU  )/ ACTGOT /,
     :STATES ( ACTCOORDS)/ ACTGOT /

*.

*  If STATE is in range, set recovery STATE
      IF ( (STATE .GT. 0) .AND. (STATE .LT. PARSE__NUMSTATE) ) THEN
          STATE = STATES( STATE )

      ELSE
*       Otherwise, set FINISHED state
         STATE = FINISHED

      ENDIF

      END
