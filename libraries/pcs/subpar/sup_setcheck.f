
      SUBROUTINE SUBPAR_SETCHECK ( FLAG, STATUS )
*+
*  Name:
*     SUBPAR_SETCHECK

*  Purpose:
*     set flag for controlling NEEDS checks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_SETCHECK ( FLAG, STATUS )

*  Description:
*     Sets whether the NEEDS lists for actions are checked for validity
*     before ACT is called.

*  Arguments:
*     FLAG=LOGICAL (given)
*        .TRUE. => the task fixed part validates the needs list of
*        the requested action before calling ACT.
*     STATUS=INTEGER

*  Algorithm:
*     Copy FLAG into the common block variable CHECKNEEDS.

*  Copyright:
*     Copyright (C) 1986, 1993 Science & Engineering Research Council.
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
*     06-MAR-1986 (BDK):
*        Original
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
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
      LOGICAL FLAG       ! .TRUE. => the task fixed part validates the
                         ! needs list of the requested action before
                         ! calling ACT.

*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      CHECKNEEDS = FLAG

      END
