      SUBROUTINE PARSECON_ACTRES ( FLAG, STATUS )
*+
*  Name:
*     PARSECON_ACTRES

*  Purpose:
*     Set flag indicating RANGE or IN limit on action.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_ACTRES ( FLAG, STATUS )

*  Description:
*     Sets the flag indicating whether the NEEDS parameter being defined
*     for the current action has a RANGE or an IN constraint.

*  Arguments:
*     FLAG=LOGICAL (given)
*        .TRUE. => RANGE constraint on the value of the parameter
*        required for the action currently being defined.
*     STATUS=INTEGER

*  Algorithm:
*     Set the value into the common-block variable.

*  Copyright:
*     Copyright (C) 1984, 1993 Science & Engineering Research Council.
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
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02.10.1984:  Original (REVAD::BDK)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
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
      LOGICAL FLAG                 ! value to be set


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      NEEDCONT(NEEDPTR) = FLAG

      END
