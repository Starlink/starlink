      SUBROUTINE PARSECON_NEWACRDS ( STATUS )
*+
*  Name:
*     PARSECON_NEWACRDS

*  Purpose:
*     initialise action menu coordinates.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_NEWACRDS ( STATUS )

*  Description:
*     Initialise the menu coordinates for the most recently declared
*     action.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     Set the common block array ACTCOORDS to (-1,-1) at the position
*     for the latest parameter.

*  Copyright:
*     Copyright (C) 1986, 1992, 1993 Science & Engineering Research Council.
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
*     13.05.1986:  Original (REVAD::BDK)
*     20.01.1992:  Renamed from _NEWACOORDS (RLVAD::AJC)
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


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      ACTCOORDS(1,ACTPTR) = -1
      ACTCOORDS(2,ACTPTR) = -1

      END
