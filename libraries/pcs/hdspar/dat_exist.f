      SUBROUTINE DAT_EXIST ( PARAM, ACCESS, LOC, STATUS )
*+
*  Name:
*     DAT_EXIST

*  Purpose:
*     Return a locator associated with a parameter.

*  Language:
*     Fortran 77

*  Invocation:
*     CALL DAT_EXIST ( PARAM, ACCESS, LOC, STATUS )

*  Description:
*     An HDS locator for the data object associated with the specified
*     parameter is returned. The parameter system will attempt to associate
*     an object if one is not already associated. This operation is identical
*     with DAT_ASSOC except under error conditions. When there is an error,
*     DAT_EXIST reports the error and returns the status value immediately,
*     whereas DAT_ASSOC repeatedly attempts to get a valid locator.
*     If the named object does not exist, status PAR__ERROR is returned.
*
*     The object will be opened with the appropriate ACCESS mode. If ACCESS
*     is incompatible with the access mode specified for the parameter in
*     the program's Interface File, status SUBPAR__ICACM will be returned.

*  Arguments:
*     PARAM=CHARACTER*(*) (given)
*        program parameter name
*     ACCESS=CHARACTER*(*) (given)
*        Access mode, 'READ', 'WRITE' or 'UPDATE'
*        (case insignificant)
*     LOC=CHARACTER*(*) (returned)
*        Locator to the associated data object
*     STATUS=INTEGER (given and returned)
*        Global status

*  Algorithm:
*     Use SUBPAR to get the parameter number, and then perform the
*     operation.

*  Copyright:
*     Copyright (C) 1984, 1992-1993 Science & Engineering Research
*     Council. Copyright (C) 1998 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (Starlink)
*     {enter_new_authors_here}

*  History:
*     25-NOV-1984 (BDK)
*        Original
*     30-JAN-1992 (AJC)
*        Inherit LOC size
*      1-SEP-1993 (AJC)
*        Correct "Arguments" section for above mod
*     16-JUN-1998 (AJC)
*        Re-format prologue
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) PARAM          ! name of parameter

      CHARACTER*(*) ACCESS         ! Access mode, 'READ', 'WRITE'
                                   ! or 'UPDATE'

*  Arguments Returned:
      CHARACTER*(*) LOC            ! Locator to data structure

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NAMECODE             ! Number of program parameter
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL SUBPAR_FINDPAR ( PARAM, NAMECODE, STATUS )
      CALL SUBPAR_EXIST ( NAMECODE, ACCESS, LOC, STATUS )

      END
