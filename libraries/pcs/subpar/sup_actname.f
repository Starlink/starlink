      SUBROUTINE SUBPAR_ACTNAME( ID, NAME, NAMELEN, STATUS)
*+
*  Name:
*     SUBPAR_ACTNAME

*  Purpose:
*     Get the name of an action.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_ACTNAME( ID, NAME, NAMELEN, STATUS)

*  Description:
*     Get the name of an action

*  Algorithm:
*     Get the name from the global variable

*  Actions:
*     ID=INTEGER(INPUT)
*           Identifier of the action
*     NAME=CHARACTER*(*)(OUTPUT)
*           Name of the action
*     NAMELEN=INTEGER(OUTPUT)
*           Length of the action name
*     STATUS=INTEGER(UPDATE)
*           SSE status variable

*  Copyright:
*     Copyright (C) 1986, 1991, 1993 Science & Engineering Research Council.
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
*     JHF: J H Fairclough (UKIRT)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-May-1986 (JHF):
*        Original
*     19-Jul_1991 (AJC):
*        Use SAI__OK not ADAM__OK
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! ADAM Symbolic Constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'

*  Arguments Given:
      INTEGER ID

*  Arguments Returned:
      CHARACTER*(*) NAME
      INTEGER NAMELEN

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.

      IF (STATUS .NE. SAI__OK) RETURN
*
*    Begin
*
      IF (ID .GE. 1 .AND. ID .LE. SUBPAR__MAXACT) THEN
         NAME = ACTNAMES(ID)
         NAMELEN = ACTLEN(ID)
      ELSE
         STATUS = SUBPAR__NOPAR
      ENDIF
*
*    End
*
      END
