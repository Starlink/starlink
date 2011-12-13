      SUBROUTINE SUBPAR_PARTYPE( ID, TYPE, STATUS)
*+
*  Name:
*     SUBPAR_PARTYPE

*  Purpose:
*     Get the type of a parameter.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL SUBPAR_PARTYPE( ID, TYPE, STATUS)

*  Description:
*     Get the type of a parameter

*  Arguments:
*     ID=INTEGER(INPUT)
*        Identifier of the parameter
*     TYPE=INTEGER(OUTPUT)
*        TYPE code in SUBPAR_PAR

*  Algorithm:
*     Get the name from the global variable

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
*     JHF: Jon Fairclough (RAL)
*     AJC: A J Chipperfield (Starlink)
*     {enter_new_authors_here}

*  History:
*     16-MAY-1986 (JHF):
*        Original version
*     24-JAN-1992 (AJC):
*        Use SAI__OK not ADAM__OK; include SAE_PAR not ADAMERRS
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! SAE Symbolic Constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'
      INCLUDE 'SUBPAR_PAR'


*  Arguments Given:
      INTEGER ID


*  Arguments Returned:
      INTEGER TYPE


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.

      IF (STATUS .NE. SAI__OK) RETURN
*
*    Begin
*
      IF (ID .GE. 1 .AND. ID .LE. SUBPAR__MAXPAR) THEN
         TYPE = PARTYPE(ID)
      ELSE
         STATUS = SUBPAR__NOPAR
      ENDIF
*
*    End
*
      END
