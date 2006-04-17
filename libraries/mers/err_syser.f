      SUBROUTINE ERR_SYSER( TOKEN, SYSTAT )
*+
*  Name:
*     ERR_SYSER

*  Purpose:
*     Assign an operating system error message to a token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERR_SYSER( TOKEN, SYSTAT )

*  Description:
*     The text of the error message associated with the operating system 
*     status value, SYSTAT, is assigned to the named message token. This
*     token may then be included in an error message.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        The message token name.
*     SYSTAT = INTEGER (Given)
*        The operating system status value.

*  System-specific:
*     The messages generated using this facility will depend on the 
*     computer system upon which the library is implemented.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-OCT-1989 (PCTR):
*        Original version.
*     15-DEC-1989 (PCTR):
*        Converted to call EMS_SYSER.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) TOKEN

      INTEGER SYSTAT

*.

*  Load message string for TOKEN.
      CALL EMS_SYSER( TOKEN, SYSTAT )

      END
