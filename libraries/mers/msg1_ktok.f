      SUBROUTINE MSG1_KTOK
*+
*  Name:
*     MSG1_KTOK

*  Purpose:
*     Clear the message token table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG1_KTOK

*  Description:
*     Clear all the message tokens at the current context level.

*  Arguments:
*     None

*  Algorithm:
*     Call EMS_EXPND with bad status - just kills tokens

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     AJC: A.J.Chipperfield  (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     22-FEB-2001 (AJC):
*        Original version - to avoid use of EMS internal EMS1_KTOK
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Local Variables:
      INTEGER ISTAT              ! Local status
      INTEGER STRLEN             ! Dummy string length

      CHARACTER * 2 STRING       ! Dummy string

*.

*  Set bad status and call EMS_EXPND
      ISTAT = SAI__ERROR
      CALL EMS_EXPND( ' ', STRING, STRLEN, ISTAT ) 

      END
