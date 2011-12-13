      PROGRAM STRING_TEST
*+
*  Name:
*     STRING_TEST

*  Purpose:
*     To test the STRING installation - not its functionality

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     ADAM A-task

*  Invocation:
*     STRING_TEST

*  Description:
*     Compiling and linking this program will give a good indication
*     that the STRING library is correctly installed

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     ENV: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1993 (ENV):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*5 STRING
*.

      STATUS = SAI__OK

*  Call a STRING routine
      CALL STRING_STRIPQUOT('''OK''', STRING, STATUS )

      PRINT *,' STRING_TEST ends ',STRING

      END
