      SUBROUTINE sgs_DEFCH (CHOSTR)
*+
*  Name:
*     DEFCH

*  Purpose:
*     Define the valid keys for choice input from the command terminal.
*     Convert the specified characters to upper case and store them in
*     COMMON.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     CHOSTR = CHAR (Given)
*         Character string containing valid keys

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
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
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     sgs_1UPCAS

*  Written To Common:
*     CHOIST      c        current valid choice characters
*     LCHOST      i        number of valid choice characters

*-

      IMPLICIT NONE

      INCLUDE 'sgscom'


      CHARACTER*(*) CHOSTR



*   Save length of string
      LCHOST = MIN(LEN(CHOSTR),MAXCHO)

*   Copy character into common block and convert to upper case
      CALL sgs_1UPCAS(CHOSTR(:LCHOST),CHOIST)

      END
