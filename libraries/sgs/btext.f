      SUBROUTINE sgs_BTEXT (X,Y)
*+
*  Name:
*     BTEXT

*  Purpose:
*     Begin a new text string.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     X = REAL (Given)
*         Position of string (X)
*     Y = REAL (Given)
*         Position of string (Y)

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
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External:
*     sgs_OTEXT

*  Read From Common:
*     NTEXT   i     length of current string

*  Written To Common:
*     XTEXT   r     position of current string (X)
*     YTEXT   r     position of current string (Y)
*     NTEXT   i     length of current string

*-

      IMPLICIT NONE

      REAL X,Y

      INCLUDE 'sgscom'




*  Flush any existing text string
      IF (NTEXT.GT.0) CALL sgs_OTEXT

*  Copy starting X,Y
      XTEXT=X
      YTEXT=Y

*  Initialise character count
      NTEXT=0

      END
