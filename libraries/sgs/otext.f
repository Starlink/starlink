      SUBROUTINE sgs_OTEXT
*+
*  Name:
*     OTEXT

*  Language:
*     Starlink Fortran 77

*  Description:
*     Output completed text string.

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

*  Constants From Sgscom:
*     LTEXT      i       length of text buffer

*  Externals:
*     GTX

*  Read From Common:
*     NTEXT      i       length of current string
*     XTEXT      r       position of current string (x)
*     YTEXT      r          "     "     "       "   (y)
*     CTEXT      c       current text string

*  Written To Common:
*     NTEXT      i       length of current string

*-

      IMPLICIT NONE

      INCLUDE 'sgscom'

      INTEGER LS



*  Make sure there's a string to output
      IF (NTEXT.GT.0) THEN

*     String length
         LS=MIN(LTEXT,NTEXT)

*     Output the string
         CALL GTX(XTEXT,YTEXT,CTEXT(:LS))

*     Reset
         NTEXT=-1
      END IF

      END
