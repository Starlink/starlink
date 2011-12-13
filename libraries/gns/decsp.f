      SUBROUTINE gns_1DECSP (SPEC,NAME,LNAME,PDEV,LPDEV)
*+
*  Name:
*     gns_1DECSP

*  Purpose:
*     Decompose a device specification into a device name and

*  Language:
*     Starlink Fortran 77

*  Description:
*     a device type.
*     A device specification consists of a device name optionally
*     followed by a physical device name separated by a semi-colon.
*     Any leading or trailing spaces are removed from the strings.
*     If a component does not exist then a length of zero is returned as
*     the length and the string set to all blanks.

*  Arguments:
*     SPEC = CHAR (Given)
*         Device specification
*     NAME = CHAR (Returned)
*         Name
*     LNAME = INTEGER (Returned)
*         Length of name
*     PDEV = CHAR (Returned)
*         Physical device name
*     LPDEV = INTEGER (Returned)
*         Length of physical device

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council. All
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
*     DLT: D L Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     2-JUN-1988 (DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External References:
*     gns_1TRIM

*  Implicit Inputs:
*     none

*  Implicit Outputs:
*     none

*-
      IMPLICIT NONE

      CHARACTER*(*) SPEC, NAME, PDEV
      INTEGER LNAME, LPDEV

      INTEGER I

      I = INDEX (SPEC,';')

      IF (I.EQ.0) THEN
         NAME = SPEC
         PDEV = ' '
      ELSE
         NAME = SPEC(:I-1)
         PDEV = SPEC(I+1:)
      END IF

      CALL gns_1TRIM (NAME,NAME,LNAME)
      CALL gns_1TRIM (PDEV,PDEV,LPDEV)

      END
