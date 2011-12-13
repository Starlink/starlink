      SUBROUTINE sgs_1UPCAS (IN, OUT)
*+
*  Name:
*     UPCAS

*  Purpose:
*     Convert character string to uppercase.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Arguments:
*     IN = CHAR (Given)
*         String to be converted
*     OUT = CHAR (Returned)
*         Converted string

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

*  Externals:
*     chr_UCASE

*-

      IMPLICIT NONE

      CHARACTER*(*) IN,OUT



      OUT = IN
      CALL chr_UCASE(OUT)

      END
