      SUBROUTINE sgs_SHMK (S)
*+
*  Name:
*     SHMK

*  Purpose:
*     Specify marker size.

*  Language:
*     Starlink Fortran 77

*  Description:
*     This routine is obsolete and does nothing other than store the
*     specified size in common.

*  Arguments:
*     S = REAL (Given)
*         Marker height (=width)

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

*  Written To Common:
*     HMK       r      current marker height

*-

      IMPLICIT NONE
      REAL S

      INCLUDE 'sgscom'




      HMK=S

      END
