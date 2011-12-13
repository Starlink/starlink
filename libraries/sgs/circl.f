      SUBROUTINE sgs_CIRCL (XCENT,YCENT, RADIUS)
*+
*  Name:
*     CIRCL

*  Purpose:
*     Plot a circle with the current SGS pen.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     XCENT = REAL (Given)
*         Centre x (World Coordinates)
*     YCENT = REAL (Given)
*         "    y (  "        "     )
*     RADIUS = REAL (Given)
*         Radius   (  "        "     )

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
*     sgs_ARC

*-

      IMPLICIT NONE

      REAL XCENT,YCENT,RADIUS


      CALL sgs_ARC(XCENT,YCENT,RADIUS,0.0,6.2832)

      END
