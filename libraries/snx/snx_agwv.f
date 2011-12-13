      SUBROUTINE snx_AGWV
*+
*  Name:
*     AGWV

*  Purpose:
*     Set up NCAR AUTOGRAPH graph window to match the
*     current GKS viewport.

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 1986 Science & Engineering Research Council. All
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
*     {enter_new_authors_here}

*  History:
*     01-APR-1986 (PTW):
*        Original.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     GQCNTN, GQNT, AGSETP

*-

      IMPLICIT NONE

      REAL WIND(4),VIEWP(4)
      INTEGER J

      INTEGER NCT



*  Inquire the current GKS transformation number
      CALL GQCNTN(J,NCT)

*  Find out what part of the NDC square is available
      CALL GQNT(NCT,J,WIND,VIEWP)

*  Establish the AUTOGRAPH coordinate system
      CALL AGSETP('GRAPH.',VIEWP,4)

      END
