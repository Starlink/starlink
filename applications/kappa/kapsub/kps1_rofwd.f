      SUBROUTINE KPS1_ROFWD( X, Y, CANGLE, SANGLE, XP, YP, STATUS )

*+
*  Name:
*     KPS1_ROFWD

*  Purpose:
*     Calculates transformed co-ordinates after a rotation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ROFWD( X, Y, CANGLE, SANGLE, XP, YP, STATUS )

*  Description:
*     This subroutine takes the input distances X and Y and
*     rotates them through ANGLE degrees clockwise (as given
*     by the sine and cosine SANGLE and CANGLE) to their new
*     values XP and YP using a rotation matrix.

*  Arguments:
*     X  =  REAL (Given)
*        X distance of point from the origin 0.0.
*     Y  =  REAL (Given)
*        Y distance of point from the origin 0.0.
*     CANGLE  =  REAL (Given)
*        Cosine of the rotation angle to be applied.
*     SANGLE  =  REAL (Given)
*        Sine of the rotation angle to be applied.
*     XP  =  REAL (Returned)
*        X distance of point from the origin after rotation.
*     YP  =  REAL (Returned)
*        Y distance of point from the origin after rotation.
*     STATUS  =  INTEGER (Given)
*        Global status value

*  Algorithm:
*     Just uses a standard rotation-matrix approach to calculate the
*     transformed distances of a point from the origin 0.0 after an
*     angular rotation of ANGLE degrees clockwise i.e.
*
*               (  cos ANGLE  sin ANGLE  ) ( X )     ( X' )
*               (                        ) (   )  =  (    )
*               ( -sin ANGLE  cos ANGLE  ) ( Y )     ( Y' )

*  Copyright:
*     Copyright (C) 1985-1986 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     MJM: Mark McCaughrean (UoE)
*     MJC: Malcolm Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-11-1985 (MJM):
*        First implementation for ROTSIZE.
*     1986 September 9 (MJC):
*        Renamed from NEWCOORDS.  Added arguments section to prologue
*        and tidied.
*     1995 May 15 (MJC):
*        Renamed from OCOORDS.  Used SST-style prologue and modern
*        variable declarations.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! no implicit typing allowed


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions


*  Arguments Given:
      REAL X
      REAL Y
      REAL CANGLE
      REAL SANGLE

*  Arguments Returned:
      REAL XP
      REAL YP

*  Status:
      INTEGER  STATUS            ! Global status

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Just apply the standard rotation matrix, noting that the rotation is
*  clockwise.
      XP  =  ( X * CANGLE ) + ( Y * SANGLE )
      YP  =  ( Y * CANGLE ) - ( X * SANGLE )

      END
