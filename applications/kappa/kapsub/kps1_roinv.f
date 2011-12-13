      SUBROUTINE KPS1_ROINV( X, Y, CANGLE, SANGLE, XP, YP, STATUS )
*+
*  Name:
*     KPS1_ROINV

*  Purpose:
*     Calculates co-ordinates of original point before rotation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ROINV( X, Y, CANGLE, SANGLE, XP, YP, STATUS )

*  Description:
*     This routine takes the real co-ordinates of a point (X,Y)
*     in an image which has been rotated from the original by ANGLE
*     degrees clockwise, as given by the sine and cosine of the angle,
*     SANGLE and CANGLE, and returns the real co-ordinates of the
*     point (XP,YP) from which X,Y was transformed in the rotation.
*     In essence it performs the inverse rotation transformation.

*  Arguments:
*     X = REAL (Given)
*        X distance of point from the origin in the transformed array.
*     Y = REAL (Given)
*        Y distance of point from the origin in the transformed array.
*     CANGLE = REAL (Given)
*        Cosine of the rotation angle applied.
*     SANGLE = REAL (Given)
*        Sine of the rotation angle applied.
*     XP = REAL (Returned)
*        X distance of point from the origin in the original frame.
*     YP = REAL (Returned)
*        Y distance of point from the origin in the original frame.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Algorithm:
*     This just uses a standard rotation matrix approach to calculate
*     the x and y distances ( XP,YP ) of a point from the origin prior
*     to a rotation of ANGLE degrees clockwise from the x and y
*     distances ( X,Y ) of the transformed point from the origin.
*     Obviously this may be considered as evaluating the transformed
*     co-ordinates of a point after an anticlockwise rotation of the
*     same amount.  The matrix used is inverse of one used in
*     KPS_ROFWD:
*
*         (  cos ANGLE  -sin ANGLE  ) ( X )     ( X' )
*         (                         ) (   ) =   (    )
*         (  sin ANGLE   cos ANGLE  ) ( Y )     ( Y' )

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
*        First implementation for ROTNRS.
*     1986 September 9 (MJC):
*        Renamed from OLDCOORDS.  Added arguments section to
*        prologue and tidied.
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

*  Just evaluate the pre-rotation distances XP,YP from the
*  post-rotation distances X,Y and the clockwise rotation ANGLE using a
*  matrix transform.
      XP = ( X * CANGLE )  - ( Y * SANGLE )
      YP = ( X * SANGLE )  + ( Y * CANGLE )

      END
