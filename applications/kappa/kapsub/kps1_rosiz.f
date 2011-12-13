      SUBROUTINE KPS1_ROSIZ( IDIMS, ANGLE, ODIMS, STATUS )
*+
*  Name:
*     KPS1_ROSIZ

*  Purpose:
*     Derives the size of the output array for ROTATE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ROSIZ( IDIMS, ANGLE, ODIMS, STATUS )

*  Description:
*     This routine works out the dimensions of the output array
*     required by ROTATE when a non-right angle rotation has been
*     requested.  Input are the input image dimensions and the rotation
*     angle in clockwise degrees, and returned are the dimensions of
*     the output array that is to be used to hold the rotated image.

*  Arguments:
*     IDIMS( 2 ) = INTEGER (Given)
*        Dimensions of image to be rotated.
*     ANGLE = REAL (Given)
*        Rotation angle in degrees
*     ODIMS( 2 ) = INTEGER (Returned)
*        Dimensions of the output array to hold the rotated image.
*     STATUS = INTEGER (Given and Returned)
*        Global status parameter.

*  Algorithm:
*     Just calls KPS1_ROFWD for the each of the array corners, which
*     returns the x and y distances of the transformed extremities
*     from the array centre.  Taking maxima and minima of these values
*     and adding them finds the size of output array required to hold
*     the rotated input array dimensions.

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
*        First implementation.
*     1986 September 9 (MJC):
*        Prologue completed, renamed from ROSIZE, renamed NEWCOORDS to
*        NCOORD and nearly conformed to Starlink standards.
*     1995 May 17 (MJC):
*        Renamed from ROTSIZ.  Used SST prologue and modern-style
*        declarations.  Shortened long lines.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! no implicit typing allowed


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Arguments Given:
      INTEGER IDIMS( 2 )
      REAL ANGLE

*  Arguments Returned:
      INTEGER ODIMS( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL DGTORD                ! Degrees to radians
      PARAMETER ( DGTORD = 0.01745329 )

*  Local Variables:
      REAL CANGLE                ! Ccosine of the rotation angle
      REAL CENTRX                ! X co-ordinate of array centre
      REAL CENTRY                ! Y co-ordinate of array centre
      REAL CX( 4 )               ! X distances of input array corners
                                 ! from array centre
      REAL CY( 4 )               ! Y distances of input array corners
                                 ! from array centre
      REAL CXP( 4 )              ! X distances of transformed corners
                                 ! from array centre
      REAL CYP( 4 )              ! Y distances of transformed corners
                                 ! from array centres
      INTEGER I                  ! Counter
      INTEGER IMAXX              ! Rounded up integer version of MAXX
      INTEGER IMAXY              ! Rounded up integer version of MAXY
      INTEGER IMINX              ! Rounded up integer version of MINX
      INTEGER IMINY              ! Rounded up integer version of MINY
      REAL MAXX                  ! Maximum transformed x co-ordinate
      REAL MAXY                  ! Maximum transformed y co-ordinate
      REAL MINX                  ! Minimum transformed x co-ordinate
      REAL MINY                  ! Minimum transformed y co-ordinate
      REAL SANGLE                ! Sine of the rotation angle

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the sine and cosine of the input angle.
      CANGLE = COS( DGTORD * ANGLE )
      SANGLE = SIN( DGTORD * ANGLE )

*  Initialise the arrays which contain the the co-ordinates of the
*  corners of the input image; we work in terms of distance from the
*  array centre throughout.
      CENTRX = REAL( IDIMS( 1 ) ) / 2.0
      CENTRY = REAL( IDIMS( 2 ) ) / 2.0

*  Bottom-left corner.
      CX( 1 ) = - CENTRX
      CY( 1 ) = - CENTRY

*  Top-left corner.
      CX( 2 ) = - CENTRX
      CY( 2 ) = + CENTRY

*  Top-right corner.
      CX( 3 ) = + CENTRX
      CY( 3 ) = + CENTRY

*  Bottom-right corner.
      CX( 4 ) = + CENTRX
      CY( 4 ) = - CENTRY

*  Now transform each of these corners into the rotated frame.
      DO I = 1, 4
         CALL KPS1_ROFWD( CX( I ), CY( I ), CANGLE, SANGLE, CXP( I ),
     :                    CYP( I ), STATUS )
      END DO

*  Get the maximum and minimum distances from the array centre.
      MAXX = MAX( CXP( 1 ), CXP( 2 ), CXP( 3 ), CXP( 4 ) )
      MAXY = MAX( CYP( 1 ), CYP( 2 ), CYP( 3 ), CYP( 4 ) )
      MINX = MIN( CXP( 1 ), CXP( 2 ), CXP( 3 ), CXP( 4 ) )
      MINY = MIN( CYP( 1 ), CYP( 2 ), CYP( 3 ), CYP( 4 ) )

*  Round up the maxima and minima to the nearest integer such that the
*  output array is both big enough and symmetric about the array centre.
      IMAXX = NINT( MAXX + 0.5 )
      IMAXY = NINT( MAXY + 0.5 )
      IMINX = NINT( MINX - 0.5 )
      IMINY = NINT( MINY - 0.5 )

*  Calculate the output array dimensions from these values.
      ODIMS( 1 ) = IMAXX - IMINX
      ODIMS( 2 ) = IMAXY - IMINY

      END
