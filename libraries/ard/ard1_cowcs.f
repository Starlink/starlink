      SUBROUTINE ARD1_COWCS( AWCS, C, UWCS, STATUS )
*+
*  Name:
*     ARD1_COWCS

*  Purpose:
*     Create a new user FrameSet from a COEFFS statement.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_COWCS( AWCS, C, UWCS, STATUS )

*  Description:
*     This routine creates a new user FrameSet (UWCS) from the
*     supplied linear Mapping coefficients.

*  Arguments:
*     AWCS = INTEGER (Given)
*        The application FrameSet.
*     C( * ) = DOUBLE PRECISION (Given)
*        The coefficients of the user->app mapping. If element 1 is
*        AST__BAD a unit Mapping is used.
*     UWCS = INTEGER (Returned)
*        An AST pointer to the returned User FrameSet. The Current Frame
*        in this FrameSet is user coords, and the Base Frame is
*        "Application co-ordinates" (Domain ARDAPP).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-JUL-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'ARD_CONST'        ! ARD private constants
      INCLUDE 'ARD_ERR'          ! ARD error constants

*  Arguments Given:
      INTEGER AWCS
      DOUBLE PRECISION C( * )

*  Arguments Returned:
      INTEGER UWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION INA( ARD__MXDIM ) ! Position A without offset
      DOUBLE PRECISION INB( ARD__MXDIM ) ! Position B without offset
      DOUBLE PRECISION M( ARD__MXDIM*ARD__MXDIM ) ! Matrix elements
      DOUBLE PRECISION OFFV              ! Offset term
      DOUBLE PRECISION STEP              ! Step between window corners
      DOUBLE PRECISION OUTA( ARD__MXDIM )! Position A with offset
      DOUBLE PRECISION OUTB( ARD__MXDIM )! Position B with offset
      INTEGER F1                 ! User coords Frame
      INTEGER F2                 ! Application coords Frame
      INTEGER I                  ! Row index
      INTEGER J                  ! Column index
      INTEGER K                  ! Index into matrix element array
      INTEGER L                  ! Index into supplied coefficient array
      INTEGER M1                 ! MatrixMap
      INTEGER M2                 ! WinMap
      INTEGER M3                 ! User to application coords Mapping
      INTEGER NDIM               ! No. of axes in user coords
*.

      UWCS = AST__NULL

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the Curent Frame in the application FrameSet has domain ARDAPP.
      IF( AST_GETC( AWCS, 'DOMAIN', STATUS ) .NE. 'ARDAPP' ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETC( 'D', AST_GETC( AWCS, 'DOMAIN', STATUS ) )
         CALL ERR_REP( 'ARD1_COWCS_ERR1', 'ARD1_COWCS: Current Frame '//
     :                 'in supplied application FrameSet has Domain '//
     :                 '''^D''. This should be ''ARDAPP'' '//
     :                 '(programming error).', STATUS )
         GO TO 999
      END IF

*  Save the number of axes in the current Frame
      NDIM = AST_GETI( AWCS, 'NAXES', STATUS )

*  IF the first element is bad, use a UnitMap.
      IF( C( 1 ) .EQ. AST__BAD ) THEN
         M3 = AST_UNITMAP( NDIM, ' ', STATUS )

*  Otherwise,
      ELSE

*  Create a MatrixMap representing the matrix part of the mapping (i.e.
*  skipping over the elements of C which represent the constant offset
*  vector.
         K = 1
         L = 1
         DO I = 1, NDIM
            L = L + 1
            DO J = 1, NDIM
               M( K ) = C( L )
               K = K + 1
               L = L + 1
            END DO
         END DO

         M1 = AST_MATRIXMAP( NDIM, NDIM, 0, M, ' ', STATUS )

*  Now create a WinMap representing the vector offset elements.
         DO I = 1, NDIM
            OFFV = C( ( 1 + NDIM )*I - NDIM )
            STEP = MIN( OFFV, 1.0D-6 )
            INA( I ) = 0.0
            INB( I ) = STEP
            OUTA( I ) = OFFV
            OUTB( I ) = STEP + OFFV
         END DO

         M2 = AST_WINMAP( NDIM, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Combine the MatrixMap and the WinMap. This is the Mapping from
*  user coords to application coords.
         M3 = AST_CMPMAP( M1, M2, .TRUE., ' ', STATUS )

*  Invert the Mapping so that the forward transformation goes from
*  application coordinates to user coordinates.
         CALL AST_INVERT( M3, STATUS )

*  Annull AST objects.
         CALL AST_ANNUL( M1, STATUS )
         CALL AST_ANNUL( M2, STATUS )

      END IF

*  Now create a Frame to represent user coords. Base this on a
*  copy of the ARDAPP Frame in the application FrameSet, in order
*  to inherit the correct class of Frame.
      F1 = AST_COPY( AST_GETFRAME( AWCS, AST__CURRENT, STATUS ),
     :               STATUS )
      CALL AST_SETC( F1, 'DOMAIN', 'ARDUSER', STATUS )
      CALL AST_SETC( F1, 'TITLE', 'ARD user coordinates', STATUS )

*  Now create a Frame to represent application coords.
      F2 = AST_COPY( F1, STATUS )
      CALL AST_SETC( F2, 'DOMAIN', 'ARDAPP', STATUS )
      CALL AST_SETC( F2, 'TITLE', 'ARD application coordinates',
     :               STATUS )

*  Construct the FrameSet, with user coords as Current Frame, and
*  application coords as Base Frame.
      UWCS = AST_FRAMESET( F2, ' ', STATUS )
      CALL AST_ADDFRAME( UWCS, AST__BASE, M3, F1, STATUS )

*  Arrive here if an error occurs.
 999  CONTINUE

*  Annull AST objects.
      CALL AST_ANNUL( M3, STATUS )
      CALL AST_ANNUL( F1, STATUS )
      CALL AST_ANNUL( F2, STATUS )

      END
