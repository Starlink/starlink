      SUBROUTINE ARD1_TWWCS( AWCS, PAR, UWCS, STATUS )
*+
*  Name:
*     ARD1_TWWCS

*  Purpose:
*     Create a new user FrameSet from a TWIST statement.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_TWWCS( AWCS, PAR, UWCS, STATUS )

*  Description:
*     This routine creates a new user FrameSet (UWCS) from the
*     supplied parameters.

*  Arguments:
*     AWCS = INTEGER (Given)
*        The application FrameSet.
*     PAR( * ) = DOUBLE PRECISION (Given)
*        The statement parameters.
*     UWCS = INTEGER (Given)
*        An AST pointer to the User FrameSet. The Current Frame
*        in this FrameSet is user coords.
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
*     12-JUL-2001 (DSB):
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
      DOUBLE PRECISION PAR( * )

*  Arguments Returned:
      INTEGER UWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DOM*30           ! Frame Domain
      DOUBLE PRECISION COST      ! Cosine of twist angle
      DOUBLE PRECISION MAT( ARD__MXDIM*ARD__MXDIM )! Matrix elements
      DOUBLE PRECISION SINT      ! Sine of twist angle
      DOUBLE PRECISION T         ! Twist angle in radians
      INTEGER FR                 ! Pointer to a Frame
      INTEGER I                  ! Loop count
      INTEGER IFRM               ! Index of ARDAPP Frame
      INTEGER M1                 ! MatrixMap
      INTEGER NDIM               ! No. of axes in user coords
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the Curent Frame in the application FrameSet has domain ARDAPP.
      IF( AST_GETC( AWCS, 'DOMAIN', STATUS ) .NE. 'ARDAPP' ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETC( 'D', AST_GETC( AWCS, 'DOMAIN', STATUS ) )
         CALL ERR_REP( 'ARD1_TWWCS_ERR1', 'ARD1_TWWCS: Current Frame '//
     :                 'in supplied application FrameSet has Domain '//
     :                 '''^D''. This should be ''ARDAPP'' '//
     :                 '(programming error).', STATUS )
         GO TO 999
      END IF

*  Save the number of axes in the current Frame
      NDIM = AST_GETI( AWCS, 'NAXES', STATUS )

*  Locate a Frame in the current User FrameSet with Domain ARDAPP.
      IFRM = AST__NOFRAME
      DO I = 1, AST_GETI( UWCS, 'NFRAME', STATUS )
         FR = AST_GETFRAME( UWCS, I, STATUS )
         DOM = AST_GETC( FR, 'DOMAIN', STATUS )
         CALL AST_ANNUL( FR, STATUS )
         IF( DOM .EQ. 'ARDAPP' ) THEN
            IFRM = I
            GO TO 10
         END IF
      END DO
 10   CONTINUE

*  If no Frame with Domain ARDAPP was found, annull the existing User
*  FrameSet and create a new one containing user and application coordinate
*  Frames connected by a UnitMap.
      IF( IFRM .EQ. AST__NOFRAME ) THEN
         CALL AST_ANNUL( UWCS, STATUS )
         CALL ARD1_COWCS( AWCS, AST__BAD, UWCS, STATUS )
         IFRM = AST_GETI( UWCS, 'BASE', STATUS )
      END IF

*  Initialize the elements of the matrix to hold a unit matrix.
      DO I = 1, NDIM
         MAT( I ) = 0.0
      END DO

      DO I = 1, NDIM
         MAT( I + ( I - 1 )*NDIM ) = 1.
      END DO

*  Store the trig terms to rotate the first two axes.
      T = PAR( 1 ) * ARD__DTOR
      SINT = SIN( T )
      COST = COS( T )
      MAT( 1 ) = COST
      MAT( 2 ) = -SINT
      MAT( NDIM + 1 ) = SINT
      MAT( NDIM + 2 ) = COST

*  Create a MatrixMap from these matrix elements.
      M1 = AST_MATRIXMAP( NDIM, NDIM, 0, MAT, ' ', STATUS )

*  Remap the application coords Frame
      CALL AST_REMAPFRAME( UWCS, IFRM, M1, STATUS )

*  Arrive here if an error occurs.
 999  CONTINUE

*  Annull AST objects.
      CALL AST_ANNUL( M1, STATUS )

      END
