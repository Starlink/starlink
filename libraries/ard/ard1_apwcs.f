      SUBROUTINE ARD1_APWCS( NDIM, TR, AWCS, STATUS )
*+
*  Name:
*     ARD1_APWCS

*  Purpose:
*     Create the Application FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_APWCS( NDIM, TR, AWCS, STATUS )

*  Description:
*     This routine creates a FrameSet in which the Base Frame corresponds
*     to pixel coordinates within the mask, and the current Frame is the
*     default user coordinate system.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of pixel axes in the mask supplied to ARD_WORK.
*     TR( * ) = REAL (Given)
*        The coefficients of the linear transformation from application
*        coords to pixel coords, supplied to ARD_WORK. Ignored if a call
*        to ARD_WCS has already been made.
*     AWCS = INTEGER (Returned)
*        An AST pointer to the returned Object. AST__NULL is returned if
*        an error occurs.
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
*     17-JUL-2001 (DSB):
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
      INCLUDE 'ARD_ERR'          ! ARD error constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'ARD_CONST'        ! ARD provate constants

*  Global Constants:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_AWCS = INTEGER (Read)
*           A pointer to the application FrameSet.
*        CMN_ADOM = CHARACTER*40 (Read)
*           The Domain name associated with pixel
*           coordinates in the mask array.

*  Arguments Given:
      INTEGER NDIM
      REAL TR( * )

*  Arguments Returned:
      INTEGER AWCS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARD1_INIT         ! Initialise ARD common blocks

*  Local Variables:
      DOUBLE PRECISION INA( ARD__MXDIM ) ! Position A without offset
      DOUBLE PRECISION INB( ARD__MXDIM ) ! Position B without offset
      DOUBLE PRECISION M( ARD__MXDIM*ARD__MXDIM ) ! Matrix elements
      DOUBLE PRECISION OFFV              ! Offset term
      DOUBLE PRECISION OUTA( ARD__MXDIM )! Position A with offset
      DOUBLE PRECISION OUTB( ARD__MXDIM )! Position B with offset
      INTEGER F1                 ! Application coords Frame
      INTEGER F2                 ! Pixel coords Frame
      INTEGER FR                 ! Another Frame
      INTEGER I                  ! Row index
      INTEGER IPIX               ! Index of PIXEL Frame
      INTEGER J                  ! Column index
      INTEGER K                  ! Index into matrix element array
      INTEGER L                  ! Index into supplied coefficient array
      INTEGER M1                 ! MatrixMap
      INTEGER M2                 ! WinMap
      INTEGER M3                 ! Pixel to application coords Mapping
*.

*  Initialise returned pointer.
      AWCS = AST__NULL

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If ARD_WCS has been called, take a copy of the FrameSet supplied to
*  ARD_WCS.
      IF( CMN_AWCS .NE. AST__NULL ) THEN
         AWCS = AST_COPY( CMN_AWCS, STATUS )

*  Check each Frame until one is found with Domain stored in CMN_ADOM.
*  This is the frame that describes pixel coordinates in the mask array.
         IPIX = AST__NOFRAME
         DO I = 1, AST_GETI( AWCS, 'NFRAME', STATUS )
            FR = AST_GETFRAME( AWCS, I, STATUS )

            IF( AST_GETC( FR, 'DOMAIN', STATUS ) .EQ. CMN_ADOM ) THEN
               CALL AST_ANNUL( FR, STATUS )
               IPIX = I
               GO TO 10
            END IF

            CALL AST_ANNUL( FR, STATUS )

         END DO

 10      CONTINUE

*  If a suitable Frame was found, make it th ebase Frame.
         IF( IPIX .NE. AST__NOFRAME ) THEN
            CALL AST_SETI( AWCS, 'BASE', IPIX, STATUS )

*  Check it has the required number of axes.
            IF( AST_GETI( AWCS, 'Nin', STATUS ) .NE. NDIM ) THEN
               IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = ARD__NOPIX
                  CALL MSG_SETC( 'D', CMN_ADOM )
                  CALL MSG_SETI( 'N', AST_GETI( AWCS, 'Nin', STATUS ) )
                  CALL MSG_SETI( 'M', NDIM )
                  CALL ERR_REP( 'ARD1_APWCS_ERR2', 'The ^D Frame '//
     :                          'specified using ARD_WCS has ^N axes '//
     :                          'but ^M are required.', STATUS )
               END IF
            END IF

*  Application coordinates are given by the current Frame of the WCS
*  FrameSet. Therefore, the Mapping from the current Frame to Application
*  Coordinates Frame is a UnitMap.
            M3 = AST_UNITMAP( AST_GETI( AWCS, 'NAXES', STATUS ), ' ',
     :                        STATUS )

*  Report an error if no pixel frame was found.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = ARD__NOPIX
            CALL MSG_SETC( 'D', CMN_ADOM )
            CALL ERR_REP( 'ARD1_APWCS_ERR1', 'The FrameSet specified '//
     :                  'using ARD_WCS has no Frame with Domain '//
     :                  '''^D'' (possible programming error).', STATUS )
         END IF

*  If no WCS FrameSet was supplied, create a new FrameSet containing just
*  a PIXEL Frame.
      ELSE
         F2 = AST_FRAME( NDIM, 'DOMAIN=PIXEL,Title=Pixel coordinates',
     :                   STATUS )
         AWCS = AST_FRAMESET( F2, ' ', STATUS )
         CALL AST_ANNUL( F2, STATUS )

*  The Mapping from PIXEL to application coords is given by argument TR. If
*  the first element of the supplied  transformation is bad, use a UnitMap to
*  connect this Frame to the PIXEL Frame.
         IF( TR( 1 ) .EQ. VAL__BADR ) THEN
            M3 = AST_UNITMAP( NDIM, ' ', STATUS )

*  Otherwise, create a CmpMap representing the Mapping from this
*  Frame to the PIXEL Frame.
         ELSE

*  First create a MatrixMap representing the matrix part of the mapping
*  (i.e. skipping over the elements of TR which represent the constant
*  offset vector).
            K = 1
            L = 1
            DO I = 1, NDIM
               L = L + 1
               DO J = 1, NDIM
                  M( K ) = TR( L )
                  K = K + 1
                  L = L + 1
               END DO
            END DO

            M1 = AST_MATRIXMAP( NDIM, NDIM, 0, M, ' ', STATUS )

*  Now create a WinMap representing the vector offset elements.
            DO I = 1, NDIM
               OFFV = TR( ( 1 + NDIM )*I - NDIM )
               INA( I ) = 0.0
               INB( I ) = OFFV
               OUTA( I ) = OFFV
               OUTB( I ) = 2*OFFV
            END DO

            M2 = AST_WINMAP( NDIM, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Combine the MatrixMap and the WinMap. This is the Mapping from
*  application coords to pixel coords.
            M3 = AST_CMPMAP( M1, M2, .TRUE., ' ', STATUS )

*  Annull AST objects.
            CALL AST_ANNUL( M1, STATUS )
            CALL AST_ANNUL( M2, STATUS )

*  Invert the Mapping to get the Mapping from PIXEL to ARDAPP.
            CALL AST_INVERT( M3, STATUS )

         END IF

      END IF

*  Create a Frame representing ARD application coords. Base this on a
*  copy of the current Frame in order to inherit the correct class of Frame.
      F1 = AST_COPY( AST_GETFRAME( AWCS, AST__CURRENT, STATUS ),
     :               STATUS )
      CALL AST_SETC( F1, 'DOMAIN', 'ARDAPP', STATUS )
      CALL AST_SETC( F1, 'TITLE', 'ARD application coordinates',
     :               STATUS )

*  Add this Frame into the returned FrameSet.
      CALL AST_ADDFRAME( AWCS, AST__CURRENT, M3, F1, STATUS )

*  Annull AST objects.
      CALL AST_ANNUL( M3, STATUS )
      CALL AST_ANNUL( F1, STATUS )

      END
