      SUBROUTINE ARD1_DMWCS( AWCS, PAR, UWCS, STATUS )
*+
*  Name:
*     ARD1_DMWCS

*  Purpose:
*     Create a new user FrameSet from a DIMENSION statement.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_DMWCS( AWCS, PAR, UWCS, STATUS )

*  Description:
*     This routine creates a new user FrameSet (UWCS) from the
*     supplied parameters. If the current Frame already has the
*     required number of axes, then it is returned unchanged.
*     Otherwise, if the current Frame has more than "NDIM" axes,
*     the first "NDIM" axex are picked form the supplied FrameSet.
*     Otherwise, if the current Frame has less than "NDIM" axes,
*     extra axes are added to the supplied FrameSet.

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
*     Copyright (C) 2009,2012 Science & Technology Facilities Council.
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
*     18-NOV-2009 (DSB):
*        Original version.
*     26-SEP-2012 (DSB):
*        Cater for cases where the original current Frame has
*        insufficient axes.
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
      INCLUDE 'ARD_CONST'        ! ARD private constants

*  Arguments Given:
      INTEGER AWCS
      DOUBLE PRECISION PAR( * )

*  Arguments Returned:
      INTEGER UWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER AXES( ARD__MXDIM ) ! Input axes to pick
      INTEGER FR                 ! Frame to pick axes from
      INTEGER FS                 ! New user FrameSet
      INTEGER I                  ! Axis index
      INTEGER IBASE              ! Base Frame index in supplied UWCS
      INTEGER ICURR              ! Current Frame index in supplied UWCS
      INTEGER IFRAME             ! Index of Frame being picked from
      INTEGER JUNK               ! Unused Mapping
      INTEGER MAP                ! Mapping for picked axes
      INTEGER NCURAX             ! No. of axes in current frame
      INTEGER NDIM               ! No. of axes in user coords
      INTEGER NEWFR              ! Frame containing picked axes
      INTEGER NFRAME             ! Number of Frames in FrameSet
      INTEGER NOUT               ! Number of output axes
      INTEGER OUTAX( ARD__MXDIM )! Output axes to pick
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the required number of current Frame axes.
      NDIM = NINT( PAR( 1 ) )

*  How many axes in the current Frame?
      NCURAX = AST_GETI( UWCS, 'NAXES', STATUS )

*  If the current Frame already has the right number of axes, do nothing.
      IF( NDIM .NE. NCURAX ) THEN

*  Create a unit array of the maximum possible size.
         DO I = 1, ARD__MXDIM
            AXES( I ) = I
         END DO

*  Pick the required axes from the current Frame.
         FR = AST_GETFRAME( UWCS, AST__CURRENT, STATUS )
         NEWFR = AST_PICKAXES( FR, NDIM, AXES, JUNK, STATUS )
         CALL AST_ANNUL( FR, STATUS )
         CALL AST_ANNUL( JUNK, STATUS )

*  Use this to form a new FrameSet. It has index 1 in the new FrameSet.
         FS = AST_FRAMESET( NEWFR, ' ', STATUS )
         CALL AST_ANNUL( NEWFR, STATUS )

*  Note the index of the current and base Frame in the supplied FrameSet.
         ICURR = AST_GETI( UWCS, 'Current', STATUS )
         IBASE = AST_GETI( UWCS, 'Base', STATUS )

*  Loop round all Frames, skipping the current Frame.
         NFRAME = AST_GETI( UWCS, 'NFrame', STATUS )
         DO IFRAME = 1, NFRAME
            IF( IFRAME .NE. ICURR ) THEN
               FR = AST_GETFRAME( UWCS, IFRAME, STATUS )

*  Get the Mapping from the current Frame to this Frame.
               MAP = AST_GETMAPPING( UWCS, AST__CURRENT, IFRAME,
     :                               STATUS )

*  If we are reducing the number of axes, split of the required axes from
*  this mapping. Report an error if this cannot be done.
               IF( NDIM .LT. NCURAX ) THEN
                  CALL AST_MAPSPLIT( MAP, NDIM, AXES, OUTAX, MAP,
     :                               STATUS )
                  IF( MAP .EQ. AST__NULL .AND.
     :                STATUS .EQ. SAI__OK ) THEN
                     STATUS = ARD__NOTAL
                     CALL MSG_SETI( 'N', NDIM )
                     CALL MSG_SETI( 'M', NDIM )
                     CALL ERR_REP( ' ', 'The ARD description is '//
     :                             '^N-dimensional, but the first ^M '//
     :                             'WCS axes cannot be used '//
     :                             'independently of the remaining '//
     :                             'WCS axes.', status )
                  END IF

*  If we are increasing the number of axes, put a UnitMap in parallel
*  with the mapping.
               ELSE
                  MAP = AST_CMPMAP( MAP, AST_UNITMAP( NDIM - NCURAX,
     :                                                ' ', STATUS ),
     :                              .FALSE., ' ', STATUS )
               END IF

*  If the mapping has been modified succesfully, pick the appropriate axes
*  from the frame, and add the picked Frame into the new FrameSet using the
*  modified mapping.
               IF( STATUS .EQ. SAI__OK ) THEN
                  NOUT = AST_GETI( MAP, 'Nout', STATUS )
                  NEWFR = AST_PICKAXES( FR, NOUT, AXES, JUNK, STATUS )
                  CALL AST_ANNUL( JUNK, STATUS )
                  CALL AST_ADDFRAME( FS, 1, MAP, NEWFR, STATUS )
                  CALL AST_ANNUL( MAP, STATUS )
                  CALL AST_ANNUL( NEWFR, STATUS )

*  If we have just added the original Base Frame into the new FrameSet,
*  set it to be the base Frame in the new FrameSet too.
                  IF( IFRAME .EQ. IBASE ) THEN
                     CALL AST_SETI( FS, 'Base',
     :                              AST_GETI( FS, 'Current', STATUS ),
     :                              STATUS )
                  END IF

               END IF
               CALL AST_ANNUL( FR, STATUS )
            END IF
         END DO

*  The original current Frame is Frame 1 in the new FrameSet.
         CALL AST_SETI( FS, 'Current', 1, STATUS )

*  If no error has occurred, use the new FrameSet in place of the
*  original FrameSet. Otherwise, annul the new FrameSet.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL AST_ANNUL( UWCS, STATUS )
            UWCS = FS
         ELSE
            CALL AST_ANNUL( FS, STATUS )
         END IF

      END IF

      END
