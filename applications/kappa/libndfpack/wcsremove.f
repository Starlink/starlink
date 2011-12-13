      SUBROUTINE WCSREMOVE( STATUS )
*+
*  Name:
*     WCSREMOVE

*  Purpose:
*     Remove co-ordinate Frames from the WCS component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL WCSREMOVE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application allows you to remove one or more co-ordinate Frames
*     from the WCS component in an NDF. The indices of any remaining Frames
*     are "shuffled down" to fill the gaps left by the removed Frames.

*  Usage:
*     wcsremove ndf frames

*  ADAM Parameters:
*     FRAMES()  = LITERAL (Read
*        The list of the indices (within the WCS component of the supplied NDF)
*        of the Frames to be removed. Any indices outside the range of the
*        available Frames are ignored. Single Frames or a set of adjacent
*        Frames may be specified, e.g. typing [4,6-9,12,14-16] will remove
*        Frames 4,6,7,8,9,12,14,15,16. (Note that the brackets are required to
*        distinguish this array of characters from a single string including
*        commas.  The brackets are unnecessary when there only one item.) If
*        you wish to remove all the files enter the wildcard *. 5-* will
*        remove from 5 to the last Frame.
*     NDF = NDF (Read and Write)
*        The NDF data structure.

*  Examples:
*     wcsremove m51 "3-5"
*        This removes Frames 3, 4 and 5 from the NDF "m51". Any remaining
*        Frames with indices higher than 5 will be re-numbered to fill the
*        gaps left by the removed Frames (i.e. the original Frame 6 will
*        become Frame 3, etc).

*  Notes:
*     - The Frames within the WCS component of an NDF may be examined
*     using application NDFTRACE.

*  Related Applications:
*     KAPPA: NDFTRACE, WCSADD, WCSFRAME, WCSATTRIB, WCSCOPY

*  Copyright:
*     Copyright (C) 1998-2000 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-APR-1998 (DSB):
*        Original version.
*     5-MAY-1999 (TDCA):
*        If an attempt is made to remove the Base Frame, WCSREMOVE now
*        refuses and continues to remove any other specified frames,
*        rather than simply reporting an error.
*     20-SEP-2000 (DSB):
*        Check STATUS before main loop.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MXGIVE             ! Max. no. of explicit Frame indices
      PARAMETER ( MXGIVE = 50 )

      INTEGER MXSPEC             ! Max. no. of index specification strings
      PARAMETER ( MXSPEC = 20 )

*  Local Variables:
      CHARACTER SPECS( MXSPEC )*10! The given index specifications strings
      INTEGER FIRST              ! First Frame index implied by specification
      INTEGER GIVEN( MXGIVE )    ! The given Frame indices, sorted
      INTEGER I                  ! Index into list of given Frame indices
      INTEGER IBASE              ! The index of the Base Frame
      INTEGER IFRM               ! The Frame index in the original FrameSet
      INTEGER IFRM0              ! The previous original Frame index
      INTEGER INDF               ! NDF identifier
      INTEGER ISPEC              ! The index of the current index specificiation
      INTEGER IWCS               ! AST pointer for WCS FrameSet
      INTEGER LAST               ! Last Frame index implied by specification
      INTEGER NFRM0              ! The number of Frames in the given NDF
      INTEGER NGIVE              ! The number of Frames given by the user
      INTEGER NGONE              ! The number of Frames removed so far
      INTEGER NSPEC              ! The number of index specifications given
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an identifier for the NDF.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )

*  Create an AST FrameSet from the WCS component of the NDF.
      CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Get the number of Frames in the FrameSet.
      NFRM0 = AST_GETI( IWCS, 'NFRAME', STATUS )

*  Get the index of the Base Frame in the FrameSet. The user is not
*  allowed to remove this Frame since it represents the basic Grid
*  co-ordinates to which all other Frames are connected.
      IBASE = AST_GETI( IWCS, 'BASE', STATUS )

*  Get a list of Frame index specifications.
      CALL PAR_GET1C( 'FRAMES', MXSPEC, SPECS, NSPEC, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise the number of indices given so far.
      NGIVE = 0

*  Process each Frame index specification in turn.
      DO ISPEC = 1, NSPEC

*  Calculate the Frame index limits implied by this specification.
         CALL KPG1_CNLIM( SPECS( ISPEC ), FIRST, LAST, STATUS )

*  A wildcard in the first part of a range or a full wildcard indicates the
*  Frames start from beginning, i.e. Frame one.
         FIRST = MAX( 1, FIRST )

*  A wildcard in the second part of the range or a full wildcard indicates
*  the Frames end at the last Frame.
         LAST = MIN( NFRM0, LAST )

*  Check each index in this range.
         DO IFRM = FIRST, LAST

*  Politely refuse if an attempt is made to remove the Base (GRID) Frame.
            IF( IFRM .EQ. IBASE ) THEN
               CALL MSG_BLANK( STATUS )
               CALL NDF_MSG( 'NDF', INDF )
               CALL MSG_OUT( 'WCSREMOVE_1', '   Attempt to remove '//
     :                       'Base Frame of ''^NDF'' ignored.', STATUS )
            ELSE

*  Add the Frame index into a list of indices. Report an error if the
*  list is full.
               NGIVE = NGIVE + 1
               IF( NGIVE .LE. MXGIVE ) THEN
                  GIVEN( NGIVE ) = IFRM

               ELSE

                  IF( STATUS .EQ. SAI__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'N', MXGIVE )
                     CALL ERR_REP( 'WCSREMOVE_2', 'Too many Frame '//
     :                          'indices given. Can only handle up '//
     :                          'to ^N.', STATUS )
                  END IF
                  GO TO 999

               END IF

            END IF

         END DO

      END DO

*  Sort the Frame indices into ascending order.
      IF( NGIVE .GT. 1 ) CALL KPG1_QSRTI( NGIVE, 1, NGIVE, GIVEN,
     :                                    STATUS )

*  Initialise the number of Frames removed so far.
      NGONE = 0

*  Initialise the index of the Frame last removed.
      IFRM0 = 0

*  Loop round each given index.
      DO I = 1, NGIVE

*  Get the index of the Frame to be removed in the original FrameSet.
         IFRM = GIVEN( I )

*  If this is the same as the previous index, the Frame has already been
*  removed so ignore it.
         IF( IFRM .NE. IFRM0 ) THEN

*  Note the original index of the Frame to be removed.
            IFRM0 = IFRM

*  Remove the Frame. The index used is reduced by the number of Frames
*  already removed because the indices of all higher Frames are reduced
*  by one each time a Frame is removed.
            CALL AST_REMOVEFRAME( IWCS, IFRM - NGONE, STATUS )

*  Increment the number of Frames removed.
            NGONE = NGONE + 1

         END IF

      END DO

*  Save a copy of the modified FrameSet in the NDF's WCS  component.
      IF( NGONE .GT. 0 ) CALL NDF_PTWCS( IWCS, INDF, STATUS )

*  Report how many Frames were removed.
      CALL MSG_BLANK( STATUS )
      CALL NDF_MSG( 'NDF', INDF )
      IF ( NGONE .EQ. 0 ) THEN
         CALL MSG_OUT( 'WCSREMOVE_3', '   No WCS Frames have been '//
     :                 'removed from ''^NDF''.', STATUS )

      ELSE IF ( NGONE .EQ. 1 ) THEN
         CALL MSG_OUT( 'WCSREMOVE_4', '   One WCS Frame has been '//
     :                 'removed from ''^NDF''.', STATUS )

      ELSE
         CALL MSG_SETI( 'NGONE', NGONE )
         CALL MSG_OUT( 'WCSREMOVE_5', '   ^NGONE WCS Frames have been'//
     :                 ' removed from ''^NDF''.', STATUS )
      END IF
      CALL MSG_BLANK( STATUS )

*  Arrive here if an error occurs.
 999  CONTINUE

*  Annul the pointer to the FrameSet.
      CALL AST_ANNUL( IWCS, STATUS )

*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'WCSREMOVE_ERR', 'WCSREMOVE: Failed to remove '//
     :                 'co-ordinate Frames from the WCS component of '//
     :                 'an NDF.', STATUS )
      END IF

      END
