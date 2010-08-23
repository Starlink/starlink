      SUBROUTINE KPG1_ASFFR( TARGET, DOMAIN, IFRM, STATUS )
*+
*  Name:
*     KPG1_ASFFR

*  Purpose:
*     Finds an Frame with a given Domain within a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASFFR( TARGET, DOMAIN, IFRM, STATUS )

*  Description:
*     This routine finds the last Frame with a given Domain within a
*     FrameSet, and returns its index. The Current Frame in the FrameSet
*     is not changed.
*
*     The first and last component Frames within CmpFrames are included
*     in the search (component Frames in the middle of a CmpFrame cannot
*     be found as yet). If a matching Frame is found within a CmpFrame,
*     then a copy of the matching Frame is appended to the FrameSet. The
*     returned Frame index refers to this extracted component Frame, rather
*     than the CmpFrame from which it was extracted.

*  Arguments:
*     TARGET = INTEGER (Given)
*        An AST pointer for a FrameSet containing the Frames to be
*        searched.
*     DOMAIN = CHARACTER * ( * ) (Given)
*        The Domain name to be searched for.
*     IFRM = INTEGER (Returned)
*        The index of the matching Frame within the returned FrameSet.
*        Returned equal to AST__NOFRAME if no match was found, or if an
*        error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1998 (DSB):
*        Original version.
*     16-DEC-1998 (DSB):
*        Modified to return the index of the last matching Frame rather
*        than the first matching Frame.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER TARGET
      CHARACTER DOMAIN*(*)

*  Arguments Returned:
      INTEGER IFRM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER AXFRM              ! Pointer to one-dimensional axis Frame
      INTEGER FMATCH             ! Pointer to matching Frame
      INTEGER FRM                ! Pointer to next Frame
      INTEGER I			 ! Frame count
      INTEGER ICURR              ! Index of Current Frame in TARGET
      INTEGER INPRM( NDF__MXDIM )! Input axis permutation array
      INTEGER J                  ! Axis count
      INTEGER MAP                ! Mapping
      INTEGER NAX                ! No. of axes in next Frame
      INTEGER NFRM               ! No. of Frames in Target FrameSet
      INTEGER NOUT               ! No. of axes in required Domain
      INTEGER OUTPRM( NDF__MXDIM )! Indices of required axes
*.

*  Initialise returned values.
      IFRM = AST__NOFRAME

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the number of Frames in the Target FrameSet.
      NFRM = AST_GETI( TARGET, 'NFRAME', STATUS )

*  Loop round each Frame in the target FrameSet. Work backwards through
*  the Frames in order to find the last matching Frame in the FrameSet.
      DO I = NFRM, 1, -1

*  Get a pointer to the Frame.
         FRM = AST_GETFRAME( TARGET, I, STATUS )

*  Does this Frame have the required Domain? If so note its index and
*  break out of the Frame loop.
         IF( AST_GETC( FRM, 'DOMAIN', STATUS ) .EQ. DOMAIN ) THEN
            IFRM = I
            GO TO 999
         END IF

*  Annul the Frame pointer.
         CALL AST_ANNUL( FRM, STATUS )

      END DO

*  If we did not find a Frame with the correct Domain, we go on to
*  check the sub-Frames within any CmpFrames in the Target.
      IF( IFRM .EQ. AST__NOFRAME ) THEN

*  Loop round each Frame in the Target FrameSet.
         DO I = NFRM, 1, -1

*  Get a pointer to the Frame.
            FRM = AST_GETFRAME( TARGET, I, STATUS )

*  Is this a CmpFrame?
            IF( AST_ISACMPFRAME( FRM, STATUS ) ) THEN

*  Get the number of axes in the CmpFrame.
               NAX = AST_GETI( FRM, 'NAXES', STATUS )

*  Indicate we have found no axes for the required Domain yet.
               NOUT = 0

*  Check each axis of the CmpFrame.
               DO J = 1, NAX

*  Pick this axis from the CmpFrame. This creates a one-dimensional Frame.
                  AXFRM = AST_PICKAXES( FRM, 1, J, MAP, STATUS )

*  Does this axis belong to the required Domain?
                  IF( AST_GETC( AXFRM, 'DOMAIN', STATUS ) .EQ.
     :                DOMAIN ) THEN

*  If so, add it to the list of axes to be copied.
                     NOUT = NOUT + 1
                     OUTPRM( NOUT ) = J
                     INPRM( J ) = NOUT

*  Otherwise, indicate that a PermMap would assign the first constant value
*  (which will be 0.0) to the axis.
                  ELSE
                     INPRM( J ) = -1
                  END IF

               END DO

*  If some axes belonging to the required Domain were found, extract them
*  into a Frame.
               IF( NOUT .GT. 0 ) THEN
                  FMATCH = AST_PICKAXES( FRM, NOUT, OUTPRM, MAP,
     :                                   STATUS )

*  Create a PermMap which will map values in the CmpFrame into the sub-Frame
*  extracted above. The un-used axes are assigned the constant value 0.0 by
*  the inverse mapping. This is a bit of a fudge. It would be better if the
*  user supplied the values to use for these un-used axes. Anyway,
*  assigning zero is better than assigning AST__BAD since this would result
*  in the inverse mapping producing unusable positions.
                  MAP = AST_PERMMAP( NAX, INPRM, NOUT, OUTPRM, 0.0D0,
     :                               ' ', STATUS )

*  Save the index of the Current Frame in the target FrameSet.
                  ICURR = AST_GETI( TARGET, 'CURRENT', STATUS )

*  Add the sub-Frame into the TARGET FrameSet, using this Mapping.
                  CALL AST_ADDFRAME( TARGET, I, MAP, FMATCH, STATUS )

*  Save the index of the new Frame.
                  IFRM = AST_GETI( TARGET, 'CURRENT', STATUS )

*  Re-instate the original Current Frame.
                  CALL AST_SETI( TARGET, 'CURRENT', ICURR, STATUS )

*  Break out of the loop.
                  GO TO 999

               END IF

            END IF

*  Annul the Frame pointer.
            CALL AST_ANNUL( FRM, STATUS )

         END DO

      END IF

 999  CONTINUE

*  Return a null index (AST__NOFRAME) if an error has occurred.
      IF( STATUS .NE. SAI__OK ) IFRM = AST__NOFRAME

*  End the AST context.
      CALL AST_END( STATUS )

      END
