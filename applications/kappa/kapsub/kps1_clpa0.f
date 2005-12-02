      SUBROUTINE KPS1_CLPA0( IWCS, AXIS, AXDIM, POS, STATUS )
*+
*  Name:
*     KPS1_CLPA0

*  Purpose:
*     Modify a WCS Frameet to account for the collapsing of a pixel axis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CLPA0( IWCS, AXIS, AXDIM, POS, STATUS )

*  Description:
*     This routine modifies the supplied FrameSet by removing the
*     specified pixel axis from the base Frame. It also removes any
*     associated current Frame axes. Note, the modified FrameSet only
*     contains two frames, corresponding to the original base and current 
*     Frames.

*  Arguments:
*     IWCS = INTEGER (Given)
*        Identifier for the WCS FrameSet to be modified.
*     AXIS = INTEGER (Given)
*        The index of the pixel axis which is to be removed.
*     AXDIM = INTEGER (Given)
*        The original number of pixels along the pixel axis which is 
*        being removed.
*     POS( * ) = DOUBLE PRECISION (Given)
*        The base Frame coords of a position which has good current Frame
*        coords.
*     STATUS = INTEGER (Given)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-DEC-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants 
      INCLUDE 'PRM_PAR'          ! VAL constants

*  Arguments Given:
      INTEGER IWCS
      INTEGER AXIS
      INTEGER AXDIM
      DOUBLE PRECISION POS(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NSAMP              ! Number of samples along removed axis
      PARAMETER ( NSAMP = 50 )

*  Local Variables:
      DOUBLE PRECISION AXVAL     ! Pixel value on removed axis
      DOUBLE PRECISION BSAMP( NSAMP, NDF__MXDIM ) ! Base frame sample positions
      DOUBLE PRECISION CONST( NDF__MXDIM ) ! Constant axis values to use 
      DOUBLE PRECISION CMAX( NDF__MXDIM ) ! Maximum axis values
      DOUBLE PRECISION CMIN( NDF__MXDIM ) ! Minimum axis values
      DOUBLE PRECISION CSAMP( NSAMP, NDF__MXDIM ) ! Current frame sample positions
      DOUBLE PRECISION DELTA     ! Gap between samples

      INTEGER AXES( NDF__MXDIM ) ! Indices of remaining axes
      INTEGER INPRM( NDF__MXDIM )! Input axis permutation array
      INTEGER OUTPRM( NDF__MXDIM )! Output axis permutation array
      INTEGER BFRM               ! Pointer to original base Frame
      INTEGER PM1                ! PermMap which selects remaining inputs
      INTEGER PM2                ! PermMap which selects remaining outputs
      INTEGER CFRM               ! Pointer to original current Frame
      INTEGER I                  ! Axis index 
      INTEGER J                  ! Sample index 
      INTEGER JUNK               ! Unused AST object pointer
      INTEGER MAP1               ! Original Mapping from base to current Frame
      INTEGER MAP2               ! Mapping from new base to new current Frame
      INTEGER NAXES              ! Number of axes
      INTEGER NBFRM              ! Pointer to new base Frame
      INTEGER NCFRM              ! Pointer to new current Frame
      INTEGER NIN                ! Original number of base Frame axes 
      INTEGER NOUT               ! Original number of current Frame axes 
      INTEGER OUT( NDF__MXDIM )  ! Indices of original Mapping outputs

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Mapping from base Frame to current Frame in the supplied
*  FrameSet, and get the number of inputs and outputs.
      MAP1 = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT, STATUS )
      NIN = AST_GETI( MAP1, 'Nin', STATUS )
      NOUT = AST_GETI( MAP1, 'Nout', STATUS )

*  Get pointers to the original base and current Frames.
      BFRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Get a list of the "NIN-1" pixel axes which are being retained.
      NAXES = NIN - 1
      DO I = 1, NAXES
         IF( I .LT. AXIS ) THEN
            AXES( I ) = I
         ELSE 
            AXES( I ) = I + 1 
         END IF
      END DO

*  Pick these axes from the original base Frame
      NBFRM = AST_PICKAXES( BFRM, NAXES, AXES, JUNK, STATUS )

*  Create a new FrameSet containing this new base Frame.
      IWCS = AST_FRAMESET( NBFRM, ' ', STATUS )

*  We need to identify which current Frame axes will remain after removal
*  of the nominated pixel axis. First see if the pixel axes which are
*  being retained feed a unique set of current Frame axes. If this is the
*  case, these axes need to be retained in the current Frame and all
*  other can be removed. For instance, this will happen when collapsing
*  an (RA,Dec,freq) cube along the frequency axis (assuming the (ra,dec)
*  plane is parallel to a pixel plane and the frequency axis is
*  perpendicular). But it will not happen when collapsing such a cube
*  along the ra or dec axis.
      CALL AST_MAPSPLIT( MAP1, NIN - 1, AXES, OUT, MAP2, STATUS )

*  If so, check that the number of current Frame axes fed by the retained
*  base Frame axes is one less than the original number of current Frame 
*  axes. If not, we cannot split the mapping effectively.
      IF( MAP2 .NE. AST__NULL ) THEN
         NAXES = AST_GETI( MAP2, 'Nout', STATUS )
         IF( NAXES .NE. NOUT - 1 ) CALL AST_ANNUL( MAP2, STATUS )
      END IF

*  If we can, pick the required axes from the original current Frame. Note,
      IF( MAP2 .NE. AST__NULL ) THEN
         NCFRM = AST_PICKAXES( CFRM, NAXES, OUT, JUNK, STATUS )

*  Add this new current Frame into the returned FrameSet.
         CALL AST_ADDFRAME( IWCS, AST__BASE, MAP2, NCFRM, STATUS )

*  If the retained pixel axes do not feed a unique set of current Frame
*  axes, then we need to work harder.
      ELSE

*  Make 50 copies the supplied base Frame coords. These coords correspond
*  to a good current Frame position. In each copy, modify the value on
*  the axis which is being removed so that the entire list of copies span
*  the given pixel range on that axis.
         DELTA = DBLE( NSAMP )/DBLE( AXDIM )
         AXVAL = 1.0
         DO J = 1, NSAMP
            DO I = 1, AXIS - 1
               BSAMP( J, I ) = POS( I )
            END DO

            BSAMP( J, AXIS ) = AXVAL
            AXVAL = AXVAL + DELTA

            DO I = AXIS + 1, NIN
               BSAMP( J, I ) = POS( I )
            END DO

         END DO

*  Transform these base Frame positions into the current Frame.
         CALL AST_TRANN( MAP1, NSAMP, NIN, NSAMP, BSAMP, .TRUE.,
     :                   NOUT, NSAMP, CSAMP, STATUS )

*  Find the minimum and maximum value on each current Frame axis. 
         DO I = 1, NOUT
            CMAX( I ) = VAL__MIND
            CMIN( I ) = VAL__MAXD
         END DO

         DO J = 1, NSAMP
            DO I = 1, NOUT

               IF( CSAMP( J, I ) .NE. AST__BAD ) THEN
                  IF( CSAMP( J, I ) .LT. CMIN( I ) ) THEN
                     CMIN( I ) = CSAMP( J, I )
                  END IF

                  IF( CSAMP( J, I ) .GT. CMAX( I ) ) THEN
                     CMAX( I ) = CSAMP( J, I )
                  END IF
               END IF

            END DO
         END DO

*  Identify the axes which have constant value. These axes are retained,
*  others are removed fRom the current Frame.
         NAXES = 0
         DO I = 1, NOUT
            IF( CMAX( I ) - CMIN( I ) .LT. 
     :          1.0E-6*ABS( CMAX( I ) + CMIN( I ) ) ) THEN
               NAXES = NAXES + 1
               AXES( NAXES ) = I
            END IF
         END DO

*  Check some axes do not vary.
         IF( NAXES .GT. 0 ) THEN       

*  Pick the required axes from the original current Frame.
            NCFRM = AST_PICKAXES( CFRM, NAXES, AXES, JUNK, STATUS )

*  Create a PermMap which transforms positions from the reduced base
*  Frame to the original base Frame. The forward transformation of this 
*  PermMap supplies a mid-point value for the removed axis.
            DO I = 1, NIN - 1
               IF( I .LT. AXIS ) THEN
                  INPRM( I ) = I
                  OUTPRM( I ) = I
               ELSE 
                  INPRM( I ) = I + 1 
                  OUTPRM( I + 1 ) = I
               END IF
            END DO
            OUTPRM( AXIS ) = -1

            PM1 = AST_PERMMAP( NIN - 1, INPRM, NIN , OUTPRM, 
     :                         BSAMP( NSAMP/2, AXIS ), ' ', STATUS )

            call ast_show( pm1, status )

*  Create a PermMap which transforms positions from the original current 
*  Frame to the reduced current Frame. The inverse transformation of this 
*  PermMap supplies a mid-point value for all removed axes.
            DO I = 1, NOUT
               INPRM( I ) = -I
               CONST( I ) = CSAMP( NSAMP/2, I )
            END DO

            DO I = 1, NAXES
               INPRM( AXES( I ) ) = I
            END DO
            
            PM2 = AST_PERMMAP( NOUT, INPRM, NAXES , AXES, CONST, ' ', 
     :                         STATUS )

*  Sandwich the original base->current Mapping between these two PermMaps
*  and simplify the resulting compound Mapping.
            MAP2 = AST_SIMPLIFY( AST_CMPMAP( AST_CMPMAP( PM1, MAP1, 
     :                                                   .TRUE., ' ', 
     :                                                   STATUS ), 
     :                                       PM2, .TRUE., ' ', STATUS ),
     :                           STATUS )

*  Add this new current Frame into the returned FrameSet.
            CALL AST_ADDFRAME( IWCS, AST__BASE, MAP2, NCFRM, STATUS )

         END IF
      END IF

*  Export the pointer to the returned FrameSet into the parent AST context.
      CALL AST_EXPORT( IWCS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
