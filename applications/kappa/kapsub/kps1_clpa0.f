      SUBROUTINE KPS1_CLPA0( IWCS, AXIS, AXDIM, POS, TRIM, GLO, GHI,
     :                        STATUS )
*+
*  Name:
*     KPS1_CLPA0

*  Purpose:
*     Modifies a WCS FrameSet to account for the collapsing of a pixel
*     axis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CLPA0( IWCS, AXIS, AXDIM, POS, TRIM, GLO, GHI, STATUS )

*  Description:
*     This routine modifies the supplied FrameSet by removing the
*     specified pixel axis from the base Frame. It also removes any
*     associated current Frame axes.

*  Arguments:
*     IWCS = INTEGER (Given)
*        Identifier for the WCS FrameSet to be modified.
*     AXIS = INTEGER (Given)
*        The index of the pixel axis which is to be removed.
*     AXDIM = INTEGER*8 (Given)
*        The original number of pixels along the pixel axis which is
*        being removed.
*     POS( * ) = DOUBLE PRECISION (Given)
*        The base-Frame co-ordinates of a position that has good
*        current Frame co-ordinatess.
*     TRIM = LOGICAL (Given)
*        If .TRUE., then the collapsed WCS and pixel axes are removed
*        from the returned FrameSet. Otherwise, they are retained and
*        the Mapping is modified so that the collapsed WCS range maps
*        onto the one remaining pixel.
*     GLO = INTEGER*8 (Given)
*        The GRID co-ordinate of the first collapsed pixel along the
*        collapse axis.
*     GHI = INTEGER*8 (Given)
*        The GRID co-ordinate of the last collapsed pixel along the
*        collapse axis.
*     STATUS = INTEGER (Given)
*        The global status.

*  Notes:
*     - The modified FrameSet only contains two frames, corresponding to
*     the original base and current Frames.
*     - The number of axes in the returned current Frame will always be
*     at least equal to the number of axes in the returned base Frame.
*     If necessary, this is achieved by duplicating some of the base
*     Frame axes within the current Frame (such duplicated axes are
*     connected by a UnitMap)

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics & Astronomy Research
*     Council. All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-DEC-2005 (DSB):
*        Original version.
*     1-MAR-2007 (DSB):
*        Ensure the returned current Frame has at least as many axes
*        as there are pixel axes. This is done by calling ATL_PXDUP to
*        duplicate pixel axes as required.
*     5-MAR-2007 (DSB):
*        - Added argument TRIM (previously a value of TRUE was always
*        assumed).
*        - Correct DELTA from "samples per pixel" to "pixels per sample".
*     2007 April 3 (MJC):
*        Added GLO and GHI arguments (based upon DSB's analysis).
*     1-OCT-2018 (DSB):
*        Change the way in which it is decided whether a WCS axis has a
*        constant value or not. Before an axis was constant if its range
*        was less than 1E-6 of its mean value. Now it is constant if its
*        range corresponds to less than 0.25 of a pixel.
*     15-JAN-2020 (DSB):
*        Add support for huge arrays.
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
      INTEGER*8 AXDIM
      DOUBLE PRECISION POS(*)
      LOGICAL TRIM
      INTEGER*8 GLO
      INTEGER*8 GHI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NSAMP              ! Number of samples along removed axis
      PARAMETER ( NSAMP = 50 )

*  Local Variables:
      DOUBLE PRECISION AXVAL     ! Pixel value on removed axis
      DOUBLE PRECISION BSAMP( NSAMP, NDF__MXDIM )! Base frm sample pos'ns
      DOUBLE PRECISION CMAX( NDF__MXDIM ) ! Maximum axis values
      DOUBLE PRECISION CMIN( NDF__MXDIM ) ! Minimum axis values
      DOUBLE PRECISION CONST( NDF__MXDIM ) ! Constant axis values to use
      DOUBLE PRECISION CSAMP( NSAMP, NDF__MXDIM )! Cur frm sample pos'ns
      DOUBLE PRECISION DELTA     ! Gap between samples
      DOUBLE PRECISION INA( NDF__MXDIM )! Input position of corner A
      DOUBLE PRECISION INB( NDF__MXDIM )! Input position of corner B
      DOUBLE PRECISION NEWPOS( NDF__MXDIM )! Reduced dimensionality POS
      DOUBLE PRECISION OUTA( NDF__MXDIM )! Output position of corner A
      DOUBLE PRECISION OUTB( NDF__MXDIM )! Output position of corner B
      DOUBLE PRECISION PXSCL( NDF__MXDIM )! WCS pixel scales
      INTEGER AXES( NDF__MXDIM ) ! Indices of remaining axes
      INTEGER BFRM               ! Pointer to original base Frame
      INTEGER CFRM               ! Pointer to original current Frame
      INTEGER I                  ! Axis index
      INTEGER INPRM( NDF__MXDIM )! Input axis permutation array
      INTEGER J                  ! Sample index
      INTEGER JUNK               ! Unused AST object pointer
      INTEGER MAP1               ! Original Mapping from base to cur Frame
      INTEGER MAP2               ! Mapping from new base to new cur Frame
      INTEGER NAXES              ! Number of axes
      INTEGER NBFRM              ! Pointer to new base Frame
      INTEGER NCFRM              ! Pointer to new current Frame
      INTEGER NIN                ! Original number of base Frame axes
      INTEGER NOUT               ! Original number of current Frame axes
      INTEGER OUT( NDF__MXDIM )  ! Indices of original Mapping outputs
      INTEGER OUTPRM( NDF__MXDIM )! Output axis permutation array
      INTEGER PM1                ! PermMap which selects remaining inputs
      INTEGER PM2                ! PermMap which selects remaining outputs

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

*  First deal with cases where the output WCS FrameSet is being trimmed
*  to remove the collapsed axis.
      IF( TRIM ) THEN

*  Get the pixel scale on each WCS axis. This is the WCS axis increment
*  produced by moving a distance of one pixel away from the supplied "POS"
*  position, along the WCS axis.
         CALL KPG1_PXSCL( IWCS, POS, PXSCL, STATUS )

*  Get a list of the "NIN-1" pixel axes which are being retained. Also
*  store the reduced dimensionality version of POS.
         NAXES = NIN - 1
         DO I = 1, NAXES
            IF( I .LT. AXIS ) THEN
               AXES( I ) = I
               NEWPOS( I ) = POS( I )
            ELSE
               AXES( I ) = I + 1
               NEWPOS( I ) = POS( I + 1 )
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
            DELTA = DBLE( AXDIM )/DBLE( NSAMP )
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
     :                      NOUT, NSAMP, CSAMP, STATUS )

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

*  Identify the axes which have constant value (i.e. vary by less than
*  0.25 of a pixel). These axes are retained, others are removed from
*  the current Frame.
            NAXES = 0
            DO I = 1, NOUT
               IF( CMAX( I ) - CMIN( I ) .LT.
     :             0.25*PXSCL( I ) ) THEN
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
     :                            BSAMP( NSAMP/2, AXIS ), ' ', STATUS )

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
     :                            STATUS )

*  Sandwich the original base->current Mapping between these two PermMaps
*  and simplify the resulting compound Mapping.
               MAP2 = AST_SIMPLIFY( AST_CMPMAP( AST_CMPMAP( PM1, MAP1,
     :                                                      .TRUE., ' ',
     :                                                      STATUS ),
     :                                          PM2, .TRUE., ' ',
     :                                          STATUS ),
     :                              STATUS )

*  Add this new current Frame into the returned FrameSet.
               CALL AST_ADDFRAME( IWCS, AST__BASE, MAP2, NCFRM, STATUS )

            END IF
         END IF

*  Ensure the WCS frame has at least as many axes as the pixel frame.
*  This is done by duplicating pixel axes to suplement the existing WDCS
*  axes.
         CALL ATL_PXDUP( IWCS, NEWPOS, STATUS )

*  Now deal with cases where the collapsed axis is being retained within
*  the output WCS FrameSet.
*  ---------------------------------------------------------------------
      ELSE

*  Create a new FrameSet containing the original base Frame.
         IWCS = AST_FRAMESET( BFRM, ' ', STATUS )

*  We use the original grid->WCS Mapping, but we modified the collapsed
*  GRID axis by adding a WinMap in series with the original Mapping.
*  This WinMap leaves all axes unchanged except for the removed axis.
*  The removed axis is scaled so that the original range of GRID axis
*  value (GLO - 0.5 to GLO + 0.5) is mapped onto the range covered by a
*  single pixel (0.5 to 1.5). Create the WinMap, and create a series
*  CmpMap containing the WinMap followed by the original grid->WCS
*  Mapping.
         DO I = 1, NIN
            INA( I ) = 1.0D0
            INB( I ) = 2.0D0
            OUTA( I ) = INA( I )
            OUTB( I ) = INB( I )
         END DO

         INA( AXIS ) = 0.5D0
         INB( AXIS ) = 1.5D0
         OUTA( AXIS ) = DBLE( GLO ) - 0.5D0
         OUTB( AXIS ) = DBLE( GHI ) + 0.5D0

         MAP2 = AST_CMPMAP( AST_WINMAP( NIN, INA, INB, OUTA, OUTB,
     :                                  ' ', STATUS ),
     :                      MAP1, .TRUE., ' ', STATUS )

*  Simplify this Mapping.
         MAP2 = AST_SIMPLIFY( MAP2, STATUS )

*  Add in the original current Frame, using this Mapping to connect it to
*  the base (GRID) Frame.
         CALL AST_ADDFRAME( IWCS, AST__BASE, MAP2, CFRM, STATUS )

      END IF

*  Export the pointer to the returned FrameSet into the parent AST context.
      CALL AST_EXPORT( IWCS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
