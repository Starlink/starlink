      SUBROUTINE KPG1_ASAPA( INDF, FRM, MAP, IAXIS, AXLOW, AXHIGH,
     :                       PAXIS, PXLOW, PXHIGH, MAP1D, STATUS )
*+
*  Name:
*     KPG1_ASAPA

*  Purpose:
*     Determines which pixel axis is most closely aligned with a WCS
*     axis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASAPA( INDF, FRM, MAP, IAXIS, AXLOW, AXHIGH, PAXIS,
*                      PXLOW, PXHIGH, MAP1D, STATUS )

*  Description:
*     This routine aligns a supplied WCS axis to the best-matching
*     pixel axis of an NDF.  It also returns pixel co-ordinates
*     corresponding to supplied WCS-axis limits, and where possible a
*     mapping with one input and one output that transforms current
*     Frame co-ordinates into pixel co-ordinates.
*
*     It first attempts to split the mapping to derive a one-to-one
*     mapping.  Failing that it

*  Arguments:
*     INDF = INTEGER (Given)
*        The identifier of the NDF containing a WCS component.
*     FRM = INTEGER (Given)
*        An AST pointer to the current Frame in the NDF.
*     MAP = INTEGER (Given)
*        The mapping from the PIXEL Frame to the current WCS Frame.
*        This can also be a mapping from GRID to the current Frame.
*        In which case the returned co-ordinates and mapping apply
*        to GRID co-ordinates instead of PIXEL.
*     IAXIS = INTEGER (Given)
*        The index of the WCS axis in the current Frame to be aligned.
*     AXLOW = DOUBLE PRECISION (Given)
*        Lower bound of the WCS axis.  If either AXLOW or AXHIGH is
*        set to AST__BAD, then the whole axis range is used.
*     AXHIGH = DOUBLE PRECISION (Given)
*        Upper bound of the WCS axis.  If either AXLOW or AXHIGH is
*        set to AST__BAD, then the whole axis range is used.
*     PAXIS = INTEGER (Returned)
*        The index of the pixel most closely aligned with the supplied
*        WCS axis.
*     PXLOW = DOUBLE PRECISION (Given)
*        Lower bound of the PIXEL axis corresponding to the AXLOW in
*        the WCS axis.
*     PXHIGH = DOUBLE PRECISION (Given)
*        Upper bound of the PIXEL axis corresponding to the AXHIGH in
*        the WCS axis.
*     MAP1D = INTEGER (Returned)
*        The mapping from the current WCS Frame to PIXEL whose input
*        is purely along the chosen axis (IAXIS).  It is set to
*        AST__NULL if the supplied mapping (MAP) could not be split.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 July 19 (MJC):
*        Original version derived from MFITTREND.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST parameters and functions
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Arguments Given:
      INTEGER INDF
      INTEGER FRM
      INTEGER MAP
      INTEGER IAXIS
      DOUBLE PRECISION AXLOW
      DOUBLE PRECISION AXHIGH

*  Arguments Returned:
      INTEGER PAXIS
      DOUBLE PRECISION PXLOW
      DOUBLE PRECISION PXHIGH
      INTEGER MAP1D

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION CPOS( 2, NDF__MXDIM ) ! Two current Frame
                                 ! positions
      DOUBLE PRECISION CURPOS( NDF__MXDIM ) ! A valid current Frame
                                 ! position
      DOUBLE PRECISION DLBND( NDF__MXDIM ) ! Lower bounds, pixel co-ords
      DOUBLE PRECISION DUBND( NDF__MXDIM ) ! Upper bounds, pixel co-ords
      INTEGER I                  ! Index of supplied string
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER NAX                ! Number of axes in current Frame
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NFEED              ! Number of pixel axes feeding WCS axis
      INTEGER PIXAXE( NDF__MXDIM )! Pixel axis indices feeding WCS axis
      DOUBLE PRECISION PIXPOS( NDF__MXDIM ) ! A valid pixel Frame
                                 ! position
      DOUBLE PRECISION PPOS( 2, NDF__MXDIM ) ! Two pixel Frame positions
      DOUBLE PRECISION PRJ       ! Vector length projected on to a pixel
                                 ! axis
      DOUBLE PRECISION PRJMAX    ! Maximum vector length projected on to
                                 ! an axis
      CHARACTER*255 TTLC         ! Title of original current Frame
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF
      LOGICAL USEALL             ! Use the entire axis?

*.

*  Initialise returned variables.
      MAP1D = AST__NULL
      PXLOW = AST__BAD
      PXHIGH = AST__BAD
      PAXIS = AST__NULL

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      USEALL = AXLOW .EQ. AST__BAD .OR. AXHIGH .EQ. AST__BAD

*  Get the bounds and dimensionality.
      CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Extract the current Frame, this is used for picking the axis and the
*  units of the ranges.
      NAX = AST_GETI( FRM, 'NAXES', STATUS )
      TTLC = AST_GETC( FRM, 'TITLE', STATUS )

*  Try splitting the mapping.
*  ==========================

*  First see if the Mapping can be split into two parallel Mappings; one
*  that feeds just the selected WCS axis, and another that feeds all the
*  other axes.
      CALL AST_INVERT( MAP, STATUS )
      CALL AST_MAPSPLIT( MAP, 1, IAXIS, PIXAXE, MAP1D, STATUS )
      CALL AST_INVERT( MAP, STATUS )

*  If so, check that the WCS axis is fed by one and only one pixel axis,
*  and get its index.
      PAXIS = 0

      IF ( MAP1D .NE. AST__NULL ) THEN
         NFEED = AST_GETI( MAP1D, 'NOUT', STATUS )
         IF ( NFEED .EQ. 1 ) THEN
            PAXIS = PIXAXE( 1 )

*  If high and low axis values were supplied, using the Mapping produced
*  by AST_MAPSPLIT to get the corresponding pixel positions.
            IF ( .NOT. USEALL ) THEN
               CALL AST_TRAN1( MAP1D, 1, AXHIGH, .TRUE., PXHIGH,
     :                         STATUS )
               CALL AST_TRAN1( MAP1D, 1, AXLOW, .TRUE., PXLOW, STATUS )
            END IF
         END IF
      END IF

*  Find an arbitrary position within the NDF which has valid current
*  Frame co-ordinates.  Both pixel and current Frame co-ordinates for
*  this position are returned.
      DO I = 1, NDIM
         DLBND( I ) = DBLE( LBND( I ) - 1 )
         DUBND( I ) = DBLE( UBND( I ) )
      END DO
      CALL KPG1_ASGDP( MAP, NDIM, NAX, DLBND, DUBND, PIXPOS, CURPOS,
     :                 STATUS )

*  If the Mapping could not be split using AST_MAPSPLIT, we attempt to
*  analyse it by transforming positions, in order to find the pixel axis
*  which is most nearly parallel to the selected WCS axis.
      IF ( PAXIS .EQ. 0 ) THEN

*  We require both forward and inverse transformations.
         IF ( .NOT. AST_GETL( MAP, 'TRANINVERSE', STATUS ) .AND.
     :        STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL MSG_SETC( 'T', TTLC )
            CALL ERR_REP( 'KPG1_ASAPA_ERR1', 'The transformation from '/
     :                    /'the current co-ordinate Frame of ''^NDF'' '/
     :                    /'(^T) to pixel co-ordinates is not defined.',
     :                    STATUS )

         ELSE IF ( .NOT. AST_GETL( MAP, 'TRANFORWARD', STATUS ) .AND.
     :             STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL MSG_SETC( 'T', TTLC )
            CALL ERR_REP( 'KPG1_ASAPA_ERR2', 'The transformation from '/
     :                    /'pixel co-ordinates to the current '/
     :                    /'co-ordinate Frame of ''^NDF'' (^T) is not '/
     :                    /'defined.', STATUS )
         END IF

*  Create two copies of the good current Frame co-ordinates.
         DO I = 1, NAX
            CPOS( 1, I ) = CURPOS( I )
            CPOS( 2, I ) = CURPOS( I )
         END DO

*  If no ranges were supplied, modify the collapse axis values in these
*  positions by an arbitrary amount.
         IF ( USEALL ) THEN
            IF ( CURPOS( IAXIS ) .NE. 0.0D0 ) THEN
               CPOS( 1, IAXIS ) = 0.9999 * CURPOS( IAXIS )
               CPOS( 2, IAXIS ) = 1.0001 * CURPOS( IAXIS )
            ELSE
               CPOS( 1, IAXIS ) = CURPOS( IAXIS ) + 1.0D-4
               CPOS( 2, IAXIS ) = CURPOS( IAXIS ) - 1.0D-4
            END IF

*  If ranges values for the collapse axis were supplied, substitute the
*  first pair into these positions.
         ELSE
            CPOS( 1, IAXIS ) = AXHIGH
            CPOS( 2, IAXIS ) = AXLOW
         END IF

*  Transform these two positions into pixel co-ordinates.
         CALL AST_TRANN( MAP, 2, NAX, 2, CPOS, .FALSE., NDIM, 2, PPOS,
     :                   STATUS )

*  Find the pixel axis with the largest projection of the vector joining
*  these two pixel positions.  The collapse will occur along this pixel
*  axis.  Report an error if the positions do not have valid pixel
*  co-ordinates.
         PRJMAX = -1.0
         DO I = 1, NDIM
            IF ( PPOS( 1, I ) .NE. AST__BAD .AND.
     :           PPOS( 2, I ) .NE. AST__BAD ) THEN

               PRJ = ABS( PPOS( 1, I ) - PPOS( 2, I ) )
               IF ( PRJ .GT. PRJMAX ) THEN
                  PAXIS = I
                  PRJMAX = PRJ
               END IF

            ELSE IF ( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'KPG1_ASAPA_ERR3', 'The WCS information '/
     :                       /'is too complex (cannot find two valid '/
     :                       /'pixel positions).', STATUS )
               GO TO 999
            END IF

         END DO

*  Report an error if the selected WCS axis is independent of pixel
*  position.
         IF ( PRJMAX .EQ. 0.0 ) THEN
            IF ( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', IAXIS )
               CALL ERR_REP( 'KPS1_ASAPA_ERR4', 'The specified WCS '/
     :                       /'axis (axis ^I) has a constant value '/
     :                       /'over the whole NDF and so cannot be '/
     :                       /'collapsed.', STATUS )
            END IF
            GO TO 999
         END IF

      END IF

  999 CONTINUE

      END
