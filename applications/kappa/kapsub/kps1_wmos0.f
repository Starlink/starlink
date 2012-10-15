      SUBROUTINE KPS1_WMOS0( INDFR, IGRP, NDIM, LBND, UBND, USEVAR,
     :                       MAPS, IWCSR, STATUS )
*+
*  Name:
*     KPS1_WMOS0

*  Purpose:
*     Extract required global information from a group of input NDFs for
*     WCSMOSAIC.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WMOS0( INDFR, IGRP, NDIM, LBND, UBND, USEVAR, MAPS,
*                      IWCSR, STATUS )

*  Description:
*     This routine extracts the global information required by WCSMOSAIC
*     from the supplied group of input NDFs.

*  Arguments:
*     INDFR = INTEGER (Given)
*        The NDF identifier for the reference NDF.
*     IGRP = INTEGER (Given)
*        The GRP identifier for the group containing the input NDF names.
*     NDIM
*        The number of pixel axes in the output NDF.
*     LBND( NDIM ) = INTEGER (Returned)
*        The lower pixel index bounds for the output NDF so that the output NDF just
*        encompasses all the input data.
*     UBND( NDIM ) = INTEGER (Returned)
*        The upper pixel index bounds for the output NDF so that the output NDF just
*        encompasses all the input data.
*     USEVAR = LOGICAL (Returned)
*        Returned .TRUE. if and only if all the input NDFs have defined
*        Variance components.
*     MAPS( * ) = INTEGER (Returned)
*        An array in which to store AST identifiers for the pixel_in to
*        pixel_out Mapping for each input NDF. The array should have the
*        same number of elements as the supplied group of input NDFs (IGRP).
*        The "pixel coordinates" used as input and output by these Mappings
*        have integral values at the centre of each pixel. Note, this is
*        different to the usual Starlink convention for pixel coordinates
*        which has integral values at the edges of each pixel, but it is the
*        convention required by AST_REBINSEQ.
*     IWCSR = INTEGER (Returned)
*        The WCS FrameSet from the reference NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-2005 (DSB):
*        Original version.
*     6-OCT-2005 (DSB):
*        Added ShiftMaps to returned Mappings because AST_REBINSEQ
*        requires "pixel" coords to have integer values at the centre
*        of pixels, but Starlink pixel coords have integer value at
*        pixel corners.
*     9-MAY-2006 (DSB):
*        Correct number of axes in input NDF.
*     1-DEC-2006 (DSB):
*        Correct the rouding of floating point pixel bounds to integer
*        pixel bounds (used to use floor/ceil, now uses nint).
*     20-DEC-2006 (DSB):
*        Back out of the 1-DEC-2006 changes since the floor/ceil approach
*        was right after all, since the pixel coordinate system used by
*        "maps" has integer values at the *centre* of each pixel (not the
*        edges as normal in Starlink sw).
*     19-JUN-2007 (DSB):
*        Added argument IWCSR.
*     3-SEP-2007 (DSB):
*        Report an error if the reference NDF does not have a defined
*        inverse WCS transformation.
*     15-OCT-2012 (DSB):
*        Allow 2D input images to align with 3D reference images, if the
*        extra reference axis is a degenerate pixel axis.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'AST_ERR'          ! AST error constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER INDFR
      INTEGER IGRP
      INTEGER NDIM

*  Arguments Returned:
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      LOGICAL USEVAR
      INTEGER MAPS( * )
      INTEGER IWCSR

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER KPG1_CEIL
      INTEGER KPG1_FLOOR

*  Local Variables:
      CHARACTER DOMLST*50        ! List of preferred alignment domains
      DOUBLE PRECISION ALBND     ! Lower axis bound
      DOUBLE PRECISION AUBND     ! Upper axis bound
      DOUBLE PRECISION CON( NDF__MXDIM )! Constant axius values
      DOUBLE PRECISION DLBND1( NDF__MXDIM )! Lower bounds of input NDF
      DOUBLE PRECISION DUBND1( NDF__MXDIM )! Upper bounds of input NDF
      DOUBLE PRECISION SHIFT( NDF__MXDIM )! 0.5 pixel shifts
      DOUBLE PRECISION XL( NDF__MXDIM )! Input position at lower bound
      DOUBLE PRECISION XU( NDF__MXDIM )! Input position at upper bound
      INTEGER I                  ! Loop count
      INTEGER IAT                ! No. of characters in string
      INTEGER IL                 ! Integer lower bound
      INTEGER INDF1              ! Input NDF identifier
      INTEGER INPRM( NDF__MXDIM )! Output axis for each input axis
      INTEGER IPIX1              ! Index of PIXEL Frame in input NDF FrameSet
      INTEGER IPIXR              ! Index of PIXEL Frame in ref. NDF FrameSet
      INTEGER IU                 ! Integer upper bound
      INTEGER IWCS1              ! AST pointer to input WCS FrameSet
      INTEGER J                  ! Axis count
      INTEGER LBND1( NDF__MXDIM )! Lower bounds of input NDF
      INTEGER LBNDR( NDF__MXDIM )! Lower bounds of reference NDF
      INTEGER NCON               ! No. of used constants in CON array
      INTEGER NDIM1              ! No. of pixel axes in input NDF
      INTEGER NDIMR              ! No. of pixel axes in reference NDF
      INTEGER NFRM               ! No. of Frames in input NDF FrameSet
      INTEGER OUTPRM( NDF__MXDIM )! Input axis for each output axis
      INTEGER PMAP               ! PermMap that assigns fixed pixel coords
      INTEGER SIZE               ! No. of input NDFs
      INTEGER SM                 ! ShiftMap pointer
      INTEGER UBND1( NDF__MXDIM )! Upper bounds of input NDF
      INTEGER UBNDR( NDF__MXDIM )! Upper bounds of reference NDF
      REAL RLBND                 ! Lower axis bound
      REAL RUBND                 ! Upper axis bound
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context
      CALL AST_BEGIN( STATUS )

*  Get the WCS FrameSet from the reference NDF. This will be inherited by
*  the output NDF.
      CALL KPG1_GTWCS( INDFR, IWCSR, STATUS )

*  Check the inverse transformation is available.
      IF( .NOT. AST_GETL( IWCSR, 'TranInverse', STATUS ) ) THEN

         IF( STATUS .EQ. SAI__OK ) THEN
            DOMLST = AST_GETC( IWCSR, 'Domain', STATUS )

            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDFR )
            CALL ERR_REP( 'KPS1_WMOS0_ERR1', 'Cannot use ^NDF as the '//
     :                    'reference NDF.', STATUS )

            CALL MSG_SETC( 'DOM', DOMLST )
            CALL ERR_REP( 'KPS1_WMOS0_ERR1', 'The inverse WCS '//
     :                    'transformation (from ^DOM to PIXEL '//
     :                    'coordinates) is undefined.', STATUS )
         END IF

         GO TO 999

      END IF

*  Find the index of the PIXEL Frame in the reference NDF.
      CALL KPG1_ASFFR( IWCSR, 'PIXEL', IPIXR, STATUS )

*  Get the pixel index bounds of the reference NDF.
      CALL NDF_BOUND( INDFR, NDF__MXDIM, LBNDR, UBNDR, NDIMR,
     :                STATUS )

*  Initialise the returned values. If the reference NDF extends over only
*  one pixel on any pixel axis, use this value as both limits. Otherwise
*  set extreme limts.
      DO J = 1, NDIM
         IF( LBNDR( J ) .EQ. UBNDR( J ) ) THEN
            LBND( J ) = LBNDR( J )
            UBND( J ) = UBNDR( J )
         ELSE
            LBND( J ) = VAL__MAXI
            UBND( J ) = VAL__MINI
         END IF
      END DO
      USEVAR = .TRUE.

*  Loop round each NDF to be processed.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
      DO I = 1, SIZE
         CALL NDG_NDFAS( IGRP, I, 'Read', INDF1, STATUS )

*  We do not use variances if any input NDF has no Variance component.
         IF( USEVAR ) CALL NDF_STATE( INDF1, 'VARIANCE', USEVAR,
     :                                STATUS )

*  Get the WCS FrameSet from the current input NDF.
         CALL KPG1_GTWCS( INDF1, IWCS1, STATUS )

*  Find the index of the PIXEL Frame in the input NDF.
         CALL KPG1_ASFFR( IWCS1, 'PIXEL', IPIX1, STATUS )

*  Save the number of Frames in the input WCS FrameSet.
         NFRM = AST_GETI( IWCS1, 'NFRAME', STATUS )

*  Store the list of preferences for the alignment Frame Domain (current
*  FRAME in the input NDF, followed by PIXEL). KPG1_ASMRG always uses the
*  Domain of the second FrameSet (IWCSR) first, so we do not need to include
*  it in this list.
         DOMLST = ' '
         IAT = 0
         CALL CHR_APPND( AST_GETC( IWCS1, 'DOMAIN', STATUS ), DOMLST,
     :                   IAT )
         CALL CHR_APPND( ',PIXEL', DOMLST, IAT )

*  Merge the reference WCS FrameSet into this NDFs WCS FrameSet, aligning
*  them in a suitable Frame (the current Frame of IWCSR by preference, or
*  the first possible domain in the above list otherwise).
         CALL KPG1_ASMRG( IWCS1, IWCSR, DOMLST( : IAT ), .TRUE., 4,
     :                    STATUS )

*  Get the simplified Mapping from input pixel Frame to reference (i.e.
*  output) pixel Frame.
         MAPS( I ) = AST_GETMAPPING( IWCS1, IPIX1,
     :                               IPIXR + NFRM, STATUS )

*  Create a ShiftMap which shifts pixel coords by 0.5 of a pixel in order
*  to put integer values at the centre of the pixel (as required by AST_REBINSEQ).
         NDIM1 = AST_GETI( MAPS( I ), 'Nin', STATUS )
         DO J = 1, NDIM1
            SHIFT( J ) = -0.5D0
         END DO
         SM = AST_SHIFTMAP( NDIM1, SHIFT, ' ', STATUS )

*  Combine this with the above Mapping.
         MAPS( I ) = AST_CMPMAP( SM, MAPS( I ), .TRUE., ' ', STATUS )

*  Do the inverse for the output pixel axes.
         DO J = 1, NDIM
            SHIFT( J ) = 0.5D0
         END DO
         SM = AST_SHIFTMAP( NDIM, SHIFT, ' ', STATUS )
         MAPS( I ) = AST_CMPMAP( MAPS( I ), SM, .TRUE., ' ', STATUS )

*  Simplify the total Mapping.
         MAPS( I ) = AST_SIMPLIFY( MAPS( I ), STATUS )

*  Get the pixel index bounds of this input NDF, and convert to double
*  precision pixel coordinates which have integral values at the centre of
*  each pixel. Note, this is different to the usual Starlink convention
*  for pixel coordinates which has integral values at the edges of each
*  pixel, but it is the convention required by AST_REBINSEQ.
         CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND1, UBND1, NDIM1,
     :                   STATUS )
         DO J = 1, NDIM1
            DLBND1( J ) = DBLE( LBND1( J ) ) - 0.5D0
            DUBND1( J ) = DBLE( UBND1( J ) ) + 0.5D0
         END DO

*  Extend the output pixel bounds to encompass this input image.
         NCON = 0
         DO J = 1, NDIM
            CALL AST_MAPBOX( MAPS( I ), DLBND1, DUBND1, .TRUE., J,
     :                       ALBND, AUBND, XL, XU, STATUS )

*  If the range of the bounding box on the current output pixel axis
*  could not be found, and the output NDF spans only a single pixel on
*  the current axis, then it is probably a degenerate pixel axis that is
*  not present in the input NDF (e.g. reference is a scuba-2 map with
*  (RA,Dec) axes and a third degenerate wavelength axis, and the input is
*  simple 2D (RA,Dec) map).
            IF( STATUS .EQ. AST__MBBNF .AND.
     :          LBNDR( J ) .EQ. UBNDR( J ) ) THEN
               CALL ERR_ANNUL( STATUS )

*  Indicate that the Mapping should be modified to assign the fixed value
*  to the current output pixel axis.
               NCON = NCON + 1
               OUTPRM( J ) = -NCON
               CON( NCON ) = LBNDR( J ) - 0.5D0

*  If the range of the bounding box on the current output pixel axis
*  was found, extend the bounding box limits.
           ELSE
               OUTPRM( J ) = J
               RLBND = REAL( ALBND )
               RUBND = REAL( AUBND )
               IU = KPG1_FLOOR( RUBND )
               IL = KPG1_CEIL( RLBND )
               IF( IU .GT. UBND( J ) ) UBND( J ) = IU
               IF( IL .LT. LBND( J ) ) LBND( J ) = IL
            END IF

            INPRM( J ) = J

         END DO

*  If any degenerate axes were found above, modify the Mapping by
*  appending a PermMap that assigns the constant output pixel value to
*  the degenerate axes.
         IF( NCON .GT. 0 ) THEN
            PMAP = AST_PERMMAP( NDIM, INPRM, NDIM, OUTPRM, CON, ' ',
     :                          STATUS )
            MAPS( I ) = AST_CMPMAP( MAPS( I ), PMAP, .TRUE., ' ',
     :                              STATUS )
            CALL AST_ANNUL( PMAP, STATUS )
         END IF

*  Export the AST pointer to the parent AST context.
         CALL AST_EXPORT( MAPS( I ), STATUS )

*  Annul the input NDF identifier.
         CALL NDF_ANNUL( INDF1, STATUS )

*  If an error occurred processing the current input NDF, abort.
         IF( STATUS .NE. SAI__OK  ) GO TO 999

      END DO

 999  CONTINUE

*  Export the IWCSR pointer to the parent AST context so that it will not
*  be annulled by the following call to AST_END.
      CALL AST_EXPORT( IWCSR, STATUS )

*  End the AST context
      CALL AST_END( STATUS )

      END
