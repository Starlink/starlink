      SUBROUTINE KPG1_ASGET8( INDF, NDIM, EXACT, TRIM, REQINV, SDIM,
     :                        SLBND, SUBND, IWCS, STATUS )
*+
*  Name:
*     KPG1_ASGET8

*  Purpose:
*     Gets an AST FrameSet from the WCS component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASGET8( INDF, NDIM, EXACT, TRIM, REQINV, SDIM, SLBND, SUBND,
*                       IWCS, STATUS )

*  Description:
*     This routine is equivalent to KPG1_ASGET except that arguments
*     SLBND and SUBND are INTEGER*8 instead of INTEGER. See KPG1_ASGET
*     for more information.

*  Arguments:
*     INDF = INTEGER (Given)
*        The identifier for the NDF.
*     NDIM = INTEGER (Given)
*        The number of dimensions required by the application.
*     EXACT = LOGICAL (Given)
*        Must the NDF have exactly NDIM significant axes? Otherwise it is
*        acceptable for the NDIM axes to include some insignificant axes.
*     TRIM = LOGICAL (Given)
*        Should the returned FrameSet be trimmed to ensure that the
*        Current Frame has NDIM axes? Otherwise, the Current Frame read
*        from the NDF is not changed.
*     REQINV = LOGICAL (Given)
*        Is the inverse mapping (from Current Frame to Base Frame)
*        required? If it is, an error is reported if the inverse mapping
*        is not available. REQINV should be supplied .TRUE. in most cases.
*     SDIM( NDIM ) = INTEGER (Returned)
*        The indices of the significant dimensions.
*     SLBND( NDIM ) = INTEGER*8 (Returned)
*        The lower pixel index bounds of the significant dimensions.  These
*        are stored in the same order as the indices in SDIM.
*     SUBND( NDIM ) = INTEGER*8 (Returned)
*        The upper pixel index bounds of the significant dimensions.  These
*        are stored in the same order as the indices in SDIM.
*     IWCS = INTEGER (Returned)
*        An AST pointer to the WCS FrameSet. Returned equal to AST__NULL
*        if an error occurs. The Base Frame is an NDIM-dimensional GRID
*        Domain.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
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
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     4-OCT-2019 (DSB):
*        Original version, copied from KPG1_ASGET and changed to use
*        INTEGER*8 bounds and dimensions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL

*  Arguments Given:
      INTEGER INDF
      INTEGER NDIM
      LOGICAL EXACT
      LOGICAL TRIM
      LOGICAL REQINV

*  Arguments Returned:
      INTEGER SDIM( NDIM )
      INTEGER*8 SLBND( NDIM )
      INTEGER*8 SUBND( NDIM )
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER SZFMT              ! Max. characters in formatted value
      PARAMETER ( SZFMT = 2 * VAL__SZD )

*  Local Variables:
      CHARACTER AXTYPE*(NDF__SZTYP)! Merged AXIS data type.
      CHARACTER DOM*30             ! Current Frame domain
      CHARACTER TTL*80             ! NDF title
      DOUBLE PRECISION OFFSET      ! Axis offset scale factor
      DOUBLE PRECISION SCALE       ! Axis scale factor
      INTEGER AX( NDF__MXDIM )     ! Axis indices to check for monotonicity
      INTEGER FRM                  ! Pointer to a Frame in IWCS
      INTEGER I                    ! Axis index
      INTEGER IAXI1                ! Index of original AXIS Frame in NEWFS
      INTEGER IAXI2                ! Index of original AXIS Frame in IWCS
      INTEGER ICURR                ! Index of original Current Frame in IWCS
      INTEGER IFRA1                ! Index of original FRACTION Frame in NEWFS
      INTEGER IFRA2                ! Index of original FRACTION Frame in IWCS
      INTEGER IGRI1                ! Index of original GRID Frame in NEWFS
      INTEGER IGRI2                ! Index of original GRID Frame in IWCS
      INTEGER INPRM( NDF__MXDIM )  ! Input axis permutation array
      INTEGER IPIX1                ! Index of original PIXEL Frame in NEWFS
      INTEGER IPIX2                ! Index of original PIXEL Frame in IWCS
      INTEGER IPWORK               ! Pointer to work space
      INTEGER LTTL                 ! Used length of TTL
      INTEGER NBAX                 ! Number of axes in GRID Frame
      INTEGER NDIMS                ! No. of genuine axes in the NDF
      INTEGER NEWFS                ! Pointer to a FrameSet with new Base Frame
      INTEGER NFRAME               ! No. of Frames in FrameSet
      INTEGER OUTPRM( NDF__MXDIM ) ! Output axis permutation array
      INTEGER PMAP                 ! AST pointer to a PermMap
      INTEGER*8 DIM                ! Pixel dimension
      INTEGER*8 LBND( NDF__MXDIM ) ! Original NDF bounds
      INTEGER*8 MXDIM              ! Largest selected pixel dimension
      INTEGER*8 UBND( NDF__MXDIM ) ! Original NDF bounds
      LOGICAL MONO                 ! Are all axes monotonic?
      REAL ROFFSET                 ! Axis offset scale factor
      REAL RSCALE                  ! Axis scale factor
*.

*  Initialise.
      IWCS = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Choose the NDF pixel axes to be used by the application.
*  ==================================================

*  Find whether or not there are exactly the required number of
*  significant dimensions in the NDF pixel array and which ones they are.
      IF ( EXACT ) THEN
         CALL KPG1_SGDIM( INDF, NDIM, SDIM, STATUS )

*  If insignificant dimensions can be used, pad out the returned
*  dimensions with insignificant dimensions if the required number
*  of significant dimensions is not present.
      ELSE
         CALL KPG1_SDIMP( INDF, NDIM, SDIM, STATUS )
      END IF

*  Obtain the bounds of the NDF.
      CALL NDF_BOUND8( INDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )

*  Return the bounds of the chosen pixel axes. Also find the largest
*  chosen pixel dimension, and ininitialise the base Frame axis
*  permutation array (INPRM).
      MXDIM = 0
      DO I = 1, NDIM
         SLBND( I ) = LBND( SDIM( I ) )
         SUBND( I ) = UBND( SDIM( I ) )
         DIM = SUBND( I ) - SLBND( I ) + 1
         IF( DIM .GT. MXDIM ) MXDIM = DIM
         INPRM( I ) = I
      END DO

*  Get a pointer to the WCS FrameSet.
      CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Remove any insignificant axes from the Current Frame. We do not do
*  this if later code will trim the current Frame to have the required
*  number of axes.
      IF( .NOT. TRIM ) CALL KPG1_ASSIG8( IWCS, NDIMS, LBND, UBND,
     :                                   STATUS )

*  Re-map the Base (GRID) Frame by selecting the chosen axes.
*  ==========================================================
*  Save the number of axes in the Base Frame.
      NBAX = AST_GETI( IWCS, 'NIN', STATUS )

*  If the number of GRID axes in the NDF is wrong, create a FrameSet
*  containing the NDF-special Frames (GRID, AXIS, PIXEL and FRACTION)
*  appropriate for an NDF with the returned bounds.
      IF( NBAX .NE. NDIM ) THEN
         CALL KPG1_ASNDF8( INDF, NDIM, SDIM, SLBND, SUBND, NEWFS,
     :                     STATUS )

*  Find the indices of the NDF special Frames within this FrameSet
         IPIX1 = -1
         IGRI1 = -1
         IFRA1 = -1
         IAXI1 = -1

         NFRAME = AST_GETI( NEWFS, 'NFRAME', STATUS )
         DO I = 1, NFRAME
            FRM = AST_GETFRAME( NEWFS, I, STATUS )
            DOM = AST_GETC( FRM, 'DOMAIN', STATUS )

            IF( DOM .EQ. 'PIXEL' ) THEN
               IPIX1 = I

            ELSE IF( DOM .EQ. 'FRACTION' ) THEN
               IFRA1 = I

            ELSE IF( DOM .EQ. 'AXIS' ) THEN
               IAXI1 = I

            ELSE IF( DOM .EQ. 'GRID' ) THEN
               IGRI1 = I

            END IF

            CALL AST_ANNUL( FRM, STATUS )

         END DO

*  Create a PermMap which goes from the NDIM-dimensional GRID Frame in this
*  FrameSet to the original NBAX-dimensional GRID Frame. First, initialise
*  the axis permutation arrays so that all input and output axes take the
*  value of the first constant supplied to AST_PERMMAP (i.e. 1.0).
         DO I = 1, NBAX
            OUTPRM( I ) = -1
         END DO

         DO I = 1, NDIM
            INPRM( I ) = -1
         END DO

*  Now over-write elements of the axis permutation arrays which correspond to
*  genuine axes.
         DO I = 1, NDIM
            IF( SDIM( I ) .LE. NBAX ) THEN
               INPRM( I ) = SDIM( I )
               OUTPRM( SDIM( I ) ) = I
            END IF
         END DO

*  Create the PermMap.
         PMAP = AST_PERMMAP( NDIM, INPRM, NBAX, OUTPRM, 1.0D0, ' ',
     :                       STATUS )

*  Find the indices of the NDF special Frames in the FrameSet read from
*  the NDF.
         IPIX2 = -1
         IGRI2 = -1
         IFRA2 = -1
         IAXI2 = -1

         NFRAME = AST_GETI( IWCS, 'NFRAME', STATUS )
         DO I = 1, NFRAME
            FRM = AST_GETFRAME( IWCS, I, STATUS )
            DOM = AST_GETC( FRM, 'DOMAIN', STATUS )

            IF( DOM .EQ. 'PIXEL' ) THEN
               IPIX2 = I

            ELSE IF( DOM .EQ. 'FRACTION' ) THEN
               IFRA2 = I

            ELSE IF( DOM .EQ. 'AXIS' ) THEN
               IAXI2 = I

            ELSE IF( DOM .EQ. 'GRID' ) THEN
               IGRI2 = I

            END IF

            CALL AST_ANNUL( FRM, STATUS )

         END DO

*  Record the original current Frame index.
         ICURR = AST_GETI( IWCS, 'Current', STATUS )

*  Record the original number of Frames in the new FrameSet.
         NFRAME = AST_GETI( NEWFS, 'NFRAME', STATUS )

*  Add the FrameSet read from the NDF into the new FrameSet, using the
*  PermMap created above to connect the Base (GRID) Frame to the new FrameSet.
*  We need to make the old GRID Frame the current Frame first since
*  AST_ADDFRAME uses the current Frame.
         CALL AST_SETI( IWCS, 'Current', IGRI2, STATUS )
         CALL AST_ADDFRAME( NEWFS, AST__BASE, PMAP, IWCS, STATUS )
         CALL AST_ANNUL( PMAP, STATUS )

*  Adjust the Frame indices found above so that they refer to the
*  expanded FrameSet.
         IPIX2 = IPIX2 + NFRAME
         IAXI2 = IAXI2 + NFRAME
         IFRA2 = IFRA2 + NFRAME
         IGRI2 = IGRI2 + NFRAME
         ICURR = ICURR + NFRAME

*  If the original current Frame was one of the NDF special Frames, set
*  the current Frame to the appropriate new Frame.
         IF( ICURR .EQ. IPIX2 ) THEN
            ICURR = IPIX1

         ELSE IF( ICURR .EQ. IFRA2 ) THEN
            ICURR = IFRA1

         ELSE IF( ICURR .EQ. IAXI2 ) THEN
            ICURR = IAXI1

         ELSE IF( ICURR .EQ. IGRI2 ) THEN
            ICURR = IGRI1
         END IF

         CALL AST_SETI( NEWFS, 'Current', ICURR, STATUS )

*  Rename the original PIXEL Frame as NEW_PIXEL.
         FRM = AST_GETFRAME( NEWFS, IPIX2, STATUS )
         CALL AST_SETC( FRM, 'DOMAIN', 'NDF_PIXEL', STATUS )
         CALL AST_ANNUL( FRM, STATUS )

*  Delete the other three original NDF special Frames.
         CALL AST_REMOVEFRAME( NEWFS, IAXI2, STATUS )
         IF( IFRA2 .GT. IAXI2 ) IFRA2 = IFRA2 - 1
         IF( IGRI2 .GT. IAXI2 ) IGRI2 = IGRI2 - 1

         CALL AST_REMOVEFRAME( NEWFS, IFRA2, STATUS )
         IF( IGRI2 .GT. IFRA2 ) IGRI2 = IGRI2 - 1

         CALL AST_REMOVEFRAME( NEWFS, IGRI2, STATUS )

*  Annul the original FrameSet and use the new one instead.
         CALL AST_ANNUL( IWCS, STATUS )
         IWCS = NEWFS

      END IF

*  Now modify the Current Frame if required to have exactly NDIM axes.
*  ===================================================================
      IF( TRIM ) THEN
         CALL PSX_CALLOC8( MXDIM*2, '_DOUBLE', IPWORK, STATUS )
         CALL KPG1_ASTRM8( IWCS, INPRM, SLBND, SUBND,
     :                    %VAL( CNF_PVAL(IPWORK) ), STATUS )
         CALL PSX_FREE( IPWORK, STATUS )
      END IF


*  Tidy up
*  =======

*  If the Current Frame has no Title, use the Title from the NDF (if any).
      TTL = ' '
      CALL NDF_CGET( INDF, 'TITLE', TTL, STATUS )
      IF( TTL .NE. ' ' .AND.
     :    .NOT. AST_TEST( IWCS, 'TITLE', STATUS ) ) THEN
         LTTL = CHR_LEN( TTL )
         CALL AST_SETC( IWCS, 'TITLE', TTL( : LTTL ), STATUS )
      END IF

*  Report an error if the inverse mapping is required, but is not
*  available.
      IF( REQINV .AND. .NOT. AST_GETL( IWCS, 'TRANINVERSE', STATUS )
     :    .AND. STATUS .EQ. SAI__OK ) THEN

         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'KPG1_ASGET8_2', 'The mapping from the current'//
     :                 ' co-ordinate Frame in ^NDF to the pixel '//
     :                 'co-ordinate Frame is not defined.', STATUS )

         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'KPG1_ASGET8_3', 'It may be possible to avoid '//
     :                 'this problem by changing the current '//
     :                 'co-ordinate Frame in ^NDF using WCSFRAME.',
     :                  STATUS )

      END IF

*  If the current Frame is AXIS, check that the AXIS structures are
*  monotonic. If not, reset the current Frame to PIXEL.
      DOM = AST_GETC( IWCS, 'Domain', STATUS )
      IF( DOM .EQ. 'AXIS' ) THEN

         CALL NDF_BOUND8( INDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )
         DO I = 1, NDIMS
            AX( I ) = I
         END DO

         CALL NDF_ATYPE( INDF, 'CENTRE', 0, AXTYPE, STATUS )
         IF( AXTYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_CHAXD( INDF, NDIMS, AX, MONO, SCALE, OFFSET,
     :                       STATUS )
         ELSE
            CALL KPG1_CHAXR( INDF, NDIMS, AX, MONO, RSCALE, ROFFSET,
     :                       STATUS )
         END IF

         IF( .NOT. MONO ) THEN
            CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX2, STATUS )
            CALL AST_SETI( IWCS, 'Current', IPIX2, STATUS )
            CALL MSG_OUT( 'KPG1_GTWCS_MSG4', 'PIXEL co-ordinates will'//
     :                    ' be used instead of AXIS co-ordinates.',
     :                    STATUS )
         END IF

      END IF

*  Export the returned FrameSet from the current AST context so that it is
*  not annulled by the following call to AST_END.
      CALL AST_EXPORT( IWCS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
