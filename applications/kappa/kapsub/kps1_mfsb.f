      SUBROUTINE KPS1_MFSB( INDF, DTAXIS, NCLIP, CLIP, FOREST, NUMBIN,
     :                      ALL, MASK, STATUS )
*+
*  Name:
*     KPS1_MFSB

*  Purpose:
*     Excludes features by setting them bad in a mask.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MFSB( INDF, DTAXIS, NCLIP, CLIP, FOREST, NUMBIN, ALL,
*                     MASK, STATUS )

*  Description:
*     This routine serves MFITTREND.  This processes each line whose
*     trend is to be fit within the NDF whose identifier is supplied.
*     The routine rebins by an integer factor to improve the
*     signal-to-noise ratio.  Then the routine fits a straight line to
*     the rebinned data, and sigma-clipped outliers rejected.  There is
*     a choice of dispersions to use for the clipping, either the
*     standard deviation from the whole array or just within the line
*     being filtered.  The regions encompassing the rejected parts of
*     the line are set bad in the supplied mask.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier of the data to be analysed.  This should be
*        a copy of the input NDF.
*     DTAXIS = INTEGER (Given)
*        The axis index of the dimension that is being detrended.
*     NCLIP = INTEGER (Given)
*        The number of clipping cycles for the rejection of outliers.
*     CLIP( NCLIP ) = REAL (Given)
*        The clipping levels in standard deviations for the rejection
*        of outliers.
*     FOREST = LOGICAL (Given)
*        If .TRUE. the data have many spectral features---a line
*        forest---for which a revised approach using the mode of the
*        histogram along each line of data.
*     NUMBIN = INTEGER (Given)
*        The number of bins in the compressed line.  This may be set
*        to the number of elements in the line to prevent compression.
*     ALL = LOGICAL ( Given)
*        If .TRUE., the clipping threshold is a factor of the standard
*        deviation of the whole data array in the NDF.  If .FALSE., the
*        dispersion comes from only the line being filtered.
*     MASK( * ) = BYTE (Returned)
*        The mask of features.  Features have the bad value VAL__BADB
*        and the remainder are set to 0.  The supplied array should
*        have the number of data-array elements contained in the
*        supplied NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:

*  Copyright:
*     Copyright (C) 2007, 2009, 2016 Science & Technology Facilities
*     Council
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
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 August 13 (MJC):
*        Original version.
*     2007 September 6 (MJC):
*        Use NDF in read mode and create a mask in new argument MASK.
*     2007 October 2 (MJC):
*        Add ALL argument.
*     2009 August 7 (MJC):
*        Allow for revised KPS1_MFEBx API.
*     2016 March 29 (MJC):
*        Add FOREST argument.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER INDF
      INTEGER DTAXIS
      INTEGER NCLIP
      REAL CLIP( NCLIP )
      LOGICAL FOREST
      INTEGER NUMBIN
      LOGICAL ALL

*  Arguments Returned:
      BYTE MASK( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER KPG1_FLOOR         ! Most positive integer .LE. given real
      INTEGER KPG1_CEIL          ! Most negative integer .GE. given real

*  Local Variables:
      INTEGER COMPRS( NDF__MXDIM ) ! Compression factors
      INTEGER D                  ! No. of o/p pixels from reference to
                                 ! i/p pixel 1
      CHARACTER DTYPE *( NDF__SZFTP ) ! Numeric type for output arrays
      INTEGER EL                 ! Number of mapped elements
      INTEGER I                  ! Loop counter
      INTEGER IDIMS( NDF__MXDIM ) ! Dimensions of input NDF
      INTEGER IPAL               ! Pointer to averaged line
      CHARACTER ITYPE * ( NDF__SZTYP ) ! Numeric type for processing
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of input NDF
      INTEGER LBNDO( NDF__MXDIM ) ! Lower bounds of output NDF
      INTEGER NBIN               ! Number of bins
      INTEGER NDFS               ! Identifier to the used section of
                                 ! the input NDF
      INTEGER NDIM               ! Number of dimensions
      INTEGER ODIMS( NDF__MXDIM )! Dimensions of output array
      INTEGER OEL                ! Number of elements in output array
      INTEGER PNTRI( 1 )         ! Pointer to input array component
      INTEGER REF( NDF__MXDIM )  ! I/p pixel co-ords at bottom left of
                                 ! a compression box
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of input NDF
      INTEGER UBNDO( NDF__MXDIM ) ! Upper bounds of output NDF
      INTEGER WPNTR1             ! Pointer to workspace
      INTEGER WPNTR2             ! Pointer to workspace

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the shape and bound of the supplied NDF.
      CALL NDF_DIM( INDF, NDF__MXDIM, IDIMS, NDIM, STATUS )
      CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Compress along the axis being detrended.
*  ========================================

*  Work out the bounds for the output array and the size of the output
*  array from the input array dimensions, compression factor and
*  alignment to origin.  Also modify the input bounds so that they
*  correspond to the section of the input array that is actually used.
      OEL = 1
      DO I = 1, NDF__MXDIM
         IF ( I .NE. DTAXIS ) THEN
            COMPRS( I ) = 1
         ELSE
            NBIN = MIN( IDIMS( I ), NUMBIN )
            COMPRS( I ) = IDIMS( I ) / NBIN
         END IF

*  Align with the origin.  However, leave it parameterised in case this
*  alignment no longer is no longer the only option.
         REF( I ) = LBND( I ) - 1
         D = KPG1_CEIL( REAL( 1 - REF( I ) ) / REAL( COMPRS( I ) ) ) - 1

*  Pad the input image to make it a whole number of compression boxes.
         LBNDO( I ) = KPG1_FLOOR( REAL( LBND( I ) - 1 - REF( I ) )
     :                           / REAL( COMPRS( I ) ) ) - D + 1
         UBNDO( I ) = MAX( LBNDO( I ),
     :                     KPG1_CEIL( REAL( UBND( I ) - REF( I ) )
     :                               / REAL( COMPRS( I ) ) ) - D )

         ODIMS( I ) = UBNDO( I ) - LBNDO( I ) + 1
         OEL = OEL * ODIMS( I )

         LBND( I ) = 1 + REF( I ) + COMPRS( I ) * ( LBNDO( I ) - 1 + D )
         UBND( I ) = REF( I ) + COMPRS( I ) * ( UBNDO( I ) + D )
         IDIMS( I ) = UBND( I ) - LBND( I ) + 1
      END DO

*  Determine the numeric type to be used for processing the sample
*  lines.  This step supports single- and double-precision
*  floating-point processing.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', INDF, INDF, 'Data', ITYPE,
     :                DTYPE, STATUS )

*  Create a section of the input NDF containing the region will actually
*  be used (i.e. excluding any pixels that lie over the edge of the
*  output image).
      CALL NDF_SECT( INDF, NDIM, LBND, UBND, NDFS, STATUS )

*  Map the array component of the section.
      CALL KPG1_MAP( NDFS, 'Data', ITYPE, 'READ', PNTRI, EL, STATUS )

*  Obtain workspace for the compressed array.
      CALL PSX_CALLOC( OEL, ITYPE, IPAL, STATUS )

*  Obtain some workspace for the averaging and map them.
      CALL PSX_CALLOC( IDIMS( 1 ), ITYPE, WPNTR1, STATUS )
      CALL PSX_CALLOC( IDIMS( 1 ), '_INTEGER', WPNTR2, STATUS )

*  Average the lines in the section.  The routine expects at least two
*  dimensions.
      IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPG1_CMAVR( MAX( 2, NDIM ), IDIMS,
     :                    %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    COMPRS, 1,  %VAL( CNF_PVAL( IPAL ) ),
     :                    %VAL( CNF_PVAL( WPNTR1 ) ),
     :                    %VAL( CNF_PVAL( WPNTR2 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_CMAVD( MAX( 2, NDIM ), IDIMS,
     :                    %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                    COMPRS, 1, %VAL( CNF_PVAL( IPAL ) ),
     :                    %VAL( CNF_PVAL( WPNTR1 ) ),
     :                    %VAL( CNF_PVAL( WPNTR2 ) ), STATUS )
      END IF

*  Free workspaces arrays used for compression.
      CALL PSX_FREE( WPNTR1, STATUS )
      CALL PSX_FREE( WPNTR2, STATUS )

*  Tidy the input data array.
      CALL NDF_UNMAP( NDFS, 'Data', STATUS )

*  Remove the enlarged-section NDF.
      CALL NDF_ANNUL( NDFS, STATUS )

*  Perform fits and iteratively reject outliers using the dispersion of
*  the whole array.
      IF ( ALL ) THEN
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_MFADR( DTAXIS, NCLIP, CLIP, ODIMS,
     :                       %VAL( CNF_PVAL( IPAL ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_MFADD( DTAXIS, NCLIP, CLIP, ODIMS,
     :                       %VAL( CNF_PVAL( IPAL ) ), STATUS )
         END IF

*  Perform fits and iteratively reject outliers using the dispersion of
*  from each line being filtered in turn.
      ELSE
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_MFLDR( DTAXIS, NCLIP, CLIP, FOREST, ODIMS,
     :                       %VAL( CNF_PVAL( IPAL ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_MFLDD( DTAXIS, NCLIP, CLIP, FOREST, ODIMS,
     :                       %VAL( CNF_PVAL( IPAL ) ), STATUS )
         END IF
      END IF

*  Inquire the shape of the full data array.  Earlier we replaced
*  these with the multiple of binning factor, hence the second
*  call.
      CALL NDF_DIM( INDF, NDF__MXDIM, IDIMS, NDIM, STATUS )

*  Transfer the bad pixels from the binned to the unbinned mask
*  array.
      IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPS1_MFEBR( COMPRS, IDIMS, ODIMS,
     :                    %VAL( CNF_PVAL( IPAL ) ), MASK, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPS1_MFEBD( COMPRS, IDIMS, ODIMS,
     :                    %VAL( CNF_PVAL( IPAL ) ), MASK, STATUS )
      END IF

*  We have finished with the averaged line.
      CALL PSX_FREE( IPAL, STATUS )

      END
