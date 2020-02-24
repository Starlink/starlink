      SUBROUTINE KPS1_MFAUR( INDF, DTAXIS, NCLIP, CLIP, NUMBIN, MAXRNG,
     :                       NRANGE, RANGES, STATUS )
*+
*  Name:
*     KPS1_MFAUR

*  Purpose:
*     Determines automatically ranges of pixels to be included in
*     fitting by MFITTREND.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MFAUR( INDF, DTAXIS, NCLIP, CLIP, NUMBIN, MAXRNG,
*                      NRANGE, RANGES, STATUS )

*  Description:
*     This routine serves MFITTREND.  It averages a section defined
*     through the supplied identifier to create a one-dimensional
*     representative line of data that is being detrended by MFITTREND.
*     This average line is rebinned by an integer factor to improve
*     the signal-to-noise ratio.  Then the routine fits a straight
*     line to the rebinned data, and sigma-clipped outliers rejected.
*     The regions encompassing the unrejected parts of the line are
*     returned in the unbinned grid co-ordinates.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier of the representative section to be
*        averaged and analysed.
*     DTAXIS = INTEGER (Given)
*        The axis index of the dimension that is being detrended.
*     NCLIP = INTEGER (Given)
*        The number of clipping cycles for the rejection of outliers.
*     CLIP( NCLIP ) = REAL (Given)
*        The clipping levels in standard deviations for the rejection
*        of outliers.
*     NUMBIN = INTEGER*8 (Given)
*        The number of bins in the compressed line.  This may be set
*        to the number of elements in the line to prevent compression.
*     MAXRNG = INTEGER (Given)
*        The maximum number of range boundaries.  This must be even.
*     NRANGE = INTEGER (Returned)
*        The number of ranges returned.  This is always a multiple of
*        two, i.e pairs of lower and upper ranges.
*     RANGES( MAXRNG ) = INTEGER*8 (Returned)
*        The ranges to include in the detrending fits found from the
*        averaged representative line.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     2006 May 31 (MJC):
*        Original version.
*     2007 January 17 (MJC):
*        Used an NDF section for the padded region being averaged.
*     2007 March 5 (MJC):
*        Trim two workspaces to required sizes.
*     2007 August 10 (MJC):
*        Add NUMBIN argument.
*     19-FEB-2020 (DSB):
*        Add support for huge NDFs.
*     {enter_further_changes_here}

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
      INTEGER*8 NUMBIN
      INTEGER MAXRNG

*  Arguments Returned:
      INTEGER NRANGE
      INTEGER*8 RANGES( MAXRNG )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER*8 KPG1_FLOOR8      ! Most positive integer .LE. given real
      INTEGER*8 KPG1_CEIL8       ! Most negative integer .GE. given real

*  Local Variables:
      INTEGER*8 COMPRS( NDF__MXDIM ) ! Compression factors
      INTEGER*8 D                ! No. of o/p pixels from reference to
                                 ! i/p pixel 1
      CHARACTER DTYPE *( NDF__SZFTP ) ! Numeric type for output arrays
      INTEGER*8 EL               ! Number of mapped elements
      INTEGER I                  ! Loop counter
      INTEGER*8 IDIMS( NDF__MXDIM )! Dimensions of input NDF
      INTEGER IPAL               ! Pointer to averaged line
      CHARACTER ITYPE * ( NDF__SZTYP ) ! Numeric type for processing
      INTEGER*8 LBND( NDF__MXDIM ) ! Lower bounds of input NDF
      INTEGER*8 LBNDO( NDF__MXDIM ) ! Lower bounds of output NDF
      INTEGER*8 NBIN               ! Number of bins
      INTEGER NDFS               ! Identifier to the used section of
                                 ! the input NDF
      INTEGER NDIM               ! Number of dimensions
      INTEGER*8 ODIMS( NDF__MXDIM )! Dimensions of output array
      INTEGER PNTRI( 1 )         ! Pointer to input array component
      INTEGER*8 REF( NDF__MXDIM )! I/p pixel co-ords at bottom left of
                                 ! a compression box
      INTEGER*8 UBND( NDF__MXDIM ) ! Upper bounds of input NDF
      INTEGER*8 UBNDO( NDF__MXDIM )! Upper bounds of output NDF
      INTEGER WPNTR1             ! Pointer to workspace
      INTEGER WPNTR2             ! Pointer to workspace

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the shape and bound of the section.
      CALL NDF_DIM8( INDF, NDF__MXDIM, IDIMS, NDIM, STATUS )
      CALL NDF_BOUND8( INDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Work out the bounds for the output array and the size of the output
*  array from the input array dimensions, compression factor and
*  alignment to origin.  Also modify the input bounds so that they
*  correspond to the section of the input array that is actually used.
      DO I = 1, MAX( 2, NDIM )
         IF ( I .NE. DTAXIS ) THEN
            COMPRS( I ) = IDIMS( I )
         ELSE
            NBIN = MIN( IDIMS( I ), NUMBIN )
            COMPRS( I ) = IDIMS( I ) / NBIN
         END IF

*  Align with the origin.  However, leave it parameterised in case this
*  alignment no longer is no longer the only option.
         REF( I ) = LBND( I ) - 1
         D = KPG1_CEIL8( REAL( 1 - REF( I ) ) / REAL( COMPRS( I ) ) )
     :                   - 1

*  Pad the input image to make it a whole number of compression boxes.
         LBNDO( I ) = KPG1_FLOOR8( REAL( LBND( I ) - 1 - REF( I ) )
     :                           / REAL( COMPRS( I ) ) ) - D + 1
         UBNDO( I ) = MAX( LBNDO( I ),
     :                     KPG1_CEIL8( REAL( UBND( I ) - REF( I ) )
     :                               / REAL( COMPRS( I ) ) ) - D )

         ODIMS( I ) = UBNDO( I ) - LBNDO( I ) + 1

         LBND( I ) = 1 + REF( I ) + COMPRS( I ) * ( LBNDO( I ) - 1
     :                                              + D )
         UBND( I ) = REF( I ) + COMPRS( I ) * ( UBNDO( I ) + D )
         IDIMS( I ) = UBND( I ) - LBND( I ) + 1
      END DO

*  Determine the numeric type to be used for processing the sample
*  lines.  This step supports single- and double-precision
*  floating-point processing.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', INDF, INDF, 'Data', ITYPE,
     :                DTYPE, STATUS )

*  Create a section of the input NDF containing the region will actually
*  be used (i.e. excluding any pixels which lie over the edge of the
*  output image).
      CALL NDF_SECT8( INDF, NDIM, LBND, UBND, NDFS, STATUS )

*  Map the array component of the section.
      CALL NDF_MAP8( NDFS, 'Data', ITYPE, 'READ', PNTRI, EL, STATUS )

*  Obtain workspace for the averaged spectrum.
      CALL PSX_CALLOC8( ODIMS( DTAXIS ), ITYPE, IPAL, STATUS )

*  Obtain some workspace for the averaging and map them.  First find
*  the quantity and the type of the counting space required.
      CALL PSX_CALLOC8( IDIMS( 1 ), ITYPE, WPNTR1, STATUS )
      CALL PSX_CALLOC8( IDIMS( 1 ), '_INTEGER', WPNTR2, STATUS )

*  Average the lines in the section.  The routine expects at least two
*  dimensions.
      IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPG1_CMAV8R( MAX( 2, NDIM ), IDIMS,
     :                     %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     COMPRS, 1,  %VAL( CNF_PVAL( IPAL ) ),
     :                     %VAL( CNF_PVAL( WPNTR1 ) ),
     :                     %VAL( CNF_PVAL( WPNTR2 ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_CMAV8D( MAX( 2, NDIM ), IDIMS,
     :                     %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                     COMPRS, 1, %VAL( CNF_PVAL( IPAL ) ),
     :                     %VAL( CNF_PVAL( WPNTR1 ) ),
     :                     %VAL( CNF_PVAL( WPNTR2 ) ), STATUS )
      END IF

*  Free workspaces arrays used for compression.
      CALL PSX_FREE( WPNTR1, STATUS )
      CALL PSX_FREE( WPNTR2, STATUS )

*  Tidy the input data array.
      CALL NDF_UNMAP( NDFS, 'Data', STATUS )

*  Remove the enlarged-section NDF.
      CALL NDF_ANNUL( NDFS, STATUS )

*  Perform fits and iteratively reject outliers.
      IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPS1_MFEDR( NCLIP, CLIP, ODIMS( DTAXIS ),
     :                    %VAL( CNF_PVAL( IPAL ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPS1_MFEDD( NCLIP, CLIP, ODIMS( DTAXIS ),
     :                    %VAL( CNF_PVAL( IPAL ) ), STATUS )
      END IF

*  Determine the ranges from the good elements remaining in the line.
      IF ( ITYPE .EQ. '_REAL' ) THEN
          CALL KPS1_MFFRR( COMPRS( DTAXIS ), ODIMS( DTAXIS ),
     :                     %VAL( CNF_PVAL( IPAL ) ), MAXRNG, NRANGE,
     :                     RANGES, STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
          CALL KPS1_MFFRD( COMPRS( DTAXIS ), ODIMS( DTAXIS ),
     :                     %VAL( CNF_PVAL( IPAL ) ), MAXRNG, NRANGE,
     :                     RANGES, STATUS )
      END IF

*  We have finished with the averaged line.
      CALL PSX_FREE( IPAL, STATUS )

      END
