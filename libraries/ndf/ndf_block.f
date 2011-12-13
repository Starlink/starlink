      SUBROUTINE NDF_BLOCK( INDF1, NDIM, MXDIM, IBLOCK, INDF2, STATUS )
*+
*  Name:
*     NDF_BLOCK

*  Purpose:
*     Obtain an NDF section containing a block of adjacent pixels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_BLOCK( INDF1, NDIM, MXDIM, IBLOCK, INDF2, STATUS )

*  Description:
*     The routine returns an identifier for an NDF section describing a
*     "block" of adjacent pixels selected from an initial NDF. The
*     routine divides the original NDF logically into a series of such
*     blocks, each of which does not exceed a specified maximum number
*     of pixels in each dimension. The routine's IBLOCK argument
*     allows one of these blocks to be selected; an NDF section for it
*     is then returned.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for the initial NDF.
*     NDIM = INTEGER (Given)
*        Number of maximum dimension sizes.
*     MXDIM( NDIM ) = INTEGER (Given)
*        Array specifying the maximum size of a block in pixels along
*        each dimension.
*     IBLOCK = INTEGER (Given)
*        Number of the block required (the first block is numbered 1).
*     INDF2 = INTEGER (Returned)
*        Identifier for an NDF section describing the block.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is intended to allow NDFs to be processed in
*     smaller pieces by selecting successive blocks, each of which may
*     then be processed individually. Note that in general not all the
*     blocks selected from an NDF will have the same shape or size,
*     although none will exceed the specified maximum number of pixels
*     in each dimension.
*     -  Corresponding blocks selected from different NDFs (or NDF
*     sections) with identical shapes will themselves have identical
*     shapes and will contain the same number of pixels.
*     -  All NDF sections obtained via this routine have the same
*     number of dimensions as the input NDF. If the number of maximum
*     dimension sizes supplied (NDIM) is less than this number, then a
*     value of 1 will be used for the extra dimension sizes. If the
*     value of NDIM is larger than this number, then the excess
*     dimension sizes will be ignored.
*     -  If the number of the requested block (IBLOCK) exceeds the
*     number of blocks available in the NDF, then a value of NDF__NOID
*     will be returned for the INDF2 argument (but no error will
*     result). This condition may be used to terminate a loop when all
*     available blocks have been processed. The NDF_NBLOC routine may
*     also be used to determine the number of blocks available.
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for the INDF2 argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.
*     -  The NDF__NOID constant is defined in the include file NDF_PAR.

*  Algorithm:
*     -  Set an initial null value for the INDF2 argument before
*     checking the inherited global status.
*     -  Import the input identifier.
*     -  Check that the number of block dimensions is valid. Report an
*     error if it is not.
*     -  Check the maximum size specified for each block dimension.
*     Report an error if these values are not all positive.
*     -  Check that the block index value is valid. Report an error if
*     it is not.
*     -  Obtain the bounds of the input NDF from its main data array
*     identifier.
*     -  Loop to determine how many blocks will fit into each dimension
*     of the input NDF.
*     -  Store the stride of each dimension expressed in blocks (this
*     is the amount by which the block index must be incremented due to
*     all the blocks lying in lower dimensions when the pixel index in
*     this dimension is incremented by one).
*     -  Obtain the input dimension size.
*     -  Obtain the maximum block size in this dimension, restricting
*     it so as not to exceed the actual dimension size. Pad with 1's if
*     the NDF has more dimensions than the requested block.
*     -  Calculate how many blocks (or partial blocks) fit into this
*     dimension.
*     -  Accumulate the total number of blocks (or partial blocks) in
*     the input NDF.
*     -  If the block index does not exceed the number of blocks
*     available, then calculate the required block bounds.
*     -  Initialise the block offset for the current dimension and loop
*     to handle each NDF dimension, starting with the highest.
*     -  Find the (zero based) block index in the current dimension by
*     dividing the (vectorised) block offset by the dimension stride
*     expressed in blocks.
*     -  Convert the block index into dimension bounds, ensuring that
*     they lie within the bounds of the input NDF.
*     -  Decrement the (vectorised) block offset to obtain the offset
*     into the next lower dimension.
*     -  Cut the required section from the input NDF and export an
*     identifier for it
*     -  If an error occurred, then report context information.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     4-OCT-1991 (RFWS):
*        Original version.
*     14-OCT-1991 (RFWS):
*        Added extra status check.
*     17-OCT-1991 (RFWS):
*        Corrected error of 1 in calculating the upper bounds of a
*        block and a further error in the number of dimensions used for
*        the output NDF section.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

*  Arguments Given:
      INTEGER INDF1
      INTEGER NDIM
      INTEGER MXDIM( * )
      INTEGER IBLOCK

*  Arguments Returned:
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BSTRID( NDF__MXDIM ) ! Dimension stride in blocks
      INTEGER BLKS               ! Number of blocks in input NDF
      INTEGER DIM                ! Input NDF dimension size
      INTEGER DIMX( NDF__MXDIM ) ! Maximum size of each block dimension
      INTEGER I                  ! Loop counter for NDF dimensions
      INTEGER IACB1              ! Index of input NDF in the ACB
      INTEGER IACB2              ! Index of output NDF in the ACB
      INTEGER IB                 ! Block offset into dimension
      INTEGER IBLK               ! Block index within a dimension
      INTEGER LBND1( NDF__MXDIM ) ! Lower bounds of input NDF
      INTEGER LBND2( NDF__MXDIM ) ! Lower bounds of output section
      INTEGER NBLK               ! Number of blocks within a dimension
      INTEGER NDIM1              ! Number of input NDF dimensions
      INTEGER UBND1( NDF__MXDIM ) ! Upper bounds of input NDF
      INTEGER UBND2( NDF__MXDIM ) ! Upper bounds of output section

*.

*  Set an initial null value for the INDF2 argument.
      INDF2 = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the input identifier.
      CALL NDF1_IMPID( INDF1, IACB1, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the number of block dimensions is valid. Report an error
*  if it is not.
         IF ( ( NDIM .LT. 1 ) .OR. ( NDIM .GT. NDF__MXDIM ) ) THEN
            STATUS = NDF__NDMIN
            CALL MSG_SETI( 'NDIM', NDIM )
            CALL MSG_SETI( 'MXDIM', NDF__MXDIM )
            CALL ERR_REP( 'NDF_BLOCK_NDIM',
     :                    'Invalid number of block dimensions ' //
     :                    '(^NDIM) specified; should be in the ' //
     :                    'range 1 to ^MXDIM (possible ' //
     :                    'programming error).', STATUS )

*  Check the maximum size specified for each block dimension. Report an
*  error if these values are not all positive.
         ELSE
            DO 1 I = 1, NDIM
               IF ( MXDIM( I ) .LT. 1 ) THEN
                  STATUS = NDF__DIMIN
                  CALL MSG_SETI( 'I', I )
                  CALL MSG_SETI( 'DIM', MXDIM( I ) )
                  CALL ERR_REP( 'NDF_BLOCK_DIM',
     :                          'Maximum block size for dimension ' //
     :                          '^I has an invalid value of ^DIM; ' //
     :                          'its value should be positive ' //
     :                          '(possible programming error).',
     :                          STATUS )
                  GO TO 2
               END IF
 1          CONTINUE
 2          CONTINUE
         END IF

*  Check that the block index value is valid. Report an error if it is
*  not.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( IBLOCK .LT. 1 ) THEN
               STATUS = NDF__IBLIN
               CALL MSG_SETI( 'IBLOCK', IBLOCK )
               CALL ERR_REP( 'NDF_BLOCK_IBL',
     :                       'Block index value (^IBLOCK) is ' //
     :                       'invalid (possible programming error).',
     :                       STATUS )
            END IF
         END IF

*  Obtain the bounds of the input NDF from its main data array
*  identifier.
      IF ( STATUS .EQ. SAI__OK ) THEN
            CALL ARY_BOUND( ACB_DID( IACB1 ), NDF__MXDIM, LBND1, UBND1,
     :                      NDIM1, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop to determine how many blocks will fit into each dimension of
*  the input NDF.
               BLKS = 1
               DO 3 I = 1, NDIM1

*  Store the stride of each dimension expressed in blocks (this is the
*  amount by which the block index must be incremented due to all the
*  blocks lying in lower dimensions when the pixel index in this
*  dimension is incremented by one).
                  BSTRID( I ) = BLKS

*  Obtain the input dimension size.
                  DIM = UBND1 ( I ) - LBND1 ( I ) + 1

*  Obtain the maximum block size in this dimension, restricting it so
*  as not to exceed the actual dimension size. Pad with 1's if the NDF
*  has more dimensions than the requested block.
                  IF ( I .LE. NDIM ) THEN
                     DIMX( I ) = MIN( MXDIM( I ), DIM )
                  ELSE
                     DIMX( I ) = 1
                  END IF

*  Calculate how many blocks (or partial blocks) fit into this
*  dimension.
                  NBLK = 1 + ( ( DIM - 1 ) / DIMX( I ) )

*  Accumulate the total number of blocks (or partial blocks) in the
*  input NDF.
                  BLKS = BLKS * NBLK
 3             CONTINUE

*  If the block index does not exceed the number of blocks available,
*  then calculate the required block bounds.
               IF ( IBLOCK .LE. BLKS ) THEN

*  Initialise the block offset for the current dimension and loop to
*  handle each NDF dimension, starting with the highest.
                  IB = IBLOCK
                  DO 4 I = NDIM1, 1, -1

*  Find the (zero based) block index in the current dimension by
*  dividing the (vectorised) block offset by the dimension stride
*  expressed in blocks.
                     IBLK = ( IB - 1 ) / BSTRID( I )

*  Convert the block index into dimension bounds, ensuring that they
*  lie within the bounds of the input NDF.
                     LBND2( I ) = LBND1( I ) + IBLK * DIMX( I )
                     UBND2( I ) = MIN( LBND2( I ) + DIMX( I ) - 1,
     :                                 UBND1( I ) )

*  Decrement the (vectorised) block offset to obtain the offset into
*  the next lower dimension.
                     IB = IB - ( IBLK * BSTRID( I ) )
 4                CONTINUE

*  Cut the required section from the input NDF and export an identifier
*  for it
                  CALL NDF1_CUT( IACB1, NDIM1, LBND2, UBND2, IACB2,
     :                           STATUS )
                  CALL NDF1_EXPID( IACB2, INDF2, STATUS )
               END IF
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_BLOCK_ERR',
     :   'NDF_BLOCK: Error obtaining a block of adjacent pixels ' //
     :   'from an NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_BLOCK', STATUS )
      END IF

      END
