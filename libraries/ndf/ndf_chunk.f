      SUBROUTINE NDF_CHUNK( INDF1, MXPIX, ICHUNK, INDF2, STATUS )
*+
*  Name:
*     NDF_CHUNK

*  Purpose:
*     Obtain an NDF section containing a chunk of contiguous pixels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_CHUNK( INDF1, MXPIX, ICHUNK, INDF2, STATUS )

*  Description:
*     The routine returns an identifier for an NDF section describing a
*     "chunk" of contiguous pixels selected from an initial NDF.  The
*     routine divides the initial NDF logically into a series of such
*     chunks, each of which follows immediately on from the previous
*     chunk, and each of which contains no more than a specified
*     maximum number (MXPIX) of contiguous pixels. The routine's ICHUNK
*     argument allows one of these chunks to be selected; an NDF
*     section for it is then returned.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for the initial NDF.
*     MXPIX = INTEGER (Given)
*        Maximum number of contiguous pixels required in each chunk.
*     ICHUNK = INTEGER (Given)
*        Number of the chunk required (the first chunk is numbered 1).
*     INDF2 = INTEGER (Returned)
*        Identifier for an NDF section describing the chunk.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is intended to allow large NDFs to be processed
*     in smaller pieces by selecting successive chunks, each of which
*     may then be processed individually. Note that in general not all
*     the chunks selected from an NDF will have the same size, although
*     none will contain more than the specified maximum number of
*     pixels.
*     -  Corresponding chunks selected from different NDFs (or NDF
*     sections) with identical shapes will themselves have identical
*     shapes and will contain the same number of pixels.
*     -  All NDF sections obtained via this routine have the same number
*     of dimensions as the input NDF.
*     -  If the number of the requested chunk (ICHUNK) exceeds the
*     number of chunks available in the NDF, then a value of NDF__NOID
*     will be returned for the INDF2 argument (but no error will
*     result). This condition may be used to terminate a loop when all
*     available chunks have been processed. The NDF_NCHNK routine may
*     also be used to determine the number of chunks available.
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for the INDF2 argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.
*     -  The NDF__NOID constant is defined in the include file NDF_PAR.

*  Algorithm:
*     -  Set an initial value for the INDF2 argument before checking
*     the inherited global status.
*     -  Import the NDF identifier.
*     -  Check that the maximum number of pixels in the chunk is
*     positive.  Report an error if it is not.
*     -  Check that the chunk index is positive. Report an error if it
*     is not.
*     -  Obtain the pixel index bounds of the NDF from its data array.
*     -  Loop to identify the dimension which must be broken in order
*     not to exceed the maximum number of contiguous pixels.
*     -  Calculate the size of the current dimension and the stride of
*     the next dimension.
*     -  If the stride of the next dimension does not exceed the
*     contiguous pixel limit, then the current dimension need not be
*     broken, so its bounds are unchanged.
*     -  Note the first dimension to be broken, which is where the
*     stride of the following dimension first exceeds the contiguous
*     pixel limit.
*     -  For this and subsequent dimensions, store the dimension size
*     and stride for later use.
*     -  Retain the stride for the next dimension.
*     -  If no dimension has to be broken, then the entire NDF can be
*     accommodated in a single chunk. The chunk index must therefore be
*     1 if a chunk is to be returned.
*     -  Otherwise, calculate by how many the pixel index of the broken
*     dimension can be incremented to produce a chunk of contiguous
*     pixels withput exceeding the contiguous pixel limit.
*     -  Find how many of these pixel index increments fit into the
*     size of the broken dimension. Allow for a partial increment at
*     the end.
*     -  As the offset in pixels from the start of the NDF increases,
*     the pixel index of the broken dimension will cycle repeatedly
*     between its lower and upper bounds. Determine how many complete
*     times this dimension has to be cycled through in order to obtain
*     chunk number ICHUNK by dividing the offset from the beginning of
*     the NDF in chunks by the number of chunks (increments) per cycle,
*     as determined above.
*     -  Find the remainder, which gives the number of chunks offset
*     into the final (partial) cycle through the broken dimension.
*     -  Convert the number of chunks offset into a lower and upper
*     bound in the broken dimension, checking that the resulting
*     chunk's upper bound does not exceed the broken dimension's upper
*     bound.
*     -  Calculate the number of pixels skipped over which must be
*     taken up by incrementing the higher dimensions (above the broken
*     one).
*     -  Check that this count is less than the total number of pixels
*     in the NDF, otherwise there will be no pixels left for the chunk
*     itself (i.e. the chunk index is too high).
*     -  Loop to calculate the bounds of the chunk in each higher
*     dimension.  In this case the lower and upper bounds are equal.
*     -  Divide the remaining pixel count by the dimension stride to
*     get the dimension offset from its lower bound. Then calculate the
*     required lower and upper bounds of the chunk in this dimension.
*     -  Decrement the remaining pixel count and return to handle the
*     next lower dimension.
*     -  If the chunk index was valid, then cut an approprite section
*     from the NDF.
*     -  If an error occurred, then report context information and call
*     the error tracing routine.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-FEB-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

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

*  Arguments Given:
      INTEGER INDF1
      INTEGER MXPIX
      INTEGER ICHUNK

*  Arguments Returned:
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDIM               ! Pixel index offset (higher dimension)
      INTEGER IINC               ! Increment offset in broken dimmension
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER IACB1              ! Index to initial NDF entry in the ACB
      INTEGER IACB2              ! Index to chunk entry in the ACB
      INTEGER BR                 ! First dimension to be broken
      INTEGER D                  ! Size of current dimension
      INTEGER EL                 ! Number of elements in NDF
      INTEGER I                  ! Loop counter for dimensions
      INTEGER INC                ! Broken dimension increment per chunk
      INTEGER LBND1( NDF__MXDIM ) ! Lower bounds of initial NDF
      INTEGER LBND2( NDF__MXDIM ) ! Lower bounds of chunk
      INTEGER NCYCLE             ! Number of broken dimension cycles
      INTEGER NINC               ! Number of increments/broken dimension
      INTEGER OFFSET             ! Pixel offset from start of NDF
      INTEGER S                  ! Stride of current dimension
      INTEGER STRIDE( NDF__MXDIM ) ! Stride of each dimension
      INTEGER DIM( NDF__MXDIM )  ! Size of each NDF dimension
      INTEGER UBND1( NDF__MXDIM ) ! Upper bounds of initial NDF
      INTEGER UBND2( NDF__MXDIM ) ! Upper bounds of chunk
      LOGICAL ISCHNK             ! Whether the specified chunk exists

*.

*  Set an initial value for the INDF2 argument.
      INDF2 = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF1, IACB1, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the maximum number of pixels in the chunk is positive.
*  Report an error if it is not.
         IF ( MXPIX .LT. 1 ) THEN
            STATUS = NDF__MXPIN
            CALL MSG_SETI( 'MXPIX', MXPIX )
            CALL ERR_REP( 'NDF_CHUNK_MXPIX',
     :      'Specified maximum number of contiguous pixels (^MXPIX) ' //
     :      'is invalid (possible programming error).', STATUS )

*  Check that the chunk index is positive. Report an error if it is
*  not.
         ELSE IF ( ICHUNK .LT. 1 ) THEN
            STATUS = NDF__ICHIN
            CALL MSG_SETI( 'ICHUNK', ICHUNK )
            CALL ERR_REP( 'NDF_CHUNK_ICHNK',
     :      'Chunk index value (^ICHUNK) is invalid (possible ' //
     :      'programming error).', STATUS )

*  Obtain the pixel index bounds of the NDF from its data array.
         ELSE
            CALL ARY_BOUND( ACB_DID( IACB1 ), NDF__MXDIM, LBND1, UBND1,
     :                      NDIM, STATUS )

*  Loop to identify the dimension which must be broken in order not to
*  exceed the maximum number of contiguous pixels.
            S = 1
            BR = 0
            DO 1 I = 1, NDIM

*  Calculate the size of the current dimension and the stride of the
*  next dimension (at this point S holds the stride of the current
*  dimension).
               D = UBND1( I ) - LBND1( I ) + 1
               EL = S * D

*  If the stride of the next dimension does not exceed the contiguous
*  pixel limit, then the current dimension need not be broken, so its
*  bounds are unchanged.
               IF ( EL .LE. MXPIX ) THEN
                  LBND2( I ) = LBND1( I )
                  UBND2( I ) = UBND1( I )

*  Note the first dimension to be broken, which is where the stride of
*  the following dimension first exceeds the contiguous pixel limit.
*  For this and subsequent dimensions, store the dimension size and
*  stride for later use.
               ELSE
                  IF ( BR .EQ. 0 ) THEN
                     BR = I
                  END IF
                  DIM( I ) = D
                  STRIDE( I ) = S
               END IF

*  Retain the stride for the next dimension. Note that on exit from
*  this loop, EL holds a count of the total number of pixels in the
*  input NDF.
               S = EL
1           CONTINUE

*  If no dimension has to be broken, then the entire NDF can be
*  accommodated in a single chunk. The chunk index must therefore be 1
*  if a chunk is to be returned.
            IF ( BR .EQ. 0 ) THEN
               ISCHNK = ICHUNK .EQ. 1

*  Otherwise, calculate by how many the pixel index of the broken
*  dimension can be incremented to produce a chunk of contiguous pixels
*  withput exceeding the contiguous pixel limit.
            ELSE
               INC = MXPIX / STRIDE( BR )

*  Find how many of these pixel index increments fit into the size of
*  the broken dimension. Allow for a partial increment at the end.
               NINC = DIM( BR ) / INC
               IF ( ( NINC * INC ) .LT. DIM( BR ) ) THEN
                  NINC = NINC + 1
               END IF

*  As the offset in pixels from the start of the NDF increases, the
*  pixel index of the broken dimension will cycle repeatedly between
*  its lower and upper bounds. Determine how many complete times this
*  dimension has to be cycled through in order to obtain chunk number
*  ICHUNK by dividing the offset from the beginning of the NDF in
*  chunks by the number of chunks (increments) per cycle, as determined
*  above.
               NCYCLE = ( ICHUNK - 1 ) / NINC

*  Find the remainder, which gives the number of chunks offset into the
*  final (partial) cycle through the broken dimension.
               IINC = ICHUNK - 1 - ( NINC * NCYCLE )

*  Convert the number of chunks offset into a lower and upper bound in
*  the broken dimension, checking that the resulting chunk's upper
*  bound does not exceed the broken dimension's upper bound.
               LBND2( BR ) = LBND1( BR ) + INC * IINC
               UBND2( BR ) = LBND2( BR ) + INC - 1
               IF ( UBND2( BR ) .GT. UBND1( BR ) ) THEN
                  UBND2( BR ) = UBND1( BR )
               END IF

*  Calculate the number of pixels skipped over which must be taken up
*  by incrementing the higher dimensions (above the broken one).
               OFFSET = NCYCLE * STRIDE( BR ) * DIM ( BR )

*  Check that this count is less than the total number of pixels in the
*  NDF, otherwise there will be no pixels left for the chunk itself
*  (i.e. the chunk index is too high).
               ISCHNK = OFFSET .LT. EL
               IF ( ISCHNK ) THEN

*  Loop to calculate the bounds of the chunk in each higher dimension.
*  In this case the lower and upper bounds are equal.
                  DO 2 I = NDIM, BR + 1, -1

*  Divide the remaining pixel count by the dimension stride to get the
*  dimension offset from its lower bound. Then calculate the required
*  lower and upper bounds of the chunk in this dimension.
                     IDIM = OFFSET / STRIDE ( I )
                     LBND2( I ) = LBND1( I ) + IDIM
                     UBND2( I ) = LBND2( I )

*  Decrement the remaining pixel count and return to handle the next
*  lower dimension.
                     OFFSET = OFFSET - ( IDIM * STRIDE( I ) )
2                 CONTINUE
               END IF
            END IF

*  If the chunk index was valid, then cut an appropriate section from
*  the NDF.
            IF ( ISCHNK ) THEN
               CALL NDF1_CUT( IACB1, NDIM, LBND2, UBND2, IACB2, STATUS )
               CALL NDF1_EXPID( IACB2, INDF2, STATUS )
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_CHUNK_ERR',
     :   'NDF_CHUNK: Error obtaining a chunk of contiguous ' //
     :   'pixels from an NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_CHUNK', STATUS )
      END IF

      END
