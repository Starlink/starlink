      SUBROUTINE NDF_NCHNK( INDF, MXPIX, NCHUNK, STATUS )
*+
*  Name:
*     NDF_NCHNK

*  Purpose:
*     Determine the number of chunks of contiguous pixels in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_NCHNK( INDF, MXPIX, NCHUNK, STATUS )

*  Description:
*     The routine determines the number of "chunks" (i.e. sections) of
*     contiguous pixels that can be obtained from an NDF, subject to
*     the constraint that no chunk should contain more than a specified
*     maximum number of pixels. More specifically, given the maximum
*     number of pixels in a chunk (MXPIX), this routine returns the
*     maximum value which can be supplied for the ICHUNK argument of
*     the routine NDF_CHUNK if a valid NDF identifier for a chunk of
*     contiguous pixels is to be returned.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     MXPIX = INTEGER (Given)
*        Maximum number of contiguous pixels required in each chunk.
*     NCHUNK = INTEGER (Returned)
*        Number of chunks which can be obtained from the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is provided to calculate an upper bound on the
*     number of chunks for DO-loops which process NDFs by dividing them
*     into separate chunks by means of calls to the routine NDF_CHUNK.
*     -  A value of zero will be returned for the NCHUNK argument if
*     this routine is called with STATUS set. The same value will also
*     be returned if the routine should fail for any reason.

*  Algorithm:
*     -  Set an initial default value for the NCHUNK argument.
*     -  Import the NDF identifier.
*     -  Check that the number of contiguous pixels specified is valid.
*     Report an error if it is not.
*     -  Determine the dimension sizes of the NDF from its data array.
*     -  Loop to identify the dimension which must be broken in order
*     not to exceed the maximum number of contiguous pixels.
*     -  Calculate the size of the current dimension and the stride of
*     the next dimension.
*     -  Note the first dimension to be broken, which is where the
*     stride of the following dimension first exceeds the contiguous
*     pixel limit. Quit the loop at this point.
*     -  If no dimension was broken, then all the NDF's pixels can be
*     accommodated in a single chunk.
*     -  Otherwise, calculate by how many the pixel index of the broken
*     dimension can be incremented to produce a chunk of contiguous
*     pixels without exceeding the contiguous pixel limit.
*     -  Find how many of these pixel index increments fit into the
*     size of the broken dimension. Allow for a partial increment at
*     the end.
*     -  Loop to multiply the number of chunks per broken dimension by
*     the number of times the broken dimension is cycled through in
*     passing from start to finish through all the NDF's pixels. This
*     is given by the product of the sizes of all the dimensions higher
*     than the broken dimension.
*     -  If an error occurred, then report context information.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     23-FEB-1990 (RFWS):
*        Original version.
*     1-OCT-1991 (RFWS):
*        Ensure that NCHUNK is returned as zero under error conditions.
*     4-OCT-1991 (RFWS):
*        Added an extra status check.
*     4-OCT-1991 (RFWS):
*        Changed to call ARY_DIM instead of ARY_BOUND.
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
      INTEGER INDF
      INTEGER MXPIX

*  Arguments Returned:
      INTEGER NCHUNK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BR                 ! First dimension to be broken
      INTEGER DIM( NDF__MXDIM )  ! NDF dimension sizes
      INTEGER EL                 ! Stride of next dimension
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER INC                ! Broken dimension increment per chunk
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER S                  ! Stride of current dimension

*.

*  Set an initial default value for the NCHUNK argument.
      NCHUNK = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the number of contiguous pixels specified is valid. Report
*  an error if it is not.
         IF ( MXPIX .LT. 1 ) THEN
            STATUS = NDF__MXPIN
            CALL MSG_SETI( 'MXPIX', MXPIX )
            CALL ERR_REP( 'NDF_NCHNK_MXPIX',
     :      'Specified maximum number of contiguous pixels (^MXPIX) ' //
     :      'is invalid (possible programming error).', STATUS )

*  Determine the dimension sizes of the NDF from its data array.
         ELSE
            CALL ARY_DIM( ACB_DID( IACB ), NDF__MXDIM, DIM, NDIM,
     :                    STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop to identify the dimension which must be broken in order not to
*  exceed the maximum number of contiguous pixels.
               S = 1
               BR = 0
               DO 1 I = 1, NDIM

*  Calculate the stride of the next dimension (at this point S holds
*  the stride of the current dimension).
                  EL = S * DIM( I )

*  Note the first dimension to be broken, which is where the stride of
*  the following dimension first exceeds the contiguous pixel limit.
*  Quit the loop at this point.
                  IF ( EL .GT. MXPIX ) THEN
                     BR = I
                     GO TO 2
                  END IF

*  Retain the stride for the next dimension.
                  S = EL
 1             CONTINUE
 2             CONTINUE

*  If no dimension was broken, then all the NDF's pixels can be
*  accommodated in a single chunk.
               IF ( BR .EQ. 0 ) THEN
                  NCHUNK = 1

*  Otherwise, calculate by how many the pixel index of the broken
*  dimension can be incremented to produce a chunk of contiguous pixels
*  without exceeding the contiguous pixel limit.
               ELSE
                  INC = MXPIX / S

*  Find how many of these pixel index increments fit into the size of
*  the broken dimension. Allow for a partial increment at the end.
                  NCHUNK = 1 + ( ( DIM( BR ) - 1 ) / INC )

*  Loop to multiply the number of chunks per broken dimension by the
*  number of times the broken dimension is cycled through in passing
*  from start to finish through all the NDF's pixels. This latter
*  number is given by the product of the sizes of all the dimensions
*  higher than the broken dimension.
                  DO 3 I = BR + 1, NDIM
                     NCHUNK = NCHUNK * DIM( I )
 3                CONTINUE
               END IF
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_NCHNK_ERR',
     :   'NDF_NCHNK: Error determining how many chunks of ' //
     :   'contiguous pixels can be obtained from an NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_NCHNK', STATUS )
      END IF

      END
