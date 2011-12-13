      SUBROUTINE NDF_NBLOC( INDF, NDIM, MXDIM, NBLOCK, STATUS )
*+
*  Name:
*     NDF_NBLOC

*  Purpose:
*     Determine the number of blocks of adjacent pixels in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_NBLOC( INDF, NDIM, MXDIM, NBLOCK, STATUS )

*  Description:
*     The routine determines the number of "blocks" (i.e. sections) of
*     adjacent pixels that can be obtained from an NDF, subject to the
*     constraint that no block should exceed a specified maximum number
*     of pixels in any dimension. More specifically, given the maximum
*     size in pixels of a block in each dimension (MXDIM), this routine
*     returns the maximum value which can be supplied for the IBLOCK
*     argument of the routine NDF_BLOCK if a valid NDF identifier for a
*     block of adjacent pixels is to be returned.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     NDIM = INTEGER (Given)
*        Number of maximum dimension sizes.
*     MXDIM( NDIM ) = INTEGER (Given)
*        Array specifying the maximum size of a block in pixels along
*        each dimension.
*     NBLOCK = INTEGER (Returned)
*        Number of blocks which can be obtained from the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is provided to calculate an upper bound on the
*     number of blocks for DO-loops which process NDFs by dividing them
*     into separate blocks by means of calls to the routine NDF_BLOCK.
*     -  If the number of maximum dimension sizes supplied (NDIM) is
*     less than the number of NDF dimensions, then a value of 1 will be
*     used for the extra dimension sizes. If the value of NDIM is
*     larger than this number, then the excess dimension sizes will be
*     ignored.
*     -  A value of zero will be returned for the NBLOCK argument if
*     this routine is called with STATUS set. The same value will also
*     be returned if the routine should fail for any reason.

*  Algorithm:
*     -  Set an initial default value for the NBLOCK argument before
*     checking the inherited global status.
*     -  Import the NDF identifier.
*     -  Check that the number of block dimensions is valid. Report an
*     error if it is not.
*     -  Check the maximum size specified for each block dimension.
*     Report an error if these values are not all positive.
*     -  Obtain the bounds of the NDF from its main data array
*     identifier.
*     -  Loop to determine how many blocks will fit into each dimension
*     of the input NDF.
*     -  Obtain the maximum block size in this dimension, restricting
*     it so as not to exceed the actual dimension size. Pad with 1's if
*     the NDF has more dimensions than the requested block.
*     -  Calculate how many blocks (or partial blocks) fit into this
*     dimension.
*     -  Accumulate the total number of blocks (or partial blocks) in
*     the NDF.
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
*        Added an extra status check.
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
      INTEGER NDIM
      INTEGER MXDIM( * )

*  Arguments Returned:
      INTEGER NBLOCK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIM( NDF__MXDIM )  ! NDF dimension sizes
      INTEGER DIMX               ! Maximum size of block dimension
      INTEGER I                  ! Loop counter for NDF dimensions
      INTEGER IACB               ! Index of NDF in the ACB
      INTEGER NBLK               ! Number of blocks within a dimension
      INTEGER NDIM1              ! Number of NDF dimensions

*.

*  Set an initial default value for the NBLOCK argument.
      NBLOCK = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the number of block dimensions is valid. Report an error
*  if it is not.
         IF ( ( NDIM .LT. 1 ) .OR. ( NDIM .GT. NDF__MXDIM ) ) THEN
            STATUS = NDF__NDMIN
            CALL MSG_SETI( 'NDIM', NDIM )
            CALL MSG_SETI( 'MXDIM', NDF__MXDIM )
            CALL ERR_REP( 'NDF_NBLOC_NDIM',
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
                  CALL ERR_REP( 'NDF_NBLOC_DIM',
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

*  Obtain the bounds of the NDF from its main data array identifier.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL ARY_DIM( ACB_DID( IACB ), NDF__MXDIM, DIM, NDIM1,
     :                    STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop to determine how many blocks will fit into each dimension of
*  the input NDF.
               NBLOCK = 1
               DO 3 I = 1, NDIM1

*  Obtain the maximum block size in this dimension, restricting it so
*  as not to exceed the actual dimension size. Pad with 1's if the NDF
*  has more dimensions than the requested block.
                  IF ( I .LE. NDIM ) THEN
                     DIMX = MIN( MXDIM( I ), DIM( I ) )
                  ELSE
                     DIMX = 1
                  END IF

*  Calculate how many blocks (or partial blocks) fit into this
*  dimension.
                  NBLK = 1 + ( ( DIM( I ) - 1 ) / DIMX )

*  Accumulate the total number of blocks (or partial blocks) in the
*  NDF.
                  NBLOCK = NBLOCK * NBLK
 3             CONTINUE
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_NBLOC_ERR',
     :   'NDF_NBLOC: Error determining the number of blocks of ' //
     :   'adjacent pixels in an NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_NBLOC', STATUS )
      END IF

      END
