      SUBROUTINE NDF_DIM( INDF, NDIMX, DIM, NDIM, STATUS )
*+
*  Name:
*     NDF_DIM

*  Purpose:
*     Enquire the dimension sizes of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_DIM( INDF, NDIMX, DIM, NDIM, STATUS )

*  Description:
*     The routine returns the size in pixels of each dimension of an
*     NDF, together with the total number of dimensions (the size of a
*     dimension is the difference between that dimension's upper and
*     lower pixel-index bounds + 1).

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     NDIMX = INTEGER (Given)
*        Maximum number of dimension sizes to return (i.e. the declared
*        size of the DIM argument).
*     DIM( NDIMX ) = INTEGER (Returned)
*        Size of each dimension in pixels.
*     NDIM = INTEGER (Returned)
*        Total number of NDF dimensions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the NDF has fewer than NDIMX dimensions, then any remaining
*     elements of the DIM argument will be filled with 1's.
*     -  If the NDF has more than NDIMX dimensions, then the NDIM
*     argument will return the actual number of dimensions. In this
*     case only the first NDIMX dimension sizes will be returned, except
*     that an error will result if the size of any of the excluded
*     dimensions exceeds 1.
*     -  If this routine is called with STATUS set, then a value of 1
*     will be returned for all elements of the DIM array and for the
*     NDIM argument, although no further processing will occur.  The
*     same values will also be returned if the routine should fail for
*     any reason.
*     -  The symbolic constant NDF__MXDIM may be used to declare the
*     size of the DIM argument so that it will be able to hold the
*     maximum number of NDF dimension sizes that this routine can
*     return. This constant is defined in the include file NDF_PAR.

*  Copyright:
*     Copyright (C) 1994 Particle Physics & Astronomy Research Council

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
*     29-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     15-MAR-1990 (RFWS):
*        Added checks on the returned status from ARY_DIM and updated
*        the prologue to reflect new behaviour if significant dimensions
*        are excluded.
*     4-DEC-1990 (RFWS):
*        Changed to return "safe" dimension information under error
*        conditions.
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
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

*  Arguments Given:
      INTEGER INDF
      INTEGER NDIMX

*  Arguments Returned:
      INTEGER DIM( NDIMX )
      INTEGER NDIM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACB               ! Index to NDF entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Import the NDF identifier.
         CALL NDF1_IMPID( INDF, IACB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Mark the error stack.
            CALL ERR_MARK

*  Obtain the dimension size information from the data array component.
            CALL ARY_DIM( ACB_DID( IACB ), NDIMX, DIM, NDIM, STATUS )

*  If the returned status value indicates that significant dimensions
*  were excluded, then annul the error and make a new error report
*  referring to the NDF data structure.
            IF ( STATUS .EQ. ARY__XSDIM ) THEN
               CALL ERR_ANNUL( STATUS )
               STATUS = NDF__XSDIM
               CALL NDF1_AMSG( 'NDF', IACB )
               CALL MSG_SETI( 'NDIMX', NDIMX )
               CALL ERR_REP( 'NDF_DIM_NDIM',
     :         'The NDF structure ^NDF has more than ^NDIMX ' //
     :         'significant dimension(s).', STATUS )
            END IF

*  Release the error stack.
            CALL ERR_RLSE
         END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'NDF_DIM_ERR',
     :      'NDF_DIM: Error enquiring the dimension sizes of an NDF.',
     :      STATUS )
            CALL NDF1_TRACE( 'NDF_DIM', STATUS )
         END IF
      END IF

*  Under error conditions, return "safe" dimension information.
      IF ( STATUS .NE. SAI__OK ) THEN
         DO 1 I = 1, NDIMX
            DIM( I ) = 1
 1       CONTINUE
         NDIM = 1
      END IF

      END
