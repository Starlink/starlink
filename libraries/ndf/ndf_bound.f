      SUBROUTINE NDF_BOUND( INDF, NDIMX, LBND, UBND, NDIM, STATUS )
*+
*  Name:
*     NDF_BOUND

*  Purpose:
*     Enquire the pixel-index bounds of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_BOUND( INDF, NDIMX, LBND, UBND, NDIM, STATUS )

*  Description:
*     The routine returns the lower and upper pixel-index bounds of
*     each dimension of an NDF, together with the total number of
*     dimensions.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     NDIMX = INTEGER (Given)
*        Maximum number of pixel-index bounds to return (i.e. the
*        declared size of the LBND and UBND arguments).
*     LBND( NDIMX ) = INTEGER (Returned)
*        Lower pixel-index bounds for each dimension.
*     UBND( NDIMX ) = INTEGER (Returned)
*        Upper pixel-index bounds for each dimension.
*     NDIM = INTEGER (Returned)
*        Total number of NDF dimensions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the NDF has fewer than NDIMX dimensions, then any remaining
*     elements of the LBND and UBND arguments will be filled with 1's.
*     -  If the NDF has more than NDIMX dimensions, then the NDIM
*     argument will return the actual number of dimensions. In this
*     case only the first NDIMX sets of bounds will be returned, except
*     that an error will result if the size of any of the remaining
*     dimensions exceeds 1.
*     -  If this routine is called with STATUS set, then a value of 1
*     will be returned for all elements of the LBND and UBND arrays and
*     for the NDIM argument, although no further processing will occur.
*     The same values will also be returned if the routine should fail
*     for any reason.
*     -  The symbolic constant NDF__MXDIM may be used to declare the
*     size of the LBND and UBND arguments so that they will be able to
*     hold the maximum number of NDF bounds that this routine can
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
*        Added checks on the returned status from ARY_BOUND and updated
*        the prologue to reflect new behaviour if significant
*        dimensions are excluded.
*     16-MAR-1990 (RFWS):
*        Added notes section to prologue.
*     4-DEC-1990 (RFWS):
*        Changed to return "safe" bounds information under error
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
*           ARY_ system identifier for the data array.

*  Arguments Given:
      INTEGER INDF
      INTEGER NDIMX

*  Arguments Returned:
      INTEGER LBND( NDIMX )
      INTEGER UBND( NDIMX )
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

*  Obtain the pixel-index bounds information from the data array
*  component.
            CALL ARY_BOUND( ACB_DID( IACB ), NDIMX, LBND, UBND, NDIM,
     :                      STATUS )

*  If the returned status value indicates that significant dimensions
*  have been excluded, then annul the error and make a new error report
*  referring to the NDF data object.
            IF ( STATUS .EQ. ARY__XSDIM ) THEN
               CALL ERR_ANNUL( STATUS )
               STATUS = NDF__XSDIM
               CALL NDF1_AMSG( 'NDF', IACB )
               CALL MSG_SETI( 'NDIMX', NDIMX )
               CALL ERR_REP( 'NDF_BOUND_NDIM',
     :         'The NDF structure ^NDF has more than ^NDIMX ' //
     :         'significant dimension(s).', STATUS )
            END IF

*  Release the error stack.
            CALL ERR_RLSE
         END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'NDF_BOUND_ERR',
     :      'NDF_BOUND: Error enquiring the pixel-index bounds of ' //
     :      'an NDF.', STATUS )
            CALL NDF1_TRACE( 'NDF_BOUND', STATUS )
         END IF
      END IF

*  Under error conditions, return "safe" bounds information.
      IF ( STATUS .NE. SAI__OK ) THEN
         DO 1 I = 1, NDIMX
            LBND( I ) = 1
            UBND( I ) = 1
 1       CONTINUE
         NDIM = 1
      END IF

      END
