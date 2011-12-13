      SUBROUTINE NDF1_MBNDP( N, NDFS, STATUS )
*+
*  Name:
*     NDF1_MBNDP

*  Purpose:
*     Match the bounds of a set of NDFs by padding.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_MBNDP( N, NDFS, STATUS )

*  Description:
*     The routine finds the pixel index bounds describing the smallest
*     region which contains all the pixels in the set of NDFs supplied
*     and "pads" each NDF so that its bounds match this region.  If
*     necessary, the routine returns an identifier for an appropriate
*     section from each NDF in place of the identifier initially
*     supplied, which is annulled.

*  Arguments:
*     N = INTEGER (Given)
*        Number of NDF identifiers.
*     NDFS( N ) = INTEGER (Given and Returned)
*        Array of identifiers for the NDFs whose bounds are to be
*        matched.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The number of dimensions of each NDF section returned is set
*     to the maximum number of dimensions of all the NDFs supplied.
*     -  NDF sections are only returned if the NDF's bounds actually
*     need changing, otherwise the original NDF identifier is returned
*     unaltered.

*  Algorithm:
*     -  Initialise the output bounds and number of dimensions.
*     -  Loop to consider each NDF supplied. Import its identifier.
*     -  Obtain the NDF's bounds and number of dimensions from its data
*     array component.
*     -  Accumulate the output bounds and number of dimensions.
*     -  Consider each NDF in turn again. Import its identifier.
*     -  Obtain its bounds and number of dimensions.
*     -  See if it is necessary to create a new section in order to
*     match the bounds. First test if the number of dimensions needs
*     changing. Then test each pair of dimension bounds.
*     -  If necessary, produce a suitable section.
*     -  Annul the original ACB entry and issue an identifier for the
*     new one describing the section.

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
*     6-FEB-1990 (RFWS):
*        Original, derived from the NDF1_MBNDT routine.
*     11-APR-1990 (RFWS):
*        Changed so that sections are only produced if the NDF bounds
*        actually need changing. Padding of bounds with 1's removed,
*        since ARY_BOUND now performs this task.
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
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

*  Arguments Given:
      INTEGER N

*  Arguments Given and Returned:
      INTEGER NDFS( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IACBS              ! Index to NDF section entry in the ACB
      INTEGER INDF               ! Loop counter for NDFs
      INTEGER LBND( NDF__MXDIM ) ! Lower output bounds
      INTEGER NDIM               ! Number of output dimensions
      INTEGER NLBND( NDF__MXDIM ) ! Lower NDF bounds
      INTEGER NNDIM              ! Number of NDF dimensions
      INTEGER NUBND( NDF__MXDIM ) ! Upper NDF bounds
      INTEGER UBND( NDF__MXDIM ) ! Upper output bounds
      LOGICAL SECT               ! Whether a section is needed

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the output bounds and number of dimensions.
      DO 1 I = 1, NDF__MXDIM
         LBND( I ) = NUM__MAXI
         UBND( I ) = NUM__MINI
1     CONTINUE
      NDIM = 1

*  Loop to consider each NDF supplied. Import its identifier.
      DO 3 INDF = 1, N
         CALL NDF1_IMPID( NDFS( INDF ), IACB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain the NDF's bounds and number of dimensions from its data array
*  component. (Note the bounds will be padded with 1's if necessary.)
            CALL ARY_BOUND( ACB_DID( IACB ), NDF__MXDIM, NLBND, NUBND,
     :                      NNDIM, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Accumulate the output bounds and number of dimensions.
               DO 2 I = 1, NDF__MXDIM
                  LBND( I ) = MIN( LBND( I ), NLBND( I ) )
                  UBND( I ) = MAX( UBND( I ), NUBND( I ) )
2              CONTINUE
               NDIM = MAX( NDIM, NNDIM )
            END IF
         END IF

*  Quit considering NDFs if an error occurs.
         IF ( STATUS .NE. SAI__OK ) GO TO 4
3     CONTINUE
4     CONTINUE

*  Consider each NDF in turn again. Import its identifier.
      IF ( STATUS .EQ. SAI__OK ) THEN
         DO 7 INDF = 1, N
            CALL NDF1_IMPID( NDFS( INDF ), IACB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain its bounds and number of dimensions.
               CALL ARY_BOUND( ACB_DID( IACB ), NDF__MXDIM, NLBND,
     :                         NUBND, NNDIM, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  See if it is necessary to create a new section in order to match the
*  bounds. First test if the number of dimensions needs changing.
                  IF ( NNDIM .NE. NDIM ) THEN
                     SECT = .TRUE.

*  Then test each pair of dimension bounds.
                  ELSE
                     SECT = .FALSE.
                     DO 5 I = 1, NDIM
                        IF ( ( NLBND( I ) .NE. LBND( I ) ) .OR.
     :                       ( NUBND( I ) .NE. UBND( I ) ) ) THEN
                           SECT = .TRUE.
                           GO TO 6
                        END IF
5                    CONTINUE
6                    CONTINUE
                  END IF

*  If necessary, produce a suitable section.
                  IF ( SECT ) THEN
                     CALL NDF1_CUT( IACB, NDIM, LBND, UBND, IACBS,
     :                              STATUS )

*  Annul the original ACB entry and issue an identifier for the new one
*  describing the section.
                     CALL NDF1_ANL( IACB, STATUS )
                     CALL NDF1_EXPID( IACBS, NDFS( INDF ), STATUS )
                  END IF
               END IF
            END IF

*  Quit considering NDFs if an error occurs.
            IF ( STATUS .NE. SAI__OK ) GO TO 8
7        CONTINUE
8        CONTINUE
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_MBNDP', STATUS )

      END
