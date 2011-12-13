      SUBROUTINE ARY1_SFT( NSHIFT, SHIFT, IACB, STATUS )
*+
*  Name:
*     ARY1_SFT

*  Purpose:
*     Apply pixel index shifts to an array entry in the ACB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_SFT( NSHIFT, SHIFT, IACB, STATUS )

*  Description:
*     The routine applies a set of pixel index shifts to an array
*     identified by its entry in the ACB. An integer shift is applied
*     to each dimension so that the array maintains the same data
*     content, although its bounds (and the indices of each pixel)
*     change by the amount of the shift applied to the corresponding
*     dimension. If the array is not a base array, then only one entry
*     in the ACB is affected. If it is a base array, then the shifts
*     are applied to the actual data object and all ACB entries which
*     refer to that object are also updated to reflect the change.

*  Arguments:
*     NSHIFT = INTEGER (Given)
*        Number of dimensions to which shifts are to be applied. If
*        more shifts are specified than there are dimensions in the
*        array, then the excess shifts are disregarded. If fewer shifts
*        are specified, then the extra dimensions are not shifted.
*     SHIFT( NSHIFT ) = INTEGER (Given)
*        The shifts to be applied to each dimension.
*     IACB = INTEGER (Given)
*        Index to the array entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Note that applying a shift to a base array affects the pixel
*     indices of all other direct references to the same base array,
*     but does not affect the pixel indices of cuts derived from it
*     (these remain unchanged as regards both data content and pixel
*     indices).
*     -  Applying a shift to a cut always affects that array only.

*  Algorithm:
*     -  If the array is a cut, then check it is not mapped for access
*     and report an error if it is.
*     -  Apply the pixel index shifts to the array bounds and to the
*     accumulated pixel shifts stored in the ACB.
*     -  If the array is a base array, then check that no other array
*     currently has it mapped for access and report an error if it does.
*     -  Apply the pixel index shifts to the data object.
*     -  Loop through all the active entries in the ACB.
*     -  Select those entries which refer to the data object being
*     shifted.
*     -  Apply the pixel index shifts to the array bounds and to the
*     accumulated pixel shifts stored in the ACB.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     8-AUG-1989 (RFWS):
*        Original version.
*     4-SEP-1989 (RFWS):
*        Fixed bug which was causing the bounds of all array entries in
*        the ACB associated with the data object to be shifted, rather
*        than simply those of base array entries.
*     5-MAR-1990 (RFWS):
*        Minor improvement to error message.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to the data object.
*        DCB_NREAD( ARY__MXDCB ) = INTEGER (Read)
*           Number of current read mappings associated with the data
*           object.
*        DCB_NWRIT( ARY__MXDCB ) = INTEGER (Read)
*           Number of current write mappings associated with the data
*           object.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_CUT( ARY__MXACB ) = LOGICAL (Read)
*           Whether the array is a cut.
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to the data object entry in the DCB.
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Read)
*           Index to mapping entry in the MCB.
*        ACB_LBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Lower array bounds.
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read)
*           Number of array dimensions.
*        ARY_SFT( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Accumulated pixel shifts since the ACB entry was created.
*        ACB_UBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Upper array bounds.

*  Arguments Given:
      INTEGER NSHIFT
      INTEGER SHIFT( NSHIFT )
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACBT              ! Index to ACB entry to test
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER NEXT               ! Next ACB entry

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the array is a cut, then check to see if it is mapped for access.
*  No pixel shifts can be applied if it is, so report an error.
      IF ( ACB_CUT( IACB ) ) THEN
         IF ( ACB_IMCB( IACB ) .NE. 0 ) THEN
            STATUS = ARY__ISMAP
            IDCB = ACB_IDCB( IACB )
            CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
            CALL ERR_REP( 'ARY1_SFT_MAP',
     :      'The array ^ARRAY is currently mapped for access ' //
     :      'through the specified identifier (possible programming ' //
     :      'error).', STATUS )

*  If it is not mapped, then apply the pixel index shifts to the array
*  bounds and to the accumulated pixel shifts stored in the ACB.
         ELSE
            DO 1 I = 1, MIN( ACB_NDIM( IACB ), NSHIFT )
               ACB_LBND( I, IACB ) = ACB_LBND( I, IACB ) + SHIFT( I )
               ACB_UBND( I, IACB ) = ACB_UBND( I, IACB ) + SHIFT( I )
               ACB_SFT( I, IACB ) = ACB_SFT( I, IACB ) + SHIFT( I )
1           CONTINUE
         END IF

*  If the array is a base array, then check to see if any mapped access
*  to it is currently in effect. If so, then pixel index shifts cannot
*  be applied, so report an error.
      ELSE
         IDCB = ACB_IDCB( IACB )
         IF ( ( DCB_NREAD( IDCB ) .NE. 0 ) .OR.
     :        ( DCB_NWRIT( IDCB ) .NE. 0 ) ) THEN
            STATUS = ARY__ISMAP
            CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
            CALL ERR_REP( 'ARY1_SFT_BMAP',
     :      'The base array ''^ARRAY'' is currently mapped for ' //
     :      'access, perhaps through another identifier (possible ' //
     :      'programming error).', STATUS )

*  Apply the pixel index shifts to the data object.
         ELSE
            CALL ARY1_DSFT( NSHIFT, SHIFT, IDCB, STATUS )

*  Loop through all the ACB entries.
            IACBT = 0
            NEXT = 0
2           CONTINUE             ! Start of 'DO WHILE' loop
            CALL ARY1_NXTSL( ARY__ACB, IACBT, NEXT, STATUS )
            IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NEXT .NE. 0 ) ) THEN

*  Select those entries which refer to the data object being shifted
*  and which describe a base array.
               IACBT = NEXT
               IF ( ( ACB_IDCB( IACBT ) .EQ. IDCB ) .AND.
     :              ( .NOT. ACB_CUT( IACBT ) ) ) THEN

*  Apply the pixel index shifts to the array bounds and to the
*  accumulated pixel shifts stored in the ACB.
                  DO 3 I = 1, MIN( ACB_NDIM( IACBT ), NSHIFT )
                     ACB_LBND( I, IACBT ) = ACB_LBND( I, IACBT ) +
     :                                      SHIFT( I )
                     ACB_UBND( I, IACBT ) = ACB_UBND( I, IACBT ) +
     :                                      SHIFT( I )
                     ACB_SFT( I, IACBT ) = ACB_SFT( I, IACBT ) +
     :                                     SHIFT( I )
3                 CONTINUE
               END IF
               GO TO 2
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_SFT', STATUS )

      END
