      SUBROUTINE ARY1_CLN( IACB1, IACB2, STATUS )
*+
*  Name:
*     ARY1_CLN

*  Purpose:
*     Clone an ACB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CLN( IACB1, IACB2, STATUS )

*  Description:
*     The routine "clones" an ACB entry, producing a new entry which is
*     a duplicate of the original (except that it is not mapped for
*     access if the original was). The data object reference count in
*     the DCB is incremented by one.

*  Arguments:
*     IACB1 = INTEGER (Given)
*        Index to the ACB entry to be cloned.
*     IACB2 = INTEGER (Returned)
*        Index to the new ACB entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If STATUS is set on entry, then a value of zero will be
*     returned for the IACB2 argument, although no further processing
*     will occur.
*     -  A value of zero will also be returned for the IACB2 argument if
*     the routine fails for any reason.

*  Algorithm:
*     -  Set an initial value of zero for the IACB2 argument before
*     checking the inherited status.
*     -  Obtain a free slot in the ACB. Reset the IACB2 argument to zero
*     if no slot could be found.
*     -  Transfer the attributes of the initial ACB entry to the new
*     one.
*     -  Mark the new entry as not being mapped for access.
*     -  Increment the data object reference count in the DCB by one.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     2-AUG-1989 (RFWS):
*        Original version.
*     1-SEP-1989 (RFWS):
*        Installed access control flags.
*     1-SEP-1989 (RFWS):
*        Corrected bug in transfer of array bounds information.
*     9-OCT-1989 (RFWS):
*        Changed to transfer access control flags without modification.
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

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_REFCT( ARY__MXDCB ) = INTEGER (Read and Write)
*           Number of ACB entries which refer to a data object.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_ACC( ARY__MXACC, ARY__MXACB ) = LOGICAL (Read and Write)
*           Array access control flags.
*        ACB_BAD( ARY__MXARY ) = LOGICAL (Read and Write)
*           Bad pixel flag for the ACB entry.
*        ACB_CUT( ARY__MXACB ) = LOGICAL (Read and Write)
*           Whether the array is a cut.
*        ACB_DTWEX( ARY__MXACB ) = LOGICAL (Read and Write)
*           Whether a data transfer window exists.
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read and Write)
*           Index to the data object entry in the DCB.
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Write)
*           Index to mapping entry in the MCB.
*        ACB_LBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Lower bounds of array access region.
*        ACB_LDTW( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Lower bounds of data transfer window.
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read and Write)
*           Number of access dimensions for the array.
*        ACB_SFT( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Accumulated pixel shifts since the ACB entry was created.
*        ACB_UBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Upper bounds of array access region.
*        ACB_UDTW( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Upper bounds of data transfer window.

*  Arguments Given:
      INTEGER IACB1

*  Arguments Returned:
      INTEGER IACB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACC               ! Loop counter for access control flags
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Set an initial value of zero for the IACB2 argument.
      IACB2 = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find a free slot in the ACB. Reset the IACB2 argument to zero if no
*  slot could be found.
      CALL ARY1_FFS( ARY__ACB, IACB2, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IACB2 = 0

*  Transfer the array attributes from the old ACB entry to the new one.
      ELSE

*  ...Whether the array is a cut.
         ACB_CUT( IACB2 ) = ACB_CUT( IACB1 )

*  ...Access control flags.
         DO 1 IACC = 1, ARY__MXACC
            ACB_ACC( IACC, IACB2 ) = ACB_ACC( IACC, IACB1 )
1        CONTINUE

*  ...Index to data object entry in the DCB.
         IDCB = ACB_IDCB( IACB1 )
         ACB_IDCB( IACB2 ) = IDCB

*  ...Number of dimensions.
         ACB_NDIM( IACB2 ) = ACB_NDIM( IACB1 )

*  ...Array bounds, data transfer window bounds and accumulated pixel
*  shifts.
         DO 2 I = 1, ARY__MXDIM
            ACB_LBND( I, IACB2 ) = ACB_LBND( I, IACB1 )
            ACB_UBND( I, IACB2 ) = ACB_UBND( I, IACB1 )
            ACB_LDTW( I, IACB2 ) = ACB_LDTW( I, IACB1 )
            ACB_UDTW( I, IACB2 ) = ACB_UDTW( I, IACB1 )
            ACB_SFT( I, IACB2 ) = ACB_SFT( I, IACB1 )
2        CONTINUE

*  ...Whether the data transfer window exists.
         ACB_DTWEX( IACB2 ) = ACB_DTWEX( IACB1 )

*  ...Bad pixel flag.
         ACB_BAD( IACB2 ) = ACB_BAD( IACB1 )

*  Mark the new ACB entry as not being mapped for access and increment
*  the data object reference count in the DCB.
         ACB_IMCB( IACB2 ) = 0
         DCB_REFCT( IDCB ) = DCB_REFCT( IDCB ) + 1
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CLN', STATUS )

      END
