      SUBROUTINE ARY1_CRNBA( IDCB, IACB, STATUS )
*+
*  Name:
*     ARY1_CRNBA

*  Purpose:
*     Create a new ACB base array entry from a DCB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CRNBA( IDCB, IACB, STATUS )

*  Description:
*     The routine creates a new ACB entry describing a base array which
*     refers to the data object whose DCB entry is supplied. The new
*     array acquires its dimensionality, bounds and access control
*     flags directly from the data object and is given a data transfer
*     window of "infinite" extent. The bad pixel flag is derived from
*     the data object's bad pixel flag and state. The reference count
*     for the DCB entry is incremented by one.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the data object entry in the DCB to which the ACB
*        entry is to refer.
*     IACB = INTEGER (Returned)
*        Index to the new ACB entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If STATUS is set on entry, then the routine will return a
*     value of zero for the IACB argument, although no further
*     processing will occur.
*     -  A value of zero will also be returned for the IACB argument if
*     the routine fails for any reason.

*  Algorithm:
*     -  Set an initial value of zero for the IACB argument before
*     checking the inherited status.
*     -  Ensure that necessary information about the data object is
*     available in the DCB.
*     -  Obtain a free slot for the new entry in the ACB.
*     -  Initialise the ACB entry.
*     -  Enter dimensionality, access control flag, bad pixel flag,
*     dimensionality, bounds and accumulated pixel shift information
*     into the ACB entry, deriving it from the DCB.
*     -  If there was no error, then increment the DCB reference count
*     for the data object.
*     -  If there was an error, then free the allocated slot in the ACB
*     and set a value of zero for the IACB argument.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-AUG-1989 (RFWS):
*        Original version.
*     1-SEP-1989 (RFWS):
*        Installed access control flags.
*     1-MAR-1990 (RFWS):
*        Removed un-referenced include file.
*     5-MAR-1990 (RFWS):
*        Changed to ensure that the new ACB entry's bad pixel flag is
*        set to .TRUE. if the data object is in an undefined state.
*        This is to cater for the case where an array is imported into
*        the ARY_ system in an undefined state but with its bad pixel
*        flag (incorrectly) set to .FALSE.. If this value were to be
*        passed on to the ACB, an incorrect bad pixel flag would be
*        obtained if the array's values later become defined as a
*        result of writing new values to it.
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
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_BAD( ARY__MXDCB ) = LOGICAL (Read)
*           Data object bad pixel flag.
*        DCB_LBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read)
*           Array of data object lower bounds.
*        DCB_MOD( ARY__MXDCB ) = CHARACTER * ( ARY__SZMOD ) (Read)
*           Data object access mode.
*        DCB_NDIM( ARY__MXDCB ) = INTEGER (Read)
*           Number of data object dimensions.
*        DCB_REFCT( ARY__MXDCB ) = INTEGER (Read and Write)
*           Number of ACB entries which refer to the data object.
*        DCB_SFT( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read)
*           Accumulated pixel shifts for the data object.
*        DCB_STA( ARY__MXDIM ) = LOGICAL (Read)
*           Whether the data object's values are defined.
*        DCB_UBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read)
*           Array of data object upper bounds.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_ACC( ARY__MXACC, ARY__MXACB ) = LOGICAL (Write)
*           Array access control flags.
*        ACB_BAD( ARY__MXARY ) = LOGICAL (Write)
*           Bad pixel flag for the ACB entry.
*        ACB_CUT( ARY__MXACB ) = LOGICAL (Write)
*           Whether the array is a cut.
*        ACB_DTWEX( ARY__MXACB ) = LOGICAL (Write)
*           Whether a data transfer window exists.
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Write)
*           Index to the data object entry in the DCB.
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Write)
*           Index to mapping entry in the MCB.
*        ACB_LBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Write)
*           Lower bounds of array access region.
*        ACB_LDTW( ARY__MXDIM, ARY__MXACB ) = INTEGER (Write)
*           Lower bounds of data transfer window.
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Write)
*           Number of access dimensions for the array.
*        ACB_SFT( ARY__MXDIM, ARY__MXACB ) = INTEGER (Write)
*           Accumulated pixel shifts since the ACB entry was created.
*        ACB_UBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Write)
*           Upper bounds of array access region.
*        ACB_UDTW( ARY__MXDIM, ARY__MXACB ) = INTEGER (Write)
*           Upper bounds of data transfer window.

*  Arguments Given:
      INTEGER IDCB

*  Arguments Returned:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACC               ! Loop counter for access control flags

*.

*  Set an initial value of zero for the IACB argument.
      IACB = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that information about the data object bounds (and number of
*  dimensions), access mode, bad pixel flag and state are available in
*  the DCB.
      CALL ARY1_DBND( IDCB, STATUS )
      CALL ARY1_DMOD( IDCB, STATUS )
      CALL ARY1_DBAD( IDCB, STATUS )
      CALL ARY1_DSTA( IDCB, STATUS )

*  Obtain an index to a free slot in the ACB.
      CALL ARY1_FFS( ARY__ACB, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the ACB entry to point to the DCB entry and indicate that
*  this is a base array which is not mapped for access.
         ACB_IDCB( IACB ) = IDCB
         ACB_CUT( IACB ) = .FALSE.
         ACB_IMCB( IACB ) = 0

*  Initialise the access control flags according to whether the data
*  object can be modified or not.
         DO 1 IACC = 1, ARY__MXACC
            ACB_ACC( IACC, IACB ) = DCB_MOD( IDCB ) .EQ. 'UPDATE'
1        CONTINUE

*  Enter the bad pixel flag value from the DCB.
         ACB_BAD( IACB ) = DCB_BAD( IDCB ) .OR.
     :                     ( .NOT. DCB_STA( IDCB ) )

*  Enter the array bounds information from the DCB, padding with 1's if
*  necessary.
         ACB_NDIM( IACB ) = DCB_NDIM( IDCB )
         DO 2 I = 1, ACB_NDIM( IACB )
            ACB_LBND( I, IACB ) = DCB_LBND( I, IDCB )
            ACB_UBND( I, IACB ) = DCB_UBND( I, IDCB )
2        CONTINUE
         DO 3 I = ACB_NDIM( IACB ) + 1, ARY__MXDIM
            ACB_LBND( I, IACB ) = 1
            ACB_UBND( I, IACB ) = 1
3        CONTINUE

*  Enter the accumulated pixel shift information.
         DO 4 I = 1, ARY__MXDIM
            ACB_SFT( I, IACB ) = DCB_SFT( I, IDCB )
4        CONTINUE

*  Set the data transfer window for the new base array to the largest
*  region possible.
         ACB_DTWEX( IACB ) = .TRUE.
         DO 5 I = 1, ARY__MXDIM
            ACB_LDTW( I, IACB ) = NUM__MINI
            ACB_UDTW( I, IACB ) = NUM__MAXI
5        CONTINUE

*  Increment the DCB reference count.
         DCB_REFCT( IDCB ) = DCB_REFCT( IDCB ) + 1

*  If there was an error, then release the slot allocated in the ACB
*  and reset the IACB argument to zero.
      ELSE
         CALL ARY1_RLS( ARY__ACB, IACB, STATUS )
         IACB = 0
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CRNBA', STATUS )

      END
