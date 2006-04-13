      SUBROUTINE ARY1_CUT( IACB1, NDIM, LBND, UBND, IACB2, STATUS )
*+
*  Name:
*     ARY1_CUT

*  Purpose:
*     Obtain a cut from an existing array with an entry in the ACB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CUT( IACB1, NDIM, LBND, UBND, IACB2, STATUS )

*  Description:
*     The routine produces a new array entry in the ACB representing a
*     "cut" from an existing array (which may itself be a cut). The
*     bounds and dimensionality of the new array are specified in the
*     call to this routine and the resulting array has access to a
*     subet (although possibly a null subset) of the data accessible to
*     the initial array from which it is derived. This accessible
*     region is determined by the new array's data transfer window,
*     which is formed from the intersection between the initial array's
*     data transfer window, the bounds of the initial array and the
*     bounds of the new array.

*  Arguments:
*     IACB1 = INTEGER (Given)
*        Index to the ACB entry for an existing array.
*     NDIM = INTEGER (Given)
*        Number of dimensions for the new cut.
*     LBND( NDIM ) = INTEGER (Given)
*        Lower dimension bounds for the cut.
*     UBND( NDIM ) = INTEGER (Given)
*        Upper dimension bounds for the cut.
*     IACB2 = INTEGER (Returned)
*        Index to the ACB entry for the new cut.
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
*     -  Obtain an index to a free slot in the ACB.
*     -  Transfer those ACB entries which are to be propagated from the
*     old array to the new one, marking the new array as a cut and as
*     not being mapped for access.
*     -  Store the number of dimensions and the dimension bounds for the
*     new array in the ACB.
*     -  Transfer the accumulated pixel shifts for the old array to the
*     new one.
*     -  If the old array did not have a data transfer window, then
*     neither does the new one. Note this fact in the ACB.
*     -  Otherwise, find the intersection region between the bounds of
*     the old array and those of the new one.
*     -  If this region does not exist, then there is no data transfer
*     region for the new array. Note this in the ACB.
*     -  Convert the intersection region (if it exists) into the
*     reference frame pixel index system and find the intersection with
*     the data transfer window of the old array.  This gives the new
*     data transfer window. Note its bounds and whether it exists in
*     the ACB.
*     -  If there was no error, then increment the reference count for
*     the data object in the DCB.
*     -  If there was an error, then release the new ACB slot and reset
*     the returned ACB index value to zero.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUL-1989 (RFWS):
*        Original version.
*     1-AUG-1989 (RFWS):
*        Partial re-write to take account of the bounds of the initial
*        array when calculating the new data transfer window.
*     1-SEP-1989 (RFWS):
*        Installed access control flags.
*     1-SEP-1989 (RFWS):
*        Fixed bug due to typo in the calculation of the data transfer
*        window bounds.
*     9-OCT-1989 (RFWS):
*        Changed to propagate the access control flags without
*        modification.
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
*           Number of ACB entries which refer to each DCB entry.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_ACC( ARY__MXACC, ARY__MXACB ) = LOGICAL (Read and Write)
*           Array access control flags.
*        ACB_BAD( ARY__MXACB ) = LOGICAL (Read and Write)
*           Whether there may be "bad" values in the data transfer
*           window (if it exists).
*        ACB_CUT( ARY__MXACB ) = LOGICAL (Write)
*           Whether the array is a cut.
*        ACB_DTWEX( ARY__MXACB ) = LOGICAL (Read and Write)
*           Whether a data transfer window exists.
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read and Write)
*           Index to associated DCB entry.
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Read and Write)
*           Index to associated MCB entry.
*        ACB_LBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Write)
*           Lower array bounds.
*        ACB_LDTW( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Lower bounds of data transfer window (if it exists).
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Write)
*           Number of array dimensions for access.
*        ACB_SFT( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Accumulated pixel shifts since the associated data object
*           was imported.
*        ACB_UBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Write)
*           Upper array bounds.
*        ACB_UDTW( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Upper bounds of data transfer window (if it exists).

*  Arguments Given:
      INTEGER IACB1
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )

*  Arguments Returned:
      INTEGER IACB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACC               ! Loop counter for access control flags
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER LX( ARY__MXDIM )   ! Lower intersection region bounds
      INTEGER UX( ARY__MXDIM )   ! Upper intersection region bounds
      LOGICAL EXIST              ! Whether intersection region exists

*.

*  Set an initial value for the IACB2 argument.
      IACB2 = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index to a free slot for the new array in the ACB.
      CALL ARY1_FFS( ARY__ACB, IACB2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Mark the new entry as a cut.
         ACB_CUT( IACB2 ) = .TRUE.

*  Transfer the access control flags.
         DO 1 IACC = 1, ARY__MXACC
            ACB_ACC( IACC, IACB2 ) = ACB_ACC( IACC, IACB1 )
1        CONTINUE

*  Transfer the index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB1 )
         ACB_IDCB( IACB2 ) = IDCB

*  The new array is not mapped for access.
         ACB_IMCB( IACB2 ) = 0

*  Transfer the bad pixel flag.
         ACB_BAD( IACB2 ) = ACB_BAD( IACB1 )

*  Store the number of dimensions and the dimension bounds for the new
*  array, padding the bounds information with 1's if necessary.
         ACB_NDIM( IACB2 ) = NDIM
         DO 2 I = 1, NDIM
            ACB_LBND( I, IACB2 ) = LBND( I )
            ACB_UBND( I, IACB2 ) = UBND( I )
2        CONTINUE
         DO 3 I = NDIM + 1, ARY__MXDIM
            ACB_LBND( I, IACB2 ) = 1
            ACB_UBND( I, IACB2 ) = 1
3        CONTINUE

*  Transfer the accumulated pixel shifts for the initial array to the
*  new array.
         DO 4 I = 1, ARY__MXDIM
            ACB_SFT( I, IACB2 ) = ACB_SFT( I, IACB1 )
4        CONTINUE

*  If the first array did not have a data transfer window, then neither
*  does the new one.
         IF ( .NOT. ACB_DTWEX( IACB1 ) ) THEN
            ACB_DTWEX( IACB2 ) = .FALSE.

*  Otherwise. find the intersection of the new array bounds with the old
*  ones, which defines the maximum region of data to which the new array
*  has access (in the current pixel index system).
         ELSE
            CALL ARY1_XSBND( ARY__MXDIM,
     :                       ACB_LBND( 1, IACB1 ), ACB_UBND(1, IACB1 ),
     :                       ARY__MXDIM,
     :                       ACB_LBND( 1, IACB2 ), ACB_UBND( 1, IACB2 ),
     :                       ARY__MXDIM, LX, UX, EXIST, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If there is no intersection region, then the new array has no data
*  transfer window.
               IF ( .NOT. EXIST ) THEN
                  ACB_DTWEX( IACB2 ) = .FALSE.

*  Otherwise, convert the bounds of the intersection region into the
*  reference frame pixel index system by subtracting the accumulated
*  ACB pixel shifts.
               ELSE
                  DO 5 I = 1, ARY__MXDIM
                     LX( I ) = LX( I ) - ACB_SFT( I, IACB2 )
                     UX( I ) = UX( I ) - ACB_SFT( I, IACB2 )
5                 CONTINUE

*  Find the intersection of this region with the data transfer window of
*  the original array. This defines the new data transfer window. Note
*  its bounds and whether it exists in the ACB entry for the new array.
                  CALL ARY1_XSBND( ARY__MXDIM,
     :                             ACB_LDTW( 1, IACB1 ),
     :                             ACB_UDTW( 1, IACB1 ),
     :                             ARY__MXDIM, LX, UX,
     :                             ARY__MXDIM,
     :                             ACB_LDTW( 1, IACB2 ),
     :                             ACB_UDTW( 1, IACB2 ),
     :                             ACB_DTWEX( IACB2 ), STATUS )
               END IF
            END IF
         END IF
      END IF
       
*  If no error occurred, then increment the data object reference count.
      IF ( STATUS .EQ. SAI__OK ) THEN
         DCB_REFCT( IDCB ) = DCB_REFCT( IDCB ) + 1

*  If there was an error, then release the ACB slot and reset its index
*  to zero.
      ELSE
         CALL ARY1_RLS( ARY__ACB, IACB2, STATUS )
         IACB2 = 0
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CUT', STATUS )

      END
