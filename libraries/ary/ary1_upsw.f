      SUBROUTINE ARY1_UPSW( IACB, TYPE, BAD, COPY, DATLOC, MLOC, PNTR,
     :                      DCE, STATUS )
*+
*  Name:
*     ARY1_UPSW

*  Purpose:
*     Unmap simple array component mapped for WRITE (or UPDATE) access.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_UPSW( IACB, TYPE, BAD, COPY, DATLOC, MLOC, PNTR, DCE,
*     STATUS )

*  Description:
*     The routine unmaps a component of a simple array which has
*     previously been mapped for WRITE (or UPDATE) access. This causes
*     the mapped data lying within the mapping transfer region (if it
*     exists) to be written back into the actual data object and
*     associated memory locations to be released.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the array entry in the ACB.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type used for mapped access; a primitive numeric HDS
*        type string (case insensitive).
*     BAD = LOGICAL (Given)
*        Whether to check for "bad" values when performing data type
*        conversion on data lying within the mapping transfer region.
*     COPY = LOGICAL (Given)
*        Whether mapped access is via a "copy" of the data; this
*        indicates whether or not the locator MLOC is associated with a
*        temporary object.
*     DATLOC = CHARACTER * ( * ) (Given)
*        HDS locator to the actual data object component into which
*        data should be written back if this is necessary.
*     MLOC = CHARACTER * ( * ) (Given and Returned)
*        Locator to the HDS object mapped to provide memory locations
*        for the mapped data. This locator will be annulled and reset
*        to ARY__NOLOC by this routine. If it is associated with a
*        temporary object, then this will be erased.
*     PNTR = INTEGER (Given and Returned)
*        Pointer to the mapped data; it is reset to zero by this
*        routine.
*     DCE = LOGICAL (Returned)
*        Whether any data conversion errors occurred.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no additional error report will be made if it
*     subsequently fails under these circumstances.
*     -  No entries in the ACB or MCB are updated by this routine.

*  Algorithm:
*     -  Save the error context on entry.
*     -  If mapped access is via a "copy" and a mapping transfer region
*     exists, then write the data lying within this region back into the
*     actual data object.
*     -  Annul the temporary object containing the copy (causing it to
*     be erased).
*     -  If mapped access is direct via HDS, then annul the associated
*     locator (causing the data to be unmapped).
*     -  Reset the pointer to the mapped data to zero.
*     -  Restore the error context.

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
*     12-JUL-1989 (RFWS):
*        Original version.
*     7-SEP-1989 (RFWS):
*        Changed to use symbolic constant to assign a null value to
*        locators. Also corrected minor errors in the comments.
*     22-MAR-1990 (RFWS):
*        Fixed dimensionality mis-match problem in call to ARY1_PTN.
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
*        DCB_LBND( ARY_MXDIM, ARY__MXDCB ) = INTEGER (Read)
*           Lower bounds of data object.
*        DCB_NDIM( ARY_MXDCB ) = INTEGER (Read)
*           Number of data object dimensions.
*        DCB_TYP( ARY__MXDCB ) = CHARACTER * ( ARY__SZTYP ) (Read)
*           Numeric type of data object.
*        DCB_UBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read)
*           Upper bounds of data object.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY_MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Read)
*           Index to mapping entry in the MCB.

      INCLUDE 'ARY_MCB'          ! ARY_ Mapping Control Block
*        MCB_LMRB( ARY__MXDIM, ARY__MXMCB ) = INTEGER (Read)
*           Lower mapping region bounds.
*        MCB_LMTR( ARY__MXDIM, ARY__MXMCB ) = INTEGER (Read)
*           Lower bounds of mapping transfer region.
*        MCB_MTREX( ARY__MXMCB ) = LOGICAL (Read)
*           Whether a mapping transfer region exists.
*        MCB_UMRB( ARY__MXDIM, ARY__MXMCB ) = INTEGER (Read)
*           Upper mapping region biunds.
*        MCB_UMTR( ARY__MXDIM, ARY__MXMCB ) = INTEGER (Read)
*           Upper bounds of mapping transfer region.

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) TYPE
      LOGICAL BAD
      LOGICAL COPY
      CHARACTER * ( * ) DATLOC

*  Arguments Given and Returned:
      CHARACTER * ( * ) MLOC
      INTEGER PNTR

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IMCB               ! Index to mapping entry in the DCB
      INTEGER TSTAT              ! Temporary status value

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  If mapped access is via a "copy", then obtain the index to the
*  mapping entry in the MCB.
      STATUS = SAI__OK
      IF ( COPY ) THEN
         IMCB = ACB_IMCB( IACB )

*  If a mapping transfer region exists, then write back the data lying
*  inside this region.
         IF ( MCB_MTREX( IMCB ) ) THEN
            IDCB = ACB_IDCB( IACB )
            CALL ARY1_PTN( BAD,
     :                     MAX( ACB_NDIM( IACB ), DCB_NDIM( IDCB ) ),
     :                     MCB_LMRB( 1, IMCB ), MCB_UMRB( 1, IMCB ),
     :                     TYPE, PNTR,
     :                     MCB_LMTR( 1, IMCB ), MCB_UMTR( 1, IMCB ),
     :                     DCB_LBND( 1, IDCB ), DCB_UBND( 1, IDCB ),
     :                     DCB_TYP( IDCB ), DATLOC, DCE, STATUS )
         END IF

*  Annul the temporary object holding the copy (causing it to be
*  erased).
         CALL ARY1_ANTMP( MLOC, STATUS )

*  If mapped access is direct via HDS, then annul the associated
*  locator (causing the data to be unmapped).
      ELSE
         CALL DAT_ANNUL( MLOC, STATUS )
         MLOC = ARY__NOLOC
      END IF

*  Reset the mapped data pointer to zero.
      PNTR = 0

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Call error tracing routine if appropriate.
         ELSE
            CALL ARY1_TRACE( 'ARY1_UPSW', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
