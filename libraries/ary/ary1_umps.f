      SUBROUTINE ARY1_UMPS( IACB, STATUS )
*+
*  Name:
*     ARY1_UMPS

*  Purpose:
*     Unmap a simple array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_UMPS( IACB, STATUS )

*  Description:
*     The routine unmaps a simple array which has previously been
*     mapped for access and updates the associated data object
*     (including its mapping reference count and state if appropriate).
*     If the routine completes successfully, then the MCB slot used for
*     the mapping is released and the ACB entry is marked as no longer
*     being mapped.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the array entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Prior Requirements:
*     -  This routine should only be called for an ACB entry which has
*     previously been used to obtain mapped access to an array and has
*     not since been unmapped.

*  Side Effects:
*     -  Write-back of data by this routine may affect the data values
*     which are accessed via other ACB entries, where the associated
*     arrays refer to the same data object and overlap spatially.
*     -  If data conversion errors occur during write-back of data,
*     then the data object bad pixel flag may be updated (if a mapping
*     transfer region exists). This may affect other ACB entries where
*     the associated arrays refer to the same data object and overlap
*     with the affected data region.

*  Notes:
*     -  This routine may also be used for unmapping primitive arrays.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Obtain indices to the associated DCB and MCB entries.
*     -  If the MCB index is not positive, then the array is not mapped,
*     so report an error.
*     -  If the array was mapped for READ access, then unmap the
*     non-imaginary component and (if mapped) the imaginary component.
*     In this case no data are written back into the data object.
*     Decrement the count of read mappings.
*     -  If the array was mapped for WRITE (or UPDATE) access, then
*     unmap the non-imaginary component, causing any data within the
*     mapping transfer region (if it exists) to be written back into the
*     corresponding component of the data object.
*     -  Similarly unmap the imaginary component if it exists and was
*     mapped.
*     -  If an imaginary component was mapped, but no such data object
*     component exists, then unmap the imaginary component as if it had
*     been mapped for READ access, so that no write-back occurs.
*     -  If an imaginary data component exists but was not mapped and a
*     mapping transfer region exists, then map the imaginary component
*     for WRITE access, initialising it to zero. Then un-map it so that
*     zeros are written back into this component of the data object.
*     -  Set the data object state to "defined". If the array was
*     mapped for WRITE access, then decrement the count of write
*     mappings.  If mapped for UPDATE access, then decrement both the
*     read and write mapping counts.
*     -  Determine whether any data conversion errors occurred while
*     unmapping data which was mapped for WRITE or UPDATE access.
*     Deduce whether "bad" values may have been written back into the
*     data object and update the bad pixel flag for the ACB entry
*     appropriately.
*     -  If the active mapping mode entry in the MCB was not recognised,
*     then report an error.
*     -  If the unmapping operation was successful, then release the MCB
*     slot and reset the ACB mapping entry index to zero to indicate
*     that the array is no longer mapped.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JUL-1989 (RFWS):
*        Original version.
*     1-AUG-1989 (RFWS):
*        Added code to set the data object state to "defined" following
*        a WRITE mapping and to decrement the appropriate mapping
*        counts.
*     7-SEP-1989 (RFWS):
*        Fixed bug in call to ARY1_UPSW.
*     11-SEP-1989 (RFWS):
*        Fixed bug which attempted to set part of the imaginary
*        component to zero even if the data object didn't have such a
*        component.
*     16-JAN-1990 (RFWS):
*        Changed to ensure that the data object state is set to
*        "defined" when unmapping in both WRITE and UPDATE mode.
*     23-FEB-1990 (RFWS):
*        Fixed bug whereby the MCB entry for the mapping was not being
*        released. Also added a check that the array is actually mapped
*        (moved into this routine from ARY1_UMP).
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
*        DCB_CPX( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the data object is complex.
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to non-imaginary data component.
*        DCB_ILOC( ARY__MXDCB ) = LOGICAL (Read)
*           Locator to imaginary data component.
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_KSTA( ARY__MXDCB ) = LOGICAL (Write)
*           Whether state information is available in the DCB.
*        DCB_NREAD( ARY__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings which read from the data object.
*        DCB_NWRIT( ARY__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings which write to the data object.
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Write)
*           Data object state (defined/undefined).
*        DCB_TYP( ARY__MXDCB ) = CHARACTER * ( ARY__SZTYP ) (Read)
*           Numeric type of the data object.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Read and Write)
*           Index to mapping entry in the MCB.

      INCLUDE 'ARY_MCB'          ! ARY_ Mapping Control Block
*        MCB_AMM( ARY__MXMCB ) = CHARACTER * ( ARY__SZAMM ) (Read)
*           Active mapping mode.
*        MCB_BAD( ARY__MXMCB ) = LOGICAL (Read)
*           Whether there may be "bad" values present in the mapped
*           data lying within the mapping transfer region (if it
*           exists).
*        MCB_CPX( ARY__MXMCB ) = LOGICAL (Read)
*           Whether mapped access was given to complex data.
*        MCB_DCOPY( ARY__MXMCB ) = LOGICAL (Read)
*           Whether mapped access to the non-imaginary array component
*           is via a "copy" of the data.
*        MCB_DLOC( ARY__MXMCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator to the object mapped to provide memory locations for
*           access to the non-imaginary array component.
*        MCB_DPNTR( ARY__MXMCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Pointer to the mapped non-imaginary array component.
*        MCB_ICOPY( ARY__MXMCB ) = LOGICAL (Read)
*           Whether mapped access to the imaginary array component is
*           via a "copy" of the data.
*        MCB_ILOC( ARY__MXMCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to the object mapped to provide memory locations for
*           access to the imaginary array component.
*        MCB_IPNTR( ARY__MXMCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Pointer to the mapped imaginary array component.
*        MCB_MTREX( ARY__MXMCB ) = LOGICAL (Read)
*           Whether a mapping transfer region exists.
*        MCB_TYP( ARY__MXMCB ) = CHARACTER * ( ARY__SZTYP ) (Read)
*           Numeric data type used for accessing the mapped data.

*  Arguments Given:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) MLOC ! Locator to mapped imaginary data
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IMCB               ! Index to the mapping entry in the MCB
      INTEGER PNTR               ! Pointer to mapped imaginary data
      INTEGER TSTAT              ! Temporary status value
      LOGICAL BAD                ! Possible "bad" values written back?
      LOGICAL COPY               ! Whether mapping involves a "copy"
      LOGICAL DCE                ! Non-imaginary data conversion error?
      LOGICAL IDCE               ! Imaginary data conversion error?

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK
       
*  Obtain indices to the associated entries in the MCB and DCB.
      STATUS = SAI__OK
      IMCB = ACB_IMCB( IACB )
      IDCB = ACB_IDCB( IACB )

*  If the MCB index is not positive, then the array is not mapped, so
*  report an error.
      IF ( IMCB .LE. 0 ) THEN
         STATUS = ARY__NTMAP
         CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
         CALL ERR_REP( 'ARY1_UMPS_NTMAP',
     :   'The array ^ARRAY is not currently mapped for access ' //
     :   'through the specified identifier (possible programming ' //
     :   'error).', STATUS )

*  If the array is mapped for READ access, then unmap the non-imaginary
*  component. No values will be written back to the data object.
      ELSE
         IF ( MCB_AMM( IMCB ) .EQ. 'READ' ) THEN
            CALL ARY1_UPSR( MCB_DCOPY( IMCB ), MCB_DLOC( IMCB ),
     :                      STATUS )

*  Similarly unmap the imaginary component if this was mapped.
            IF ( MCB_CPX( IMCB ) ) THEN
               CALL ARY1_UPSR( MCB_ICOPY( IMCB ), MCB_ILOC( IMCB ),
     :                         STATUS )
            END IF

*  Decrement the data object mapping count.
            IF ( STATUS .EQ. SAI__OK ) THEN
               DCB_NREAD( IDCB ) = DCB_NREAD( IDCB ) - 1
            END IF

*  If the array is mapped for WRITE or UPDATE access, then unmap the
*  non-imaginary component. If there is a mapping transfer region, then
*  the values lying within it will be written back into the data
*  object.
         ELSE IF ( ( MCB_AMM( IMCB ) .EQ. 'WRITE' ) .OR.
     :             ( MCB_AMM( IMCB ) .EQ. 'UPDATE' ) ) THEN
            DCE = .FALSE.
            CALL ARY1_UPSW( IACB, MCB_TYP( IMCB ), MCB_BAD( IMCB ),
     :                      MCB_DCOPY( IMCB ), DCB_DLOC( IDCB ),
     :                      MCB_DLOC( IMCB ), MCB_DPNTR( IMCB ), DCE,
     :                      STATUS )

*  If access was given to a complex component and there is a complex
*  component in the data object, then unmap this component in the same
*  way, causing values within the mapping transfer region to be written
*  back.
            IDCE = .FALSE.
            IF ( MCB_CPX( IMCB ) ) THEN
               IF ( DCB_CPX( IDCB ) ) THEN
                  CALL ARY1_UPSW( IACB, MCB_TYP( IMCB ),
     :                            MCB_BAD( IMCB ), MCB_ICOPY( IMCB ),
     :                            DCB_ILOC( IDCB ), MCB_ILOC( IMCB ),
     :                            MCB_IPNTR( IMCB ), IDCE, STATUS )

*  If access to a complex component was given, but there is no such
*  component in the data object, then unmap it as if it were mapped for
*  READ access. No data values are then written back and any associated
*  temporary object is erased.
               ELSE
                  CALL ARY1_UPSR( MCB_ICOPY( IMCB ), MCB_ILOC( IMCB ),
     :                            STATUS )
               END IF

*  If access was only obtained to the non-imaginary component, but an
*  imaginary component exists in the data object, then the data type
*  conversion rules require that values of the imaginary component
*  lying within the mapping transfer region should be set to zero in
*  the data object. Only do this if a mapping transfer region exists.
            ELSE
               IF( DCB_CPX( IDCB ) .AND. MCB_MTREX( IMCB ) ) THEN

*  Map the imaginary component for WRITE access, specifying
*  initialisation to zero, then unmap it so that the zeros are written
*  back into the data object. Choose the data type so as to avoid any
*  type conversion overheads.
                  CALL ARY1_MPSW( IACB, DCB_ILOC( IDCB ),
     :                            DCB_TYP( IDCB ), 'ZERO', MLOC, COPY,
     :                            PNTR, STATUS )
                  CALL ARY1_UPSW( IACB, DCB_TYP( IDCB ), .FALSE.,
     :                            COPY, DCB_ILOC( IDCB ), MLOC, PNTR,
     :                            IDCE, STATUS )
               END IF
            END IF

*  Set the DCB state entry to "defined".
            IF ( STATUS .EQ. SAI__OK ) THEN
               DCB_STA( IDCB ) = .TRUE.
               DCB_KSTA( IDCB ) = .TRUE.

*  If the array was mapped for WRITE access, then decrement the count
*  of current mappings which write to the data object.
               IF ( MCB_AMM( IMCB ) .EQ. 'WRITE' ) THEN
                  DCB_NWRIT( IDCB ) = DCB_NWRIT( IDCB ) - 1

*  In UPDATE mode, decrement both the read and write mapping counts.
               ELSE IF ( MCB_AMM( IMCB ) .EQ. 'UPDATE' ) THEN
                  DCB_NREAD( IDCB ) = DCB_NREAD( IDCB ) - 1
                  DCB_NWRIT( IDCB ) = DCB_NWRIT( IDCB ) - 1
               END IF

*  Note if any data conversion errors occurred while unmapping data
*  mapped with WRITE or UPDATE access. Determine whether any "bad"
*  values may have been written back into the data object and update
*  the bad pixel flag accordingly.
               DCE = DCE .OR. IDCE
               BAD = MCB_BAD( IMCB ) .OR. DCE
               CALL ARY1_SBD( BAD, IACB, STATUS )
            END IF

*  If the active mapping mode entry in the MCB is not valid, then report
*  an error.
         ELSE
            STATUS = ARY__FATIN
            CALL MSG_SETC( 'BADMAP', MCB_AMM( IMCB ) )
            CALL ERR_REP( 'ARY1_UMPS_BMAP',
     :      'Invalid active mapping mode ''^BADMAP'' found in ' //
     :      'Mapping Control Block (internal programming error).',
     :      STATUS )
         END IF
      END IF
       
*  If the unmapping operation was successful, then release the MCB slot
*  and reset the ACB mapping entry index to zero, indicating that the
*  array is no longer mapped.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ARY1_RLS( ARY__MCB, IMCB, STATUS )
         ACB_IMCB( IACB ) = 0
      END IF

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Call error tracing routine if appropriate.
         ELSE
            CALL ARY1_TRACE( 'ARY1_UMPS', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
