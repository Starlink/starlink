      SUBROUTINE NDF1_ADUMP( IAX, IACB, STATUS )
*+
*  Name:
*     NDF1_ADUMP

*  Purpose:
*     Unmap an axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADUMP( IAX, IACB, STATUS )

*  Description:
*     The routine unmaps an NDF axis data array which has previously
*     been mapped for access with the NDF1_ADMAP routine. An error will
*     be reported if the specified axis array is not currently mapped
*     for access. The NDF is identified by its index in the ACB.

*  Arguments:
*     IAX = INTEGER (Given)
*        Number of the axis whose data array is to be unmapped.
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine will attempt to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Check that the specified axis data array is mapped for access.
*     Report an error if it is not.
*     -  Unmap the axis data array by annulling its ARY_ system
*     identifier in the ACB (if a temporary array has been mapped, then
*     it will be deleted at this point).
*     -  If successful, then note the array is no longer mapped and
*     reset the mapping pointer in the ACB to zero.
*     -  Decrement the DCB mapping counts.
*     -  Restore the error context.

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
*     8-OCT-1990 (RFWS):
*        Original version.
*     10-OCT-1990 (RFWS):
*        Added clearing of the ACB mapping type entry.
*     19-OCT-1990 (RFWS):
*        Removed incorrect status check on entry.
*     30-OCT-1990 (RFWS):
*        Removed clearing of the ACB mapping type entry after
*        re-consideration.
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
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_NADMP( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings to each axis data array.
*        DCB_NMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Total number of current mappings to each data object.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ADMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether NDF axis data arrays are currently mapped for
*           access.
*        ACB_ADMID( NDF__MXDIM, NDF__MXACB ) = INTEGER (Read and Write)
*           ARY_ system identifier for axis data array mappings.
*        ACB_ADMPT( NDF__MXDIM, NDF__MXACB ) = INTEGER (Write)
*           Pointers to mapped axis data arrays.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IAX
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER TSTAT              ! Temporary status variable

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Obtain an index to the data object entry in the DCB.
      STATUS = SAI__OK
      IDCB = ACB_IDCB( IACB )

*  Check that the specified axis data array is mapped for access. Report
*  an error if it is not.
      IF ( .NOT. ACB_ADMAP( IAX, IACB ) ) THEN
         STATUS = NDF__NTMAP
         CALL MSG_SETI( 'AXIS', IAX )
         CALL NDF1_AMSG( 'NDF', IACB )
         CALL ERR_REP( 'NDF1_ADUMP_MAP',
     :                 'The centre array for axis ^AXIS of the NDF ' //
     :                 'structure ^NDF is not mapped for access ' //
     :                 'through the specified identifier (possible ' //
     :                 'programming error).', STATUS )

*  Unmap the axis data array by annulling its ARY_ system identifier in
*  the ACB (if a temporary array has been mapped, then it will be
*  deleted at this point).
      ELSE
         CALL ARY_ANNUL( ACB_ADMID( IAX, IACB ), STATUS )

*  If successful, then note the array is no longer mapped and reset the
*  mapping pointer in the ACB to zero.
         IF ( STATUS .EQ. SAI__OK ) THEN
            ACB_ADMAP( IAX, IACB ) = .FALSE.
            ACB_ADMPT( IAX, IACB ) = 0

*  Decrement the DCB mapping counts.
            DCB_NADMP( IAX, IDCB ) = DCB_NADMP( IAX, IDCB ) - 1
            DCB_NMAP( IDCB ) =   DCB_NMAP( IDCB ) - 1
         END IF
      END IF

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Call the error tracing routine if appropriate.
         ELSE
            CALL NDF1_TRACE( 'NDF1_ADUMP', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
