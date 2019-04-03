      SUBROUTINE NDF1_DUMP( IACB, STATUS )
*+
*  Name:
*     NDF1_DUMP

*  Purpose:
*     Unmap the data array component for an ACB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DUMP( IACB, STATUS )

*  Description:
*     The routine unmaps the data array component for an ACB entry which
*     has previously been mapped for access. An error is reported if it
*     has not previously been mapped.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the ACB entry whose data array is to be unmapped.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Check that the data array is mapped and report an error if it
*     is not.
*     -  See if the temporary mapped array identifier in the ACB is
*     valid.
*     -  If so, then a temporary array was mapped for read access and
*     can simply be annulled.
*     -  If the bad pixel flag for the mapped values has been modified,
*     then set the new value for the data array before unmapping it.
*     -  Unmap the data array.
*     -  If successful, then note the array is not mapped and decrement
*     the DCB mapping counts for the NDF.
*     -  Clear the pointers to the mapped values.
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
*     2-OCT-1989 (RFWS):
*        Original version.
*     15-DEC-1989 (RFWS):
*        Installed code to decrement the DCB mapping counts.
*     21-MAR-1990 (RFWS):
*        Changed to take account of the bad pixel flag for the mapped
*        values.
*     23-MAR-1990 (RFWS):
*        Clear the ACB pointers tp the mapped values.
*     3-APR-1990 (RFWS):
*        Added code to handle temporary mapped arrays.
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

      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_NDMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings for the NDF's data array.
*        DCB_NMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Total number of current mappings for the NDF.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_DMAP( NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether the NDF's data array is mapped for access.
*        ACB_DMBAD( NDF__MXACB ) = LOGICAL (Read)
*           Bad pixel flag for the mapped data values.
*        ACB_DMBMD( NDF__MXACB ) = LOGICAL (Read)
*           Whether the ACB_DMBAD value has been modified.
*        ACB_DMDPT( NDF__MXACB ) = INTEGER (Write)
*           Pointer to the mapped non-imaginary data values.
*        ACB_DMIPT( NDF__MXACB ) = INTEGER (Write)
*           Pointer to the mapped imaginary data values.
*        ACB_DMTID( NDF__MXACB ) = INTEGER (Read and Write)
*           ARY_ system identifier for temporary array used when mapping
*           the NDF's data component.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER TSTAT              ! Temporary status variable
      LOGICAL TEMP               ! Whether a temporary array is mapped

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Obtain an index to the data object entry in the DCB.
      STATUS = SAI__OK
      IDCB = ACB_IDCB( IACB )

*  Check that the data component is mapped for access and report an
*  error if it is not.
      IF ( .NOT. ACB_DMAP( IACB ) ) THEN
         STATUS = NDF__NTMAP
         CALL NDF1_AMSG( 'NDF', IACB )
         CALL ERR_REP( 'NDF1_DUMP_MAP',
     :   'The data component in the NDF structure ^NDF is not ' //
     :   'mapped for access through the specified identifier ' //
     :   '(possible programming error).', STATUS )

*  See if the temporary mapped array identifier in the ACB is valid.
      ELSE
         CALL ARY_VALID( ACB_DMTID( IACB ), TEMP, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then a temporary array was mapped for read access and can
*  simply be annulled.
            IF ( TEMP ) THEN
               CALL ARY_ANNUL( ACB_DMTID( IACB ), STATUS )

*  If the bad pixel flag value for the mapped values has been modified,
*  then set the new value for the data array before unmapping it.
            ELSE
               IF ( ACB_DMBMD( IACB ) ) THEN
                  CALL ARY_SBAD( ACB_DMBAD( IACB ), ACB_DID( IACB ),
     :                           STATUS )
               END IF

*  Unmap the data array.
               CALL ARY_UNMAP( ACB_DID( IACB ), STATUS )
            END IF
         END IF

*  If successful, then note the array is not mapped and decrement the
*  DCB mapping counts for the NDF.
         IF ( STATUS .EQ. SAI__OK ) THEN
            ACB_DMAP( IACB ) = .FALSE.
            DCB_NDMAP( IDCB ) = DCB_NDMAP( IDCB ) - 1
            DCB_NMAP( IDCB ) = DCB_NMAP( IDCB ) - 1

*  Clear the pointers to the mapped values.
            ACB_DMDPT( IACB ) = 0
            ACB_DMIPT( IACB ) = 0
         END IF
      END IF

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Call error tracing routine if appropriate.
         ELSE
            CALL NDF1_TRACE( 'NDF1_DUMP', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
