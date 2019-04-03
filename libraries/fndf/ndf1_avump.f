      SUBROUTINE NDF1_AVUMP( IAX, IACB, STATUS )
*+
*  Name:
*     NDF1_AVUMP

*  Purpose:
*     Unmap an axis variance array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AVUMP( IAX, IACB, STATUS )

*  Description:
*     The routine unmaps an NDF axis variance array which has
*     previously been mapped for access with the NDF1_AVMAP routine. An
*     error will be reported if the specified axis array is not
*     currently mapped for access. The NDF is identified by its index
*     in the ACB.

*  Arguments:
*     IAX = INTEGER (Given)
*        Number of the axis whose variance array is to be unmapped.
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
*     -  Check that the specified axis variance array is mapped for
*     access.  Report an error if it is not.
*     -  See whether conversion to standard deviations has been
*     performed when the variance array's values were mapped and if
*     values are to be written back to the array.
*     -  If so, then back-conversion is now required. Determine the
*     number of array elements to convert from the size of the mapped
*     array.
*     -  Convert the mapped standard deviations back to variances.
*     -  If a conversion error occurred, then report context
*     information.
*     -  Unmap the axis variance array by annulling its ARY_ system
*     identifier in the ACB (if a temporary array has been mapped, then
*     it will be deleted at this point).
*     -  If successful (or the only error was to encounter negative
*     standard deviation values), then note the array is no longer
*     mapped. Reset the mapping pointer in the ACB to zero.
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
*     16-OCT-1990 (RFWS):
*        Original version, derived from the NDF1_AVUMP routine.
*     19-OCT-1990 (RFWS):
*        Removed incorrect status check on entry.
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
*        DCB_NAVMP( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings to each axis variance array.
*        DCB_NMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Total number of current mappings to each variance object.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_AVMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether NDF axis variance arrays are currently mapped for
*           access.
*        ACB_AVMID( NDF__MXDIM, NDF__MXACB ) = INTEGER (Read and Write)
*           ARY_ system identifier for axis variance array mappings.
*        ACB_AVMMD( NDF__MXDIM, NDF__MXACB ) = CHARACTER * ( NDF__SZMOD
*        ) (Read)
*           Access mode used to map axis variance arrays.
*        ACB_AVMPT( NDF__MXDIM, NDF__MXACB ) = INTEGER (Read and Write)
*           Pointers to mapped axis variance arrays.
*        ACB_AVMST( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Read)
*           Whether mapped axis variance values are standard deviations.
*        ACB_AVMTP( NDF__MXDIM, NDF__MXACB ) = CHARACTER * ( NDF__SZTYP
*        ) (Read)
*           Numeric type used to map axis variance arrays.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IAX
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EL                 ! Number of mapped array elements
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER TSTAT              ! Temporary status variable
      LOGICAL DCE                ! Data conversion error?

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Obtain an index to the data object entry in the DCB.
      STATUS = SAI__OK
      IDCB = ACB_IDCB( IACB )

*  Check that the specified axis variance array is mapped for access.
*  Report an error if it is not.
      IF ( .NOT. ACB_AVMAP( IAX, IACB ) ) THEN
         STATUS = NDF__NTMAP
         CALL MSG_SETI( 'AXIS', IAX )
         CALL NDF1_AMSG( 'NDF', IACB )
         CALL ERR_REP( 'NDF1_AVUMP_NTMAP',
     :                 'The variance array for axis ^AXIS of the ' //
     :                 'NDF structure ^NDF is not mapped for access ' //
     :                 'through the specified identifier (possible ' //
     :                 'programming error).', STATUS )

*  See whether conversion to standard deviations has been performed
*  when the variance array's values were mapped and if values are to be
*  written back to the array.
      ELSE
         IF ( ACB_AVMST( IAX, IACB ) .AND.
     :        ( .NOT. ACB_CUT( IACB ) ) .AND.
     :        ( ACB_AVMMD( IAX, IACB ) .NE. 'READ' ) ) THEN

*  If so, then back-conversion is now required. Determine the number of
*  array elements to convert from the size of the mapped array.
            CALL ARY_SIZE( ACB_AVMID( IAX, IACB ), EL, STATUS )

*  Convert the mapped standard deviations back to variances.
            CALL NDF1_S2V( .TRUE., ACB_AVMTP( IAX, IACB ), EL,
     :                     ACB_AVMPT( IAX, IACB ), DCE, STATUS )

*  If a conversion error occurred, then report context information.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETI( 'AXIS', IAX )
               CALL NDF1_AMSG( 'NDF', IACB )
               CALL ERR_REP( 'NDF1_AVUMP_CONV',
     :                       'Error converting axis standard ' //
     :                       'deviations (errors) into variance ' //
     :                       'values for axis ^AXIS of the NDF ' //
     :                       'structure ^NDF', STATUS )
            END IF
         END IF

*  Unmap the axis variance array by annulling its ARY_ system identifier
*  in the ACB (if a temporary array has been mapped, then it will be
*  deleted at this point).
         CALL ARY_ANNUL( ACB_AVMID( IAX, IACB ), STATUS )

*  If successful (or the only error was to encounter negative standard
*  deviation values), then note the array is no longer mapped. Reset
*  the mapping pointer in the ACB to zero.
         IF ( ( STATUS .EQ. SAI__OK ) .OR.
     :        ( STATUS .EQ. NDF__NGSTD ) ) THEN
            ACB_AVMAP( IAX, IACB ) = .FALSE.
            ACB_AVMPT( IAX, IACB ) = 0

*  Decrement the DCB mapping counts.
            DCB_NAVMP( IAX, IDCB ) = DCB_NAVMP( IAX, IDCB ) - 1
            DCB_NMAP( IDCB ) = DCB_NMAP( IDCB ) - 1
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
            CALL NDF1_TRACE( 'NDF1_AVUMP', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
