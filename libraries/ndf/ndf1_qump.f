      SUBROUTINE NDF1_QUMP( IACB, STATUS )
*+
*  Name:
*     NDF1_QUMP

*  Purpose:
*     Unmap the quality component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_QUMP( IACB, STATUS )

*  Description:
*     The routine unmaps the quality component of an NDF, which has
*     previously been mapped by NDF1_QMAP. If write or update access is
*     in effect, then the mapped values are written back to the array.
*     The NDF is identified by its ACB entry.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF's ACB entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine will attempt to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Prior Requirements:
*     -  The quality component must previously have been mapped.  An
*     error will be reported if this is not the case.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Obtain an index to the data object entry in the DCB.
*     -  If the quality array for the specified ACB entry is not
*     mapped, then report an error.
*     -  If the quality array has been mapped as an array of logical
*     values, then unmap and delete the object holding these values.
*     -  Otherwise, see if the temporary mapped array identifier in the
*     ACB is valid.  If so, then a temporary array was mapped for read
*     access and can be annulled.
*     -  Otherwise, unmap the ACB's quality array
*     -  If values have been written back to the array and its storage
*     form is not primitive, then ensure that its bad pixel flag
*     remains set to .FALSE..
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
*     31-JAN-1990 (RFWS):
*        Original, derived from the NDF1_VUMP routine.
*     15-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
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
*        DCB_NMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Total number of current mappings for the NDF.
*        DCB_NQMAP( NDF__MXDCB ) = INTEGER (Read)
*           Number of current mappings for the NDF's quality array.
*        DCB_QID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's quality array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_QID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the quality array.
*        ACB_QMAP( NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether the quality component is mapped.
*        ACB_QMMOD( NDF__MXACB ) = CHARACTER * ( NDF__SZMOD ) (Read)
*           Quality array access mode for mapping.
*        ACB_QMPTR( NDF__MXACB ) = INTEGER (Read and Write)
*           Pointer to mapped quality values.
*        ACB_QMTID( NDF__MXACB ) = INTEGER (Read and Write)
*           ARY_system identifier for temporary array used when mapping
*           the quality component.
*        ACB_QMTLC( NDF__MXACB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator for temporary object used to map the quality array.
*        ACB_QMTYP( NDF__MXACB ) = CHARACTER * ( NDF__SZTYP ) (Read)
*           Data type used to map the quality array.

*  Arguments Given:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZFRM ) FORM ! Quality array storage form
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER TSTAT              ! Temporary status variable
      LOGICAL TEMP               ! Whether a temporary array is mapped

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  If the quality array for the specified ACB entry is not mapped, then
*  report an error.
      STATUS = SAI__OK
      IF ( .NOT. ACB_QMAP( IACB ) ) THEN
         STATUS = NDF__NTMAP
         CALL NDF1_AMSG( 'NDF', IACB )
         CALL ERR_REP( 'NDF1_QUMP_NMAP',
     :   'The quality component in the NDF structure ^NDF is ' //
     :   'not mapped for access through the specified identifier ' //
     :   '(possible programming error).', STATUS )

*  If the quality array has been mapped as an array of logical values,
*  then unmap and annul the object holding these values.
      ELSE
         IF ( ACB_QMTYP( IACB ) .EQ. '_LOGICAL' ) THEN
             CALL NDF1_ANTMP( ACB_QMTLC( IACB ), STATUS )

*  Otherwise, see if the temporary mapped array identifier in the ACB
*  is valid.
         ELSE
            CALL ARY_VALID( ACB_QMTID( IACB ), TEMP, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then a temporary array was mapped for read access and can
*  be annulled.
               IF ( TEMP ) THEN
                  CALL ARY_ANNUL( ACB_QMTID( IACB ), STATUS )

*  Otherwise, unmap the ACB's quality array.
               ELSE
                  CALL ARY_UNMAP( ACB_QID( IACB ), STATUS )

*  If values have been written back to the array and the array's storage
*  form is not primitive, then ensure that its bad pixel flag remains
*  set to .FALSE.. This must be done via the base array identifier
*  stored in the DCB, since setting a bad pixel flag to .FALSE. for an
*  array section may not affect the base array.
                  CALL ARY_FORM( DCB_QID( IDCB ), FORM, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( ( FORM .NE. 'PRIMITIVE' ) .AND.
     :                    ( ACB_QMMOD( IACB ) .EQ. 'WRITE' ) .OR.
     :                    ( ACB_QMMOD( IACB ) .EQ. 'UPDATE' ) ) THEN
                        CALL ARY_SBAD( .FALSE., DCB_QID( IDCB ),
     :                                 STATUS )
                     END IF
                  END IF
               END IF
            END IF
         END IF

*  If no error occurred, then note that the array is no longer mapped
*  and decrement the DCB mapping counts.
         IF ( STATUS .EQ. SAI__OK ) THEN
            ACB_QMAP( IACB ) = .FALSE.
            DCB_NQMAP( IDCB ) = DCB_NQMAP( IDCB ) - 1
            DCB_NMAP( IDCB ) = DCB_NMAP( IDCB ) - 1

*  Reset the pointers to the mapped values.
            ACB_QMPTR( IACB ) = 0
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
            CALL NDF1_TRACE( 'NDF1_QUMP', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
