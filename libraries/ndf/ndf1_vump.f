      SUBROUTINE NDF1_VUMP( IACB, STATUS )
*+
*  Name:
*     NDF1_VUMP

*  Purpose:
*     Unmap the variance component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VUMP( IACB, STATUS )

*  Description:
*     The routine unmaps the variance component of an NDF, which has
*     previously been mapped by NDF1_VMAP. If write or update access is
*     in effect, then the mapped values are written back to the array
*     (with back-conversion from standard deviations to variance values
*     if necessary). The NDF is identified by its ACB entry.

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
*     -  The variance array must previously have been mapped by a call
*     to NDF1_VMAP. An error will be reported if this is not the case.

*  Side Effects:
*     -  The bad pixel flag for the array may be altered if data
*     conversion errors occur during the process of writing values back
*     to the variance array.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Obtain an index to the data object entry in the DCB.
*     -  If the variance array for the specified ACB entry is not
*     mapped, then report an error.
*     -  See if the temporary mapped array identifier in the ACB is
*     valid. If so, then a temporary array was mapped for read access
*     and can simply be annulled.
*     -  If read access was in effect, but did not involve a temporary
*     array, then simply unmap the ACB's variance array.
*     -  If the access mode requires values to be written back to the
*     variance array, then see if conversion to standard deviations was
*     specified when the array was mapped.
*     -  If so, then back-conversion is now required. Determine the
*     number of array elements to convert from the size of the data
*     array.  Convert the array's non-imaginary values to variances,
*     noting if any conversion errors occur.
*     -  If access to complex values is in effect, then similarly
*     convert the imaginary values. Ensure that this second conversion
*     executes even if "negative standard deviation" errors
*     (NDF__NGSTD) occurred during the first conversion, so that the
*     results returned to the array are still usable. Update STATUS if
*     an error occurred during the second conversion.
*     -  If data conversion errors occurred, then ensure that the bad
*     pixel flag for the mapped values is set to .TRUE. and note it has
*     been modified.
*     -  If the bad pixel flag for the mapped values has been modified,
*     then set the new value for the mapped array before it is
*     unmapped.  Ensure that this is done even if STATUS is set to
*     NDF__NGSTD. Update STATUS if an error occurred while setting the
*     bad pixel flag.
*     -  Unmap the array.
*     -  If no error occurred, or the only error was a "negative
*     standard deviation" error, then note that the array is no longer
*     mapped and decrement the DCB mapping counts.
*     -  Reset the pointers to the mapped values.
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
*     15-DEC-1989 (RFWS):
*        Original version.
*     20-DEC-1989 (RFWS):
*        Fixed bug in calls to NDF1_S2V.
*     11-JAN-1990 (RFWS):
*        Improved error message.
*     20-MAR-1990 (RFWS):
*        Fixed bug in use of ICSTAT temporary status variable.
*     21-MAR-1990 (RFWS):
*        Changed handling of the bad pixel flag for mapped values to
*        take account of whether it has been modified.
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
*        DCB_NVMAP( NDF__MXDCB ) = INTEGER (Read)
*           Number of current mappings for the NDF's variance array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_VID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the variance array.
*        ACB_VMAP( NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether the variance component is mapped.
*        ACB_VMBAD( NDF__MXACB ) = LOGICAL (Read and Write)
*           Bad pixel flag for mapped variance values.
*        ACB_VMBMD( NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether the VMBAD value has been modified.
*        ACB_VMCPX( NDF__MXACB ) = LOGICAL (Read)
*           Whether mapped access is to compex variance values.
*        ACB_VMDPT( NDF__MXACB ) = INTEGER (Read and Write)
*           Pointer to mapped non-imaginary variance values.
*        ACB_VMIPT( NDF__MXACB ) = INTEGER (Read and Write)
*           Pointer to mapped imaginary variance values.
*        ACB_VMMOD( NDF__MXACB ) = CHARACTER * ( NDF__SZMOD ) (Read)
*           Access mode for mapping variance array.
*        ACB_VMSTD( NDF__MXACB ) = LOGICAL (Read)
*           Whether conversion from variance to standard deviation has
*           been performed.
*        ACB_VMTID( NDF__MXACB ) = INTEGER (Read and Write)
*           ARY_system identifier for temporary array used when mapping
*           the variance component.
*        ACB_VMTYP( NDF__MXACB ) = CHARACTER * ( NDF__SZTYP ) (Read)
*           Numeric data type used to map the variance component.

*  Arguments Given:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BSTAT              ! Setting bad pixel flag error status
      INTEGER EL                 ! Number of mapped values
      INTEGER ICSTAT             ! Data conversion error status
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER TSTAT              ! Temporary status variable
      LOGICAL DDCE               ! Non-imaginary conversion error?
      LOGICAL IDCE               ! Imaginary conversion error
      LOGICAL TEMP               ! Whether a temporary array is mapped

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Obtain an index to the data object entry in the DCB.
      STATUS = SAI__OK
      IDCB = ACB_IDCB( IACB )

*  If the variance array for the specified ACB entry is not mapped, then
*  report an error.
      IF ( .NOT. ACB_VMAP( IACB ) ) THEN
         STATUS = NDF__NTMAP
         CALL NDF1_AMSG( 'NDF', IACB )
         CALL ERR_REP( 'NDF1_VUMP_NMAP',
     :   'The variance component in the NDF structure ^NDF is ' //
     :   'not mapped for access through the specified identifier ' //
     :   '(possible programming error).', STATUS )

*  See if the temporary mapped array identifier in the ACB is valid.
      ELSE
         CALL ARY_VALID( ACB_VMTID( IACB ), TEMP, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then a temporary array was mapped for read access and can
*  simply be annulled.
            IF ( TEMP ) THEN
               CALL ARY_ANNUL( ACB_VMTID( IACB ), STATUS )

*  If read access was in effect, but did not involve a temporary array,
*  then simply unmap the ACB's variance array.
            ELSE
               IF ( ACB_VMMOD( IACB ) .EQ. 'READ' ) THEN
                  CALL ARY_UNMAP( ACB_VID( IACB ), STATUS )

*  If the access mode requires values to be written back to the variance
*  array, then see if conversion to standard deviations was specified
*  when the array was mapped.
               ELSE IF ( ( ACB_VMMOD( IACB ) .EQ. 'WRITE' ) .OR.
     :                   ( ACB_VMMOD( IACB ) .EQ. 'UPDATE' ) ) THEN
                  IF ( ACB_VMSTD( IACB ) ) THEN

*  If so, then back-conversion is now required. Determine the number of
*  array elements to convert from the size of the data array.
                     DDCE = .FALSE.
                     IDCE = .FALSE.
                     CALL ARY_SIZE( ACB_DID( IACB ), EL, STATUS )

*  Convert the array's non-imaginary values to variances, noting if any
*  conversion errors occur.
                     CALL NDF1_S2V( ACB_VMBAD( IACB ),
     :                              ACB_VMTYP( IACB ), EL,
     :                              ACB_VMDPT( IACB ), DDCE, STATUS )

*  If access to complex values is in effect, then similarly convert the
*  imaginary values. Ensure that this second conversion executes even
*  if "negative standard deviation" errors (NDF__NGSTD) occurred during
*  the first conversion, so that the results returned to the array are
*  still usable.
                     IF ( ACB_VMCPX( IACB ) ) THEN
                        IF ( ( STATUS .EQ. SAI__OK ) .OR.
     :                       ( STATUS .EQ. NDF__NGSTD ) ) THEN
                           ICSTAT = SAI__OK
                           CALL NDF1_S2V( ACB_VMBAD( IACB ),
     :                                    ACB_VMTYP( IACB ), EL,
     :                                    ACB_VMIPT( IACB ), IDCE,
     :                                    ICSTAT )

*  Update STATUS if an error occurred during the second conversion.
                           IF ( ICSTAT .NE. SAI__OK ) THEN
                              STATUS = ICSTAT
                           END IF
                        END IF
                     END IF

*  If data conversion errors occurred, then ensure that the bad pixel
*  flag for the mapped values is set to .TRUE. and note it has been
*  modified.
                     IF ( DDCE .OR. IDCE ) THEN
                        ACB_VMBAD( IACB ) = .TRUE.
                        ACB_VMBMD( IACB ) = .TRUE.
                     END IF
                  END IF

*  If the bad pixel flag for the mapped values has been modified, then
*  set the new value for the mapped array before it is unmapped,
*  ensuring that this is done even if STATUS is set to NDF__NGSTD.
                  IF ( ( STATUS .EQ. SAI__OK ) .OR.
     :                 ( STATUS .EQ. NDF__NGSTD ) ) THEN
                     IF ( ACB_VMBMD( IACB ) ) THEN
                        BSTAT = SAI__OK
                        CALL ARY_SBAD( ACB_VMBAD( IACB ),
     :                                 ACB_VID( IACB ), BSTAT )

*  Update STATUS if an error occurred while setting the bad pixel flag.
                        IF ( BSTAT .NE. SAI__OK ) THEN
                           STATUS = BSTAT
                        END IF
                     END IF
                  END IF

*  Unmap the array.
                  CALL ARY_UNMAP( ACB_VID( IACB ), STATUS )
               END IF
            END IF
         END IF

*  If no error occurred, or the only error was a "negative standard
*  deviation" error, then note that the array is no longer mapped and
*  decrement the DCB mapping counts.
         IF ( ( STATUS .EQ. SAI__OK ) .OR.
     :        ( STATUS .EQ. NDF__NGSTD ) ) THEN
            ACB_VMAP( IACB ) = .FALSE.
            DCB_NVMAP( IDCB ) = DCB_NVMAP( IDCB ) - 1
            DCB_NMAP( IDCB ) = DCB_NMAP( IDCB ) - 1

*  Reset the pointers to the mapped values.
            ACB_VMDPT( IACB ) = 0
            ACB_VMIPT( IACB ) = 0
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
            CALL NDF1_TRACE( 'NDF1_VUMP', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
