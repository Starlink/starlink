      SUBROUTINE NDF1_DMAP( IACB, TYPE, CMPLX, MMOD, MASK, DPNTR, IPNTR,
     :                      STATUS )
*+
*  Name:
*     NDF1_DMAP

*  Purpose:
*     Map the data array component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DMAP( IACB, TYPE, CMPLX, MMOD, MASK, DPNTR, IPNTR,
*     STATUS )

*  Description:
*     The routine obtains mapped access to the data array component of
*     an NDF identified by its ACB entry.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric data type to be used to access the data; an HDS
*        primitive numeric data type string (case insensitive).
*     CMPLX = LOGICAL (Given)
*        Whether access to complex data is required.
*     MMOD = CHARACTER * ( * ) (Given)
*        Mapping mode to be used to access the data (case insensitive).
*     MASK = LOGICAL (Given)
*        This argument specifies whether the mapped data values may
*        later be masked using quality information. If so, then this
*        routine will ensure tht a writeable buffer is used to return
*        the mapped values; this may require that a copy of the mapped
*        values be made. If MASK is .FALSE., then this routine may
*        return a read-only copy of the mapped values (i.e. as obtained
*        from HDS).
*     DPNTR = INTEGER (Returned)
*        Pointer to the mapped non-imaginary data component.
*     IPNTR = INTEGER (Returned)
*        Pointer to the mapped imaginary data component (not used if
*        CMPLX is .FALSE.).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Validate the mapping mode, decomposing it into an access mode
*     and an initialisation option.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Check that the data array is not already mapped. Report an
*     error if it is.
*     -  Set an initial null value for the ARY_ system temporary mapped
*     array identifier.
*     -  Map the data array, using the ARY_ system identifier in the
*     ACB.
*     -  Obtain the bad pixel flag for the mapped values and store it in
*     the ACB.
*     -  If a modifiable copy of the mapped values is required, but
*     they may be held in a read-only buffer, then a copy must be made
*     in a temporary array. Obtain the size of the array from the NDF's
*     data array identifier in the ACB.
*     -  Obtain a placeholder for the temporary array.
*     -  Create the array, storing the temporary ARY_ system identifier
*     in the ACB. Then map it as required and move the original mapped
*     values into it. Unmap the original values and save the pointer(s)
*     to the new copy.
*     -  If there was no error, then note that the data array is mapped
*     and update the DCB mapping counts.
*     -  Store the mapping type (and complex value flag) and note that
*     the bad pixel flag for the mapped values has not been modified.
*     -  Store pointers to the mapped values.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1989 (RFWS):
*        Original version.
*     15-DEC-1989 (RFWS):
*        Installed code to update the DCB mapping counts.
*     24-JAN-1990 (RFWS):
*        Changed to call ARY_MAP and ARY_MAPZ instead of ARY_MAPV and
*        ARY_MAPZV.
*     21-MAR-1990 (RFWS):
*        Changed to obtain a bad pixel flag value for the mapped
*        values.
*     23-MAR-1990 (RFWS):
*        Modified to store additional information about the mapped
*        values in the ACB.
*     3-APR-1990 (RFWS):
*        Added the MASK argument and code to support it.
*     1-SEP-2008 (DSB):
*        Raise an NDF event when the DATA array is mapped.
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
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_NDMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings to the NDF's data array.
*        DCB_NMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Total number of current mappings for the NDF.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_DMAP( NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether the NDF's data array is mapped for access.
*        ACB_DMBAD( NDF__MXACB ) = LOGICAL (Write)
*           Bad pixel flag for the mapped data values.
*        ACB_DMBMD( NDF__MXACB ) = LOGICAL (Write)
*           Whether the ACB_DMBAD value has been modified.
*        ACB_DMCPX( NDF__MXACB ) = LOGICAL (Write)
*           Whether complex access was obtained to the mapped data.
*        ACB_DMDPT( NDF__MXACB ) = INTEGER (Write)
*           Pointer to the mapped non-imaginary data values.
*        ACB_DMIPT( NDF__MXACB ) = INTEGER (Write)
*           Pointer to the mapped imaginary data values.
*        ACB_DMTID( NDF__MXACB ) = INTEGER (Write)
*           ARY_ system identifier for temporary array used when
*           mapping the NDF's data component.
*        ACB_DMTYP( NDF__MXACB ) = CHARACTER * ( NDF__SZTYP ) (Write)
*           Numeric data type used to map the data component.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) TYPE
      LOGICAL CMPLX
      CHARACTER * ( * ) MMOD
      LOGICAL MASK

*  Arguments Returned:
      INTEGER DPNTR
      INTEGER IPNTR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZFTP ) CTYPE ! Complex data type string
      CHARACTER * ( NDF__SZIOP ) INOPT ! Initialisation option
      CHARACTER * ( NDF__SZMOD ) MODE ! Mapping access mode
      INTEGER DPT                ! Temporary non-imaginary value pointer
      INTEGER EL                 ! Number of data elements mapped
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IPT                ! Temporary imaginary value pointer
      INTEGER LBND( NDF__MXDIM ) ! NDF lower pixel index bounds
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER PLACE              ! ARY_ system temporary placeholder
      INTEGER UBND( NDF__MXDIM ) ! NDF upper pixel index bounds

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the mapping mode, decomposing it into an access mode and an
*  initialisation option.
      CALL NDF1_VMMD( MMOD, MODE, INOPT, STATUS )

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB ( IACB )

*  Check that the data array is not already mapped through the
*  specified ACB entry and report an error if it is.
      IF ( ACB_DMAP( IACB ) ) THEN
         STATUS = NDF__ISMAP
         CALL NDF1_AMSG( 'NDF', IACB )
         CALL ERR_REP( 'NDF1_DMAP_MAP',
     :   'The data component in the NDF structure ^NDF is already ' //
     :   'mapped for access through the specified identifier ' //
     :   '(possible programming error).', STATUS )

*  Set an initial null ARY_ system identifier for the temporary mapped
*  array.
      ELSE
         ACB_DMTID( IACB ) = ARY__NOID

*  Map the data array component using the identifier in the ACB.
         IF ( CMPLX ) THEN
            CALL ARY_MAPZ( ACB_DID( IACB ), TYPE, MMOD, DPNTR, IPNTR,
     :                     EL, STATUS )
         ELSE
            CALL ARY_MAP( ACB_DID( IACB ), TYPE, MMOD, DPNTR, EL,
     :                    STATUS )
         END IF

*  Obtain the bad pixel flag for the mapped array values and store it in
*  the ACB.
         CALL ARY_BAD( ACB_DID( IACB ), .FALSE., ACB_DMBAD( IACB ),
     :                 STATUS )

*  If a modifiable copy of the mapped values is required, but they may
*  be held in a read-only buffer, then a copy must be made in a
*  temporary array.
         IF ( MASK .AND. ( MODE .EQ. 'READ' ) ) THEN

*  Obtain the size of the array from the NDF's data array identifier in
*  the ACB.
            CALL ARY_BOUND( ACB_DID( IACB ), NDF__MXDIM, LBND, UBND,
     :                      NDIM, STATUS )

*  Obtain a placeholder for the temporary array.
            CALL ARY_TEMP( PLACE, STATUS )

*  Create the array, storing the temporary ARY_ system identifier in
*  the ACB. Then map the new array as required and move the original
*  mapped values into it. Unmap the original values and save the
*  pointer(s) to the new copy.
            DPT = 0
            IPT = 0

*  ...Complex data.
            IF ( CMPLX ) THEN
               CTYPE = 'COMPLEX' // TYPE
               CALL ARY_NEW( CTYPE, NDIM, LBND, UBND, PLACE,
     :                       ACB_DMTID( IACB ), STATUS )
               CALL ARY_MAPZ( ACB_DMTID( IACB ), TYPE, 'WRITE', DPT,
     :                        IPT, EL, STATUS )
               CALL NDF1_MOVE( TYPE, EL, DPNTR, DPT, STATUS )
               CALL NDF1_MOVE( TYPE, EL, IPNTR, IPT, STATUS )
               CALL ARY_UNMAP( ACB_DID( IACB ), STATUS )
               DPNTR = DPT
               IPNTR = IPT

*  ...Non-complex data.
            ELSE
               CALL ARY_NEW( TYPE, NDIM, LBND, UBND, PLACE,
     :                       ACB_DMTID( IACB ), STATUS )
               CALL ARY_MAP( ACB_DMTID( IACB ), TYPE, 'WRITE', DPT, EL,
     :                       STATUS )
               CALL NDF1_MOVE( TYPE, EL, DPNTR, DPT, STATUS )
               CALL ARY_UNMAP( ACB_DID( IACB ), STATUS )
               DPNTR = DPT
            END IF
         END IF

*  If there was no error, then note that the data array is mapped and
*  update the DCB mapping counts.
         IF ( STATUS .EQ. SAI__OK ) THEN
            ACB_DMAP( IACB ) = .TRUE.
            DCB_NDMAP( IDCB ) = DCB_NDMAP( IDCB ) + 1
            DCB_NMAP( IDCB ) = DCB_NMAP( IDCB ) + 1

*  Store the mapping type (and complex value flag) and note that the
*  bad pixel flag for the mapped values has not been modified.
            ACB_DMTYP( IACB ) = TYPE
            ACB_DMCPX( IACB ) = CMPLX
            ACB_DMBMD( IACB ) = .FALSE.

*  Store pointers to the mapped values.
            ACB_DMDPT( IACB ) = DPNTR
            IF ( CMPLX ) ACB_DMIPT( IACB ) = IPNTR
         END IF
      END IF

*  If no error has occurred, we use NDF1_EVENT to flag a "data array
*  mapped" event. If the caller has registered a handler for this type of
*  event (using NDF_HNDLR), it will be called.
      IF( STATUS .EQ. SAI__OK ) THEN

*  Assign the name of the data file to the MSG token "NDF_EVENT"
         CALL NDF1_EVMSG( 'NDF_EVENT', ACB_IDCB( IACB ) )

*  Raise an appropriate NDF event.
         IF( MODE .EQ. 'READ' ) THEN
            CALL NDF1_EVENT( 'READ_DATA', STATUS )

         ELSE IF( MODE .EQ. 'WRITE' ) THEN
            CALL NDF1_EVENT( 'WRITE_DATA', STATUS )

         ELSE IF( MODE .EQ. 'UPDATE' ) THEN
            CALL NDF1_EVENT( 'UPDATE_DATA', STATUS )
         END IF

      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DMAP', STATUS )

      END
