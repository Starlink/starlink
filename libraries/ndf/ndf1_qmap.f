      SUBROUTINE NDF1_QMAP( IACB, TYPE, MMOD, PNTR, STATUS )
*+
*  Name:
*     NDF1_QMAP

*  Purpose:
*     Map the quality component of an NDF for access.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_QMAP( IACB, TYPE, MMOD, PNTR, STATUS )

*  Description:
*     The routine maps the quality component of an NDF for access and
*     returns a pointer to the mapped values.  The NDF is identified by
*     its ACB entry. The mapped ACB entry may subsequently be unmapped
*     by the NDF1_QUMP routine and cannot be re-mapped until this has
*     been done.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF's ACB entry.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numeric data type required for access to the quality
*        values (case insensitive).
*     MMOD = CHARACTER * ( * ) (Given)
*        The mapping mode for access (case insensitive).
*     PNTR = INTEGER (Returned)
*        Pointer to the mapped quality values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Obtain an index to the data object entry in the DCB.
*     -  If the quality component is already mapped through this ACB
*     entry, then report an error.
*     -  Validate the mapping mode, decomposing it into an access mode
*     and an initialisation option.
*     -  Ensure that quality information is available in the DCB and
*     ACB.
*     -  See if the ARY_ system identifier for the quality array is
*     valid. If not, then the array does not exist.
*     -  Set an initial null value for the temporary mapped quality
*     array identifier.
*     -  If the quality array exists, then map it.
*     -  If the quality array does not exist, then see if the access
*     mode and initialisation option require it to be created.  If so,
*     then create it (thereby importing identifiers for it into the
*     ACB).
*     - Map the array.
*     - If the quality array does not exist and READ access with an
*     initialisation option was requested, then a temporary array must
*     be created.  Determine its bounds from the NDF's data array
*     identifier in the ACB.
*     -  Obtain a placeholder for a temporary array.
*     -  Create the array, storing the temporary ARY_ system identifier
*     in the ACB. Then map it as required.
*     -  If the array does not exist and the access mode is not WRITE
*     and no initialisation option was specified, then report an error.
*     -  If there were no errors, then note that the ACB entry is
*     mapped and increment the DCB counts of mappings to this quality
*     array and of total mappings to this NDF.
*     -  Store the mapping type and mode in the ACB.
*     -  Return a pointer to the mapped values.

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
*     29-JAN-1990 (RFWS):
*        Original, derived from the NDF1_VMAP routine.
*     {enter_changes_here}

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
*        DCB_NMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Total number of current mappings to the NDF's components.
*        DCB_NQMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings to the NDF's quality array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_QID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the quality array.
*        ACB_QMAP( NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether the quality component is mapped.
*        ACB_QMPTR( NDF__MXACB ) = INTEGER (Write)
*           Pointer to mapped quality values.
*        ACB_QMMOD( NDF__MXACB ) = CHARACTER * ( NDF__SZMOD ) (Write)
*           Access mode for mapping quality array.
*        ACB_QMTID( NDF__MXACB ) = INTEGER (Write)
*           ARY_system identifier for temporary array used when mapping
*           the quality component.
*        ACB_QMTYP( NDF__MXACB ) = CHARACTER * ( NDF__SZTYP ) (Write)
*           Numeric data type used to map the quality component.

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) MMOD

*  Arguments Returned:
      INTEGER PNTR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZIOP ) INOPT ! Initialisation option
      CHARACTER * ( NDF__SZMOD ) MODE ! Mapping access mode
      INTEGER EL                 ! Number of elements mapped
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER LBND( NDF__MXDIM ) ! NDF lower pixel index bounds
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER PLACE              ! ARY_ system temporary placeholder
      INTEGER UBND( NDF__MXDIM ) ! NDF upper pixel index bounds
      LOGICAL THERE              ! Whether quality array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  If the quality component is already mapped through this ACB entry,
*  then report an error.
      IF ( ACB_QMAP( IACB ) ) THEN
         STATUS = NDF__ISMAP
         CALL NDF1_AMSG( 'NDF', IACB )
         CALL ERR_REP( 'NDF1_QMAP_MAP',
     :   'The quality component in the NDF structure ^NDF is ' //
     :   'already mapped for access through the specified ' //
     :   'identifier (possible programming error).', STATUS )

*  Validate the mapping mode, decomposing it into an access mode and an
*  initialisation option.
      ELSE
         CALL NDF1_VMMD( MMOD, MODE, INOPT, STATUS )

*  Ensure that quality information is available in the DCB and ACB.
         CALL NDF1_QIMP( IACB, STATUS )

*  See if the ARY_ system identifier for the quality array is valid. If
*  not, then the array does not exist.
         CALL ARY_VALID( ACB_QID( IACB ), THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Set an initial null value for the temporary mapped quality array
*  identifier.
            ACB_QMTID( IACB ) = ARY__NOID

*  If the quality array exists, then map it.
            IF ( THERE ) THEN
               CALL ARY_MAP( ACB_QID( IACB ), TYPE, MMOD,
     :                       ACB_QMPTR( IACB ), EL, STATUS )

*  If the quality array does not exist, then see if the access mode and
*  initialisation option require it to be created.
            ELSE IF ( ( MODE .EQ. 'WRITE' ) .OR.
     :              ( ( MODE .EQ. 'UPDATE' ) .AND.
     :                ( INOPT .NE. ' ' ) ) ) THEN

*  If so, then create it (thereby importing identifiers for it into the
*  ACB).
               CALL NDF1_QCRE( IACB, STATUS )

*  Map the array.
               CALL ARY_MAP( ACB_QID( IACB ), TYPE, MMOD,
     :                       ACB_QMPTR( IACB ), EL, STATUS )

*  If the quality array does not exist and READ access with an
*  initialisation option was requested, then a temporary array must be
*  created.  Determine its bounds from the NDF's data array identifier
*  in the ACB.
            ELSE IF ( ( MODE .EQ. 'READ' ) .AND.
     :                ( INOPT .NE. ' ' ) ) THEN
               CALL ARY_BOUND( ACB_DID( IACB ), NDF__MXDIM,
     :                         LBND, UBND, NDIM, STATUS )

*  Obtain a placeholder for a temporary array.
               CALL ARY_TEMP( PLACE, STATUS )

*  Create the array, storing the temporary ARY_ system identifier in the
*  ACB. Then map it as required.
               CALL ARY_NEW( TYPE, NDIM, LBND, UBND, PLACE,
     :                       ACB_QMTID( IACB ), STATUS )
               CALL ARY_MAP( ACB_QMTID( IACB ), TYPE, MMOD,
     :                       ACB_QMPTR( IACB ), EL, STATUS )

*  If the array does not exist and the access mode is not WRITE and no
*  initialisation option was specified, then report an error.
            ELSE
               STATUS = NDF__QUDEF
               CALL NDF1_AMSG( 'NDF', IACB )
               CALL ERR_REP( 'NDF1_QMAP_UDEF',
     :         'The quality component in the NDF structure ^NDF ' //
     :         'is in an undefined state (possible programming error).',
     :         STATUS )
            END IF
         END IF
      END IF

*  If there were no errors, then note that the ACB entry is mapped and
*  increment the DCB counts of mappings to this quality array and of
*  total mappings to this NDF.
      IF ( STATUS .EQ. SAI__OK ) THEN
         ACB_QMAP( IACB ) = .TRUE.
         DCB_NQMAP( IDCB ) = DCB_NQMAP( IDCB ) + 1
         DCB_NMAP( IDCB ) = DCB_NMAP( IDCB ) + 1

*  Store the mapping type and mode in the ACB.
         ACB_QMTYP( IACB ) = TYPE
         ACB_QMMOD( IACB ) = MODE

*  Return a pointer to the mapped values.
         PNTR = ACB_QMPTR( IACB )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_QMAP', STATUS )

      END
