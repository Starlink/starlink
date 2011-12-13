      SUBROUTINE NDF1_VMAP( IACB, TYPE, CMPLX, MMOD, STDEV, MASK, DPNTR,
     :                      IPNTR, STATUS )
*+
*  Name:
*     NDF1_VMAP

*  Purpose:
*     Map the variance component of an NDF for access.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VMAP( IACB, TYPE, CMPLX, MMOD, STDEV, MASK, DPNTR,
*     IPNTR, STATUS )

*  Description:
*     The routine maps the variance component of an NDF for access and
*     returns a pointer (or pointers) to the mapped values.  The NDF is
*     identified by its ACB entry. The mapped ACB entry may
*     subsequently be unmapped by the NDF1_VUMP routine and cannot be
*     re-mapped until this has been done.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF's ACB entry.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numeric data type required for access to the variance
*        values (case insensitive).
*     CMPLX = LOGICAL (Given)
*        Whether access to complex values is required.
*     MMOD = CHARACTER * ( * ) (Given)
*        The mapping mode for access (case insensitive).
*     STDEV = LOGICAL (Given)
*        Whether conversion of the mapped variance values to standard
*        deviations is required (as opposed to accessing the variance
*        values directly).
*     MASK = LOGICAL (Given)
*        This argument specifies whether the mapped variance values may
*        later be masked using quality information. If so, then this
*        routine will ensure tht a writeable buffer is used to return
*        the mapped values; this may require that a copy of the mapped
*        values be made. If MASK is .FALSE., then this routine may
*        return a read-only copy of the mapped values (i.e. as obtained
*        from HDS).
*     DPNTR = INTEGER (Returned)
*        Pointer to the mapped non-imaginary values.
*     IPNTR = INTEGER (Returned)
*        Pointer to the mapped imaginary values (not used if CMPLX is
*        .FALSE.).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Obtain an index to the data object entry in the DCB.
*     -  If the variance component is already mapped through this ACB
*     entry, then report an error.
*     -  Validate the mapping mode, decomposing it into an access mode
*     and an initialisation option.
*     -  Ensure that variance information is available in the DCB and
*     ACB.
*     -  See if the ARY_ system identifier for the variance array is
*     valid. If not, then the array does not exist.
*     -  Set an initial null value for the temporary mapped variance
*     array identifier.
*     -  Initialise the flag indicating if the mapped values may be in
*     a read-only buffer.
*     -  If the variance array exists, then map it for complex or
*     non-complex access, as required.
*     -  Obtain the bad pixel flag for the mapped values.
*     -  If the array does not exist, then see if the access mode and
*     initialisation option require it to be created.
*     -  If so, then create a new variance array, causing variance
*     information to be imported into the DCB and ACB, and map it for
*     the required type of access.
*     -  Obtain the bad pixel flag for the mapped values.
*     -  If the variance array does not exist, and READ access with an
*     initialisation option was specified, then a temporary array must
*     be created.
*     -  Determine its bounds from the NDF's data array identifier in
*     the DCB and create a temporary array of the required type, storing
*     its identifier in the ACB. Map this array for access.
*     -  Obtain the bad pixel flag.
*     -  Note the mapped values are in a writeable buffer.
*     -  If the variance array does not exist and READ access was
*     specified without an initialisation option, then report an error.
*     -  If mapped values are to be modified, but they may reside in a
*     read-only buffer, then a modifiable copy must be made in a
*     temporary array. Determine the size of this array from the data
*     array identifier in the ACB.
*     -  Obtain a placeholder for the temporary array.
*     -  Create the array, storing the temporary ARY_ system identifier
*     in the ACB. Then map it as required and move the original mapped
*     values into it. Unmap the original values and save the pointer(s)
*     to the new copy.
*     -  If mapping was successful, then convert the mapped
*     non-imaginary variance values to standard deviations, if required,
*     noting whether a data conversion error occurs.
*     -  If complex access is in effect, then perform the same process,
*     if required, on the imaginary component, allowing this to take
*     place even if a "negative variance" error was produced while
*     processing the non-imaginary component.
*     -  If there were no errors, or only "negative variance" errors,
*     then note that the ACB entry is mapped and increment the DCB
*     mapping counts relating to the data object.
*     -  Store general information about the mapping in the ACB.
*     -  Store the bad pixel flag for the mapped values in the ACB. If
*     conversion errors occurred during conversion to standard
*     deviations, then ensure that this bad pixel flag is set to .TRUE.
*     and note it has been modified.
*     -  Store pointers to the mapped values.

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
*     11-DEC-1989 (RFWS):
*        Original version.
*     20-DEC-1989 (RFWS):
*        Fixed bug in call to NDF1_V2S.
*     24-JAN-1990 (RFWS):
*        Changed to call ARY_MAP and ARY_MAPZ instead of ARY_MAPV and
*        ARY_MAPZV.
*     27-FEB-1990 (RFWS):
*        Improved error message and removed declarations of unused
*        functions.
*     1-MAR-1990 (RFWS):
*        Fixed illegal character string concatenation.
*     21-MAR-1990 (RFWS):
*        Changed handling of the bad pixel flag for the mapped values
*        to keep a record of whether it has been modified.
*     3-APR-1990 (RFWS):
*        Added the MASK argument and code to support it. Also changed to
*        use WRITE mapping mode for temporary arrays.
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
*        DCB_NMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Total number of current mappings to the NDF's components.
*        DCB_NVMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings to the NDF's variance array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_VID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the variance array.
*        ACB_VMAP( NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether the variance component is mapped.
*        ACB_VMBAD( NDF__MXACB ) = LOGICAL (Write)
*           Bad pixel flag for mapped variance values.
*        ACB_VMBMD( NDF__MXACB ) = LOGICAL (Write)
*           Whether the ACB_VMBAD value has been modified.
*        ACB_VMCPX( NDF__MXACB ) = LOGICAL (Write)
*           Whether mapped access is to comlpex variance values.
*        ACB_VMDPT( NDF__MXACB ) = INTEGER (Write)
*           Pointer to mapped non-imaginary variance values.
*        ACB_VMIPT( NDF__MXACB ) = INTEGER (Write)
*           Pointer to mapped imaginary variance values.
*        ACB_VMMOD( NDF__MXACB ) = CHARACTER * ( NDF__SZMOD ) (Write)
*           Access mode for mapping variance array.
*        ACB_VMSTD( NDF__MXACB ) = LOGICAL (Write)
*           Whether conversion from variance to standard deviation has
*           been performed.
*        ACB_VMTID( NDF__MXACB ) = INTEGER (Write)
*           ARY_system identifier for temporary array used when mapping
*           the variance component.
*        ACB_VMTYP( NDF__MXACB ) = CHARACTER * ( NDF__SZTYP ) (Write)
*           Numeric data type used to map the variance component.

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) TYPE
      LOGICAL CMPLX
      CHARACTER * ( * ) MMOD
      LOGICAL STDEV
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
      INTEGER EL                 ! Number of elements mapped
      INTEGER ICSTAT             ! Imaginary conversion status
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IPT                ! Temporary imaginary value pointer
      INTEGER LBND( NDF__MXDIM ) ! NDF lower pixel index bounds
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER PLACE              ! ARY_ system temporary placeholder
      INTEGER UBND( NDF__MXDIM ) ! NDF upper pixel index bounds
      LOGICAL BAD                ! Bad pixel flag for mapped values
      LOGICAL DDCE               ! Non-imaginary data conversion error?
      LOGICAL IDCE               ! Imaginary data conversion error?
      LOGICAL RDONLY             ! Values may be in a read-only buffer?
      LOGICAL THERE              ! Whether variance array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  If the variance component is already mapped through this ACB entry,
*  then report an error.
      IF ( ACB_VMAP( IACB ) ) THEN
         STATUS = NDF__ISMAP
         CALL NDF1_AMSG( 'NDF', IACB )
         CALL ERR_REP( 'NDF1_VMAP_MAP',
     :   'The variance component in the NDF structure ^NDF is ' //
     :   'already mapped for access through the specified ' //
     :   'identifier (possible programming error).', STATUS )

*  Validate the mapping mode, decomposing it into an access mode and an
*  initialisation option.
      ELSE
         CALL NDF1_VMMD( MMOD, MODE, INOPT, STATUS )

*  Ensure that variance information is available in the DCB and ACB.
         CALL NDF1_VIMP( IACB, STATUS )

*  See if the ARY_ system identifier for the variance array is valid. If
*  not, then the array does not exist.
         CALL ARY_VALID( ACB_VID( IACB ), THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Set an initial null value for the temporary mapped variance array
*  identifier.
            ACB_VMTID( IACB ) = ARY__NOID

*  Initialise the flag indicating if the mapped values may be in a
*  read-only buffer.
            RDONLY = MODE .EQ. 'READ'

*  Case 1:
*  ======
*  If the variance array exists, then map it; either for complex or
*  non-complex access, as required.
            IF ( THERE ) THEN
               IF ( CMPLX ) THEN
                  CALL ARY_MAPZ( ACB_VID( IACB ), TYPE, MMOD, DPNTR,
     :                           IPNTR, EL, STATUS )
               ELSE
                  CALL ARY_MAP( ACB_VID( IACB ), TYPE, MMOD, DPNTR, EL,
     :                          STATUS )
               END IF

*  Obtain the bad pixel flag for the mapped values.
               CALL ARY_BAD( ACB_VID( IACB ), .FALSE., BAD, STATUS )

*  Case 2:
*  ======
*  If the variance array does not exist, then see if the access mode and
*  initialisation option require it to be created.
            ELSE IF ( ( MODE .EQ. 'WRITE' ) .OR.
     :              ( ( MODE .EQ. 'UPDATE' ) .AND.
     :                ( INOPT .NE. ' ' ) ) ) THEN

*  If so, then create it (thereby importing identifiers for it into the
*  ACB).
               CALL NDF1_VCRE( IACB, STATUS )

*  Map the array, as required.
               IF ( CMPLX ) THEN
                  CALL ARY_MAPZ( ACB_VID( IACB ), TYPE, MMOD, DPNTR,
     :                           IPNTR, EL, STATUS )
               ELSE
                  CALL ARY_MAP( ACB_VID( IACB ), TYPE, MMOD, DPNTR,
     :                          EL, STATUS )
               END IF

*  Obtain the bad pixel flag for the mapped values.
               CALL ARY_BAD( ACB_VID( IACB ), .FALSE., BAD, STATUS )

*  Case 3:
*  ======
*  If the variance array does not exist and READ access with an
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

*  ...Complex data.
               IF ( CMPLX ) THEN
                  CTYPE = 'COMPLEX' // TYPE
                  CALL ARY_NEW( CTYPE, NDIM, LBND, UBND, PLACE,
     :                          ACB_VMTID( IACB ), STATUS )
                  CALL ARY_MAPZ( ACB_VMTID( IACB ),
     :                           TYPE, 'WRITE' // '/' // INOPT, DPNTR,
     :                           IPNTR, EL, STATUS )

*  ...Non-complex data.
               ELSE
                  CALL ARY_NEW( TYPE, NDIM, LBND, UBND, PLACE,
     :                          ACB_VMTID( IACB ), STATUS )
                  CALL ARY_MAP( ACB_VMTID( IACB ),
     :                          TYPE, 'WRITE' // '/' // INOPT, DPNTR,
     :                          EL, STATUS )
               END IF

*  Note the mapped values are in a writeable buffer.
               RDONLY = .FALSE.

*  Obtain the bad pixel flag for the mapped values.
               CALL ARY_BAD( ACB_VMTID( IACB ), .FALSE., BAD, STATUS )

*  Case 4:
*  ======
*  If the array does not exist and the access mode is not WRITE and no
*  initialisation option was specified, then report an error.
            ELSE
               STATUS = NDF__VUDEF
               CALL NDF1_AMSG( 'NDF', IACB )
               CALL ERR_REP( 'NDF1_VMAP_UDEF',
     :         'The variance component in the NDF structure ^NDF ' //
     :         'is in an undefined state.', STATUS )
            END IF
         END IF
      END IF

*  If mapped values are to be modified, but they may reside in a
*  read-only buffer, then a modifiable copy must be made in a temporary
*  array. Determine the size of this array from the ARY_ system data
*  array identifier in the ACB.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( ( MASK .OR. STDEV ) .AND. RDONLY ) THEN
            CALL ARY_BOUND( ACB_DID( IACB ), NDF__MXDIM, LBND, UBND,
     :                      NDIM, STATUS )

*  Obtain a placeholder for the temporary array.
            CALL ARY_TEMP( PLACE, STATUS )

*  Create the array, storing the temporary ARY_ system identifier in
*  the ACB. Then map it as required and move the original mapped values
*  into it. Unmap the original values and save the pointer(s) to the
*  new copy.

*  ...Complex data.
            IF ( CMPLX ) THEN
               CTYPE = 'COMPLEX' // TYPE
               CALL ARY_NEW( CTYPE, NDIM, LBND, UBND, PLACE,
     :                       ACB_VMTID( IACB ), STATUS )
               CALL ARY_MAPZ( ACB_VMTID( IACB ), TYPE, 'WRITE', DPT,
     :                        IPT, EL, STATUS )
               CALL NDF1_MOVE( TYPE, EL, DPNTR, DPT, STATUS )
               CALL NDF1_MOVE( TYPE, EL, IPNTR, IPT, STATUS )
               CALL ARY_UNMAP( ACB_VID( IACB ), STATUS )
               DPNTR = DPT
               IPNTR = IPT

*  ...Non-complex data.
            ELSE
               CALL ARY_NEW( TYPE, NDIM, LBND, UBND, PLACE,
     :                       ACB_VMTID( IACB ), STATUS )
               CALL ARY_MAP( ACB_VMTID( IACB ), TYPE, 'WRITE', DPT, EL,
     :                       STATUS )
               CALL NDF1_MOVE( TYPE, EL, DPNTR, DPT, STATUS )
               CALL ARY_UNMAP( ACB_VID( IACB ), STATUS )
               DPNTR = DPT
            END IF
         END IF
      END IF

*  If the variance values were mapped successfully, then see if
*  conversion from variances to standard deviation values was
*  specified. It is unnecessary to actually convert the values unless
*  they have been read from a pre-existing variance array.
      DDCE = .FALSE.
      IDCE = .FALSE.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( STDEV .AND. THERE .AND. ( MODE .NE. 'WRITE' ) ) THEN

*  If required, then convert the non-imaginary component, noting if a
*  data conversion error occurred.
            CALL NDF1_V2S( BAD, TYPE, EL, DPNTR, DDCE, STATUS )

*  Similarly convert the imaginary component, if present. Ensure that
*  this stage executes if a NDF__NGVAR (negative variance) error was
*  produced above, so that the mapped values can still be used if
*  required.
            IF ( CMPLX ) THEN
               IF ( ( STATUS .EQ. SAI__OK ) .OR.
     :              ( STATUS .EQ. NDF__NGVAR ) ) THEN
                  ICSTAT = SAI__OK
                  CALL NDF1_V2S( BAD, TYPE, EL, IPNTR, IDCE, ICSTAT )

*  Update the STATUS value if an error occurred during the second
*  conversion.
                  IF ( ICSTAT .NE. SAI__OK ) THEN
                     STATUS = ICSTAT
                  END IF
               END IF
            END IF
         END IF
      END IF

*  If there were no errors (or the only error was to detect negative
*  variance values during conversion to standard deviations), then note
*  that the ACB entry is mapped and increment the DCB counts of mappings
*  to this variance array and of total mappings to this NDF.
      IF ( ( STATUS .EQ. SAI__OK ) .OR.
     :     ( STATUS .EQ. NDF__NGVAR ) ) THEN
         ACB_VMAP( IACB ) = .TRUE.
         DCB_NVMAP( IDCB ) = DCB_NVMAP( IDCB ) + 1
         DCB_NMAP( IDCB ) = DCB_NMAP( IDCB ) + 1

*  Store the mapping type (and complex value flag) and mapping mode in
*  the ACB. Also note if conversion from variance to standard deviation
*  was specified.
         ACB_VMTYP( IACB ) = TYPE
         ACB_VMCPX( IACB ) = CMPLX
         ACB_VMMOD( IACB ) = MODE
         ACB_VMSTD( IACB ) = STDEV

*  Store the bad pixel flag value. If conversion errors occurred during
*  conversion to standard deviations, then set the bad pixel flag for
*  the mapped values to .TRUE. and note it has been modified.
         ACB_VMBAD( IACB ) = BAD
         ACB_VMBMD( IACB ) = .FALSE.
         IF ( DDCE .OR. IDCE ) THEN
            ACB_VMBAD( IACB ) = .TRUE.
            ACB_VMBMD( IACB ) = .TRUE.
         END IF

*  Store pointers to the mapped values.
         ACB_VMDPT( IACB ) = DPNTR
         IF ( CMPLX ) ACB_VMIPT( IACB ) = IPNTR
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VMAP', STATUS )

      END
