      SUBROUTINE NDF_MAPQL( INDF, PNTR, EL, BAD, STATUS )
*+
*  Name:
*     NDF_MAPQL

*  Purpose:
*     Map the quality component of an NDF as an array of logical
*     values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_MAPQL( INDF, PNTR, EL, BAD, STATUS )

*  Description:
*     The routine maps the quality component of an NDF for read access,
*     returning a pointer to an array of logical values. Elements of
*     this array are set to .TRUE. if the bit-wise "AND" of the
*     corresponding quality value and its effective bad-bits mask gives
*     a zero result, indicating that the corresponding NDF pixel may be
*     used in subsequent processing. Other array elements are set to
*     .FALSE., indicating that corresponding NDF pixels should be
*     excluded from subsequent processing.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     PNTR = INTEGER (Returned)
*        Pointer to the mapped array of logical values.
*     EL = INTEGER (Returned)
*        Number of values mapped.
*     BAD = LOGICAL (Returned)
*        This argument is set to .TRUE. if any of the mapped values is
*        set to .FALSE. (i.e. if any NDF pixels are to be excluded as a
*        consequence of the associated quality values). Otherwise it is
*        set to .FALSE..
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the quality component's values are undefined, then this
*     routine will return a pointer to an array of .TRUE. values.
*     -  Note that this routine only obtains read access to the quality
*     component; changes made to the mapped values will not be
*     reflected in changes to the NDF's quality values.
*     -  This routine disables automatic quality masking, so that
*     subsequent access to other NDF array components via the same
*     identifier will take no account of the possible presence of
*     associated quality values.
*     -  If this routine is called with STATUS set, then a value of 1
*     will be returned for the EL argument, although no further
*     processing will occur.  The same value will also be returned if
*     the routine should fail for any reason.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Obtain an index to the data object entry in the DCB.
*     -  If the quality component is already mapped through the
*     specified ACB entry, then report an error.
*     -  Obtain the effective bad-bits mask value.
*     -  If the bad-bits mask is zero, then there is no need to read
*     the quality array.  If it is non-zero, then check if there is a
*     quality array with defined values to read.
*     -  Obtain the number of pixel values to be mapped from the NDF's
*     data array entry in the ACB.
*     -  Create a temporary _LOGICAL array of the correct size and map
*     it for write access.
*     -  If a quality array has to be read, then map it. Convert the
*     quality values into logical values. Then unmap the quality array.
*     -  If there is no quality array to read, then simply generate
*     .TRUE.  logical values and note there are no .FALSE. values
*     present.
*     -  If an error occurred, then unmap and delete the temporary
*     _LOGICAL object which may have been acquired.
*     -  If no error has occurred, then note that the quality component
*     is mapped and increment the DCB count of quality mappings and
*     total mappings for the data object.
*     -  Reset the quality masking flag, so that quality masking will
*     not subsequently be applied automatically to other NDF components
*     when they are mapped.
*     -  Store the mapping type and access mode.
*     -  Return a pointer to the mapped logical values.
*     -  Under error conditions, return a "safe" value of EL.

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
*     1-FEB-1990 (RFWS):
*        Original version.
*     21-MAR-1990 (RFWS):
*        Minor change to error message.
*     4-DEC-1990 (RFWS):
*        Changed to return a "safe" value of EL under error conditions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

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
*        ACB_QMF( NDF__MXACB ) = LOGICAL (Write)
*           Quality masking flag.
*        ACB_QMPTR( NDF__MXACB ) = INTEGER (Write)
*           Pointer to mapped quality values.
*        ACB_QMMOD( NDF__MXACB ) = CHARACTER * ( NDF__SZMOD ) (Write)
*           Access mode for mapping quality array.
*        ACB_QMTID( NDF__MXACB ) = INTEGER (Write)
*           ARY_system identifier for temporary array used when mapping
*           the quality component.
*        ACB_QMTLC( NDF__MXACB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to temporary object used for mapping quality values.
*        ACB_QMTYP( NDF__MXACB ) = CHARACTER * ( NDF__SZTYP ) (Write)
*           Numeric data type used to map the quality component.

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      INTEGER PNTR
      INTEGER EL
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      BYTE ZEROUB                ! Zero as an unsigned byte value
      PARAMETER ( ZEROUB = 0 )

*  Local Variables:
      BYTE BADBIT                ! Effective bad-bits mask value
      INTEGER DIM( 1 )           ! Dimension array
      INTEGER IACB               ! Index to the NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER QPNTR              ! Pointer to mapped quality values
      LOGICAL ISQUAL             ! Whether a quality array must be read

*.

*  Check inherited global status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Import the NDF identifier.
         CALL NDF1_IMPID( INDF, IACB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
            IDCB = ACB_IDCB( IACB )

*  If the quality component is already mapped through the specified ACB
*  entry, then report an error.
            IF ( ACB_QMAP( IACB ) ) THEN
               STATUS = NDF__ISMAP
               CALL NDF1_AMSG( 'NDF', IACB )
               CALL ERR_REP( 'NDF_MAPQL_MAP',
     :         'The quality component in the NDF structure ^NDF is ' //
     :         'already mapped for access through the specified ' //
     :         'identifier (possible programming error).', STATUS )

*  Obtain the effective bad-bits mask value.
            ELSE
               CALL NDF1_GTBB( IACB, BADBIT, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If the bad-bits mask is zero, then there is no need to read the
*  quality array.  If it is non-zero, then check if there is a quality
*  array with defined values to read.
                  ISQUAL = BADBIT .NE. ZEROUB
                  IF ( ISQUAL ) THEN
                     CALL NDF1_QSTA( IACB, ISQUAL, STATUS )
                  END IF

*  Obtain the number of pixel values to be mapped from the NDF's data
*  array entry in the ACB.
                  CALL ARY_SIZE( ACB_DID( IACB ), EL, STATUS )

*  Create a temporary _LOGICAL array of the correct size and map it for
*  write access.
                  DIM( 1 ) = EL
                  CALL NDF1_TEMP( '_LOGICAL', 1, DIM, ACB_QMTLC( IACB ),
     :                            STATUS )
                  CALL DAT_MAPL( ACB_QMTLC( IACB ), 'WRITE', 1, DIM,
     :                           ACB_QMPTR( IACB ), STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If a quality array has to be read, then map it. Convert the quality
*  values into logical values. Then unmap the quality array.
                     IF ( ISQUAL ) THEN
                        CALL ARY_MAP( ACB_QID( IACB ), '_UBYTE', 'READ',
     :                                QPNTR, EL, STATUS )
                        CALL NDF1_QMLOG( BADBIT, EL,
     :                                   %VAL( CNF_PVAL( QPNTR ) ),
     :                            %VAL( CNF_PVAL( ACB_QMPTR( IACB ) ) ),
     :                                   BAD, STATUS )
                        CALL ARY_UNMAP( ACB_QID( IACB ), STATUS )

*  If there is no quality array to read, then simply generate .TRUE.
*  logical values and note there are no .FALSE. values present.
                     ELSE
                        CALL NDF1_TRUE( EL,
     :                            %VAL( CNF_PVAL( ACB_QMPTR( IACB ) ) ),
     :                                  STATUS )
                        BAD = .FALSE.
                     END IF
                  END IF

*  If an error occurred, then unmap and delete the temporary _LOGICAL
*  object which may have been acquired.
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL NDF1_ANTMP( ACB_QMTLC( IACB ), STATUS )
                  END IF
               END IF
            END IF

*  If no error has occurred, then note that the quality component is
*  mapped and increment the DCB count of quality mappings and total
*  mappings for the data object.
            IF ( STATUS .EQ. SAI__OK ) THEN
               ACB_QMAP( IACB ) = .TRUE.
               DCB_NQMAP( IDCB ) = DCB_NQMAP( IDCB ) + 1
               DCB_NMAP( IDCB ) = DCB_NMAP( IDCB ) + 1

*  Reset the quality masking flag, so that quality masking will not
*  subsequently be applied automatically to other NDF components when
*  they are mapped.
               ACB_QMF( IACB ) = .FALSE.

*  Store the mapping type and access mode.
               ACB_QMTYP( IACB ) = '_LOGICAL'
               ACB_QMMOD( IACB ) = 'READ'

*  Return a pointer to the mapped logical values.
               PNTR = ACB_QMPTR( IACB )
            END IF
         END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'NDF_MAPQL_ERR',
     :      'NDF_MAPQL: Error mapping the quality component of an ' //
     :      'NDF as an array of logical values.', STATUS )
            CALL NDF1_TRACE( 'NDF_MAPQL', STATUS )
         END IF
      END IF

*  Under error conditions, return a "safe" value of EL.
      IF ( STATUS .NE. SAI__OK ) THEN
         EL = 1
      END IF

      END
