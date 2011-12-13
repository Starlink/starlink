      SUBROUTINE NDF1_VSFTP( FTYPE, IACB, STATUS )
*+
*  Name:
*     NDF1_VSFTP

*  Purpose:
*     Set a new full data type for the variance component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VSFTP( FTYPE, IACB, STATUS )

*  Description:
*     The routine sets a new full data type for the variance component
*     of an NDF. If the component is defined, then its data values will
*     be converted as necessary. The NDF is identified by its ACB
*     index.

*  Arguments:
*     FTYPE = CHARACTER * ( * ) (Given)
*        The new full data type.
*     IACB = INTEGER (Given)
*        Index to the NDF's ACB entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The data type of the variance component of an NDF cannot be
*     changed with this routine while mapped access to any part of it
*     is in effect.
*     -  The data type of the variance component of an NDF section
*     cannot be changed with this routine. If the NDF specified is not
*     a base NDF, then it will return without action.

*  Algorithm:
*     -  Ensure that variance information is available in the DCB and
*     ACB.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Check that the variance component is not mapped for access
*     through the current ACB entry. Report an error if it is.
*     -  Only take further action if the ACB entry describes a base
*     NDF.  Check if any part of the variance component is mapped.
*     Report an error if it is.
*     -  See if the DCB ARY_ system identifier for the variance array is
*     valid. If not, then the array does not exist.
*     -  If it exists, then set a new full data type for it.
*     -  If it does not exist, then decompose the full type
*     specification into a numeric type string and a complex value flag
*     and store these in the DCB as default attributes for the variance
*     array.
*     -  If the default storage form is primitive and the new data type
*     is complex, then convert the storage form to simple.

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
*     8-DEC-1989 (RFWS):
*        Original version.
*     24-JAN-1990 (RFWS):
*        Changed to call ARY_STYPE instead of ARY_SFTYP.
*     15-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     21-MAR-1990 (RFWS):
*        Tidied the checks on whether the variance component is mapped.
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
*        DCB_NVMAP( NDF__MXDCB ) = INTEGER (Read)
*           Number of mappings in effect for the NDF's variance
*           component.
*        DCB_VCPX( NDF__MXDCB ) = LOGICAL (Write)
*           Whether the NDF's variance array holds complex values by
*           default.
*        DCB_VID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's variance array.
*        DCB_VTYP( NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP ) (Write)
*           Default numeric data type for the NDF's variance array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether the ACB entry describes an NDF section.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_VMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the variance component is mapped through the ACB
*           entry.

*  Arguments Given:
      CHARACTER * ( * ) FTYPE
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB
      LOGICAL VALID              ! Variance array identifier valid?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that variance information is available in the DCB and ACB.
      CALL NDF1_VIMP( IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Check that the variance component is not mapped for access through
*  the current ACB entry. Report an error if it is.
         IF ( ACB_VMAP( IACB ) ) THEN
            STATUS = NDF__ISMAP
            CALL NDF1_AMSG( 'NDF', IACB )
            CALL ERR_REP( 'NDF1_VSFTP_MAP',
     :      'The variance component in the NDF structure ^NDF is ' //
     :      'already mapped for access through the specified ' //
     :      'identifier (possible programming error).', STATUS )

*  Only take further action if the NDF is a base NDF. Check that the
*  variance component is not mapped at all. Report an error if it is.
         ELSE IF ( .NOT. ACB_CUT( IACB ) ) THEN
            IF ( DCB_NVMAP( IDCB ) .NE. 0 ) THEN
               STATUS = NDF__ISMAP
               CALL NDF1_DMSG( 'NDF', IDCB )
               CALL ERR_REP( 'NDF1_VSFTP_BMAP',
     :         'The variance component in the NDF structure ^NDF is ' //
     :         'already mapped for access through another ' //
     :         'identifier (possible programming error).', STATUS )

*  See if the DCB ARY_ system identifier for the variance array is
*  valid. If not, then the variance array does not exist.
            ELSE
               CALL ARY_VALID( DCB_VID( IDCB ), VALID, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If it exists, then set a new full data type for it.
                  IF ( VALID ) THEN
                     CALL ARY_STYPE( FTYPE, DCB_VID( IDCB ), STATUS )
                     CALL NDF1_CMPAC( IDCB, 'VARIANCE', STATUS )

*  Otherwise, decompose the full type specification into a numeric type
*  and a complex value flag and store these in the DCB as default
*  attributes for the variance component.
                  ELSE
                     CALL NDF1_CHFTP( FTYPE, DCB_VTYP( IDCB ),
     :                                DCB_VCPX( IDCB ), STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  If the default storage form is primitive and the new data type is
*  complex, then convert the storage form to simple.
                        IF ( ( DCB_VFRM( IDCB ) .EQ. 'PRIMITIVE' )
     :                       .AND. DCB_VCPX( IDCB ) ) THEN
                           DCB_VFRM( IDCB ) = 'SIMPLE'
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VSFTP', STATUS )

      END
