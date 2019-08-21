      SUBROUTINE NDF1_VCRE( IACB, STATUS )
*+
*  Name:
*     NDF1_VCRE

*  Purpose:
*     Create a variance component for an NDF, if necessary.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VCRE( IACB, STATUS )

*  Description:
*     The routine ensures that a variance array exists for an NDF,
*     creating one if necessary. The array is created using the default
*     variance array attributes stored in the DCB. ARY_ system
*     identifiers for the new array (and appropriate sections thereof)
*     are entered into the DCB and also into those ACB entries which
*     refer to the NDF data object in question. The NDF is identified
*     to this routine by its ACB entry.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the ACB entry for the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Ensure that variance information is available in the DCB and
*     ACB.
*     -  Obtain an index to the data object entry in the DCB.
*     -  See if the ARY_ system identifier for the variance array is
*     valid. If not, then the array does not exist and must be created.
*     -  Obtain the NDF's bounds from its data array.
*     -  Obtain a placeholder for the new variance component and then
*     create a new array using the default attributes stored in the
*     DCB.
*     -  If the default storage form was not recognised, then report an
*     error.
*     -  Loop to identify all the ACB entries which refer to the data
*     object, selecting those with the correct DCB index.
*     -  Create a section from the variance array which matches the
*     ACB's data array section and store the resulting ARY_ system
*     identifier in the ACB.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     8-DEC-1989 (RFWS):
*        Installed the NDF1_VIMP routine.
*     15-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     20-JAN-1999 (RFWS):
*        Call NDF1_SSDUP to eliminate problems with ARY_SSECT when the
*        dimensionality of a section differs from that of the base
*        array.
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
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_VCPX( NDF__MXDCB ) = LOGICAL (Read)
*           Whether the NDF's variance component holds complex data.
*        DCB_VFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Read)
*           The default form of array used to store data in the NDF's
*           variance component.
*        DCB_VID( NDF__MXDCB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the NDF's variance array.
*        DCB_VTYP( NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP ) (Read)
*           Default numeric data type of the NDF's variance component.

         INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_VID( NDF__MXACB ) = INTEGER (Write)
*           ARY_ system identifier for the NDF's variance array.

*  Arguments Given:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACBT              ! ACB entry to test
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IDCBT              ! DCB index to test
      INTEGER LBND( NDF__MXDIM ) ! NDF lower pixel index bounds
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NEXT               ! Next ACB entry to test
      INTEGER PLACE              ! Placeholder for variance array
      INTEGER UBND( NDF__MXDIM ) ! NDF upper pixel index bounds
      LOGICAL THERE              ! Whether the variance array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that variance information is available in the DCB and ACB.
      CALL NDF1_VIMP( IACB, STATUS )

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  See if the ARY_ system identifier for the variance array is valid. If
*  not, then the array does not exist and must be created.
      CALL ARY_VALID( DCB_VID( IDCB ), THERE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( .NOT. THERE ) THEN

*  Obtain the NDF bounds from its data array.
            CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM, LBND, UBND,
     :                      NDIM, STATUS )

*  Obtain a placeholder for the variance array, then handle the
*  creation of each form of array in turn using the default attributes
*  stored in the DCB.
            CALL ARY_PLACE( DCB_LOC( IDCB ), 'VARIANCE', PLACE, STATUS )

*  Primitive array.
*  ===============
            IF ( DCB_VFRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN
               CALL ARY_NEWP( DCB_VTYP( IDCB ), NDIM, UBND, PLACE,
     :                        DCB_VID( IDCB ), STATUS )

*  Simple array.
*  ============
            ELSE IF ( DCB_VFRM( IDCB ) .EQ. 'SIMPLE' ) THEN
               IF ( DCB_VCPX( IDCB ) ) THEN
                  CALL ARY_NEW( 'COMPLEX_' // DCB_VTYP( IDCB ), NDIM,
     :                          LBND, UBND, PLACE, DCB_VID( IDCB ),
     :                          STATUS )
               ELSE
                  CALL ARY_NEW( DCB_VTYP( IDCB ), NDIM, LBND, UBND,
     :                          PLACE, DCB_VID( IDCB ), STATUS )
               END IF

*  If the default variance storage form entry in the DCB was not
*  recognised, then report an error.
            ELSE
               STATUS = NDF__FATIN
               CALL MSG_SETC( 'BADFORM', DCB_VFRM( IDCB ) )
               CALL ERR_REP( 'NDF1_VCRE_FORM',
     :         'Invalid array storage form ''^BADFORM'' encountered ' //
     :         'in the NDF_ system Data Control Block (internal ' //
     :         'programming error).', STATUS )
            END IF

*  Loop to identify all the ACB entries which refer to this DCB entry.
            NEXT = 0
            IACBT = 0
1           CONTINUE             ! Start of 'DO WHILE' loop
            CALL NDF1_NXTSL( NDF__ACB, IACBT, NEXT, STATUS )
            IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NEXT .NE. 0 ) ) THEN
               IACBT = NEXT

*  Select those entries with the correct DCB index.
               IDCBT = ACB_IDCB( IACBT )
               IF ( IDCBT .EQ. IDCB ) THEN

*  Create a section from the variance array which matches the ACB's data
*  array section and store the resulting ARY_ system identifier in the
*  ACB.
                  CALL NDF1_SSDUP( DCB_VID( IDCB ), ACB_DID( IACBT ),
     :                             ACB_VID( IACBT ), STATUS )
               END IF
               GO TO 1
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VCRE', STATUS )

      END
