      SUBROUTINE ARY1_DMOD( IDCB, STATUS )
*+
*  Name:
*     ARY1_DMOD

*  Purpose:
*     Ensure that access mode information is available for a data
*     object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DMOD( IDCB, STATUS )

*  Description:
*     The routine ensures that information about the access mode of an
*     array is available. It does nothing if this information is already
*     present in the DCB. Otherwise, it obtains the information by
*     inspecting the data object itself and stores the result in the
*     DCB. Only those checks needed to obtain the access mode
*     information are performed on the data object.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to data object entry in DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Do nothing if access mode information is already available in
*     the DCB.
*     -  Ensure that form information is available for the array.
*     -  Handle each form of array in turn.
*     -  For primitive and simple arrays, ensure that data type
*     information, component locators and bounds information are
*     available.
*     -  Locate the first pixel in the non-imaginary data component.
*     -  Select an access mode which will not conflict with the
*     component's HDS state.
*     -  Map the first pixel. If the mapping succeeds, then UPDATE
*     access is available; unmap and reset the component's HDS state.
*     -  If the mapping fails, then only READ access is available;
*     annul the error.
*     -  Clean up.
*     -  If the form information in the DCB is invalid, then report an
*     error.
*     -  Note whether access mode information is now available in the
*     DCB.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1989 (RFWS):
*        Original version.
*     16-AUG-1989 (RFWS):
*        Changed initialisation of locators to use global constant.
*     24-AUG-1989 (RFWS):
*        Tidied the code a bit.
*     18-SEP-1989 (RFWS):
*        Changed DAT_UNMAP call to ARY1_HUNMP, to ensure unmapping
*        under error conditions.
*     12-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes
      INCLUDE 'DAT_ERR'          ! HDS error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to non-imaginary array component.
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           The form of the array.
*        DCB_KMOD( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether the access mode information is up to date.
*        DCB_MOD( ARY__MXDCB ) = CHARACTER * ( ARY__SZMOD ) (Write)
*           The array access mode.
*        DCB_NDIM( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read)
*           Number of array dimensions.
*        DCB_TYP( ARY__MXDCB ) = CHARACTER * ( DAT__SZTYP ) (Read)
*           Array data type.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( DAT__SZLOC ) LOCCEL ! Locator to first pixel
      CHARACTER * ( DAT__SZMOD ) MAP ! Mode for mapping first pixel
      INTEGER DIMCEL( ARY__MXDIM ) ! First pixel position
      INTEGER DUMMY( 1 )         ! Dummy dimension array
      INTEGER I                  ! Loop counter for dimensions
      INTEGER PNTR               ! Pointer to first pixel
      LOGICAL SET                ! HDS state of component

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do nothing if access mode information is already available in the
*  DCB.
      IF ( .NOT. DCB_KMOD( IDCB ) ) THEN

*  Ensure that form information is available for the data object.
         CALL ARY1_DFRM( IDCB, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  Primitive and simple arrays.
*  ===========================
*  These are both handled in the same way.
         IF ( ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) .OR.
     :        ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' ) ) THEN

*  Ensure that type (and complexity) information, component locators and
*  array bounds information are available.
            CALL ARY1_DTYP( IDCB, STATUS )
            CALL ARY1_DBND( IDCB, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  Obtain a locator to the first pixel in the non-imaginary array
*  component.
            DO 1 I = 1, DCB_NDIM( IDCB )
               DIMCEL( I ) = 1
1           CONTINUE
            LOCCEL = ARY__NOLOC
            CALL DAT_CELL( DCB_DLOC( IDCB ), DCB_NDIM( IDCB ), DIMCEL,
     :                     LOCCEL, STATUS )

*  Obtain the component's HDS state and select an access mode for
*  mapping the first pixel which will not conflict with this.
            CALL DAT_STATE( DCB_DLOC( IDCB ), SET, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( SET ) THEN
                  MAP = 'UPDATE'
               ELSE
                  MAP = 'WRITE'
               END IF

*  Mark the error stack and map the first pixel.
               CALL ERR_MARK
               CALL DAT_MAP( LOCCEL, DCB_TYP( IDCB ), MAP, 0, DUMMY,
     :                       PNTR, STATUS )

*  If the mapping succeeded, then UPDATE access is available. Unmap the
*  first pixel and restore the component's HDS state.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DCB_MOD( IDCB ) = 'UPDATE'
                  CALL ARY1_HUNMP( LOCCEL, STATUS )
                  IF( .NOT. SET ) CALL DAT_RESET( DCB_DLOC( IDCB ),
     :                                            STATUS )

*  If mapping failed with an access conflict, then the array is READ
*  protected. Annul the error.
               ELSE IF ( STATUS .EQ. DAT__ACCON ) THEN
                  DCB_MOD( IDCB ) = 'READ'
                  CALL ERR_ANNUL( STATUS )
               END IF

*  Release the error stack.
               CALL ERR_RLSE
            END IF

*  Annul the locator to the first pixel.
            CALL DAT_ANNUL( LOCCEL, STATUS )
            LOCCEL = ARY__NOLOC

*  If the form information in the DCB is not valid, then report an
*  error.
         ELSE
            STATUS = ARY__FATIN
            CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
            CALL ERR_REP( 'ARY1_DMOD_FRM',
     :      'Unsupported array form ''^BADFORM'' found in Data ' //
     :      'Control Block (internal programming error).', STATUS )
         END IF

*  Note if access mode information is now available in the DCB.
9999     CONTINUE
         DCB_KMOD( IDCB ) = STATUS .EQ. SAI__OK
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DMOD', STATUS )

      END
