      SUBROUTINE ARY1_DRST( IDCB, STATUS )
*+
*  Name:
*     ARY1_DRST

*  Purpose:
*     Reset the state of a data object to "undefined".

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DRST( IDCB, STATUS )

*  Description:
*     The routine sets the HDS state of the data component(s) of an
*     array to "undefined" and reflects this change in the data
*     object's DCB entry.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Ensure that form information is available in the DCB.
*     -  Handle each form of array in turn.
*     -  For primitive arrays, ensure that type information and data
*     component locators are available in the DCB and reset the HDS
*     state of the data component.
*     -  For simple arrays, ensure that type information and data
*     component locators are available in the DCB and reset the HDS
*     state of the data component(s).
*     -  If the DCB form information was not recognised, then report an
*     error.
*     -  Reset the DCB state entry and indicate whether this new value
*     is valid.

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
*     DSB: David S. Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     17-JUL-1989 (RFWS):
*        Original version.
*     31-JUL-1989 (RFWS):
*        Changed operation of routine so it always resets the HDS
*        components, even if the DCB information indicates the array is
*        already undefined. This is to cope with the case where only
*        one component of a complex array has an HDS undefined state
*        when it is imported. A subsequent reset will now ensure the
*        other component is reset as well.
*     18-SEP-1989 (RFWS):
*        Added support for the DCB_INIT array.
*     12-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     5-MAY-2006 (DSB):
*        Installed support for scaled arrays.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_CPX( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the data object is complex.
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to the non-imaginary data component.
*        DCB_FRM( ARY_MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           The form of the data object.
*        DCB_ILOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to the imaginary data component.
*        DCB_INIT( ARY__MXDCB ) = LOGICAL (Write)
*           Whether the data object's values have been initialised.
*        DCB_KSTA( ARY__MXDCB ) = LOGICAL (Write)
*           Whether the DCB state entry is valid.
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Write)
*           Whether the data object is in the "defined" state.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that form information is available in the DCB.
      CALL ARY1_DFRM( IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Primitive arrays.
*  ================
         IF ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN

*  Ensure that data type information and component locators are
*  available in the DCB.
            CALL ARY1_DTYP( IDCB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Reset the state of the HDS data component.
               CALL DAT_RESET( DCB_DLOC( IDCB ), STATUS )
            END IF

*  Simple and scaled arrays.
*  =========================
         ELSE IF ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' .OR.
     :             DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN

*  Ensure that data type information and component locators are
*  available in the DCB.
            CALL ARY1_DTYP( IDCB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Reset the state of the HDS data component(s).
               CALL DAT_RESET( DCB_DLOC( IDCB ), STATUS )
               IF ( DCB_CPX( IDCB ) ) THEN
                  CALL DAT_RESET( DCB_ILOC( IDCB ), STATUS )
               END IF
            END IF

*  If the array form was not recognised, then report an error.
         ELSE
            STATUS = ARY__FATIN
            CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
            CALL ERR_REP( 'ARY1_DRST_FORM',
     :      'Unsupported array form ''^BADFORM'' found in ' //
     :      'Data Control Block (internal programming error).',
     :      STATUS )
         END IF
      END IF

*  Reset the data object's DCB state and initialisation entry and note
*  whether the information is valid.
      DCB_STA( IDCB ) = .FALSE.
      DCB_INIT( IDCB ) = .FALSE.
      DCB_KSTA( IDCB ) = STATUS .EQ. SAI__OK
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DRST', STATUS )

      END
