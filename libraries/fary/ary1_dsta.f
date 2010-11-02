      SUBROUTINE ARY1_DSTA( IDCB, STATUS )
*+
*  Name:
*     ARY1_DSTA

*  Purpose:
*     Ensure that state information is available for a data object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DSTA( IDCB, STATUS )

*  Description:
*     The routine ensures that information about the state (i.e.
*     defined or undefined) of an array is available. It does nothing
*     if the state information is already available in the DCB.
*     Otherwise, it obtains this information by inspecting the data
*     object itself, storing the result in the DCB. Only those checks
*     needed for obtaining the state information are performed on the
*     data object.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index of the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Do nothing if state information is already available in the
*     DCB.
*     -  Ensure that form information is available for the data object.
*     -  Handle each form of array separately.
*     -  For primitive arrays, ensure that type (and complexity)
*     information and component locators are available.
*     -  Test the state of the non-imaginary component.
*     -  For simple arrays, ensure that type information and component
*     locators are available.
*     -  Test the state of the non-imaginary component.
*     -  If the non-imaginary component is defined and the array is
*     complex, then test the state of the imaginary component also.
*     -  If the form information in the DCB is not valid, then report an
*     error.
*     -  Note whether state information is now available in the DCB.
*     -  Note whether the data object's values have been initialised.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.
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
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1989 (RFWS):
*        Original version.
*     18-SEP-1989 (RFWS):
*        Added support for the DCB_INIT array.
*     13-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     5-MAY-2006 (DSB):
*        Installed support for scaled arrays.
*     29-OCT-2010 (DSB):
*        Include support for delta compressed arrays.
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

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Form of the data object.
*        DCB_INIT( ARY__MXDCB ) = LOGICAL (Write)
*           Whether the data object's values have been initialised.
*        DCB_KSTA( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether the DCB state information is up to date.
*        DCB_LOCD( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to non-imaginary array component.
*        DCB_LOCI( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to imaginary array component.
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Write)
*           Whether the data content of the array is defined.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do nothing if state information is already available in the DCB.
      IF ( .NOT. DCB_KSTA( IDCB ) ) THEN

*  Ensure that form information is available for the data object.
         CALL ARY1_DFRM( IDCB, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  Primitive arrays.
*  ================
         IF ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN

*  Ensure that type (and complexity) information and component locators
*  are available.
            CALL ARY1_DTYP( IDCB, STATUS )

*  Test the state of the non-imaginary component.
            CALL DAT_STATE( DCB_DLOC( IDCB ), DCB_STA( IDCB ), STATUS )

*  Simple, scaled and delta arrays.
*  ================================
         ELSE IF ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' .OR.
     :             DCB_FRM( IDCB ) .EQ. 'SCALED' .OR.
     :             DCB_FRM( IDCB ) .EQ. 'DELTA' ) THEN

*  Ensure that type (and complexity) information and component locators
*  are available.
            CALL ARY1_DTYP( IDCB, STATUS )

*  Test the state of the non-imaginary component.
            CALL DAT_STATE( DCB_DLOC( IDCB ), DCB_STA( IDCB ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  If the array is complex and the non-imaginary component is defined,
*  then test the imaginary component also (both must be defined if the
*  entire array is to be defined).
            IF ( DCB_CPX( IDCB ) .AND. DCB_STA( IDCB ) ) THEN
               CALL DAT_STATE( DCB_ILOC( IDCB ), DCB_STA( IDCB ),
     :         STATUS )
            END IF

*  If the form entry in the DCB is not valid, then report an error.
         ELSE
            STATUS = ARY__FATIN
            CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
            CALL ERR_REP( 'ARY1_DSTA_FRM',
     :      'Unsupported array form ''^BADFORM'' found in Data ' //
     :      'Control Block (internal programming error).', STATUS )
         END IF

*  Note if state information is now available in the DCB.
9999     CONTINUE
         DCB_KSTA( IDCB ) = STATUS .EQ. SAI__OK

*  Note if the data object's values have been initialised.
         IF ( STATUS .EQ. SAI__OK ) DCB_INIT( IDCB ) = DCB_STA( IDCB )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DSTA', STATUS )

      END
