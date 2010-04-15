      SUBROUTINE ARY1_DVFY( IDCB, STATUS )
*+
*  Name:
*     ARY1_DVFY

*  Purpose:
*     Verify that a data object is correctly constructed.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DVFY( IDCB, STATUS )

*  Description:
*     The routine checks that a data object, identified by its DCB
*     entry, is correctly constructed and contains no "rogue"
*     components. An error is reported if a problem is found,
*     otherwise the routine returns without further action.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Reset the DCB flag and obtain fresh form information for the
*     array.
*     -  Handle each form of array in turn.
*     -  For primitive arrays, clear all relevant DCB flags and annul
*     associated locators.
*     -  Get new information about the data object, causing all
*     associated checking to be performed. If this stage completes
*     successfully, then the data object is correctly constructed.
*     -  For simple arrays, reset all relevant DCB flags and annul
*     associated locators.  Obtain new information about the data
*     object, causing all associated checks to be performed.
*     -  Find the number of components in the array structure and loop
*     through each one.
*     -  Check the name of each component found and report an error if
*     it is not one of the permitted component names.

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
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     24-AUG-1989 (RFWS):
*        Original version.
*     13-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     9-MAR-1990 (RFWS):
*        Added annulling of locators associated with DCB flags.
*     8-MAY-2006 (DSB):
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
*        DCB_KBAD( ARY__MXDCB ) = LOGICAL (Write)
*           Whether bad pixel flag information is available in the DCB.
*        DCB_KBND( ARY__MXDCB ) = LOGICAL (Write)
*           Whether array bounds information is available in the DCB.
*        DCB_KFRM( ARY__MXDCB ) = LOGICAL (Write)
*           Whether form information is available in the DCB.
*        DCB_KMOD( ARY__MXDCB ) = LOGICAL (Write)
*           Whether access mode information is available in the DCB.
*        DCB_KSTA( ARY__MXDCB ) = LOGICAL (Write)
*           Whether array state information is available in the DCB.
*        DCB_KTYP( ARY__MXDCB ) = LOGICAL (Write)
*           Whether data type information is available in the DCB.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Component locator
      CHARACTER * ( DAT__SZNAM ) NAME ! Component name
      INTEGER ICOMP              ! Loop counter for array components
      INTEGER NCOMP              ! Number of array components

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Clear the DCB flag and obtain fresh form information for the array.
      DCB_KFRM( IDCB ) = .FALSE.
      CALL ARY1_DFRM( IDCB, STATUS )

*  Handle each form of array in turn.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Primitive arrays.
*  ================
         IF ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN

*  Clear all relevant DCB flags and annul associated locators.
            IF ( DCB_KTYP( IDCB ) ) THEN

*  Annul the non-imaginary component locator.
               CALL DAT_ANNUL( DCB_DLOC( IDCB ), STATUS )
               DCB_DLOC( IDCB ) = ARY__NOLOC
               DCB_KTYP( IDCB ) = .FALSE.
            END IF
            DCB_KBND( IDCB ) = .FALSE.
            DCB_KBAD( IDCB ) = .FALSE.
            DCB_KSTA( IDCB ) = .FALSE.
            DCB_KMOD( IDCB ) = .FALSE.

*  Get new information about the data object, causing all associated
*  checking to be performed. If this stage completes successfully, then
*  the data object is correctly constructed.
            CALL ARY1_DTYP( IDCB, STATUS )
            CALL ARY1_DBND( IDCB, STATUS )
            CALL ARY1_DBAD( IDCB, STATUS )
            CALL ARY1_DSTA( IDCB, STATUS )
            CALL ARY1_DMOD( IDCB, STATUS )

*  Simple and scaled arrays.
*  =========================
         ELSE IF ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' .AND.
     :             DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN

*  Clear all relevant DCB flags and annul associated locators.
            IF ( DCB_KTYP( IDCB ) ) THEN

*  Annul the non-imaginary component locator (and the imaginary
*  component locator, if present).
               CALL DAT_ANNUL( DCB_DLOC( IDCB ), STATUS )
               DCB_DLOC( IDCB ) = ARY__NOLOC
               IF ( DCB_CPX( IDCB ) ) THEN
                  CALL DAT_ANNUL( DCB_ILOC( IDCB ), STATUS )
                  DCB_ILOC( IDCB ) = ARY__NOLOC
               END IF
               DCB_KTYP( IDCB ) = .FALSE.
            END IF
            DCB_KBND( IDCB ) = .FALSE.
            DCB_KBAD( IDCB ) = .FALSE.
            DCB_KSTA( IDCB ) = .FALSE.
            DCB_KMOD( IDCB ) = .FALSE.
            DCB_KSCL( IDCB ) = .FALSE.

*  Get new information about the data object, causing all associated
*  checking to be performed. If this stage completes successfully, then
*  the data object is correctly constructed, although it may still
*  contain rogue components.
            CALL ARY1_DTYP( IDCB, STATUS )
            CALL ARY1_DBND( IDCB, STATUS )
            CALL ARY1_DBAD( IDCB, STATUS )
            CALL ARY1_DSTA( IDCB, STATUS )
            CALL ARY1_DMOD( IDCB, STATUS )
            CALL ARY1_DSCL( IDCB, STATUS )

*  Obtain the number of components in the array structure.
            CALL DAT_NCOMP( DCB_LOC( IDCB ), NCOMP, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop to examine each component.
               DO 1 ICOMP = 1, NCOMP

*  Locate the component and obtain its name, then annul its locator.
                  LOC = ARY__NOLOC
                  CALL DAT_INDEX( DCB_LOC( IDCB ), ICOMP, LOC, STATUS )
                  CALL DAT_NAME( LOC, NAME, STATUS )
                  CALL DAT_ANNUL( LOC, STATUS )
                  LOC = ARY__NOLOC
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  Test the name against all the permitted component names.
                     IF ( ( NAME .NE. 'VARIANT' ) .AND.
     :                    ( NAME .NE. 'DATA' ) .AND.
     :                    ( NAME .NE. 'IMAGINARY_DATA' ) .AND.
     :                    ( NAME .NE. 'ORIGIN' ) .AND.
     :                    ( NAME .NE. 'BAD_PIXEL' ) ) THEN

*  Report an error if a rogue component is found.
                        STATUS = ARY__ROGUE
                        CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                        CALL MSG_SETC( 'NAME', NAME )
                        CALL ERR_REP( 'ARY1_DVFY_ROGE',
     :                  'The array structure ^ARRAY contains a ' //
     :                  'rogue ^NAME component.', STATUS )
                     END IF
                  END IF

*  Quit examining components as soon as an error occurs.
                  IF ( STATUS .NE. SAI__OK ) GO TO 2
1              CONTINUE
2              CONTINUE
            END IF

*  If the DCB form entry was not recognised, then report an error.
         ELSE
            STATUS = ARY__FATIN
            CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
            CALL ERR_REP( 'ARY1_DVFY_FORM',
     :      'Unsupported array form ''^BADFORM'' found in Data ' //
     :      'Control Block (internal programming error).', STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DVFY', STATUS )

      END
