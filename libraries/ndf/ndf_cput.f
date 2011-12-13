      SUBROUTINE NDF_CPUT( VALUE, INDF, COMP, STATUS )
*+
*  Name:
*     NDF_CPUT

*  Purpose:
*     Assign a value to an NDF character component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_CPUT( VALUE, INDF, COMP, STATUS )

*  Description:
*     The routine assigns a value to the specified character component
*     of an NDF (i.e. to the LABEL, TITLE or UNITS component). Any
*     previous value is over-written.

*  Arguments:
*     VALUE = CHARACTER * ( * ) (Given)
*        The value to be assigned.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the character component whose value is to be assigned:
*        'LABEL', 'TITLE' or 'UNITS'.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The entire VALUE string (including trailing blanks if present)
*     is assigned to the specified component, whose length is adjusted
*     to accommodate it.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Validate the component name.
*     -  Check that WRITE access to the NDF is available.
*     -  Obtain an index to the data object entry in the DCB and ensure
*     that information about the required character component is
*     available in the DCB.
*     -  If the component is already present in the NDF, then determine
*     its length.
*     -  If the length does not match that of the value to be assigned,
*     then annul the component's DCB locator and erase the component.
*     -  If the component does not (now) exist, then create a new one
*     with the required length.
*     -  Assign the value.

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
*     DSB: David S. Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1989 (RFWS):
*        Original version.
*     7-FEB-1990 (RFWS):
*        Changed the argument order.
*     26-JUN-2008 (DSB):
*        - Check for zero length VALUE, and  use a single space instead.
*        - Include component name in the final context message.
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

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_CCN( NDF__MXCCN ) = CHARACTER * ( DAT__SZNAM ) (Read)
*           Names of NDF character components.
*        DCB_CLOC( NDF__MXCCN, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC
*        ) (Read and Write)
*           Locators to NDF character components.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXDCB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      CHARACTER * ( * ) VALUE
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER ICCOMP             ! Identifier for character component
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER L                  ! Length of character component
      INTEGER LV                 ! Used length of VALUE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Validate the component name.
      CALL NDF1_VCCN( COMP, ICCOMP, STATUS )

*  Check that WRITE access to the NDF is available.
      CALL NDF1_CHACC( IACB, 'WRITE', STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Ensure that information about the required character component is
*  available in the DCB.
         CALL NDF1_DC( IDCB, ICCOMP, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Get the length of the VALUE string. If it has zero length we will
*  use a single space instead.
            LV = LEN( VALUE )

*  If the component is already present in the NDF, then determine its
*  length.
            IF ( DCB_CLOC( ICCOMP, IDCB ) .NE. DAT__NOLOC ) THEN
               CALL DAT_LEN( DCB_CLOC( ICCOMP, IDCB ), L, STATUS )

*  If the length does not match that of the value to be assigned, then
*  annul the component's locator and erase the component.
               IF ( L .NE. MAX( LV, 1 ) ) THEN
                  CALL DAT_ANNUL( DCB_CLOC( ICCOMP, IDCB ), STATUS )
                  CALL DAT_ERASE( DCB_LOC( IDCB ), DCB_CCN( ICCOMP ),
     :                            STATUS )
               END IF
            END IF

*  If the component does not (now) exist, then create a new one with the
*  required length.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( DCB_CLOC( ICCOMP, IDCB ) .EQ. DAT__NOLOC ) THEN
                  CALL DAT_NEW0C( DCB_LOC( IDCB ), DCB_CCN( ICCOMP ),
     :                            MAX( LV, 1 ), STATUS )

*  Obtain a locator to the new component.
                  CALL DAT_FIND( DCB_LOC( IDCB ), DCB_CCN( ICCOMP ),
     :                           DCB_CLOC( ICCOMP, IDCB ), STATUS )
               END IF

*  Assign the value.
               IF( LV .GT. 0 ) THEN
                  CALL DAT_PUT0C( DCB_CLOC( ICCOMP, IDCB ), VALUE,
     :                            STATUS )
               ELSE
                  CALL DAT_PUT0C( DCB_CLOC( ICCOMP, IDCB ), ' ',
     :                            STATUS )
               END IF

            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'C', COMP )
         CALL ERR_REP( 'NDF_CPUT_ERR',
     :   'NDF_CPUT: Error assigning a value to NDF character ' //
     :   'component ''^C''.', STATUS )
         CALL NDF1_TRACE( 'NDF_CPUT', STATUS )
      END IF

      END
