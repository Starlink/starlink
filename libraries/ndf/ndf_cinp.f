      SUBROUTINE NDF_CINP( PARAM, INDF, COMP, STATUS )
*+
*  Name:
*     NDF_CINP

*  Purpose:
*     Obtain an NDF character component value via the ADAM parameter
*     system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_CINP( PARAM, INDF, COMP, STATUS )

*  Description:
*     The routine obtains a new value for a character component of an
*     NDF via the ADAM parameter system and uses it to replace any
*     pre-existing value of that component in the NDF.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the character component for which a value is to be
*        obtained: 'LABEL', 'TITLE' or 'UNITS'.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A "null" parameter value is interpreted as indicating that no
*     new value should be set for the character component. In this
*     event, the routine will return without action (and without
*     setting a STATUS value). A suitable default value for the
*     character component should therefore be established before this
*     routine is called.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Validate the component name.
*     -  Check that WRITE access to the NDF is available.
*     -  Mark the error stack and obtain a character value for the NDF
*     component via the ADAM parameter system.
*     -  If a "null" value is obtained, then annul the error and take
*     no further action.
*     -  If an "abort" was requested, then annul any error message and
*     issue an appropriate new one.
*     -  If a suitable value was obtained, then get an index to the
*     data object entry in the DCB.
*     -  Ensure that information about the required character component
*     is available in the DCB.
*     -  Obtain the length in characters of the value to be assigned.
*     -  If the component is already present in the NDF, then determine
*     its length.
*     -  If the length does not match that of the value to be assigned,
*     then annul the component's locator and erase the component.
*     -  If the component does not (now) exist, then create a new one
*     with the required length.
*     -  Obtain a locator to the new component.
*     -  Assign the value.
*     -  Release the error stack.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     20-FEB-1990 (RFWS):
*        Original, derived from the NDF_CPUT routine.
*     4-DEC-1991 (RFWS):
*        Removed a temporary fix which reported an error. This is now
*        done by lower-level facilities such as the parameter system
*        and HDS.
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
      INCLUDE 'PAR_ERR'          ! PAR_ error codes

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
      CHARACTER * ( * ) PARAM
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( NDF__SZPAR ) VALUE ! Value to be assigned
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER ICCOMP             ! Identifier for character component
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER L                  ! Length of character component
      INTEGER LVAL               ! Length of value to be assigned
      INTEGER TSTAT              ! Temporary status variable

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

*  Mark the error stack and obtain a character value for the NDF
*  component via the ADAM parameter system.
         CALL ERR_MARK
         VALUE = ' '
         CALL PAR_GET0C( PARAM, VALUE, STATUS )

*  If a "null" value is obtained, then annul the error and take no
*  further action.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

*  If an "abort" was requested, then annul any error message and issue
*  an appropriate new one.
         ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
            TSTAT = STATUS
            CALL ERR_ANNUL( TSTAT )
            CALL MSG_SETC( 'PARAM', PARAM )
            CALL ERR_REP( 'NDF_CINP_ABORT',
     :      'Aborted attempt to obtain an NDF character component ' //
     :      'value via the ''%^PARAM'' parameter.', STATUS )

*  If a suitable value was obtained, then get an index to the data
*  object entry in the DCB.
         ELSE
            IDCB = ACB_IDCB( IACB )

*  Ensure that information about the required character component is
*  available in the DCB.
            CALL NDF1_DC( IDCB, ICCOMP, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain the length in characters of the value to be assigned.
               LVAL = MAX( 1, CHR_LEN( VALUE ) )

*  If the component is already present in the NDF, then determine its
*  length.
               IF ( DCB_CLOC( ICCOMP, IDCB ) .NE. DAT__NOLOC ) THEN
                  CALL DAT_LEN( DCB_CLOC( ICCOMP, IDCB ), L, STATUS )

*  If the length does not match that of the value to be assigned, then
*  annul the component's locator and erase the component.
                  IF ( L .NE. LVAL ) THEN
                     CALL DAT_ANNUL( DCB_CLOC( ICCOMP, IDCB ), STATUS )
                     CALL DAT_ERASE( DCB_LOC( IDCB ), DCB_CCN( ICCOMP ),
     :                               STATUS )
                  END IF
               END IF

*  If the component does not (now) exist, then create a new one with the
*  required length.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( DCB_CLOC( ICCOMP, IDCB ) .EQ. DAT__NOLOC ) THEN
                     CALL DAT_NEW0C( DCB_LOC( IDCB ), DCB_CCN( ICCOMP ),
     :                               LVAL, STATUS )

*  Obtain a locator to the new component.
                     CALL DAT_FIND( DCB_LOC( IDCB ), DCB_CCN( ICCOMP ),
     :                              DCB_CLOC( ICCOMP, IDCB ), STATUS )
                  END IF

*  Assign the value.
                  CALL DAT_PUT0C( DCB_CLOC( ICCOMP, IDCB ),
     :                            VALUE( : LVAL ), STATUS )
               END IF
            END IF
         END IF

*  Release the error stack.
         CALL ERR_RLSE
      END IF

*  If an error occurred and it was not an "abort" request, then report
*  context information and call the error tracing routine.
      IF ( ( STATUS .NE. SAI__OK ) .AND.
     :     ( STATUS .NE. PAR__ABORT ) ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_CINP_ERR',
     :   'NDF_CINP: Error obtaining an NDF character component ' //
     :   'value via the ''%^PARAM'' parameter.', STATUS )
         CALL NDF1_TRACE( 'NDF_CINP', STATUS )
      END IF

      END
