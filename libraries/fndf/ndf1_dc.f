      SUBROUTINE NDF1_DC( IDCB, ICCOMP, STATUS )
*+
*  Name:
*     NDF1_DC

*  Purpose:
*     Ensure that character component information is available in the
*     DCB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DC( IDCB, ICCOMP, STATUS )

*  Description:
*     The routine ensures that information about the specified NDF
*     character component is available in the DCB. It does nothing if
*     this information is already available. Otherwise, it inspects the
*     actual data object to obtain this information and enters it into
*     the DCB, performing necessary validation checks in the process.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the DCB entry for which information is required.
*     ICCOMP = INTEGER (Given)
*        Identifier for the character component for which information is
*        required; one of the symbolic constants NDF__LABEL, NDF__TITLE
*        or NDF__UNITS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Check whether the required information is already available.
*     There is nothing to do if it is.
*     -  If not, then initialise the DCB character component locator
*     and see if the component is present in the data object.
*     -  If present, then obtain a locator to it, storing this in the
*     DCB.
*     -  Determine the component's data type and shape, reporting an
*     error if it does not have a character type or is not scalar.
*     -  If the component is not suitable, then annul its DCB locator.
*     -  Note whether DCB information for the character component is
*     now available.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     21-SEP-1989 (RFWS):
*        Original version.
*     26-SEP-1989 (RFWS):
*        Corrected error in IF block structure.
*     27-NOV-1989 (RFWS):
*        Changed incorrect use of NDF__DIMIN constant to NDF__NDMIN.
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
*        DCB_CCN( NDF__MXCCN ) = CHARACTER * ( DAT__SZNAM ) (Read)
*           NDF character component names.
*        DCB_CLOC( NDF__MXCCN, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC
*        ) (Write)
*           Locators to NDF character components.
*        DCB_KC( NDF__MXCCN, NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether character component information is available in the
*           DCB.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

*  Arguments Given:
      INTEGER IDCB
      INTEGER ICCOMP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZTYP ) TYPE ! Component data type
      INTEGER DIM( DAT__MXDIM )  ! Component dimensions
      INTEGER NDIM               ! Number of component dimensions
      LOGICAL THERE              ! Whether component is present in NDF

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check whether the required information is already available. There is
*  nothing to do if it is.
      IF ( .NOT. DCB_KC( ICCOMP, IDCB ) ) THEN

*  If not, then initialise the DCB character component locator and see
*  if the component is present in the data object.
         DCB_CLOC( ICCOMP, IDCB ) = DAT__NOLOC
         CALL DAT_THERE( DCB_LOC( IDCB ), DCB_CCN( ICCOMP ), THERE,
     :                   STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If present, then obtain a locator to it, storing this in the DCB.
            IF ( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB ), DCB_CCN( ICCOMP ),
     :                        DCB_CLOC( ICCOMP, IDCB ), STATUS )

*  Determine the component's data type and shape.
               CALL DAT_TYPE( DCB_CLOC( ICCOMP, IDCB ), TYPE, STATUS )
               CALL DAT_SHAPE( DCB_CLOC( ICCOMP, IDCB ), DAT__MXDIM,
     :                         DIM, NDIM, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Report an error if it does not have a character data type.
                  IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                     STATUS = NDF__TYPIN
                     CALL MSG_SETC( 'CCOMP', DCB_CCN( ICCOMP ) )
                     CALL NDF1_DMSG( 'NDF', IDCB )
                     CALL MSG_SETC( 'BADTYPE', TYPE )
                     CALL ERR_REP( 'NDF1_DC_TYPE',
     :               'The ^CCOMP component in the NDF structure ' //
     :               '^NDF has an invalid HDS type of ''^BADTYPE''; ' //
     :               'it should be of type ''_CHAR''.', STATUS )

*  Report an error if it is not scalar.
                  ELSE IF ( NDIM .NE. 0 ) THEN
                     STATUS = NDF__NDMIN
                     CALL MSG_SETC( 'CCOMP', DCB_CCN( ICCOMP ) )
                     CALL NDF1_DMSG( 'NDF', IDCB )
                     CALL MSG_SETI( 'BADNDIM', NDIM )
                     CALL ERR_REP( 'NDF1_DC_NDIM',
     :               'The ^CCOMP component in the NDF structure ' //
     :               '^NDF is ^BADNDIM-dimensional; it should be ' //
     :               'scalar.', STATUS )
                  END IF
               END IF

*  If the component is not suitable, then annul its DCB locator.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL DAT_ANNUL( DCB_CLOC( ICCOMP, IDCB ), STATUS )
               END IF
            END IF
         END IF

*  Note whether DCB information is now available for this character
*  component.
         DCB_KC( ICCOMP, IDCB ) = STATUS .EQ. SAI__OK
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DC', STATUS )

      END
