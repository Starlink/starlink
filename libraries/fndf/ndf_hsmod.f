      SUBROUTINE NDF_HSMOD( HMODE, INDF, STATUS )
*+
*  Name:
*     NDF_HSMOD

*  Purpose:
*     Set the history update mode for an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HSMOD( HMODE, INDF, STATUS )

*  Description:
*     The routine sets the mode to be used for updating the history
*     component of an NDF. This allows control over the amount of
*     history information subsequently recorded. It also allows history
*     recording to be disabled completely.

*  Arguments:
*     HMODE = CHARACTER * ( * ) (Given)
*        The history update mode required: 'DISABLED', 'QUIET',
*        'NORMAL' or 'VERBOSE'. This value may be abbreviated, to no
*        less than three characters. In addition, 'SKIP' may be supplied.
*        This is similar to 'DISABLED', in that no history record will
*        be added to the NDF when the NDF is closed. However, 'SKIP' makes
*        no permanent change to the update mode - the next time the NDF
*        is accessed it will have its original update mode.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     18-MAY-1993 (RFWS):
*        Original version.
*     4-JUL-2008 (DSB):
*        Added SKIP mode.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for NDF history component.
*        DCB_HUMOD( NDF__MXDCB ) = INTEGER (Write)
*           History recording update mode.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      CHARACTER * ( * ) HMODE
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String comparison with abbreviation

*  Local Variables:
      CHARACTER * ( NDF__SZHUM ) MODE ! Required mode
      CHARACTER * ( NDF__SZHUM ) TEXT ! UPDATE_MODE value
      INTEGER DIM( 1 )           ! Dummy dimension size array
      INTEGER HUM                ! History update mode code
      INTEGER IACB               ! Index of NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER NC                 ! Number of characters in TEXT
      LOGICAL SKIP               ! Was SKIP supplied?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If the history update mode is SKIP, set a flag and use DISABLED instead.
         SKIP = NDF1_SIMLR( HMODE, 'SKIP', 3 )
         IF( SKIP ) THEN
            MODE = 'DISABLED'
         ELSE
            MODE = HMODE
         END IF

*  Check that WRITE access to the NDF is available and validate the
*  history update mode string.
         CALL NDF1_CHACC( IACB, 'WRITE', STATUS )
         CALL NDF1_CHHUM( MODE, HUM, STATUS )

*  Obtain an index to the data object entry in the DCB and ensure that
*  DCB history information is available.
         IDCB = ACB_IDCB( IACB )
         CALL NDF1_DH( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that a history component is present and report an error if it
*  is not.
            IF ( DCB_HLOC( IDCB ) .EQ. DAT__NOLOC ) THEN
               STATUS = NDF__NOHIS
               CALL NDF1_DMSG( 'NDF', IDCB )
               CALL ERR_REP( 'NDF_HSMOD_NOHIS',
     :                       'There is no history component present ' //
     :                       'in the NDF structure ^NDF (possible ' //
     :                       'programming error).', STATUS )

*  Otherwise, test for each update mode value and assign the
*  appropriate UPDATE_MODE component string.
            ELSE IF ( HUM .EQ. NDF__HDISA ) THEN
               TEXT = 'DISABLED'
               NC = 8
            ELSE IF ( HUM .EQ. NDF__HQUIE ) THEN
               TEXT = 'QUIET'
               NC = 5
            ELSE IF ( HUM .EQ. NDF__HNORM ) THEN
               TEXT = 'NORMAL'
               NC = 6
            ELSE IF ( HUM .EQ. NDF__HVERB ) THEN
               TEXT = 'VERBOSE'
               NC = 7

*  If the update mode is not recognised, then report an error.
            ELSE
               STATUS = NDF__FATIN
               CALL MSG_SETI( 'HUM', HUM )
               CALL ERR_REP( 'NDF_HSMOD_HUM',
     :                       'Invalid history update mode code ' //
     :                       '(^HUM) encountered (internal ' //
     :                       'programming error).', STATUS )
            END IF

*  Ensure that an UPDATE_MODE component of the required type and shape
*  exists in the NDF history structure and write the new value to it.
*  Skip this bit if SKIP was supplied, so that no permanent change is made
*  to the NDF.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF( .NOT. SKIP ) THEN
                  CALL CMP_MODC( DCB_HLOC( IDCB ), 'UPDATE_MODE', NC,
     :                           0, DIM, STATUS )
                  CALL CMP_PUT0C( DCB_HLOC( IDCB ), 'UPDATE_MODE',
     :                            TEXT( : NC ), STATUS )
               END IF

*  Modify the corresponding DCB update mode value.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DCB_HUMOD( IDCB ) = HUM
               END IF
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_HSMOD_ERR',
     :   'NDF_HSMOD: Error setting the history update mode for an ' //
     :   'NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_HSMOD', STATUS )
      END IF

      END
