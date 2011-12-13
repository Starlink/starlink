      PROGRAM REF_TEST
*+
*  Name:
*     REF_TEST

*  Purpose:
*     Test installation of REF.

*  Language:
*     Starlink Fortran

*  Description:
*     This program runs through the REF package using both good and bad
*     data in order to test it. If it succeeds, a success message is
*     output.

*  Copyright:
*     Copyright (C) 1992, 1993 Science & Engineering Research Council.
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
*     AJC: A.J. Chipperfield (STARLINK, RAL)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     19-JAN-88 (AJC):
*        Original version.
*     1-MAR-90 (AJC):
*        Add REF_ANNUL and HDS_SHOW outputs.
*     20-FEB-1992 (RFWS):
*        Adapted for portability.
*     6-JAN-1993 (RFWS):
*        Removed use of ERR/MSG in favour of EMS routines and WRITE
*        statements.
*     8-JAN-1993 (RFWS):
*        Reduced the level of output to make the test result easier to
*        interpret.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'DAT_ERR'          ! DAT_ error codes
      INCLUDE 'REF_PAR'          ! REF_ public constants
      INCLUDE 'REF_ERR'          ! REF_ error codes

*  Local Variables:
      CHARACTER * ( 40 ) PATH    ! Object pathname
      CHARACTER * ( DAT__SZLOC ) LOC1 ! Locator to "refobj" top level
      CHARACTER * ( DAT__SZLOC ) LOC2 ! Locator to "nonrefobj" top level
      CHARACTER * ( DAT__SZLOC ) LOCNREF ! Locator to nonrefobj.OBJREF
      CHARACTER * ( DAT__SZLOC ) LOCREF ! Locator to referenced object
      CHARACTER * ( DAT__SZLOC ) TLOC1 ! Temporary locator
      CHARACTER * ( REF__SZREF ) FILE ! Object file name
      INTEGER DIM( 1 )           ! Dummy dimension array
      INTEGER LEVEL              ! Final error context
      INTEGER LEVEL0             ! Initial error context
      INTEGER NLEV               ! Levels in pathname
      INTEGER STATUS             ! Global status

*.

*  Record the initial error context level.
      CALL EMS_LEVEL( LEVEL0 )

*  Initialise status and start HDS.
      STATUS = SAI__OK
      CALL HDS_START( STATUS )

*  Create file "refobj" and a component within it.
      CALL HDS_NEW( 'refobj', 'REFOBJ', 'STRUCTURE', 0, DIM, LOC1,
     :              STATUS )
      CALL DAT_NEW( LOC1, 'OBJECT', '_REAL', 0, DIM, STATUS )

*  Create nonrefobj.OBJREF - an object of the wrong type.
      CALL HDS_NEW( 'nonrefobj', 'NONREFOBJ', 'STRUCT', 0, DIM, LOC2,
     :              STATUS )
      CALL DAT_NEW( LOC2, 'OBJREF', 'NON_REFOBJ', 0, DIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Check REF_FIND on a non-reference object.
*  ========================================
*  Locate internal object.
      CALL REF_FIND( LOC1, 'OBJECT', 'READ', TLOC1, STATUS )

*  Locate nonrefobj.OBJREF.
      CALL REF_FIND( LOC2, 'OBJREF', 'READ', LOCNREF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Test internal references.
*  ========================
*  Create and put reference to refobj.OBJECT into refobj.OBJREF.
      CALL REF_CRPUT( LOC1, 'OBJREF', TLOC1, .TRUE., STATUS )
      CALL REF_ANNUL( TLOC1, STATUS )

*  Locate refobj.OBJECT.
      CALL REF_FIND( LOC1, 'OBJREF', 'READ', LOCREF, STATUS )

*  Check object found.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL HDS_TRACE( LOCREF, NLEV, PATH, FILE, STATUS )
         IF ( PATH .NE. 'REFOBJ.OBJECT' ) THEN
            STATUS = SAI__ERROR
            CALL EMS_REP( 'REF_TEST_ERR2',
     :                    '*** Wrong object located ***', STATUS )
         ENDIF
      ENDIF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Annul the locator and expect refcnt of "refobj" to decrease.
      CALL HDS_SHOW( 'FILES', STATUS )
      CALL REF_ANNUL( LOCREF, STATUS )
      WRITE( *, * )
      WRITE( *, * ) '      Expect REFCNT of "refobj" to decrease'
      CALL HDS_SHOW( 'FILES', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Test external references.
*  ========================
*  Create and put reference to nonrefobj.OBJECT into refobj.OBJREF.
      CALL REF_CRPUT( LOC1, 'OBJREF', LOCNREF, .FALSE., STATUS )

*  Locate refobj.OBJECT.
      CALL REF_FIND( LOC1, 'OBJREF', 'READ', LOCREF, STATUS )

*  Check object found.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL HDS_TRACE( LOCREF, NLEV, PATH, FILE, STATUS )
         IF ( PATH .NE. 'NONREFOBJ.OBJREF' ) THEN
            STATUS = SAI__ERROR
            CALL EMS_REP('REF_TEST_ERR3',
     :                   '*** Wrong object located ***', STATUS )
         ENDIF
      ENDIF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Annul the referenced locator.
      CALL HDS_SHOW( 'FILES', STATUS )
      CALL REF_ANNUL( LOCREF, STATUS )
      WRITE( *, * )
      WRITE( *, * ) '      Expect REFCNT of "nonrefobj" to decrease'
      CALL HDS_SHOW( 'FILES', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Try to put reference in non-reference object.
*  ============================================
      CALL EMS_MARK
      CALL REF_CRPUT( LOC2, 'OBJREF', LOC1, .FALSE., STATUS )
      IF ( STATUS .EQ. REF__OBJIN ) CALL EMS_ANNUL( STATUS )
      CALL EMS_RLSE
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Try to get reference from non-reference object.
*  ==============================================
      CALL EMS_MARK
      CALL REF_GET( LOCNREF, 'READ', TLOC1, STATUS )
      IF ( STATUS .EQ. REF__OBJIN ) CALL EMS_ANNUL( STATUS )
      CALL EMS_RLSE
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Prepare for tests with missing objects. Erase "nonrefobj".
      CALL REF_ANNUL( LOCNREF, STATUS )
      CALL HDS_ERASE( LOC2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Try to get a locator to nonrefobj.OBJREF via refobj.OBJREF which has
*  ====================================================================
*  just been erased.
*  ================
      CALL EMS_MARK
      CALL REF_FIND( LOC1, 'OBJREF', 'READ', LOCNREF, STATUS )
      IF ( STATUS .EQ. DAT__FILNF ) CALL EMS_ANNUL( STATUS )
      CALL EMS_RLSE
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Try to reference with an invalid locator.
*  ========================================
      LOCNREF = DAT__NOLOC
      CALL EMS_MARK
      CALL REF_CRPUT( LOC1, 'OBJREF', LOCNREF, .FALSE., STATUS )
      IF ( STATUS .EQ. DAT__LOCIN ) CALL EMS_ANNUL( STATUS )
      CALL EMS_RLSE
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Test ANNUL with bad status.
*  ==========================
      CALL EMS_MARK
      STATUS = SAI__ERROR
      CALL EMS_REP( 'TEST_8',
     : '*** This error message should not appear ***', STATUS )
      CALL REF_ANNUL( LOCNREF, STATUS )
      IF ( STATUS .EQ. SAI__ERROR ) CALL EMS_ANNUL( STATUS )
      CALL EMS_RLSE
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Final check on files, locators and error context level.
*  ======================================================
*  Erase "refobj" created at the beginning.
      CALL HDS_ERASE( LOC1, STATUS )

*  Check on final files and locators.
      WRITE( *, * )
     : '      Expect no files or locators to be listed here...'
      CALL HDS_SHOW( 'FILES', STATUS )
      CALL HDS_SHOW( 'LOCATORS', STATUS )

*  Close down HDS.
      CALL HDS_STOP( STATUS )

*  Check the original error context level has been restored.
      CALL EMS_LEVEL( LEVEL )
      IF ( LEVEL .NE. LEVEL0 ) THEN
         STATUS = SAI__ERROR
         CALL EMS_SETI( 'LEVEL0', LEVEL0 )
         CALL EMS_SETI( 'LEVEL', LEVEL )
         CALL EMS_REP( 'REF_TEST_LEV',
     :                 'Error in error context level: initially ' //
     :                 'was ^LEVEL0, is now ^LEVEL.', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Final message.
 99   CONTINUE
      IF ( STATUS .EQ.SAI__OK ) THEN
         WRITE( *, * )
         WRITE( *, * ) '   REF installation test succeeded.'
         WRITE( *, * )
      ELSE
         CALL EMS_REP( 'REF_TEST_ERR',
     :                 'REF_TEST: REF installation test FAILED.',
     :                 STATUS )
      ENDIF

      END
