      SUBROUTINE ARY1_IMP( LOC, IACB, STATUS )
*+
*  Name:
*     ARY1_IMP

*  Purpose:
*     Import an array structure into the ACB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_IMP( LOC, IACB, STATUS )

*  Description:
*     The routine imports an array data structure, identified by its
*     HDS locator, into the ACB, returning the index to the base array
*     entry in the ACB allocated for it. This routine detects if the
*     same data object has previously been imported and takes account
*     of this possibility.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        HDS locator to the data object to be imported.
*     IACB = INTEGER (Returned)
*        Index to the resulting base array entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If STATUS is set on entry, then a value of zero will be
*     returned for the IACB argument, although no further processing
*     will occur.
*     -  A value of zero will also be returned for the IACB argument if
*     the routine fails for any reason.

*  Algorithm:
*     -  Set an initial value for the IACB argument before checking the
*     inherited status.
*     -  Import the data object into the DCB.
*     -  Search through the DCB to determine whether the same data
*     object has been imported previously.
*     -  If the DCB entry is duplicated, then ensure that access mode
*     information is available for both entries and decide which one to
*     keep so that all available access to the data object is
*     maintained.
*     -  Transfer the mapping and reference count information to the new
*     DCB entry if necessary.
*     -  Reset the reference count and annul the other DCB entry.
*     -  Loop through all ACB entries, searching for those which refer
*     to the DCB entry which has been annulled. Change these ACB entries
*     to point at the DCB entry which was kept.
*     -  Create a new ACB base array entry to describe the new data
*     object.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1989 (RFWS):
*        Original version.
*     14-SEP-1989 (RFWS):
*        Fixed array addressing bug.
*     14-SEP-1989 (RFWS):
*        Changed call to ARY1_DANL to include new argument.
*     10-APR-1990 (RFWS):
*        Removed reference count incrementation, which is performed by
*        ARY1_CRNBA, so is not needed in this routine.
*     11-APR-1990 (RFWS):
*        Moved the call to ARY1_CRNBA to the end of the routine and
*        slightly re-structured so that this routine now matches the
*        structure of the equivalent NDF_ routine (as was originally
*        the case before bug fixes were carried out).
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

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_FILE( ARY__MXDCB ) = CHARACTER * ( ARY__SZFIL ) (Write)
*           Data object container file name.
*        DCB_MOD( ARY__MXDCB ) = CHARACTER * ( ARY__SZMOD ) (Read)
*           Data object access mode.
*        DCB_NREAD( ARY__MXDCB ) = INTEGER (Read and Write)
*           Number of read mappings to data object.
*        DCB_NWRIT( ARY__MXDCB ) = INTEGER (Read and Write)
*           Number of write mappings to data object.
*        DCB_PATH( ARY__MXDCB ) = CHARACTER * ( ARY__SZPTH ) (Write)
*           Data object path name.
*        DCB_REFCT( ARY__MXDCB ) = INTEGER (Read and Write)
*           Number of ACB entries referring to data object.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read and Write)
*           Pointer to data object entry in the DCB.

*  Arguments Given:
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACBT              ! ACB entry to be tested
      INTEGER IDCB               ! Index to new DCB entry
      INTEGER IDCBA              ! DCB entry to be annulled
      INTEGER IDCBK              ! DCB entry to be kept
      INTEGER IDCBT              ! DCB entry to be tested/compared
      INTEGER NEXT               ! Next common block entry to consider
      INTEGER TEMP               ! Temporary store for DCB slot number
      LOGICAL DUPE               ! Whether DCB entry is duplicated

*.

*  Set an initial value for the IACB argument.
      IACB = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the data object into the DCB, occupying a new DCB slot.
      CALL ARY1_DIMP( LOC, IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop through all the DCB entries to see whether this same data object
*  has previously been imported.
         DUPE = .FALSE.
         NEXT = 0
         IDCBT = 0
1        CONTINUE                ! Start of 'DO WHILE' loop
         CALL ARY1_NXTSL( ARY__DCB, IDCBT, NEXT, STATUS )
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NEXT .NE. 0 ) ) THEN
            IDCBT = NEXT

*  Search for DCB entries which differ from the one just created, but
*  which have the same data file and object path name.
            IF ( ( IDCBT .NE. IDCB ) .AND.
     :           ( DCB_FILE( IDCBT ) .EQ. DCB_FILE( IDCB ) ) .AND.
     :           ( DCB_PATH( IDCBT ) .EQ. DCB_PATH( IDCB ) ) ) THEN
               DUPE = .TRUE.
               GO TO 2
            END IF
            GO TO 1
         END IF
2        CONTINUE

*  If duplicate DCB entries exist, then they must be combined into a
*  single entry, but account must be taken of possible differences in
*  the access mode when the same data object is imported several times.
*  Ensure that access mode information is available for both DCB
*  entries.
         IDCBK = IDCB
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( DUPE ) THEN
               CALL ARY1_DMOD( IDCB, STATUS )
               CALL ARY1_DMOD( IDCBT, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  For preference, we keep the DCB entry which was there first, and
*  annul the new one.
                  IDCBK = IDCBT
                  IDCBA = IDCB

*  However, if the new entry has UPDATE access to the data object,
*  whereas the first one does not, then the new DCB entry has to be
*  kept at the expense of the old one.
                  IF ( ( DCB_MOD( IDCBK ) .NE. 'UPDATE' ) .AND.
     :                 ( DCB_MOD( IDCBA ) .EQ. 'UPDATE' ) ) THEN
                     IDCBK = IDCB
                     IDCBA = IDCBT

*  Transfer the reference count and mapping counts to the new DCB
*  entry.
                     DCB_REFCT( IDCB ) = DCB_REFCT( IDCBT )
                     DCB_NREAD( IDCB ) = DCB_NREAD( IDCBT )
                     DCB_NWRIT( IDCB ) = DCB_NWRIT( IDCBT )
                  END IF

*  Reset the reference count for the other DCB entry to 1 and annul it,
*  so that it is removed. Retain the DCB slot number for use later.
                  DCB_REFCT( IDCBA ) = 1
                  TEMP = IDCBA
                  CALL ARY1_DANL( .FALSE., IDCBA, STATUS )
                  IDCBA = TEMP

*  Loop through all the entries in the ACB to make adjustments to any
*  which referred to the DCB entry which has just been removed.
                  NEXT = 0
                  IACBT = 0
3                 CONTINUE       ! Start of 'DO WHILE' loop
                  CALL ARY1_NXTSL( ARY__ACB, IACBT, NEXT, STATUS )
                  IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :                 ( NEXT .NE. 0 ) ) THEN
                     IACBT = NEXT

*  Any ACB entries which point to the annulled DCB entry are changed to
*  point to the one which was kept instead.
                     IF ( ACB_IDCB( IACBT ) .EQ. IDCBA ) THEN
                        ACB_IDCB( IACBT ) = IDCBK
                     END IF
                     GO TO 3
                  END IF
               END IF
            END IF
         END IF

*  Create a new ACB base array entry to describe the new data object.
         CALL ARY1_CRNBA( IDCBK, IACB, STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_IMP', STATUS )

      END
