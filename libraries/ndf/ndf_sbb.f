      SUBROUTINE NDF_SBB( BADBIT, INDF, STATUS )
*+
*  Name:
*     NDF_SBB

*  Purpose:
*     Set a bad-bits mask value for the quality component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_SBB( BADBIT, INDF, STATUS )

*  Description:
*     The routine assigns a new unsigned byte bad-bits mask value to
*     the quality component of an NDF.

*  Notes:
*     -  If WRITE access to the NDF is not available, or if an NDF
*     section is supplied (as opposed to a base NDF), then no permanent
*     change to the data object will be made. In this case, the new
*     bad-bits value will be associated with the NDF identifier and
*     will subsequently be used by other NDF_ routines which access the
*     NDF through this identifier. The new value will also be
*     propagated to any new identifiers derived from it.

*  Arguments:
*     BADBIT = BYTE (Given)
*        The unsigned byte bad-bits mask value.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Check inherited global status.
*     -  Import the NDF identifier.
*     -  If an NDF section was specified, then the actual data object
*     value is not altered, so set the ACB bad-bits value and note it
*     has been set.
*     -  If a base NDF was specified, then obtain an index to the data
*     object entry in the DCB.
*     -  See if WRITE access to the NDF is available.
*     -  If access is not available, then the data object cannot be
*     modified, so the DCB over-ride value is set.
*     -  If the data object can be modified, then cancel any previous
*     DCB over-ride value.
*     -  Ensure that a quality component exists.
*     -  See if it contains a BADBITS object.
*     -  If not, then create one.
*     -  Obtain a locator to the BADBITS object.
*     -  Assign the new value to it and update the value stored in the
*     DCB.
*     -  Annul the locator.
*     -  Loop to inspect ACB entries.
*     -  Identify base ACB entries which refer to the modified data
*     object.
*     -  Modify those ACB entries to hold the new bad-bits value and to
*     note if an over-ride value is in effect.
*     -  If an error occurred, then report context information and call
*     the error tracing routine.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     1-FEB-1990 (RFWS):
*        Original version.
*     7-FEB-1990 (RFWS):
*        Extended to set an override value if the NDF is a section or
*        if write access is not available.
*     27-MAR-1990 (RFWS):
*        Extended to use the DCB bad-bits override values.
*     16-JUL-1996 (RFWS):
*        Fixed bug - NDF1_QCRE was being passed a DCB index instead of
*        an ACB index.
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
*        DCB_QBB( NDF__MXDCB ) = LOGICAL (Write)
*           Quality bad bits mask value.
*        DCB_QLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Quality component locator.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_ISQBB( NDF__MXACB ) = LOGICAL (Write)
*           Whether a bad-bits override value has been set.
*        ACB_QBB( NDF__MXACB ) = BYTE (Write)
*           Bad-bits override value.

*  Arguments Given:
      BYTE BADBIT
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      BYTE ZEROUB                ! Zero as an unsigned byte value
      PARAMETER ( ZEROUB = 0 )

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCBB ! Locator to BADBITS object
      INTEGER DUMMY( 1 )         ! Dummy dimension array
      INTEGER IACB               ! Index to the NDF entry in the ACB
      INTEGER IACBT              ! ACB index to test
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER NEXT               ! Next ACB entry
      LOGICAL MODIFY             ! Whether to modify the data object
      LOGICAL THERE              ! Whether component exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If an NDF section was specified, then the actual data object value
*  is not altered, so set the ACB bad-bits value and note it has been
*  set.
         IF ( ACB_CUT( IACB ) ) THEN
            ACB_QBB( IACB ) = BADBIT
            ACB_ISQBB( IACB ) = .TRUE.

*  If a base NDF was specified, then obtain an index to the data object
*  entry in the DCB.
         ELSE
            IDCB = ACB_IDCB( IACB )

*  See if WRITE access to the NDF is available.
            CALL NDF1_ACCOK( IACB, 'WRITE', MODIFY, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If access is not available, then the data object cannot be modified,
*  so the DCB over-ride value is set.
               IF ( .NOT. MODIFY ) THEN
                  DCB_ISQBB( IDCB ) = .TRUE.
                  DCB_OVQBB( IDCB ) = BADBIT

*  If the data object can be modified, then cancel any previous DCB
*  over-ride value.
               ELSE
                  DCB_ISQBB( IDCB ) = .FALSE.
                  DCB_OVQBB( IDCB ) = ZEROUB

*  Ensure that a quality component exists.
                  CALL NDF1_QCRE( IACB, STATUS )

*  See if it contains a BADBITS object.
                  CALL DAT_THERE( DCB_QLOC( IDCB ), 'BADBITS', THERE,
     :                            STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If not, then create one.
                     IF ( .NOT. THERE ) THEN
                        DUMMY( 1 ) = 0
                        CALL DAT_NEW( DCB_QLOC( IDCB ), 'BADBITS',
     :                                '_UBYTE', 0, DUMMY, STATUS )

                     END IF

*  Obtain a locator to the BADBITS object.
                     CALL DAT_FIND( DCB_QLOC( IDCB ), 'BADBITS', LOCBB,
     :                              STATUS )

*  Assign the new value to it and update the value stored in the DCB.
                     DUMMY( 1 ) = 0
                     CALL DAT_PUT( LOCBB, '_UBYTE', 0, DUMMY, BADBIT,
     :                             STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        DCB_QBB( IDCB ) = BADBIT
                     END IF

*  Annul the locator.
                     CALL DAT_ANNUL( LOCBB, STATUS )
                  END IF
               END IF
            END IF

*  Loop to inspect ACB entries.
            IACBT = 0
            NEXT = 0
1           CONTINUE
            CALL NDF1_NXTSL( NDF__ACB, IACBT, NEXT, STATUS )
            IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NEXT .NE. 0 ) ) THEN

*  Identify base ACB entries which refer to the modified data object.
               IACBT = NEXT
               IF ( ( .NOT. ACB_CUT( IACBT ) ) .AND.
     :              ( ACB_IDCB( IACBT ) .EQ. IDCB ) ) THEN

*  Modify those ACB entries to hold the new bad-bits value and to note
*  if an over-ride value is in effect.
                  ACB_ISQBB( IACBT ) = .NOT. MODIFY
                  ACB_QBB( IACBT ) = BADBIT
               END IF
               GO TO 1
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_SBB_ERR',
     :   'NDF_SBB: Error setting a bad-bits mask value for the ' //
     :   'quality component of an NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_SBB', STATUS )
      END IF

      END
