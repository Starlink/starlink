      SUBROUTINE NDF1_CLN( IACB1, IACB2, STATUS )
*+
*  Name:
*     NDF1_CLN

*  Purpose:
*     Clone an ACB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CLN( IACB1, IACB2, STATUS )

*  Description:
*     The routine "clones" an ACB entry, producing a new entry which is
*     a duplicate of the original (except that no components are mapped
*     for access). The data object reference count in the DCB is
*     incremented by one.

*  Arguments:
*     IACB1 = INTEGER (Given)
*        Index to the ACB entry to be cloned.
*     IACB2 = INTEGER (Returned)
*        Index to the new ACB entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If STATUS is set on entry, then a value of zero will be
*     returned for the IACB2 argument, although no further processing
*     will occur.
*     -  A value of zero will also be returned for the IACB2 argument if
*     the routine fails for any reason.

*  Algorithm:
*     -  Set an initial value of zero for the IACB2 argument before
*     checking the inherited status.
*     -  Obtain a free slot in the ACB. Reset the IACB2 argument to zero
*     if no slot could be found.
*     -  Transfer the attributes of the initial ACB entry to the new
*     one, cloning the data array identifier, the quality array
*     identifier and the variance array identifier (if valid).
*     -  If an error occurred, then clean up by annulling any
*     identifiers which may have been acquired, releasing the new ACB
*     slot, and resetting the IACB2 argument to zero.
*     -  Otherwise, increment the data object reference count in the
*     DCB by one.

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
*     6-OCT-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     12-DEC-1989 (RFWS):
*        Installed support for the variance component.
*     30-JAN-1990 (RFWS):
*        Installed support for the quality component.
*     1-FEB-1990 (RFWS):
*        Installed propagation of quality component control flags.
*     23-OCT-1990 (RFWS):
*        Removed initialisation of ACB quantities which are now
*        processed by NDF1_FFS.
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
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_REFCT( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of ACB entries which refer to a data object.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ACC( NDF__MXACC, NDF__MXACB ) = LOGICAL (Read and Write)
*           NDF access control flags.
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether the NDF is a cut.
*        ACB_DID( NDF__MXACB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read and Write)
*           Index to the data object entry in the DCB.
*        ACB_ISQBB( NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether a quality component bad-bits override value has been
*           set.
*        ACB_QBB( NDF__MXACB ) = BYTE (Read and Write)
*           Quality component bad-bits override value.
*        ACB_QID( NDF__MXACB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the quality array.
*        ACB_VID( NDF__MXACB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the variance array.

*  Arguments Given:
      INTEGER IACB1

*  Arguments Returned:
      INTEGER IACB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACC               ! Loop counter for access control flags
      INTEGER IDCB               ! Index to data object entry in the DCB
      LOGICAL VALID              ! Whether array identifier is valid

*.

*  Set an initial value of zero for the IACB2 argument.
      IACB2 = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find a free slot in the ACB. Reset the IACB2 argument to zero if no
*  slot could be found.
      CALL NDF1_FFS( NDF__ACB, IACB2, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IACB2 = 0

*  Transfer the NDF attributes from the old ACB entry to the new one.
      ELSE

*  ...Whether the ACB entry describes an NDF section.
         ACB_CUT( IACB2 ) = ACB_CUT( IACB1 )

*  ...Access control flags.
         DO 1 IACC = 1, NDF__MXACC
            ACB_ACC( IACC, IACB2 ) = ACB_ACC( IACC, IACB1 )
1        CONTINUE

*  ...Index to data object entry in the DCB.
         ACB_IDCB( IACB2 )= ACB_IDCB( IACB1 )

*  ...Quality component control flags.
         ACB_ISQBB( IACB2 ) = ACB_ISQBB( IACB1 )
         ACB_QBB( IACB2 ) = ACB_QBB( IACB1 )

*  DATA component:
*  ==============
*  Clone the ARY_ system identifier for the NDF's data array.
         CALL ARY_CLONE( ACB_DID( IACB1 ), ACB_DID( IACB2 ), STATUS )

*  QUALITY component:
*  ==================
*  See if the ARY_ system identifier for the NDF's quality array in
*  the input ACB entry is valid.
         CALL ARY_VALID( ACB_QID( IACB1 ), VALID, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then clone it.
            IF ( VALID ) THEN
               CALL ARY_CLONE( ACB_QID( IACB1 ), ACB_QID( IACB2 ),
     :                         STATUS )

*  Otherwise, set a null identifier in the new ACB entry.
            ELSE
               ACB_QID( IACB2 ) = ARY__NOID
            END IF
         END IF

*  VARIANCE component:
*  ==================
*  See if the ARY_ system identifier for the NDF's variance array in
*  the input ACB entry is valid.
         CALL ARY_VALID( ACB_VID( IACB1 ), VALID, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then clone it.
            IF ( VALID ) THEN
               CALL ARY_CLONE( ACB_VID( IACB1 ), ACB_VID( IACB2 ),
     :                         STATUS )

*  Otherwise, set a null identifier in the new ACB entry.
            ELSE
               ACB_VID( IACB2 ) = ARY__NOID
            END IF
         END IF

*  If an error occurred, then annul any identifiers which might have
*  been acquired and release the new ACB slot.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ARY_ANNUL( ACB_DID( IACB2 ), STATUS )
            CALL ARY_ANNUL( ACB_QID( IACB2 ), STATUS )
            CALL ARY_ANNUL( ACB_VID( IACB2 ), STATUS )
            CALL NDF1_RLS( NDF__ACB, IACB2, STATUS )

*  Otherwise, increment the data object reference count in the DCB.
         ELSE
            IDCB = ACB_IDCB( IACB1 )
            DCB_REFCT( IDCB ) = DCB_REFCT( IDCB ) + 1
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CLN', STATUS )

      END
