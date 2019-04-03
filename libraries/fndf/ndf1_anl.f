      SUBROUTINE NDF1_ANL( IACB, STATUS )
*+
*  Name:
*     NDF1_ANL

*  Purpose:
*     Annul an NDF entry in the ACB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ANL( IACB, STATUS )

*  Description:
*     The routine annuls an NDF entry in the ACB. If any NDF component
*     is currently mapped for access through this entry, then it is
*     first unmapped. The ACB entry is then annulled, i.e. the slot is
*     released and made available for re-use. If, as a result, the
*     reference count for the associated data object drops to zero,
*     then the object will be released from the NDF_ system and may be
*     deleted, according to its disposal mode.

*  Arguments:
*     IACB = INTEGER (Given and Returned)
*        The ACB entry to be annulled. A value of zero is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Copyright:
*     Copyright (C) 1994 Particle Physics and Astronomy Research Council

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
*     5-OCT-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     12-DEC-1989 (RFWS):
*        Installed support for the variance component.
*     8-JAN-1990 (RFWS):
*        Changed wild card character in call to NDF1_UMP.
*     30-JAN-1990 (RFWS):
*        Installed support for the quality component.
*     29-NOV-1990 (RFWS):
*        Removed unnecessary zeroing of ACB slot number.
*     25-APR-1994 (RFWS):
*        Report an error if an invalid ACB index is supplied.
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
      INCLUDE 'AST_PAR'          ! AST_ public constants

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Write)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read and Write)
*           Index to data object entry in the DCB.
*        ACB_QID( NDF__MXACB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the NDF's quality array.
*        ACB_VID( NDF__MXACB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the NDF's variance array.

*  Arguments Given and Returned:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL VALID              ! Whether array identifier is valid

*.

*  Begin a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Check that the ACB index supplied is valid and report an error if it
*  is not.
      IF ( ( IACB .LT. 1 ) .OR. ( IACB .GT. NDF__MXACB ) ) THEN
         STATUS = NDF__FATIN
         CALL MSG_SETC( 'ROUTINE', 'NDF1_ANL' )
         CALL MSG_SETI( 'IACB', IACB )
         CALL ERR_REP( 'NDF1_ANL_IACB',
     :        'Routine ^ROUTINE called with an invalid IACB ' //
     :        'argument of ^IACB - internal programming error.',
     :        STATUS )

*  Unmap all the NDF components.
      ELSE
        CALL NDF1_UMP( IACB, '*', STATUS )

*  Release the ARY_ system identifiers held in the ACB.

*  DATA component:
*  ==============
        CALL ARY_ANNUL( ACB_DID( IACB ), STATUS )

*  QUALITY component:
*  ==================
*  See if the quality array identifier is valid. If so, then annul it.
        CALL ARY_VALID( ACB_QID( IACB ), VALID, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
           IF ( VALID ) THEN
              CALL ARY_ANNUL( ACB_QID( IACB ), STATUS )
           END IF
        END IF

*  VARIANCE component:
*  ==================
*  See if the variance array identifier is valid. If so, then annul it.
        CALL ARY_VALID( ACB_VID( IACB ), VALID, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
           IF ( VALID ) THEN
              CALL ARY_ANNUL( ACB_VID( IACB ), STATUS )
           END IF
        END IF

*  Annul the associated data object and set the data object index in
*  the ACB to zero.
        CALL NDF1_DANL( .TRUE., ACB_IDCB( IACB ), STATUS )
        ACB_IDCB( IACB ) = 0

*  Release the ACB slot.
        CALL NDF1_RLS( NDF__ACB, IACB, STATUS )
      END IF

*  Reset the ACB index.
      IACB = 0

*  Call error tracing routine.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ANL', STATUS )

*  End the error reporting environment.
      CALL ERR_END( STATUS )

      END
