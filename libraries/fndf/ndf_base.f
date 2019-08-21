      SUBROUTINE NDF_BASE( INDF1, INDF2, STATUS )
*+
*  Name:
*     NDF_BASE

*  Purpose:
*     Obtain an identifier for a base NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_BASE( INDF1, INDF2, STATUS )

*  Description:
*     The routine returns an identifier for the base NDF with which an
*     NDF section is associated.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for an existing NDF section (the routine will also
*        work if this is already a base NDF).
*     INDF2 = INTEGER (Returned)
*        Identifier for the base NDF with which the section is
*        associated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for the INDF2 argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The NDF__NOID
*     constant is defined in the include file NDF_PAR.

*  Algorithm:
*     -  Set an initial value for the INDF2 argument before checking the
*     inherited status.
*     -  Import the input NDF identifier.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Create a new base NDF entry in the ACB to describe it.
*     -  Transfer the access control flags from the old ACB entry to the
*     new one.
*     -  Export an identifier for the new base NDF.
*     -  If an error occurred, then annul the new ACB entry.

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
*     27-NOV-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
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
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ACC( NDF__MXACC, NDF__MXACB ) = LOGICAL (Read and Write)
*           Access control flags.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF1

*  Arguments Returned:
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB1              ! Index to input NDF entry in ACB
      INTEGER IACB2              ! Index to output NDF entry in ACB
      INTEGER IACC               ! Loop counter for access control flags
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Set an initial value for the INDF2 argument.
      INDF2 = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the input NDF identifier.
      CALL NDF1_IMPID( INDF1, IACB1, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB1 )

*  Create a new base NDF entry in the ACB to describe it.
         CALL NDF1_CRNBN( IDCB, IACB2, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Transfer the access control flags from the old ACB entry to the new
*  one.
            DO 1 IACC = 1, NDF__MXACC
               ACB_ACC( IACC, IACB2 ) = ACB_ACC( IACC, IACB1 )
1           CONTINUE

*  Export an identifier for the new base NDF.
            CALL NDF1_EXPID( IACB2, INDF2, STATUS )

*  If an error occurred, then annul the new ACB entry.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL NDF1_ANL( IACB2, STATUS )
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_BASE_ERR',
     :   'NDF_BASE: Error obtaining an identifier for a base NDF.',
     :   STATUS )
         CALL NDF1_TRACE( 'NDF_BASE', STATUS )
      END IF

      END
