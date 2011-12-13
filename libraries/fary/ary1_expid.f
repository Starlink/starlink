      SUBROUTINE ARY1_EXPID( IACB, IARY, STATUS )
*+
*  Name:
*     ARY1_EXPID

*  Purpose:
*     Export an array identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_EXPID( IACB, IARY, STATUS )

*  Description:
*     The routine converts an index to an entry in the ACB into an
*     array identifier which can be issued to an application to refer
*     to that entry. The identifier issued is saved in the ACB so that
*     a check on its validity can later be made.  Array identifiers are
*     encoded so that it is extremely unlikely that two identical ones
*     will ever be issued, even if the ARY_ system is closed down
*     completely and restarted (an identifier which is still valid can,
*     of course, never be duplicated). This makes it possible to detect
*     invalid identifiers and to report problems with "dangling"
*     identifier values if an application neglects to annul them.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to an entry in the ACB.
*     IARY = INTEGER (Returned)
*        Array identifier for the ACB entry. A value of ARY__NOID is
*        returned if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS not equal to SAI__OK,
*     then it returns a value of ARY__NOID for the IARY argument.

*  Algorithm:
*     _  Set a default value of ARY__NOID for the IARY argument.
*     _  Check the global status.
*     -  Check that the ACB index supplied is valid and report an error
*     if it is not.
*     -  Increment the count of identifiers issued so far.
*     -  Encode the identifier value.
*     -  Save the identifier value in the ACB for subsequent checking.

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
*     14-JUN-1989 (RFWS):
*        Original version.
*     7-SEP-1989 (RFWS):
*        Added message token for routine name, to prevent '$' from
*        affecting error messages.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Global Variables:
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_CHK( ARY__MXACB ) = INTEGER (Write)
*           Record of identifiers issued, for subsequent checking.
*        ACB_IDCNT = INTEGER (Read and Write)
*           Count of identifiers issued so far.

*  Arguments Given:
      INTEGER IACB

*  Arguments Returned:
      INTEGER IARY

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      EXTERNAL ARY1_INIT         ! Initialise common blocks

*  Local variables:
      INTEGER IDCODE             ! Value for encoding identifiers

*.

*  Set a default value of ARY__NOID for the IARY argument.
      IARY = ARY__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the ACB index is valid and report an error if it is not.
      IF ( ( IACB .LT. 1 ) .OR. ( IACB .GT. ARY__MXACB ) ) THEN
         STATUS = ARY__FATIN
         CALL MSG_SETC( 'ROUTINE', 'ARY1_EXPID' )
         CALL MSG_SETI( 'BADIACB', IACB )
         CALL ERR_REP( 'ARY1_EXPID_IACB',
     :   'Routine ^ROUTINE called with an invalid IACB argument' //
     :   'of ^BADIACB (internal programming error).', STATUS )

*  Increment the count of identifiers issued so far. This value is
*  never reset, so it allows unique identifiers to be generated (apart
*  from the effects of protecting against overflow - see below).
      ELSE
         ACB_IDCNT = ACB_IDCNT + 1

*  Encode the identifier, including protection against arithmetic
*  overflow if too many identifiers are issued.
         IDCODE = MOD( ACB_IDCNT, ( NUM__MAXI / ARY__MXACB ) )
         IARY = IACB + ARY__MXACB * IDCODE

*  Save the identifier in the ACB.
         ACB_CHK( IACB ) = IARY
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_EXPID', STATUS )

      END
