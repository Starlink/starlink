      SUBROUTINE GRP1_EXPID( SLOT, IGRP, STATUS )
*+
*  Name:
*     GRP1_EXPID

*  Purpose:
*     Export a GRP identifier.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_EXPID( SLOT, IGRP, STATUS )

*  Description:
*     The routine converts an index to an entry in the arrays held in
*     common (an "array slot") into a GRP identifier which can be
*     issued to an application to refer to that entry. The identifier
*     issued is saved in common so that a check on its validity can
*     later be made. GRP identifiers are encoded so that it is
*     extremely unlikely that two identical ones will ever be issued,
*     even if the GRP_ system is closed down completely and restarted
*     (an identifier which is still valid can, of course, never be
*     duplicated). This makes it possible to detect invalid identifiers
*     and to report problems with "dangling" identifier values if an
*     application neglects to annul them.

*  Arguments:
*     SLOT = INTEGER (Given)
*        Index to an entry in the common arrays.
*     IGRP = INTEGER (Returned)
*        GRP identifier for the array index. A value of GRP__NOID is
*        returned if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS not equal to SAI__OK,
*     then it returns a value of GRP__NOID for the IGRP argument.

*  Algorithm:
*     _  Set a default value of GRP__NOID for the IGRP argument.
*     _  Check the global status.
*     -  Check that the array slot supplied is valid and report an
*     error if it is not.
*     -  Increment the count of identifiers issued so far.
*     -  Encode the identifier value.
*     -  Save the identifier value in common for subsequent checking.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     -  At the moment no use is made of CMN_CTX and CMN_IDCTX.
*     {note_new_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP_ constants
      INCLUDE 'GRP_ERR'          ! GRP_ error codes
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP_ common blocks.
*        CMN_CHK( GRP__MAXG ) = INTEGER (Write)
*           Record of identifiers issued, for subsequent checking.
*        CMN_CTX( GRP__MAXG ) = INTEGER (Write)
*           Identifier context level for each array slot.
*        CMN_IDCNT = INTEGER (Read and Write)
*           Count of identifiers issued so far.
*        CMN_IDCTX = INTEGER (Read)
*           Current identifier context level.

*  Arguments Given:
      INTEGER SLOT

*  Arguments Returned:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      EXTERNAL GRP1_INIT         ! Initialise common blocks

*  Local variables:
      INTEGER IDCODE             ! Value for encoding identifiers

*.

*  Set a default value of GRP__NOID for the IGRP argument.
      IGRP = GRP__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the array slot is valid and report an error if it is not.
      IF ( ( SLOT .LT. 1 ) .OR. ( SLOT .GT. GRP__MAXG ) ) THEN
         STATUS = GRP__INTER
         CALL MSG_SETI( 'BADSLOT', SLOT )
         CALL ERR_REP( 'GRP1_EXPID_ERR1',
     :   'GRP1_EXPID: An invalid SLOT argument of ^BADSLOT supplied '//
     :   '(internal programming error).', STATUS )

*  Increment the count of identifiers issued so far. This value is
*  never reset, so it allows unique identifiers to be generated (apart
*  from the effects of protecting against overflow - see below).
      ELSE
         CMN_IDCNT = CMN_IDCNT + 1

*  Encode the identifier, including protection against arithmetic
*  overflow if too many identifiers are issued.
         IDCODE = MOD( CMN_IDCNT, ( NUM__MAXI / GRP__MAXG ) )
         IGRP = SLOT + GRP__MAXG * IDCODE

*  Save the identifier in the array slot.
         CMN_CHK( SLOT ) = IGRP

*  Assign the current identifier context level to the array slot.
         CMN_CTX( SLOT ) = CMN_IDCTX

*  Check if the identifier is being watched.
         IF( IGRP .EQ. CMN_WATCH ) CALL GRP_ALARM( IGRP, 'CREATE',
     :                                             STATUS )

      END IF

      END
