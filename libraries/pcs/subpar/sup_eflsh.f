      SUBROUTINE SUBPAR_EFLSH( STATUS )
*+
*  Name:
*     SUBPAR_EFLSH

*  Purpose:
*     To flush error messages at the SUBPAR level.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_EFLSH( STATUS )

*  Description:
*     The routine loops until STATUS is SAI__OK, calling EMS_ELOAD to
*     obtain any error messages stacked in the current error context
*     and outputs them by calling SUBPAR_WRERR.
*     If SUBPAR_WRERR fails, we WRITE the message to standard output.

*  Arguments:
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
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
*     AJC: A J Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-APR-1992 (AJC):
*        Original version.
*     25-AUG-1992 (PCTR):
*        Use SUBPAR_WRERR to deliver the error messages and check the
*        returned status.
*     13-JUL-1994 (AJC):
*        Changed behaviour of EMS_ELOAD
*        and re-report messages which fail to deliver
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'EMS_PAR'          ! EMS public constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EMSLEN             ! Used length of EMSNAM
      INTEGER ISTAT              ! Local status
      INTEGER MESLEN             ! Used length of error message
      INTEGER TSTAT              ! Local status

      CHARACTER * ( EMS__SZMSG ) EMESS ! Error message
      CHARACTER * ( EMS__SZPAR ) EMSNAM ! Error message name
      CHARACTER * 3 MPRE         ! Output message prefix

*.

*  Set the error message prefix for first message in a flush.
      MPRE = '!! '

*  Initialise the local status.
      TSTAT = SAI__OK

*  Now loop getting and delivering the error messages.
*  DO WHILE loop.
 10   CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL EMS_ELOAD( EMSNAM, EMSLEN, EMESS, MESLEN, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            ISTAT = SAI__OK
            CALL SUBPAR_WRERR( MPRE // EMESS( : MESLEN ), ISTAT )

*        Check the returned status value.
*        If it's bad re-report the error - it may come out later
            IF ( ISTAT .NE. SAI__OK ) THEN
               TSTAT = ISTAT
               CALL EMS_REP( 'SUP_EFLSH1', EMESS( : MESLEN ), STATUS )
            END IF

*        Set the error message prefix for subsequent messages.
            MPRE = '!  '
         END IF
      GO TO 10
      END IF

*  Check the local status for SUBPAR_WRERR failures, set the returned
*  status and attempt to deliver a warning if a failure has occurred.
*  Assume that SUBPAR_WRERR would fail again.
      IF ( TSTAT .NE. SAI__OK ) THEN
         STATUS = TSTAT
         ISTAT = SAI__OK
         CALL EMS_REP( 'SUP_EFLSH2',
     :   'SUBPAR: Error message delivery faults.', TSTAT )
      END IF

      END
