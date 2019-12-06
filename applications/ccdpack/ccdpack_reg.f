      SUBROUTINE CCDPACK_REG( STATUS )
*+
*  Name:
*     CCDPACK_REG

*  Purpose:
*     Top-level monolith for registration tasks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCDPACK( STATUS )

*  Description:
*     This routine gets the action name directly from the Unix kernel.
*     It then calls the appropriate routine to perform the
*     specified action. An error will be reported and STATUS will
*     be set if the action name is not recognized.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1995, 1999-2000 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     31-OCT-1995 (PDRAPER):
*        Original version
*     10-MAR-1999 (MBT):
*        Added new tasks ASTIMP and ASTEXP.
*     15-APR-1999 (MBT):
*        Added new task WCSREG.
*     18-MAY-1999 (MBT):
*        Added new task WCSEDIT.
*     21-SEP-1999 (MBT):
*        Added new task DRIZZLE.
*     22-DEC-2000 (MBT):
*        Added new task DRAWNDF.
*     27-NOV-2007 (DSB):
*        Use NDG_BEGPV/ENDPV to provide automatic provenance propagation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! Parameter system constants
      INCLUDE 'CCD1_PAR'         ! VERS value.

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( PAR__SZNAM ) ACTION ! Action name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( ACTION, STATUS )

*  Set the NDF application name.
      CALL NDF_HAPPN( ACTION // VERS, STATUS )

*  Tweak the numerics on a RedHat 7 Linux system.
      CALL CCD1_LINFLT

*  Begin a provenance block. This causes event handlers to be registered
*  with the NDF library so that a handler routine in NDG is called every
*  time an NDF is opened. This handler routine keeps a record of all NDFs
*  that are opened for input or output, until the block is closed by
*  calling NDG_ENDPV.
      CALL NDG_BEGPV( STATUS )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...
      IF ( ACTION .EQ. 'ASTEXP' ) THEN
         CALL ASTEXP( STATUS )

      ELSE IF ( ACTION .EQ. 'ASTIMP' ) THEN
         CALL ASTIMP( STATUS )

      ELSE IF ( ACTION .EQ. 'CCDEDIT' ) THEN
         CALL CCDEDIT( STATUS )

      ELSE IF ( ACTION .EQ. 'DRAWNDF' ) THEN
         CALL DRAWNDF( STATUS )

      ELSE IF ( ACTION .EQ. 'DRIZZLE' ) THEN
         CALL DRIZZLE( STATUS )

      ELSE IF ( ACTION .EQ. 'FINDCENT' ) THEN
         CALL FINDCENT( STATUS )

      ELSE IF ( ACTION .EQ. 'FINDOBJ' ) THEN
         CALL FINDOBJ( STATUS )

      ELSE IF ( ACTION .EQ. 'FINDOFF' ) THEN
         CALL FINDOFF( STATUS )

      ELSE IF ( ACTION .EQ. 'IDICURS' ) THEN
         CALL IDICURS( STATUS )

      ELSE IF ( ACTION.EQ. 'MAKEMOS' ) THEN
         CALL MAKEMOS( STATUS )

      ELSE IF ( ACTION.EQ. 'PAIRNDF' ) THEN
         CALL PAIRNDF( STATUS )

      ELSE IF ( ACTION.EQ. 'PLOTLIST' ) THEN
         CALL PLOTLIST( STATUS )

      ELSE IF ( ACTION.EQ. 'REGISTER' ) THEN
         CALL REGISTER( STATUS )

      ELSE IF ( ACTION.EQ. 'TRANLIST' ) THEN
         CALL TRANLIST( STATUS )

      ELSE IF ( ACTION .EQ. 'TRANNDF' ) THEN
         CALL TRANNDF( STATUS )

      ELSE IF ( ACTION .EQ. 'WCSEDIT' ) THEN
         CALL WCSEDIT( STATUS )

      ELSE IF ( ACTION .EQ. 'WCSREG' ) THEN
         CALL WCSREG( STATUS )

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ACTION', ACTION )
         CALL ERR_REP( 'CCDPACK_ERR',
     :                 'CCDPACK: The action name ''^ACTION'' is ' //
     :                 'not recognised by the CCDPACK '//
     :                 'monolith.', STATUS )
      END IF

*  End the provenance block. This will result in every output NDF being
*  given a provenance extension containing a record of the input NDFs
*  that the application accessed in order to create the output NDF. Any
*  output NDF that already contains a provenance extension is left
*  unchanged (so individual application can over-ride this automatic
*  provenance handling by adding a provenance extension to the output NDF
*  itself).
      CALL NDG_ENDPV( 'CCDPACK:'//ACTION, STATUS )

*  Log the task and its parameters to a log file specified by enviromnent
*  variable CCDPACK_LOG.
      CALL KPG1_LGCMD( ACTION, 'CCDPACK', STATUS )

      END
* $Id$
