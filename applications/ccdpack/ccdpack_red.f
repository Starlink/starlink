      SUBROUTINE CCDPACK_RED( STATUS )
*+
*  Name:
*     CCDPACK_RED

*  Purpose:
*     Top-level monolith for reduction tasks.

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
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     30-OCT-1995 (PDRAPER):
*        Original version.
*     27-NOV-2007 (DSB):
*        Use NDG_BEGPV/ENDPV to provide automatic provenance propagation.
*     06-JUL-2020 (GSB):
*        Update KPG1_LGCMD call adding new CPUTIM argument.
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
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( PAR__SZNAM ) ACTION ! Action name
      INTEGER CPUTIM( 4 )               ! Context info for KPG1_CPUTM

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( ACTION, STATUS )

*  Set the NDF application name.
      CALL NDF_HAPPN( ACTION // VERS, STATUS )

*  Tweak the numerics on a RedHat 7 Linux system.
      CALL CCD1_LINFLT

*  Record the current CPU time in CPUTIM.
      CALL KPG1_CPUTM( CPUTIM, VAL__BADD )

*  Begin a provenance block. This causes event handlers to be registered
*  with the NDF library so that a handler routine in NDG is called every
*  time an NDF is opened. This handler routine keeps a record of all NDFs
*  that are opened for input or output, until the block is closed by
*  calling NDG_ENDPV.
      CALL NDG_BEGPV( STATUS )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...
      IF ( ACTION .EQ. 'CALCOR' ) THEN
         CALL CALCOR( STATUS )

      ELSE IF ( ACTION .EQ. 'DEBIAS' ) THEN
         CALL DEBIAS( STATUS )

      ELSE IF ( ACTION .EQ. 'FLATCOR' ) THEN
         CALL FLATCOR( STATUS )

      ELSE IF ( ACTION .EQ. 'MAKEBIAS' ) THEN
         CALL MAKEBIAS( STATUS )

      ELSE IF ( ACTION .EQ. 'MAKECAL' ) THEN
         CALL MAKECAL( STATUS )

      ELSE IF ( ACTION .EQ. 'MAKEFLAT' ) THEN
         CALL MAKEFLAT( STATUS )

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
      CALL KPG1_LGCMD( ACTION, 'CCDPACK', CPUTIM, STATUS )

      END
* $Id$
