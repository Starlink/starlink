      SUBROUTINE AGP_ASPEC( AGINAM, GNS, SPEC, STATUS )
*+
*  Name:
*     AGP_ASPEC

*  Purpose:
*     Find the GNS or native PGPLOT specification for an AGI workstation name.

*  Invocation:
*     CALL AGP_ASPEC( AGINAM, GNS, SPEC, STATUS )

*  Description:
*     Returns the GNS device specification for a supplied AGI workstation
*     name. If the application is linked with native PGPLOT through the
*     agp_link_adam command, the native PGPLOT device specification can
*     instead be returned by supplying .FALSE. for the GNS argument.
*
*     An error is reported if the supplied AGI workstation name is not known.

*  Arguments:
*     AGINAM = CHARACTER*(*) (Given)
*        The AGI workstation name. No abbreviations allowed.
*     GNS = LOGICAL (Given)
*        Supplied .TRUE. if the GNS device specification is required, and
*        .FALSE. if the native PGPLOT device specification is required.
*        An error is reported if a native PGPLOT device specifications is
*        requested but cannot be produced (e.g. because the application
*        is not linked with native PGPLOT).
*     SPEC = CHARACTER*(*) (Returned)
*        The returned PGPLOT or GNS device specification.
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)

*  History:
*     31-OCT-2001 (DSB):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER AGINAM*(*)
      LOGICAL GNS

*  Arguments Returned:
      CHARACTER SPEC*(*)

*  Status:
      INTEGER STATUS

*.

*  Initialise the returned spec.
      SPEC = ' '

*  Check status on entry
      IF( STATUS .NE. SAI__OK ) RETURN

*  This is the Starlink PGPLOT version of this routine, so just use GNS
*  to find the GNS spec.
      IF( GNS ) THEN
         CALL GNS_IGAG( AGINAM, SPEC, STATUS )

*  Report an error if the native PGPLOT name is requested.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', AGINAM )
         CALL ERR_REP( 'AGP_ASPEC_ERR1', 'Unable to find native '//
     :                 'PGPLOT device for ''^TYPE''.', STATUS )
         CALL ERR_REP( 'AGP_ASPEC_ERR2', 'AGP_ASPEC: Application is '//
     :                 'not linked with native PGPLOT (programming '//
     :                 'error).', STATUS )
      END IF

      END
