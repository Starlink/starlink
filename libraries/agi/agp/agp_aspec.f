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
      INCLUDE 'AGP_CONST'

*  Global Variables:
      INCLUDE 'AGP_COM'

*  Arguments Given:
      CHARACTER AGINAM*(*)
      LOGICAL GNS

*  Arguments Returned:
      CHARACTER SPEC*(*)

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AGP1_INIT          ! Initializes AGP common blocks
      INTEGER CHR_LEN             ! Used length of a string

*  Local Variables:
      CHARACTER LTYPE*(AGP__SZANM)! Local copy of device type
      INTEGER F                   ! Index of first non-blank character
      INTEGER IAT                 ! Used length of a string
      INTEGER I                   ! Loop count
      INTEGER L                   ! Index of last non-blank character
      INTEGER NMATCH              ! Number of matching devices
      INTEGER UL                  ! Used length of string
*.

*  Initialise the returned spec.
      SPEC = ' '

*  Check status on entry
      IF( STATUS .NE. SAI__OK ) RETURN

*  Take an upper case copy of the supplied AGI name and remove
*  spaces.
      IF( AGINAM .NE. ' ' ) THEN
         CALL CHR_FANDL( AGINAM, F, L )
         LTYPE = AGINAM( F:L )
         CALL CHR_RMBLK( LTYPE )
         CALL CHR_UCASE( LTYPE )
         UL = CHR_LEN( LTYPE )
      ELSE
         LTYPE = ' '
         UL = 1
      END IF

*  Loop through the known devices, counting the number which match the
*  supplied AGI name.
      NMATCH = 0
      DO I = 1, AGP__NDEV
         IF( AGP_ANM( I ) .EQ. LTYPE( : UL ) ) THEN
            NMATCH = NMATCH + 1

*  If this device matches, store the requested specification.
            IF( GNS ) THEN
               SPEC = AGP_GTY( I )
            ELSE
               IAT = 0
               CALL CHR_APPND( AGP_PFN( I ), SPEC, IAT )
               CALL CHR_APPND( '/', SPEC, IAT )
               CALL CHR_APPND( AGP_PTY( I ), SPEC, IAT )
            END IF

         END IF
      END DO

*  If no matches, report an error.
      IF( NMATCH .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', AGINAM )
         CALL ERR_REP( 'AGP_ASPEC_ERR1', 'Unknown AGI workstation '//
     :                 'name ''^TYPE'' specified.', STATUS )

*  If more than one match, report an error.
      ELSE IF( NMATCH .GT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', AGINAM )
         CALL ERR_REP( 'AGP_ASPEC_ERR2', 'Ambiguous AGI workstation '//
     :                 'name ''^TYPE'' specified.', STATUS )
      END IF

      END
