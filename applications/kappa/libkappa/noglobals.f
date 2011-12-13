      SUBROUTINE NOGLOBALS( STATUS )
*+
*  Name:
*     NOGLOBALS

*  Purpose:
*     Resets the KAPPA global parameters.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NOGLOBALS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application resets the KAPPA global parameters, and so makes
*     their values undefined.

*  Usage:
*     noglobals

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 December 1 (MJC):
*        Original version.
*     1995 August 31 (MJC):
*        Removes the current transformation.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'DAT_ERR'          ! HDS error definitions

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of character string ignoring
                                 ! trailing blanks

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to the global file
      CHARACTER * ( 24 ) MACHIN  ! Machine name
      INTEGER NC                 ! Number of characters
      CHARACTER * ( 20 ) NODE    ! Node name
      CHARACTER * ( 132 ) PATH   ! Path name of ADAM_USER
      CHARACTER * ( 10 ) RELEAS  ! Release of operating system
      CHARACTER * ( 10 ) SYSNAM  ! Operating system
      LOGICAL THERE              ! Data object is present in the GLOBAL
                                 ! file
      CHARACTER * ( 10 ) VERSIO  ! Sub-version of operating system

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the host operating system.
*  ====================================
*
*  This assumes that the system is either VMS or UNIX.  It is needed
*  to specify the path of the file containing the global parameters.
      CALL PSX_UNAME( SYSNAM, NODE, RELEAS, VERSIO, MACHIN, STATUS )

*  Open the global file.
*  =====================
      LOC = ' '

*  Look for VMS or VAX/VMS.  If found use thre ADAM_USER logical name.
      IF ( INDEX( SYSNAM, 'VMS' ) .NE. 0 ) THEN
         CALL HDS_OPEN( 'ADAM_USER:GLOBAL', 'UPDATE', LOC, STATUS )

*  Assume that it is a UNIX system.
      ELSE

         CALL ERR_MARK

*  Translate the ADAM_USER environment variable and its length.
         CALL PSX_GETENV( 'ADAM_USER', PATH, STATUS )
         NC = CHR_LEN( PATH )

*  First look in ADAM_USER.  If this is not defined deal with the error
*  silently and try the default directory.
         CALL HDS_OPEN( PATH( :NC )//'/GLOBAL', 'UPDATE', LOC,
     :                  STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )

*  Translate the HOME environment variable.  Get its length.
            CALL PSX_GETENV( 'HOME', PATH, STATUS )

            NC = CHR_LEN( PATH )

            LOC = ' '
            CALL HDS_OPEN( PATH( :NC )//'/adam/GLOBAL', 'UPDATE', LOC,
     :                     STATUS )
         END IF
         CALL ERR_RLSE
      END IF

*  Erase the various global parameters.
*  ======================================

*  Erase the current data-array parameter, if it is present.
      CALL DAT_THERE( LOC, 'DATA_ARRAY', THERE, STATUS )
      IF ( THERE ) CALL DAT_ERASE( LOC, 'DATA_ARRAY', STATUS )

*  Erase the current graphics-device parameter, if it is present.
      CALL DAT_THERE( LOC, 'GRAPHICS_DEVICE', THERE, STATUS )
      IF ( THERE ) CALL DAT_ERASE( LOC, 'GRAPHICS_DEVICE', STATUS )

*  Erase the current image-display parameter, if it is present.
      CALL DAT_THERE( LOC, 'IMAGE_DISPLAY', THERE, STATUS )
      IF ( THERE ) CALL DAT_ERASE( LOC, 'IMAGE_DISPLAY', STATUS )

*  Erase the current image-overlay parameter, if it is present.
      CALL DAT_THERE( LOC, 'IMAGE_OVERLAY', THERE, STATUS )
      IF ( THERE ) CALL DAT_ERASE( LOC, 'IMAGE_OVERLAY', STATUS )

*  Erase the current LUT-file parameter, it it is present.
      CALL DAT_THERE( LOC, 'LUT', THERE, STATUS )
      IF ( THERE ) CALL DAT_ERASE( LOC, 'LUT', STATUS )

*  Erase the current transformation-structure parameter, if it is
*  present.
      CALL DAT_THERE( LOC, 'TRANSFORM', THERE, STATUS )
      IF ( THERE ) CALL DAT_ERASE( LOC, 'TRANSFORM', STATUS )

*  Erase the current interaction-mode parameter, if it is present.
      CALL DAT_THERE( LOC, 'INTERACTIONMODE', THERE, STATUS )
      IF ( THERE ) CALL DAT_ERASE( LOC, 'INTERACTIONMODE', STATUS )

*  Erase the current co-ordinate-system parameter, if it is present.
      CALL DAT_THERE( LOC, 'COORD_SYSTEM', THERE, STATUS )
      IF ( THERE ) CALL DAT_ERASE( LOC, 'COORD_SYSTEM', STATUS )

*  Close the global file.
*  =====================
      CALL HDS_CLOSE( LOC, STATUS )

*  Closedown.
*  ==========

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NOGLOBALS_ERR',
     :     'NOGLOBALS: Error resetting the KAPPA global parameters.',
     :     STATUS )
      END IF

      END
