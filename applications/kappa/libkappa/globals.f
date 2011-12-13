      SUBROUTINE GLOBALS( STATUS )
*+
*  Name:
*     GLOBALS

*  Purpose:
*     Displays the values of the KAPPA global parameters.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GLOBALS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This procedure lists the meanings and values of the KAPPA global
*     parameters.  If a global parameter does not have a value, the
*     string "<undefined>" is substituted where the value would have
*     been written.

*  Usage:
*     globals

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     Copyright (C) 1995, 2001 Central Laboratory of the Research
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1991 July 18 (MJC):
*        Original version.
*     1992 June 5 (MJC):
*        Finds the operating system in order to define the path to the
*        global file.
*     1995 August 31 (MJC):
*        Lists the current transformation.
*     1995 December 6 (MJC):
*        Revised the displayed wording for the current DATA_ARRAY to
*        apply to foreign formats too.
*     4-DEC-2001 (DSB):
*        Removed image display and image overlay globals.
*     2009 July 21 (MJC):
*        Reports the message-reporting level, and its own reports
*        use MSG_OUTIF at the QUIET level.
*     29-JUL-2009 (TIMJ):
*        Use MSG_IFLEV to obtain the message filtering level string.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'DAT_ERR'          ! HDS error definitions
      INCLUDE 'MSG_PAR'          ! Message-system constants
      INCLUDE 'PSX_ERR'          ! PSX error constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of character string ignoring
                                 ! trailing blanks

*  Local Variables:
      CHARACTER*132 GLOVAL       ! Global variable
      CHARACTER*( DAT__SZLOC ) LOC ! Locator to the global file
      CHARACTER*24 MACHIN        ! Machine name
      INTEGER FILTER             ! Message filter level
      INTEGER NC                 ! Number of characters
      CHARACTER*20 NODE          ! Node name
      CHARACTER*132 PATH         ! Path name of ADAM_USER
      CHARACTER*( DAT__SZLOC ) PLOC ! Locator to the parameter-system
                                 ! structure containing the file and
                                 ! device names
      CHARACTER*10 RELEAS        ! Release of operating system
      CHARACTER*10 SYSNAM        ! Operating system
      CHARACTER*10 VERSIO        ! Sub-version of operating system

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
         CALL HDS_OPEN( 'ADAM_USER:GLOBAL', 'READ', LOC, STATUS )

*  Assume that it is a UNIX system.
      ELSE

         CALL ERR_MARK

*  Translate the ADAM_USER environment variable and its length.
         CALL PSX_GETENV( 'ADAM_USER', PATH, STATUS )
         NC = CHR_LEN( PATH )

*  First look in ADAM_USER.  If this is not defined deal with the error
*  silently and try the default directory.
         CALL HDS_OPEN( PATH( :NC )//'/GLOBAL', 'READ', LOC,
     :                  STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )

*  Translate the HOME environment variable.  Get its length.
            CALL PSX_GETENV( 'HOME', PATH, STATUS )
            NC = CHR_LEN( PATH )

            LOC = ' '
            CALL HDS_OPEN( PATH( :NC )//'/adam/GLOBAL', 'READ', LOC,
     :                     STATUS )
         END IF
         CALL ERR_RLSE
      END IF

*  Inquire the various global parameters.
*  ======================================

*  Bracket each with an error context, since we want to annul the
*  error if the object is not found (DAT__OBJNF) and substitute another
*  report.

*  The data array.  This is a file name so it is inside a structure.
      CALL ERR_MARK
      PLOC = ' '

*  Find and obtain the value.
      CALL DAT_FIND( LOC, 'DATA_ARRAY', PLOC, STATUS )
      CALL CMP_GET0C( PLOC, 'NAMEPTR', GLOVAL, STATUS )

*  Create the appropriate token when there is no object.
      IF ( STATUS .EQ. DAT__OBJNF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'GLOVAL', '<undefined>' )
      ELSE
         CALL MSG_SETC( 'GLOVAL', GLOVAL )
      END IF

*  Report the current global value.
      CALL MSG_OUTIF( MSG__QUIET, 'GLOBAL1',
     :  'The current data file is             : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*  The graphics device.
      CALL ERR_MARK
      PLOC = ' '
      CALL DAT_FIND( LOC, 'GRAPHICS_DEVICE', PLOC, STATUS )
      CALL CMP_GET0C( PLOC, 'NAMEPTR', GLOVAL, STATUS )
      IF ( STATUS .EQ. DAT__OBJNF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'GLOVAL', '<undefined>' )
      ELSE
         CALL MSG_SETC( 'GLOVAL', GLOVAL )
      END IF
      CALL MSG_OUTIF( MSG__QUIET, 'GLOBAL2',
     :  'The current graphics device is       : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*  The lookup table.
      CALL ERR_MARK
      PLOC = ' '
      CALL DAT_FIND( LOC, 'LUT', PLOC, STATUS )
      CALL CMP_GET0C( PLOC, 'NAMEPTR', GLOVAL, STATUS )
      IF ( STATUS .EQ. DAT__OBJNF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'GLOVAL', '<undefined>' )
      ELSE
         CALL MSG_SETC( 'GLOVAL', GLOVAL )
      END IF
      CALL MSG_OUTIF( MSG__QUIET, 'GLOBAL5',
     :  'The current lookup table file is     : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*  The transformation.
      CALL ERR_MARK
      PLOC = ' '
      CALL DAT_FIND( LOC, 'TRANSFORM', PLOC, STATUS )
      CALL CMP_GET0C( PLOC, 'NAMEPTR', GLOVAL, STATUS )
      IF ( STATUS .EQ. DAT__OBJNF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'GLOVAL', '<undefined>' )
      ELSE
         CALL MSG_SETC( 'GLOVAL', GLOVAL )
      END IF
      CALL MSG_OUTIF( MSG__QUIET, 'GLOBAL6',
     :  'The current transformation is        : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*  The interaction mode.
      CALL ERR_MARK
      CALL CMP_GET0C( LOC, 'INTERACTIONMODE', GLOVAL, STATUS )
      IF ( STATUS .EQ. DAT__OBJNF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'GLOVAL', '<undefined>' )
      ELSE
         CALL MSG_SETC( 'GLOVAL', GLOVAL )
      END IF
      CALL MSG_OUTIF( MSG__QUIET, 'GLOBAL7',
     :  'The current interaction mode is      : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*  The message-reporting level.  Note this comes not from the global
*  parameter file, but from the environment variable MSG_FILTER.  This
*  avoids having a parameter in every application to control this.
*  MSG_IFLEV should always work given that MSG_IFGETENV is called
*  earlier.
      CALL ERR_MARK
      CALL MSG_IFLEV( FILTER, GLOVAL, STATUS )
      CALL MSG_SETC( 'GLOVAL', GLOVAL )
      CALL MSG_OUTIF( MSG__QUIET, 'GLOBAL8',
     :  'The current message-report level is  : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*  Close the global file.
*  =====================
      CALL HDS_CLOSE( LOC, STATUS )

*  Closedown.
*  ==========

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GLOBALS_ERR',
     :     'GLOBALS: Error listing the KAPPA global parameters.',
     :     STATUS )
      END IF

      END
