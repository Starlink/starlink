#if HAVE_CONFIG_H
#include "config.h"
#endif
      SUBROUTINE DTASK_CONTROL( PATH, ACTION, VALUE, MESSID, STATUS )
*+
*  Name:
*     DTASK_CONTROL

*  Purpose:
*     Process a task CONTROL context message

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DTASK_CONTROL( PATH, ACTION, VALUE, MESSID, STATUS )

*  Description:
*     Check that ACTION is recognised. At present this can only
*     be 'DEFAULT' to get or set the task current working directory
*        'SETENV' to set the value of an environment variable for the task
*     or 'PAR_RESET' to de-activate the parameter system.
*
*     If the action is validated as 'DEFAULT' then:-
*         If the given value has non-zero length set the default directory
*         to this value. (No environment variable translation is done by this
*         routine.)
*         Acknowledge the message returning the current default directory
*         in the VALUE parameter.
*     If the action is validated as 'SETENV' then:-
*         The VALUE parameter is assumed to be in the form "VAR=string" in
*         which case environment variable VAR is set to string. If string
*         contains leading spaces, the whole of value must be quoted.
*     If the action is validated as 'PAR_RESET' then:-
*         SUBPAR_DEACT is called with TTYPE R for the task action specified
*         by VALUE - all parameter states will be reset to the GROUND state.
*         If no action is specified, it is done for all actions in the task/
*         monolith.

*  Arguments:
*     PATH = INTEGER (Given)
*        The path identifier to the controlling task
*     ACTION = CHARACTER * ( * ) (Given)
*        A valid 'action' for CONTROL context
*     VALUE = CHARACTER * ( * ) (Given)
*       The parameter for the specified action.
*     MESSID = INTEGER (Given)
*        The message identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Deficiencies:
*     Unix specific version

*  Copyright:
*     Copyright (C) 1991-1993 Science & Engineering Research Council.
*     Copyright (C) 1995-1997, 2000 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2009 Science and Technology Facilities Council.
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-FEB-1991 (AJC):
*        Original version.
*     8-APR-1992 (BKM):
*        Extend original DEFAULT context into CONTROL context with actions.
*        Revised logic
*     10-OCT-1992 (AJC):
*        Use SAI__OK not ADAM__OK
*        No need to INCLUDE MESDEFNS
*     15-OCT-1992 (AJC):
*        Use DTASK_ESETK and ERR not EMS
*        Add report on INVCONTROL
*        Correctly shut down comms
*      8-MAR-1993 (AJC):
*        Remove include DDMSG
*     30-SEP-1993 (BKM):
*        Unix version
*     13-JUN-1995 (AJC):
*        Add PAR_RESET action
*     30-JAN-1996 (AJC):
*        Add TTYPE argument for SUBPAR_DEACT
*     10-OCT-1996 (AJC):
*        Use TTYPE R on PAR_RESET
*     19-MAY-1997 (BKM):
*        Correct return string size and content
*     12-AUG-1997 (AJC):
*        Add SETENV action
*      4-FEB-2000 (AJC):
*        Allow task action name in PAR_RESET message
*        Strip quotes and blank following = from the setenv string
*     11-MAY-2009 (BRADC):
*        Replace intrinsic PUTENV with call to PSX_PUTENV.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAE constants
      INCLUDE 'DTASK_ERR'        ! DTASK error codes
      INCLUDE 'ADAM_DEFNS'        ! ADAM constants
      INCLUDE 'SUBPAR_SYS'       ! SUBPAR constants
      INCLUDE 'MESSYS_PAR'       ! MESSYS constants

*  Global Variables:

*  Arguments Given:
      INTEGER PATH
      CHARACTER*(*) ACTION
      CHARACTER*(*) VALUE
      INTEGER MESSID

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN           ! Used length of string
      INTEGER CHR_LEN
#if HAVE_INTRINSIC_GETCWD
      INTRINSIC GETCWD           ! Get current working directory
      INTEGER GETCWD
#endif
#if HAVE_INTRINSIC_CHDIR
      INTRINSIC CHDIR            ! Change working directory
      INTEGER CHDIR
#endif

*  Local Variables:
      CHARACTER*(MESSYS__VAL_LEN) RVALUE ! Return VALUE string
      CHARACTER*(SUBPAR__NAMELEN) UACT ! ACTION in upper case
      CHARACTER*(MESSYS__VAL_LEN) DEFN ! Processed VALUE string
      INTEGER LENGTH             ! Local string length variable
      INTEGER RLEN               ! Return string length
      INTEGER EQPTR              ! Index of = within string
      INTEGER MESSTATUS          ! Status returned to initiating task
      INTEGER ISTAT              ! System routine status
      INTEGER NAMECODE           ! task action index
*.
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initial defaults for return VALUE string and string length
      RVALUE = ' '
      RLEN = 1
      LENGTH = CHR_LEN( VALUE )

*  Check if ACTION is valid in CONTROL context
*  Work with ACTION in upper case.
      UACT = ACTION
      CALL CHR_UCASE( UACT )

      IF ( UACT .EQ. 'DEFAULT' ) THEN

#if HAVE_INTRINSIC_CHDIR && HAVE_INTRINSIC_GETCWD
*  Get the required directory name

         IF ( LENGTH .GT. 0) THEN

*  Non zero new directory length - effect change
            ISTAT = CHDIR( VALUE(1:LENGTH) )
            IF ( ISTAT .NE. 0) THEN
*  Report failure to set directory
               STATUS = DTASK__BADDEF
               CALL EMS_REP( 'DTASK_CONTROL',
     :                       'Failed to set new default directory',
     :                       STATUS )

            ENDIF
         ENDIF

*  Return new current directory
         ISTAT = GETCWD( RVALUE )
         RLEN = CHR_LEN( RVALUE )
         IF ( ISTAT .NE. 0) THEN

*  Report failure to get directory
            STATUS = DTASK__BADDEF
            CALL EMS_REP( 'DTASK_CONTROL',
     :                    'Failed to get current default directory',
     :                    STATUS )
         ENDIF
#else
*  Simply report that we can't chnage directory.  It appears that
*  most Fortrans do have these intrinsics.  If you're looking at this because
*  you have one that doesn't, then consider implementing this by adding
*  a function in the psx component.
         STATUS = DTASK__BADDEF
         CALL EMS_REP( 'DTASK_CONTROL',
     :        'DEFAULT unimplemented',
     :        STATUS )
#endif

      ELSE IF ( UACT .EQ. 'SETENV' ) THEN

*   Strip quotes and blank following the = in the given string.
*   This allows the string to be given quoted or unquoted in an ICL
*   SEND command.

*   Check for valid format - first strip quotes
         CALL SUBPAR_UNQUOTE( VALUE, DEFN, STATUS )
         LENGTH = CHR_LEN( DEFN )
         EQPTR = INDEX( DEFN(1:LENGTH), "=" )
         IF ( EQPTR .LE. 1 ) THEN
            STATUS = DTASK__BADDEF
            CALL EMS_REP( 'DTASK_CONTROL',
     :                    'Invalid environment variable specification',
     :                    STATUS )

         ELSE
*   OK format
            IF ( VALUE(1:LENGTH) .EQ. DEFN(1:LENGTH) ) THEN
*            It wasn't a quoted string - remove any space after =
               IF ( EQPTR+1 .LT. LENGTH )
     :             CALL CHR_LDBLK( DEFN(EQPTR+1:LENGTH) )
            ENDIF
*   Reset length again
            LENGTH = CHR_LEN( DEFN )

*   and set the environment variable
            CALL PSX_PUTENV( DEFN(1:EQPTR-1), DEFN(EQPTR+1:LENGTH),
     :                       STATUS )

         ENDIF

      ELSE IF ( UACT .EQ. 'PAR_RESET' ) THEN
         RVALUE = VALUE
*  Find the action number
*  SUBPAR_FINDACT should set PROGNUM correctly for all types of task
*  and to 0 if the action is not found (including VALUE blank).
*  STATUS is returned bad if it's a monolith and the action is not
*  found - this is acceptable only if value is blank.
         CALL SUBPAR_FINDACT( VALUE, NAMECODE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL SUBPAR_DEACT( 'R', STATUS )
         ELSEIF ( LENGTH .EQ. 0 ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL SUBPAR_DEACT( 'R', STATUS )
         ENDIF

      ELSE
*  Invalid CONTROL action
         STATUS = DTASK__INVCONTROL
         CALL ERR_REP( 'DTASK_CONTROL1',
     :   'DTASK: Invalid ACTION request for CONTROL context',
     :    STATUS )

      ENDIF

*  Acknowledge control message to requesting task

      MESSTATUS = STATUS
      STATUS = SAI__OK
      CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, CONTROL, ACTION,
     : RVALUE(1:RLEN), STATUS )

      END
