      SUBROUTINE MESSYS_ASTINT ( STATUS )
*+
*  Name:
*     MESSYS_ASTINT

*  Purpose:
*     Cause MESSYS_RECEIVE to exit

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL MESSYS_ASTINT ( STATUS )

*  Description:
*     This routine should only be called from within an AST handler
*     routine. Its purpose is to enable an "ASTINT" event to be
*     signalled to the main-line code.
*     It causes MESSYS_RECEIVE to detect an "ASTINT" condition
*     and to return with a suitable status. No information is passed
*     using this routine other than the fact that an interrupt has
*     occurred.
*     See also MESSYS_ASTMSG.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     Call the FAMS library.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
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
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     09-MAR-1988 (REVAD::BDK):
*        Original
*     14-MAR-1988 (REVAD::BDK):
*        Don't use NULL_Q as reply queue
*     02-NOV-1992: implement using MESSYS_ASTMSG prior to retirement
*                 (RLVAD::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL MESSYS_ASTMSG( ' ', 1, ' ', STATUS )

      END


      SUBROUTINE MESSYS_ASTMSG ( NAME, LENGTH, VALUE, STATUS )
*+
*  Name:
*     MESSYS_ASTMSG

*  Purpose:
*     Cause MESSYS_RECEIVE to return an AST message

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL MESSYS_ASTMSG ( NAME, LENGTH, VALUE, STATUS )

*  Description:
*     This routine should only be called from within an AST handler
*     routine. Its purpose is to enable an "ASTINT" event to be
*     signalled to the main-line code.
*     It causes MESSYS_RECEIVE to detect an "ASTINT" condition
*     and to return with a suitable status and the content of the AST
*     message.

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*          name of the action to be rescheduled
*     LENGTH=INTEGER (given)
*          number of significant bytes in VALUE
*     VALUE=CHARACTER*(*) (given)
*
*          message to be passed to main-line code
*     STATUS=INTEGER

*  Algorithm:
*     Call the FAMS library.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     09-APR-1991 (REVAD::BDK):
*        Original
*     12-APR-1994 (REVAD::BDK):
*        Unix version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) NAME     ! name of the action to be rescheduled
      INTEGER LENGTH         ! number of significant bytes in VALUE
      CHARACTER*(*) VALUE    ! message to be passed to main-line code

*  Status:
      INTEGER STATUS

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_ASTMSG ( NAME, LENGTH, VALUE, STATUS )

      END


      SUBROUTINE MESSYS_EXTINT ( STATUS )
*+
*  Name:
*     MESSYS_EXTINT

*  Purpose:
*     Cause MESSYS_RECEIVE to exit

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL MESSYS_EXTINT ( STATUS )

*  Description:
*     This routine should only be called from within an AST handler
*     routine. Its purpose is to enable an "EXTINT" event to be
*     signalled to the main-line code.
*     It causes MESSYS_RECEIVE or MESSYS_GETREPLY to detect an "EXTINT"
*     condition and to return with a suitable status. No information is
*     passed using this routine other than the fact that an interrupt
*     has occurred.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     Call the FAMS library.

*  Copyright:
*     Copyright (C) 1988, 1994 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     09-MAR-1988 (REVAD::BDK):
*        Original
*     14-MAR-1988 (REVAD::BDK):
*        Don't use NULL_Q as the reply queue
*     12-APR-1994 (REVAD::BDK):
*        Unix version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_EXTINT ( STATUS )

      END


      SUBROUTINE MESSYS_GETREPLY ( TIMEOUT, PATH, MESSID, STUFF,
     :  STATUS )
*+
*  Name:
*     MESSYS_GETREPLY

*  Purpose:
*     Receive a message from a defined path and messid

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL MESSYS_GETREPLY ( TIMEOUT, PATH, MESSID, STUFF,
*     :  STATUS )

*  Description:
*     Receive a data-structure as a message from a specified other
*     process as part of a specified transaction or from an AST event.
*     The receive is performed with a timeout facility.

*  Arguments:
*     TIMEOUT=INTEGER (given)
*            timeout time in milliseconds
*     PATH=INTEGER (given)
*            path number
*     MESSID=INTEGER (given)
*            message number
*     STUFF=CHARACTER*(*) (returned)
*            message structure received
*     STATUS=INTEGER

*  Algorithm:
*     Call the FAMS library.
*     If a message is received which has to be returned to the software
*     calling this routine, then translate it from internal to external
*     message structure format and return it along with status
*     information.

*  Implementation Deficiencies:
*     NB. Unlike ADAM v1, GETREPLY no longer handles INIT requests.

*  Copyright:
*     Copyright (C) 1988, 1994 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     Charlie Richardson (REVS::DCR) 18Feb85
*     B.D.Kelly (REVAD::BDK) 7Mar88
*     {enter_new_authors_here}

*  History:
*     07-MAR-1988 (REVAD::BDK):
*        Total rewrite to use MSP
*     17-MAR-1988: return MSGFUNC status instead of signalling
*                  (REVAD::BDK)
*     17-MAR-1988 (REVAD::BDK):
*        Pass MESSID to ADDREST
*     15-APR-1988 (REVAD::BDK):
*        Add networking
*     10-MAY-1988 (REVAD::BDK):
*        Improve error handling and comments
*     10-MAY-1988 (REVAD::BDK):
*        Don't trample PATH and MESSID
*     12-APR-1994 (REVAD::BDK):
*        Unix version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_LEN'

*  Arguments Given:
      INTEGER TIMEOUT          ! timeout time in milliseconds
      INTEGER PATH             ! pointer to the path
      INTEGER MESSID           ! message number of incoming message

*  Arguments Returned:
      CHARACTER STUFF*(*)      ! message structure received in external
                               ! format
*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER MESSAGE_FUNCTION         ! message function
      INTEGER MESSAGE_STATUS           ! message status
      INTEGER MESSAGE_CONTEXT          ! message context
      CHARACTER*(MSG_NAME_LEN-1) MESSAGE_NAME       ! message name
      INTEGER MESSAGE_LENGTH           ! length of value string
      CHARACTER*(MSG_VAL_LEN) MESSAGE_VALUE      ! value string
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_GETREPLY ( TIMEOUT, PATH, MESSID, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  STATUS )
      MESSAGE_FUNCTION = MESSYS__MESSAGE
      CALL MESSYS_PACK ( MESSAGE_FUNCTION, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  STUFF, STATUS )

      END


      SUBROUTINE MESSYS_INIT ( OWN_NAME, STATUS )
*+
*  Name:
*     MESSYS_INIT

*  Purpose:
*     Task initialisation routine

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL MESSYS_INIT ( OWN_NAME, STATUS )

*  Description:
*     Initialises the task into the message system.

*  Algorithm:
*     Call the FAMS library.

*  Copyright:
*     Copyright (C) 1987-1988, 1991, 1993-1994 Science & Engineering
*     Research Council. All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     John Cooke (REVS::JAC) 21Mar84
*     B.D.Kelly (REVAD::BDK) 07Mar88
*     {enter_new_authors_here}

*  History:
*     07-MAR-1987 (REVAD::BDK):
*        Total rewrite to use MSP
*     21-MAR-1988 (REVAD::BDK):
*        Call MESSYS_PREFIX
*     15-APR-1988 (REVAD::BDK):
*        Initialise MACHINE_NAMES
*     04-MAY-1988: initialise ADAMNET_Q, remove THIS_TASK_INIT_ACK_Q,
*                  change OTHER_TASK_T_TRANS_NUM to OTHER_TRANSNUM
*                  (REVAD::BDK)
*     10-MAY-1988 (REVAD::BDK):
*        Check status before using DCLEXH
*     23-MAY-1991 (REVAD::BDK):
*        Add KICK_Q
*     23-JUL-1991 (REVAD::BMC):
*        Add multiple networks
*     21-JAN-1993: use MESSYS__NETNAME as root of network process names
*                  (RLVAD::AJC)
*     12-APR-1994 (REVAD::BDK):
*        Unix version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER OWN_NAME*(*)     ! name of this task

*  Status:
      INTEGER STATUS
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_INIT ( OWN_NAME, STATUS )

      END


      SUBROUTINE MESSYS_KICK ( NAME, LENGTH, VALUE, STATUS )
*+
*  Name:
*     MESSYS_KICK

*  Purpose:
*     Cause MESSYS_RECEIVE to return a KICK message

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL MESSYS_KICK ( NAME, LENGTH, VALUE, STATUS )

*  Description:
*     This routine should only be called from main-line code.
*     Its purpose is to enable a "KICK" event to be queued to the task.
*     It causes MESSYS_RECEIVE to detect a "KICK" condition
*     and to return with a suitable status and the content of the KICK
*     message.

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*          name of the action to be rescheduled
*     LENGTH=INTEGER (given)
*          number of significant bytes in VALUE
*     VALUE=CHARACTER*(*) (given)
*          message to be passed to application code
*     STATUS=INTEGER

*  Algorithm:
*     Call the FAMS library.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     23-MAY-1991 (REVAD::BDK):
*        Original
*     12-APR-1994 (REVAD::BDK):
*        Unix version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
*  Arguments Given:
      CHARACTER*(*) NAME     ! name of the action to be rescheduled
      INTEGER LENGTH         ! number of significant bytes in VALUE
      CHARACTER*(*) VALUE    ! message to be passed to application code

*  Status:
      INTEGER STATUS

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_KICK ( NAME, LENGTH, VALUE, STATUS )

      END

      SUBROUTINE MESSYS_PACK ( MESSAGE_FUNCTION, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  STUFF, STATUS )
*+
*  Name:
*     MESSYS_PACK

*  Purpose:
*     Convert a message from internal to external format

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     SUBROUTINE MESSYS_PACK ( MESSAGE_FUNCTION, MESSAGE_STATUS,
*     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
*     :  STUFF, STATUS )

*  Description:
*     Translate an incoming message from internal to external format.

*  Arguments:
*     MESSAGE_FUNCTION=INTEGER (given)
*           message function
*     MESSAGE_STATUS=INTEGER (given)
*           message status
*     MESSAGE_CONTEXT=INTEGER (given)
*           message context
*     MESSAGE_NAME=CHARACTER*(*) (given)
*           message name
*     MESSAGE_LENGTH=INTEGER (given)
*           length of value string
*     MESSAGE_VALUE=CHARACTER*(*) (given)
*           value string
*     STUFF=CHARACTER*(*) (returned)
*           the message in external format
*     STATUS=INTEGER

*  Algorithm:
*     Interpret the message.

*  Copyright:
*     Copyright (C) 1988, 1994 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     03-MAR-1988 (REVAD::BDK):
*        Original
*     11-MAY-1988 (REVAD::BDK):
*        Trap invalid messid
*     13-MAY-1988 (REVAD::BDK):
*        Store other task's transaction details
*     12-APR-1994 (REVAD::BDK):
*        Unix version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_LEN'

*    Data structures :
      INCLUDE 'MESSYS_STRUC'

*  Arguments Given:
      INTEGER MESSAGE_FUNCTION         ! message function
      INTEGER MESSAGE_STATUS           ! message status
      INTEGER MESSAGE_CONTEXT          ! message context
      CHARACTER*(*) MESSAGE_NAME       ! message name
      INTEGER MESSAGE_LENGTH           ! length of value string
      CHARACTER*(*) MESSAGE_VALUE      ! value string

*  Arguments Returned:
      CHARACTER*(*) STUFF       ! the message in external format

*  Status:
      INTEGER STATUS
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      MSG_NUMBER = 0
      MSG_STATUS = MESSAGE_STATUS
      MSG_FUNCTION = MESSAGE_FUNCTION
      MSG_CONTEXT = MESSAGE_CONTEXT
      MSG_NAME = MESSAGE_NAME
      MSG_LENGTH = MESSAGE_LENGTH
      MSG_VAL = MESSAGE_VALUE(1:MSG_LENGTH)

      STUFF = MSG

      END


      SUBROUTINE MESSYS_PATH ( TASK_NAME, PATH, STATUS )
*+
*  Name:
*     MESSYS_PATH

*  Purpose:
*     Obtain message path to/from another task

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL MESSYS_PATH ( TASK_NAME, PATH, STATUS )

*  Description:
*     Obtains a pointer to path information ( stored in common in the
*     message system routines ).  This pointer points to complete
*     round-path information.  The task to which the path is being set
*     up is also requested to initialise a connection with this calling
*     task.

*  Arguments:
*     TASK_NAME=CHARACTER*(*) (given)
*            name of task to which path is required
*            including machine name if networking
*     PATH=INTEGER ( returned)
*            pointer to the path
*     STATUS=INTEGER

*  Algorithm:
*     Call the FAMS library.

*  Copyright:
*     Copyright (C) 1988, 1991-1992, 1994 Science & Engineering
*     Research Council. All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     John Cooke (REVS::JAC) 22Mar84
*     B.D.Kelly (REVAD::BDK) 07Mar88
*     {enter_new_authors_here}

*  History:
*     07-MAR-1988 (REVAD::BDK):
*        Total rewrite to use MSP
*     15-MAR-1988 (REVAD::BDK):
*        Parameterise RMTFLAG values
*     15-MAR-1988: ignore returned MSG_STATUS - unused for ACK_INIT
*                  (REVAD::BDK)
*     11-APR-1988 (REVAD::BDK):
*        Add networking
*     28-APR-1988 (REVAD::BDK):
*        Split remote taskname from nodename
*     10-MAY-1988 (REVAD::BDK):
*        Trap overlength taskname
*     23-JUL-1991 (REVAD::BMC):
*        Handle multiple networks
*     17-AUG-1992 (RLVAD::AJC/AAOEPP:TJF):
*        Correct to allow ADAMNET_4
*     12-APR-1994 (REVAD::BDK):
*        Unix version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) TASK_NAME     ! name of task to which path is
                                  ! required, including machine name if
                                  ! networking
*  Arguments Returned:
      INTEGER PATH                ! pointer to the path

*  Status:
      INTEGER STATUS
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_PATH ( TASK_NAME, PATH, STATUS )

      END


      SUBROUTINE MESSYS_PLOOKUP ( PATH, NAME, STATUS )
*+
*  Name:
*     MESSYS_PLOOKUP

*  Purpose:
*     Look up task name for given path

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL MESSYS_PLOOKUP ( PATH, NAME, STATUS )

*  Description:
*     Return the task name corresponding to the given path.

*  Arguments:
*     PATH=INTEGER (given)
*           the path number
*     NAME=CHARACTER*(*) (returned)
*           the task name
*     STATUS=INTEGER

*  Algorithm:
*     Call the FAMS library.

*  Copyright:
*     Copyright (C) 1988, 1994 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     John Cooke (REVS::JAC) 27Mar84
*     B.D.Kelly (REVAD::BDK) 14Mar88
*     {enter_new_authors_here}

*  History:
*     14-MAR-1988 (REVAD::BDK):
*        Rewrite for MSP
*     10-MAY-1988 (REVAD::BDK):
*        Concatenate machine name if networking
*     12-APR-1994 (REVAD::BDK):
*        Unix version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER PATH        ! the path number

*  Arguments Returned:
      CHARACTER*(*) NAME  ! the task name

*  Status:
      INTEGER STATUS

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_PLOOKUP ( PATH, NAME, STATUS )

      END


      SUBROUTINE MESSYS_RECEIVE ( TIMEOUT, PATH, STUFF, MESSID, STATUS )
*+
*  Name:
*     MESSYS_RECEIVE

*  Purpose:
*     Receive a message

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL MESSYS_RECEIVE ( TIMEOUT, PATH, STUFF, MESSID, STATUS )

*  Description:
*     Receive a data-structure as a message from another process or from
*     an AST event. The path and messid associated with the message are
*     returned to enable the higher-level software to generate replies.
*     The receive is performed with a timeout facility.
*     RECEIVE also handles initialisation of tasks with each other. This
*     is done "invisibly" at lower levels.

*  Arguments:
*     TIMEOUT=INTEGER (given)
*            timeout time in milliseconds
*     PATH=INTEGER (returned)
*            pointer to the path
*     STUFF=CHARACTER*(*) (returned)
*            message structure received
*     MESSID=INTEGER (returned)
*            message number of incoming message
*     STATUS=INTEGER

*  Algorithm:
*     Call the FAMS library.
*     If a message is received which has to be returned to the software
*     calling this routine, then translate it from internal to external
*     message structure format and return it along with status information.

*  Copyright:
*     Copyright (C) 1988, 1991, 1994 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     John Cooke (REVS::JAC) 27Mar84
*     Charlie Richardson (REVS::DCR) 5Mar85
*     B.D.Kelly (REVAD::BDK) 2Mar88
*     {enter_new_authors_here}

*  History:
*     02-MAR-1988 (REVAD::BDK):
*        Total rewrite to use MSP
*     15-APR-1988 (REVAD::BDK):
*        Add networking
*     11-MAY-1988 (REVAD::BDK):
*        Improve error handling
*     23-MAY-1991 (REVAD::BDK):
*        Add KICK_Q
*     12-APR-1994 (REVAD::BDK):
*        Unix version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'
      INCLUDE 'MESSYS_LEN'

*  Arguments Given:
      INTEGER TIMEOUT          ! timeout time in milliseconds

*  Arguments Returned:
      INTEGER PATH             ! pointer to the path
      CHARACTER*(*) STUFF      ! message structure received in external
                               ! format
      INTEGER MESSID           ! message number of incoming message

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER MESSAGE_FUNCTION         ! message function
      INTEGER MESSAGE_STATUS           ! message status
      INTEGER MESSAGE_CONTEXT          ! message context
      CHARACTER*(MSG_NAME_LEN-1) MESSAGE_NAME       ! message name
      INTEGER MESSAGE_LENGTH           ! length of value string
      CHARACTER*(MSG_VAL_LEN) MESSAGE_VALUE      ! value string
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_RECEIVE ( TIMEOUT, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  PATH, MESSID, STATUS )
      MESSAGE_FUNCTION = MESSYS__MESSAGE
      CALL MESSYS_PACK ( MESSAGE_FUNCTION, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  STUFF, STATUS )

      END


      SUBROUTINE MESSYS_REPLY ( PATH, STUFF, MESSID, STATUS )
*+
*  Name:
*     MESSYS_REPLY

*  Purpose:
*     Send a reply as part of a continuing transaction

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL MESSYS_REPLY ( PATH, STUFF, MESSID, STATUS )

*  Description:
*     Send a data-structure as a message to another process. This must
*     occur as part of a previously initiated transaction - ie both PATH
*     and MESSID are valid.

*  Arguments:
*     PATH=INTEGER (given)
*            the path number for communicating with the other task
*     STUFF=CHARACTER*(*) (given)
*            the message to be sent
*     MESSID=INTEGER (given)
*            the number identifying the transaction
*     STATUS=INTEGER

*  Algorithm:
*     Construct the message
*     Call the FAMS library.

*  Implementation Deficiencies:
*     Note that the concept of END-OF-TRANSACTION implies an unfortunate
*     link between MESSYS and the tasking model.

*  Copyright:
*     Copyright (C) 1988, 1990, 1994 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     John Cooke (REVS::JAC) 22Mar84
*     B.D.Kelly (REVAD::BDK) 09Mar88
*     {enter_new_authors_here}

*  History:
*     09-MAR-1988 (REVAD::BDK):
*        Total rewrite to use MSP
*     14-MAR-1988 (REVAD::BDK):
*        Don't use NULL_Q as the reply queue
*     15-MAR-1988 (REVAD::BDK):
*        Don't try to delete a NULL_Q
*     16-MAR-1988 (REVAD::BDK):
*        Copy the value string into the message!
*     15-APR-1988 (REVAD::BDK):
*        Add networking
*     11-MAY-1988 (REVAD::BDK):
*        Remove ACK_INIT
*     19-APR-1990: Add MESSYS__TRIGGER to the list of continuing transaction
*           types (AAO::TJF)
*     19-APR-1990: Add MESSYS__SYNCREP to the list of continuing transaction
*           types (BDK)
*     12-APR-1994 (REVAD::BDK):
*        Unix version
*     {enter_further_changes_here}

*  Bugs:
*     None known
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_LEN'

*  Arguments Given:
      INTEGER PATH         ! the path number for communicating with the
                           ! other task

      CHARACTER*(*) STUFF  ! the message to be sent

      INTEGER MESSID       ! the number identifying the transaction

*  Status:
      INTEGER     STATUS

*  Local Variables:
      INTEGER MESSAGE_FUNCTION         ! message function
      INTEGER MESSAGE_STATUS           ! message status
      INTEGER MESSAGE_CONTEXT          ! message context
      CHARACTER*(MSG_NAME_LEN-1) MESSAGE_NAME       ! message name
      INTEGER MESSAGE_LENGTH           ! length of value string
      CHARACTER*(MSG_VAL_LEN) MESSAGE_VALUE      ! value string
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL MESSYS_UNPACK ( STUFF, MESSAGE_FUNCTION, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  STATUS )
      CALL FAMS_REPLY ( PATH, MESSID, MESSAGE_FUNCTION, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  STATUS )

      END


      SUBROUTINE MESSYS_RESMSG ( LENGTH, VALUE, STATUS )
*+
*  Name:
*     MESSYS_RESMSG

*  Purpose:
*     Cause MESSYS_RECEIVE to return a reschedule message

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL MESSYS_RESMSG ( LENGTH, VALUE, STATUS )

*  Description:
*     This routine should only be called from within an AST handler
*     routine. Its purpose is to enable a "RESCHED" event to be
*     signalled to the main-line code.
*     It causes MESSYS_RECEIVE to detect a "RESCHED" condition
*     and to return with a suitable status and the content of the
*     RESCHED message.

*  Arguments:
*     LENGTH=INTEGER (given)
*          number of significant bytes in VALUE
*     VALUE=CHARACTER*(*) (given)
*          message to be passed to main-line code
*     STATUS=INTEGER

*  Algorithm:
*     Call the FAMS library.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     13-MAY-1991 (REVAD::BDK):
*        Original
*     12-APR-1994 (REVAD::BDK):
*        Unix version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER LENGTH         ! number of significant bytes in VALUE
      CHARACTER*(*) VALUE    ! message to be passed to main-line code

*  Status:
      INTEGER STATUS

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_RESMSG ( LENGTH, VALUE, STATUS )

      END


      SUBROUTINE MESSYS_SEND ( PATH, STUFF, MESSID, STATUS )
*+
*  Name:
*     MESSYS_SEND

*  Purpose:
*     Message system low-level routine to send a message

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL MESSYS_SEND ( PATH, STUFF, MESSID, STATUS )

*  Description:
*     Send a MSG data-structure as a message to another process.
*     The path to this process must already have been established using
*     the PATH call.
*     Use of SEND implies that the other process is expected to send an
*     acknowledgment.
*     The path structure is added within this routine.

*  Algorithm:
*     translate the message to internal format
*     Call the FAMS library.

*  Copyright:
*     Copyright (C) 1988, 1994 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     John Cooke (REVS::JAC) 22Mar84
*     B.D.Kelly (REVAD::BDK) 08Mar88
*     {enter_new_authors_here}

*  History:
*     08-MAR-1988 (REVAD::BDK):
*        Total rewrite to use MSP
*     14-MAR-1988 (REVAD::BDK):
*        Don't use NULL_Q as reply queue
*     17-MAR-1988 (REVAD::BDK):
*        Don't create ACK_Q for DE_INIT
*     11-APR-1988 (REVAD::BDK):
*        Add networking
*     28-APR-1988: don't append nodename to task when networking
*                  (REVAD::BDK)
*     04-MAY-1988: change OTHER_TASK_T_TRANS_NUM to OTHER_TRANSNUM
*                  (REVAD::BDK)
*     12-APR-1994 (REVAD::BDK):
*        Unix version
*     {enter_further_changes_here}

*  Bugs:
*     None known
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_LEN'

*  Arguments Given:
      INTEGER PATH             ! pointer to the path
      CHARACTER*(*) STUFF      ! structure to be sent

*  Arguments Returned:
      INTEGER MESSID           ! message number issued by this task

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER MESSAGE_FUNCTION         ! message function
      INTEGER MESSAGE_STATUS           ! message status
      INTEGER MESSAGE_CONTEXT          ! message context
      CHARACTER*(MSG_NAME_LEN-1) MESSAGE_NAME       ! message name
      INTEGER MESSAGE_LENGTH           ! length of value string
      CHARACTER*(MSG_VAL_LEN) MESSAGE_VALUE      ! value string
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL MESSYS_UNPACK ( STUFF, MESSAGE_FUNCTION, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  STATUS )
      CALL FAMS_SEND ( PATH, MESSAGE_FUNCTION, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  MESSID, STATUS )

      END



      SUBROUTINE MESSYS_UNPACK ( STUFF, MESSAGE_FUNCTION,
     :  MESSAGE_STATUS, MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH,
     :  MESSAGE_VALUE, STATUS )
*+
*  Name:
*     MESSYS_UNPACK

*  Purpose:
*     Convert a message from internal to external format

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL MESSYS_UNPACK ( STUFF, MESSAGE_FUNCTION, MESSAGE_STATUS,
*     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
*     :  STATUS )

*  Description:
*     Translate a message to be sent from external to internal format.

*  Arguments:
*     STUFF=CHARACTER*(*) (given)
*           the message in external format
*     MESSAGE_FUNCTION=INTEGER (returned)
*           message function
*     MESSAGE_STATUS=INTEGER (returned)
*           message status
*     MESSAGE_CONTEXT=INTEGER (returned)
*           message context
*     MESSAGE_NAME=CHARACTER*(*) (returned)
*           message name
*     MESSAGE_LENGTH=INTEGER (returned)
*           length of value string
*     MESSAGE_VALUE=CHARACTER*(*) (returned)
*           value string
*     STATUS=INTEGER

*  Algorithm:
*     Interpret the message.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     12-APR-1994 (REVAD::BDK):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_LEN'

*    Data structures :
      INCLUDE 'MESSYS_STRUC'

*  Arguments Given:
      INTEGER MESSAGE_FUNCTION         ! message function
      INTEGER MESSAGE_STATUS           ! message status
      INTEGER MESSAGE_CONTEXT          ! message context
      CHARACTER*(*) MESSAGE_NAME       ! message name
      INTEGER MESSAGE_LENGTH           ! length of value string
      CHARACTER*(*) MESSAGE_VALUE      ! value string

*  Arguments Returned:
      CHARACTER*(*) STUFF       ! the message in external format

*  Status:
      INTEGER STATUS

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      MSG = STUFF

      MESSAGE_STATUS = MSG_STATUS
      MESSAGE_FUNCTION = MSG_FUNCTION
      MESSAGE_CONTEXT = MSG_CONTEXT
      MESSAGE_NAME = MSG_NAME
      MESSAGE_LENGTH = MSG_LENGTH
      MESSAGE_VALUE = MSG_VAL(1:MSG_LENGTH)

      END
