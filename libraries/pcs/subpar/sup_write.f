      SUBROUTINE SUBPAR_WRITE( STRING, STATUS )
*+
*  Name:
*     SUBPAR_WRITE

*  Purpose:
*     Delivery a character string to the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_WRITE ( STRING, STATUS )

*  Description:
*     The given text string is delivered to the user.

*  Arguments:
*     STRING=CHARACTER * ( * ) (Given)
*        The text to be delivered.
*     STATUS=INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     If running as a DCL task, then
*        Write the message to the terminal
*     else
*        The inter-task PATH to the task which issued the RUN command is
*        obtained from the SUBPAR common blocks, and the text-string is
*        sent to that task.
*     endif

*  Copyright:
*     Copyright (C) 1984, 1985, 1987, 1989, 1991, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 2001 Central Laboratory of the Research Councils.
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
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-NOV-1984 (BDK):
*        Original version.
*     23-JUN-1985 (BDK):
*        Use RUNID and MESSYS_REPLY.
*     15-NOV-1985 (BDK):
*        Handle DCL tasks.
*     25-NOV-1985 (BDK):
*        Trim trailing blanks if typing.
*     5-MAY-1987 (BDK):
*        Make RUNFACE an integer.
*     11-DEC-1989 (AJC):
*        Use CHR_LEN for used length in both cases.
*        Use used length for messys message.
*     25-NOV-1991 (BDK):
*        Use ADAM_ACKNOW.
*     5-MAR-1992 (AJC):
*        Use WRITE not TYPE, and use format not *.
*     24-AUG-1992 (PCTR):
*        Trap Fortran I/O errors which may occur under DCL invocation.
*     27-AUG-1992 (PCTR):
*        Limit string length to EMS__MXOUT under DCL.
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*        Change MESERRS to MESSYS_ERR
*      8-MAR-1993 (AJC):
*        Remove include MESDEFNS
*      9-AUG-1993 (AJC):
*        INCLUDE SUBPAR_PARERR not PAR_ERR
*     21-DEC-1995 (AJC):
*        Remove length limit of EMS_MXOUT. This was put in to prevent
*        a crash on VMS if you tried to write more than 132 chars.
*     13-JUN-2001 (AJC):
*        Call AMS (FAMS) directly, not via ADAM/MESSYS
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'
      INCLUDE 'SUBPAR_PARERR'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'

*  Arguments Given:
      CHARACTER * ( * ) STRING       ! The text string to be output

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*    External routines:
      INTEGER CHR_LEN                ! Used length of string

*  Local Constants:
      INTEGER NOCONTEXT
      PARAMETER ( NOCONTEXT = 0 )

      CHARACTER * ( * ) NONAME
      PARAMETER ( NONAME = ' ' )

*  Local Variables:
      INTEGER IOSTAT                 ! Fortran I/O status
      INTEGER MESSID                 ! Message number of the RUN message
      INTEGER OUTLEN                 ! Length of output string
      INTEGER USRPATH                ! PATH to the controlling task

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the used length of given string.
      OUTLEN = CHR_LEN( STRING )

*  Check the environment.
      IF ( RUNFACE .EQ. SUBPAR__TERM ) THEN

*     Run from shell - output the string directly to terminal.
         WRITE ( *, '( A )', IOSTAT=IOSTAT ) STRING( : OUTLEN )

*     Check the returned Fortran I/O status.
         IF ( IOSTAT .NE. 0 ) STATUS = SUBPAR__OPTER
      ELSE IF ( RUNFACE .EQ. SUBPAR__TASK ) THEN

*     Run from an ADAM command language - check that the PATH exists.
         IF ( RUNPATH .GT. 0 ) THEN
            USRPATH = RUNPATH
            MESSID = RUNID
            CALL FAMS_REPLY( RUNPATH, RUNID,
     :        MESSYS__MESSAGE, MESSYS__INFORM, NOCONTEXT, NONAME,
     :        OUTLEN, STRING( : OUTLEN), STATUS )

         ELSE
            STATUS = PAR__NOUSR
         END IF
      ELSE

*     A UTASK - call the UTASK library, when available.
*     For now, ERROR.
         STATUS = PAR__NOUSR
      END IF

      END
