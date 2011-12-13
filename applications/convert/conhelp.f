      SUBROUTINE CONHELP( STATUS )
*+
*  Name:
*     CONHELP

*  Purpose:
*     Gives help about CONVERT.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CONHELP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Displays help about CONVERT.  The help information has classified
*     and alphabetical lists of commands, general information about
*     CONVERT and related material; it describes individual commands in
*     detail.
*
*     Here are some of the main options:
*        CONHELP
*           No parameter is given so the introduction and the top-level
*           help index is displayed.
*        CONHELP application/topic
*           This gives help about the specified application or topic.
*        CONHELP application/topic subtopic
*           This lists help about a subtopic of the specified
*           application or topic. The hierarchy of topics has a maximum
*           of four levels.
*        CONHELP SUMMARY
*           This shows a one-line summary of each application.
*
*     Once in the help library, it can be navigated in the normal
*     way.  CTRL/Z (on VMS) and CTRL/D (on UNIX) to exit from any level,
*     and <CR> to move up a level in the hierarchy.

*  Usage:
*     CONHELP [TOPIC] [SUBTOPIC] [SUBSUBTOPIC] [SUBSUBSUBTOPIC]

*  ADAM Parameters:
*     TOPIC = LITERAL (Read)
*        Topic for which help is to be given. [" "]
*     SUBTOPIC = LITERAL (Read)
*        Subtopic for which help is to be given. [" "]
*     SUBSUBTOPIC = LITERAL (Read)
*        Subsubtopic for which help is to be given. [" "]
*     SUBSUBSUBTOPIC = LITERAL (Read)
*        Subsubsubtopic for which help is to be given. [" "]

*  Algorithm:
*     -  Check for error on entry; return if not o.k.
*     -  Obtain topic and subtopics required.  Concatenate them
*        separated by spaces.
*     -  If an error has occurred set all topics to be null.
*     -  Get help on required topic.

*  Implementation Status:
*     -  Uses the portable help system.
*     -  The help libraries are slightly different for VMS and UNIX.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1993 July 30 (MJC):
*        Original based on KAPHELP.
*     2004 July 28 (TIMJ):
*        Switch to SHL library
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Status:
      INTEGER STATUS

*  External References:

*  Local Constants:
      CHARACTER * ( 12 ) LIBNAM  ! Name of the CONVERT help library
      PARAMETER ( LIBNAM = 'CONVERT_HELP' )

*  Local Variables:

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the help library application layer
      CALL SHL_ADAM( LIBNAM, .TRUE., STATUS)

      END
