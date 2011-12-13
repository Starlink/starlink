      SUBROUTINE CCDHELP( STATUS )
*+
*  Name:
*     CCDHELP

*  Purpose:
*     Gives help about CCDPACK.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CCDHELP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Displays help about CCDPACK using the CCDHELP.SHL library.
*
*     Here are some of the main options:
*        CCDHELP
*           No parameter is given so the introduction and the top-level
*           help index is displayed.
*        CCDHELP application
*           Gives help on the named application.
*
*     Once in the help library, it can be navigated in the normal
*     way.  CTRL/Z to exit from any level, and <CR> to move up a
*     level in the hierarchy.

*  Usage:
*     CCDHELP [TOPIC] [SUBTOPIC] [SUBSUBTOPIC] [SUBSUBSUBTOPIC]

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

*  Copyright:
*     Copyright (C) 1986, 1988, 1991-1993 Science & Engineering
*     Research Council. Copyright (C) 2004 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1986 November 14 (MJC):
*        Original.
*     1988 October 11 (MJC):
*        Fixed bug to enable subtopics to be accessed from the command
*        line.
*     1991 September 25 (MJC):
*        Corrected the description for the new layout in the new help
*        library, KAPPA.
*     1992 June 17 (MJC):
*        UNIX version using portable HELP.
*     1992 August 19 (MJC):
*        Rewrote the description, added usage and implementation status.
*     1993 January 5 (PDRAPER):
*        Extract kaphelp system from KAPPA and converted for
*        CCDPACK use. As few changes as possible made.
*     2004 July 30 (MBT):
*        Changed GETHLP to SHL_GETHLP to use new external help library.
*     2004 December 1 (TIMJ):
*        Use the ADAM helper code from SHL rather than low level SHL_GETHLP
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions

*  Status:
      INTEGER STATUS

*  External References:

*  Local Constants:
      CHARACTER*12 LIBNAM      ! Name of the KAPPA help library
      PARAMETER ( LIBNAM = 'CCDPACK_HELP' )

*  Local Variables:

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call out to the help support library
      CALL SHL_ADAM( LIBNAM, .TRUE., STATUS )

      END
* $Id$
