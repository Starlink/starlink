
      SUBROUTINE ESPHELP( STATUS )
*+
*  Name:
*     ESPHELP

*  Purpose:
*     Gives help about the ESP package.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ESPHELP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Displays help about ESPPACK using the ESPHELP.SHL library.
*
*     Here are some of the main options:
*        ESPHELP
*           No parameter is given so the introduction and the top-level
*           help index is displayed.
*        ESPHELP application
*           Gives help on the named application.
*
*     Once in the help library, it can be navigated in the normal
*     way.  CTRL/Z to exit from any level, and <CR> to move up a
*     level in the hierarchy.

*  Usage:
*     ESPHELP [TOPIC] [SUBTOPIC] [SUBSUBTOPIC] [SUBSUBSUBTOPIC]

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

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     GJP: Grant Privett (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1994 November 15 (GJP):
*          Taken from the CCDHELP application - originally
*          written by Malcolm Currie and Peter Draper.
*     2004 July 28 (TIMJ)
*          Switch to SHL library
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
      CHARACTER*12 LIBNAM      ! Name of the ESP help library
      PARAMETER ( LIBNAM = 'ESP_HELP' )

*  Local Variables:
                               ! less trailing blanks
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the help library application layer
      CALL SHL_ADAM( LIBNAM, .TRUE., STATUS)

      END
