      SUBROUTINE POLHELP( STATUS )
*+
*  Name:
*     POLHELP

*  Purpose:
*     Display information about POLPACK.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLHELP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application displays information about POLPACK. This includes
*     general topics common to all applications, as well as detailed
*     descriptions of each application. The information is organised in a
*     hierarchical structure of topics, subtopics, sub-subtopics, etc.
*     Each entry ends with a list of related sub-topics. You can then
*     examine any of these sub-topics or return to the previous level.

*  Usage:
*     polhelp [topic] [subtopic] [subsubtopic] [subsubsubtopic]

*  ADAM Parameters:
*     TOPIC = LITERAL (Read)
*        Topic for which help is to be given. [" "]
*     SUBTOPIC = LITERAL (Read)
*        Subtopic for which help is to be given. [" "]
*     SUBSUBTOPIC = LITERAL (Read)
*        Subsubtopic for which help is to be given. [" "]
*     SUBSUBSUBTOPIC = LITERAL (Read)
*        Subsubsubtopic for which help is to be given. [" "]

*  Navigating the Help Tree:
*     The text for each topic is displayed in screen-fulls. A prompt is issued
*     at the end of each topic at which you may:
*
*        -  enter a topic and/or subtopic name(s) to display the help for
*           that topic or subtopic, so for example, "polka parameters dpi"
*           gives help on DPI, which is a subtopic of Parameters, which
*           in turn is a subtopic of POLKA;
*        -  press the RETURN key to see more text at a "Press RETURN to
*           continue ..." request;
*        -  press the RETURN key at topic and subtopic prompts to move up
*           one level in the hierarchy, and if you are at the top level it
*           will terminate the help session;
*        -  enter CTRL/D (i.e. press the CTRL and D keys simultaneously) in
*           response to any prompt will terminate the help session;
*        -  enter a question mark "?" to redisplay the text for the current
*           topic, including the list of topic or subtopic names; or
*        -  enter an ellipsis "..." to display all the text below the
*           current point in the hierarchy.  For example, "POLKA..."
*           displays information on the POLKA topic as well as
*           information on all the subtopics under POLKA.
*
*     You can abbreviate any topic or subtopic using the following
*     rules.
*
*        -  Just give the first few characters, e.g. "PARA" for
*           Parameters.
*        -  Some topics are composed of several words separated by
*           underscores.  Each word of the keyword may be abbreviated,
*           e.g. "Colour_Set" can be shortened to "C_S".
*        -  The characters "%" and "*" act as wild-cards, where the
*           percent sign matches any single character, and asterisk
*           matches any sequence of characters.  Thus to display
*           information on all available topics, type an asterisk in
*           reply to a prompt.
*        -  If a word contains, but does end with an asterisk wild-card,
*           it must not be truncated.
*        -  The entered string must not contain leading or embedded
*           spaces.
*
*     Ambiguous abbreviations result in all matches being displayed.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1997 July 1 (DSB):
*        Original, copied from kaphelp.f written by MJC.
*     2004 July 28 (TIMJ):
*        Switch to SHL library
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
      CHARACTER*12 LIBNAM      ! Name of the POLPACK help library
      PARAMETER ( LIBNAM = 'POLPACK_HELP' )

*  Local Variables:

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the help library application layer
      CALL SHL_ADAM( LIBNAM, .TRUE., STATUS)

      END
