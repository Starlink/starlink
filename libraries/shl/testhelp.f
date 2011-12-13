      SUBROUTINE TESTHELP( STATUS )
*+
*  Name:
*     TESTHELP

*  Purpose:
*     Gives help about specified application

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Starlink A-task

*  Invocation:
*     CALL TESTHELP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Displays help from the specified help library and provides a method
*     to navigate the help system. Intended to be called from an ADAM
*     task subroutine directly with the name of the library to browse.
*     Expectation is that the ADAM parameters specified below will
*     be present in a correspondingly named IFL file. A template
*     IFL file is available.
*
*     See the Section "Navigating the Help Library" for details how to
*     move around the help information, and to select the topics you
*     want to view.

*  Usage:
*     testhelp [topic] [subtopic] [subsubtopic] [subsubsubtopic]

*  ADAM Parameters:
*     TOPIC = LITERAL (Read)
*        Topic for which help is to be given. [" "]
*     SUBTOPIC = LITERAL (Read)
*        Subtopic for which help is to be given. [" "]
*     SUBSUBTOPIC = LITERAL (Read)
*        Subsubtopic for which help is to be given. [" "]
*     SUBSUBSUBTOPIC = LITERAL (Read)
*        Subsubsubtopic for which help is to be given. [" "]

*  Navigating the Help Library:
*     The help information is arranged hierarchically.  You can move
*     around the help information whenever XXXHELP prompts.  This
*     occurs when it has either presented a screen's worth of text or
*     has completed displaying the previously requested help.  The
*     information displayed by XXXHELP on a particular topic includes a
*     description of the topic and a list of subtopics that further
*     describe the topic.
*
*     At a prompt you may enter:
*        o  a topic and/or subtopic name(s) to display the help for that
*           topic or subtopic, so for example, "block parameters box"
*           gives help on BOX, which is a subtopic of Parameters, which
*           in turn is a subtopic of BLOCK;
*
*        o  a <CR> to see more text at a "Press RETURN to continue ..."
*           request;
*
*        o  a <CR>} at topic and subtopic prompts to move up one level
*           in the hierarchy, and if you are at the top level it will
*           terminate the help session;
*
*        o  a CTRL/D (pressing the CTRL and D keys simultaneously) in
*           response to any prompt will terminate the help session;
*
*        o  a question mark "?" to redisplay the text for the current
*           topic, including the list of topic or subtopic names; or
*
*        o  an ellipsis "..." to display all the text below the
*           current point in the hierarchy.  For example, "BLOCK..."
*           displays information on the BLOCK topic as well as
*           information on all the subtopics under BLOCK.
*
*     You can abbreviate any topic or subtopic using the following
*     rules.
*
*        o  Just give the first few characters, e.g. "PARA" for
*           Parameters.
*
*        o  Some topics are composed of several words separated by
*           underscores.  Each word of the keyword may be abbreviated,
*           e.g. "Colour_Set" can be shortened to "C_S".
*
*        o  The characters "%" and "*" act as wildcards, where the
*           percent sign matches any single character, and asterisk
*           matches any sequence of characters.  Thus to display
*           information on all available topics, type an asterisk in
*           reply to a prompt.
*
*        o  If a word contains, but does end with an asterisk wildcard,
*           it must not be truncated.
*
*        o  The entered string must not contain leading or embedded
*           spaces.
*
*     Ambiguous abbreviations result in all matches being displayed.

*  Implementation Status:
*     -  Uses the portable help system.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25 July 2004 (TIMJ):
*        Simple template for new SHL system.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*.

*     The test reads a known help file
      CALL SHL_ADAM( './demo', .FALSE., STATUS )

*  In general, an environment variable would be used.
*  $DEMO_HELP in these (commented out) examples
C      CALL SHL_ADAM( 'DEMO', .TRUE., STATUS )
C      CALL SHL_ADAM( 'DEMO_HELP', .TRUE., STATUS )

C  Obviously nothing stops you adding an extra parameter
C  that could be used to request the help system prior to calling
C  SHL_ADAM

      END
