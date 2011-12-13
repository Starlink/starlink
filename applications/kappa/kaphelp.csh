#!/bin/csh
#+
#  Name:
#     KAPHELP

#  Purpose:
#     Gives help about KAPPA.

#  Language:
#     C-shell

#  Type of Module:
#     C-shell script

#  Description:
#     Displays help about KAPPA.  The help information has classified
#     and alphabetical lists of commands, general information about
#     KAPPA and related material; it describes individual commands in
#     detail.
#
#     Here are some of the main options.
#        kaphelp
#           No parameter is given so the introduction and the top-level
#           help index is displayed.
#        kaphelp application/topic
#           This gives help about the specified application or topic.
#        kaphelp application/topic subtopic
#           This lists help about a subtopic of the specified
#           application or topic. The hierarchy of topics has a maximum
#           of four levels.
#        kaphelp Hints
#           This gives hints for new and intermediate users.
#        kaphelp summary
#           This shows a one-line summary of each application.
#        kaphelp classified classification
#           This lists a one-line summary of each application in the
#           given functionality classification.
#
#     See the Section "Navigating the Help Library" for details how to
#     move around the help information, and to select the topics you
#     want to view.

#  Usage:
#     kaphelp [topic] [subtopic] [subsubtopic] [subsubsubtopic]

#  ADAM Parameters:
#     TOPIC = LITERAL (Read)
#        Topic for which help is to be given. [" "]
#     SUBTOPIC = LITERAL (Read)
#        Subtopic for which help is to be given. [" "]
#     SUBSUBTOPIC = LITERAL (Read)
#        Subsubtopic for which help is to be given. [" "]
#     SUBSUBSUBTOPIC = LITERAL (Read)
#        Subsubsubtopic for which help is to be given. [" "]

#  Implementation Status:
#     -  Uses the portable help system.

#  Navigating The Help Library:
#     The help information is arranged hierarchically.  You can move
#     around the help information whenever KAPHELP prompts.  This
#     occurs when it has either presented a screen's worth of text or
#     has completed displaying the previously requested help.  The
#     information displayed by KAPHELP on a particular topic includes a
#     description of the topic and a list of subtopics that further
#     describe the topic.
#
#     At a prompt you may enter:
#        o  a topic and/or subtopic name(s) to display the help for that
#           topic or subtopic, so for example, "block parameters box"
#           gives help on BOX, which is a subtopic of Parameters, which
#           in turn is a subtopic of BLOCK;
#
#        o  a <CR> to see more text at a "Press RETURN to continue ..."
#           request;
#
#        o  a <CR>} at topic and subtopic prompts to move up one level
#           in the hierarchy, and if you are at the top level it will
#           terminate the help session;
#
#        o  a CTRL/D (pressing the CTRL and D keys simultaneously) in
#           response to any prompt will terminate the help session;
#
#        o  a question mark "?" to redisplay the text for the current
#           topic, including the list of topic or subtopic names; or
#
#        o  an ellipsis "..." to display all the text below the
#           current point in the hierarchy.  For example, "BLOCK..."
#           displays information on the BLOCK topic as well as
#           information on all the subtopics under BLOCK.
#
#     You can abbreviate any topic or subtopic using the following
#     rules.
#
#        o  Just give the first few characters, e.g. "PARA" for
#           Parameters.
#
#        o  Some topics are composed of several words separated by
#           underscores.  Each word of the keyword may be abbreviated,
#           e.g. "Colour_Set" can be shortened to "C_S".
#
#        o  The characters "%" and "*" act as wildcards, where the
#           percent sign matches any single character, and asterisk
#           matches any sequence of characters.  Thus to display
#           information on all available topics, type an asterisk in
#           reply to a prompt.
#
#        o  If a word contains, but does end with an asterisk wildcard,
#           it must not be truncated.
#
#        o  The entered string must not contain leading or embedded
#           spaces.
#
#     Ambiguous abbreviations result in all matches being displayed.

#  Copyright:
#     Copyright (C) 2001 Central Laboratory of the Research Councils.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     DSB: David S. Berry (STARLINK)
#     {enter_new_authors_here}

#  History:
#     31-OCT-2001 (DSB):
#        Original C-shell version.
#     {enter_further_changes_here}

#-

   $STARHELP_DIR/starhelp HELPFILE=$KAPPA_HELP $1 $2 $3 $4

