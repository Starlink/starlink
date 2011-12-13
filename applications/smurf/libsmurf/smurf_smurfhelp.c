/*
*+
*  Name:
*     SMURFHELP

*  Purpose:
*     Gives help about SMURF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     void smurf_smurfhelp( int * status );

*  Arguments:
*     status = int * (Given and Returned)
*        The global status.

*  Description:
*     Displays help about SMURF.  The help information has classified
*     and alphabetical lists of commands, general information about
*     SMURF and related material; it describes individual commands in
*     detail.
*
*     Here are some of the main options:
*       - smurfhelp
*           No parameter is given so the introduction and the top-level
*           help index is displayed.
*       - smurfhelp application/topic
*           This gives help about the specified application or topic.
*       - smurfhelp application/topic subtopic
*           This lists help about a subtopic of the specified
*           application or topic. The hierarchy of topics has a maximum
*           of four levels.
*       - smurfhelp Hints
*           This gives hints for new and intermediate users.
*       - smurfhelp summary
*           This shows a one-line summary of each application.
*       - smurfhelp classified classification
*           This lists a one-line summary of each application in the
*           given functionality classification.
*
*     See the Section "Navigating the Help Library" below for details
*     how to move around the help information, and to select the
*     topics you want to view.

*  Usage:
*     smurfhelp [topic] [subtopic] [subsubtopic] [subsubsubtopic]

*  ADAM Parameters:
*     SUBTOPIC = LITERAL (Read)
*        Subtopic for which help is to be given. [" "]
*     SUBSUBTOPIC = LITERAL (Read)
*        Subsubtopic for which help is to be given. [" "]
*     SUBSUBSUBTOPIC = LITERAL (Read)
*        Subsubsubtopic for which help is to be given. [" "]
*     TOPIC = LITERAL (Read)
*        Topic for which help is to be given. [" "]

*  Algorithm:
*     -  Calls shlAdam

*  Implementation Status:
*     -  Uses the portable help system.

*  Navigating The Help Library:
*     The help information is arranged hierarchically.  You can move
*     around the help information whenever SMURFHELP prompts.  This
*     occurs when it has either presented a screen's worth of text or
*     has completed displaying the previously requested help.  The
*     information displayed by SMURFHELP on a particular topic includes a
*     description of the topic and a list of subtopics that further
*     describe the topic.
*
*     At a prompt you may enter:
*        -  a topic and/or subtopic name(s) to display the help for that
*           topic or subtopic, so for example, "block parameters box"
*           gives help on BOX, which is a subtopic of Parameters, which
*           in turn is a subtopic of BLOCK;
*
*        -  a <CR> to see more text at a "Press RETURN to continue ..."
*           request;
*
*        -  a <CR>} at topic and subtopic prompts to move up one level
*           in the hierarchy, and if you are at the top level it will
*           terminate the help session;
*
*        -  a CTRL/D (pressing the CTRL and D keys simultaneously) in
*           response to any prompt will terminate the help session;
*
*        -  a question mark "?" to redisplay the text for the current
*           topic, including the list of topic or subtopic names; or
*
*        -  an ellipsis "..." to display all the text below the
*           current point in the hierarchy.  For example, "BLOCK..."
*           displays information on the BLOCK topic as well as
*           information on all the subtopics under BLOCK.
*
*     You can abbreviate any topic or subtopic using the following
*     rules.
*
*        -  Just give the first few characters, e.g. "PARA" for
*           Parameters.
*
*        -  Some topics are composed of several words separated by
*           underscores.  Each word of the keyword may be abbreviated,
*           e.g. "Colour_Set" can be shortened to "C_S".
*
*        -  The characters "%" and "*" act as wildcards, where the
*           percent sign matches any single character, and asterisk
*           matches any sequence of characters.  Thus to display
*           information on all available topics, type an asterisk in
*           reply to a prompt.
*
*        -  If a word contains, but does end with an asterisk wildcard,
*           it must not be truncated.
*
*        -  The entered string must not contain leading or embedded
*           spaces.
*
*     Ambiguous abbreviations result in all matches being displayed.

*  Copyright:
*     Copyright (C) 1986, 1988, 1991-1992 Science & Engineering
*     Research Council. Copyright (C) 1995, 2004 Central Laboratory of
*     the Research Councils. Copyright (C) 2006 Particle Physics
*     and Astronomy Research Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     01-NOV-2006 (TIMJ):
*        Import from kaphelp.f
*     2009-10-15 (TIMJ):
*        Rename to smurf_smurfhelp so that the documentation appears
*        in the correct alphabetical order.
*     {enter_further_changes_here}

*-
*/

#include "star/shl.h"
#include "sae_par.h"
#include "smurflib.h"

void smurf_smurfhelp( int * status ) {

  if (*status != SAI__OK) return;

  /*  Open the help library application layer */
  shlAdam( "SMURF", 1, status );

}
