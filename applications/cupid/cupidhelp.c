#include "sae_par.h"
#include "cupid.h"
#include "star/shl.h"

void cupidhelp( int *status ){
/*
*+
*  Name:
*     CUPIDHELP

*  Purpose:
*     Display information about CUPID.

*  Language:
*     C

*  Type of Module:
*     ADAM A-task

*  Description:
*     This application displays information about CUPID. This includes
*     general topics common to all applications, as well as detailed
*     descriptions of each application. The information is organised in a
*     hierarchical structure of topics, subtopics, sub-subtopics, etc.
*     Each entry ends with a list of related sub-topics. You can then
*     examine any of these sub-topics or return to the previous level.

*  Usage:
*     cupidhelp [topic] [subtopic] [subsubtopic] [subsubsubtopic]

*  ADAM Parameters:
*     TOPIC = LITERAL (Read)
*        Topic for which help is to be given. [" "]
*     SUBTOPIC = LITERAL (Read)
*        Subtopic for which help is to be given. [" "]
*     SUBSUBTOPIC = LITERAL (Read)
*        Subsubtopic for which help is to be given. [" "]
*     SUBSUBSUBTOPIC = LITERAL (Read)
*        Subsubsubtopic for which help is to be given. [" "]

*  Navigating The Help Tree:
*     The text for each topic is displayed in screen-fulls. A prompt is issued
*     at the end of each topic at which you may:
*
*        -  enter a topic and/or subtopic name(s) to display the help for
*           that topic or subtopic, so for example, "polka parameters dpi"
*           gives help on DPI, which is a subtopic of Parameters, which
*           in turn is a subtopic of FINDCLUMPS;
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
*           current point in the hierarchy.  For example, "FINDCLUMPS..."
*           displays information on the FINDCLUMPS topic as well as
*           information on all the subtopics under FINDCLUMPS.
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

*  Synopsis:
*     void cupidhelp( int *status );

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-SEP-2005 (DSB):
*        Original version.
*     12-MAY-2011 (TIMJ):
*        Use the official C interface to SHL
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Open the help library application layer */
   shlAdam( "CUPID", 1, status );

}
