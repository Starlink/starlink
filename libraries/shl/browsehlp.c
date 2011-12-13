/*
*+
 *  Name:
 *    browsehlp
 *
 *  Purpose:
 *    Generic help browser
 *
 *  Language:
 *    Starlink ANSI C
 *

 * Usage:
 *    helpc [-l library.shl] [topic [subtopic [subsubtopic ...]]]

 * Description:
 *    The following description assumes this library function is
 *    called as described below by a thin calling routine from main.
 *    The application itself has been factored into a subroutine to simplify
 *    code re-use. Whenever "application" is used it refers to this
 *    library function. The usage described above is assumed since this
 *    module process the command line arguments.
 *
 *    The subroutine is designed to be called with argv and returned
 *    directly to the main routine.
 *
 *    This application is an interactive browser to display the contents of a
 *    Starlink help library on an alphanumeric terminal. The user can navigate
 *    through the library with the following responses to the prompt:
 *
 *    -  A blank response gets you one level up in the topic hierarchy.
 *    -  A question mark (?) re-displays the current topic.
 *    -  An end-of-file character exits. This is usually Ctrl-d.
 *    -  Any normal text specifies (sub-) topics to look for.
 *    -  Each blank-separated word stands for one topic in the
 *       hierarchy. E.g. three blank-separated words go down three
 *       levels in the hierarchy.
 *    -  Each underscore-separated word stands for an underscore-separated
 *       word in a single topic.
 *    -  Words (whether separated by blanks or underscores) that are not
 *       unique topics or include wild card characters are expanded and
 *       help is given on all matching topics. Wild card characters are
 *       % for a single character and * for any number of characters
 *       including none. In the word expansion A_G_N would match
 *       active_galactic_nuclei, which is one topic. The same is true
 *       for A*_G*_N* or active or active*.
 *
 *    When the help text to be printed is longer than the terminal page,
 *    then the user is asked to press the Return key before proceeding
 *    with output. At this point, too, can an end-of-file character be
 *    given to exit immediately.

 * Parameters:
 *    -l:
 *       Next parameter is the name of the Starlink help library to be opened.
 *       If given, this parameter must be the first, the library name the
 *       second.
 *    library.shl:
 *       The name of the Starlink help library. These names usually end in
 *       ".shl". If given, "-l" must be the first and this the second
 *       parameter. ".shl" is added if none is present.
 *    topic, subtopic etc.:
 *       The initial entry point in the hierarchy of topics and subtopics in
 *       the help library.

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
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

 * Authors:
 *    timj: Tim Jenness (JAC, Hawaii)

 * History:
 *    26-JUL-2005 (TIMJ):
 *       Extract from helpm.c (figaro)
 *    2-SEP-2005 (TIMJ):
 *       Initialise fortran runtime

 * Bugs:
 *    {Enter_new_bugs_here}

 *-
 */


#if HAVE_CONFIG_H
# include <config.h>
#endif

/* If a Fortran main is defined, provide a dummy entry point to
   satisfy potential linker problems */
#if HAVE_FC_MAIN
void FC_MAIN () {}
#endif

#include <stdlib.h>
#include "shl.h"
#include "f77.h"

int main( int argc, char **argv )
{
  /* Make sure fortran is ready for us */
  cnfInitRTL( argc, argv );

  /* Really need to prompt for a library if -l has not been specified */
  return shl_standalone( NULL, 0, argc, argv );
}
