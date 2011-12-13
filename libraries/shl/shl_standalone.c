/*+
 * Name:
 *    shl_standalone

 * Purpose:
 *    Browse through a Starlink help library.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    Library function

 * Invocation:
 *    status = shl_standalone( helplb, isenv, argc, argv );

 * Arguments:
 *    helplb = const char * (Given)
 *       Name of default help library to open. There must be a corresponding
 *       environment variable named HELPLB_HELP containing the help
 *       file to be opened. Will be ignored if argv contains a -l parameter.
 *       Can be a NULL pointer only if -l is guaranteed. _HELP is added
 *       to the parameter automatically if not present. ".shl" is added
 *       if not present.
 *    isenv = int (Given)
 *       If 1, indicates that helplb refers to a environment variable.
 *       If 0, indicates that it refers to a file. Ignored if -l
 *       option is present in argv or if helplb is NULL
 *    argc = int (Given)
 *       The number of elements in argv.
 *    argv = char ** (Given)
 *       Array of strings, ostensibly read from the command line.

 * Return value:
 *    status = int
 *       Contains EXIT_SUCCESS on success and EXIT_FAILURE on failure.
 *       These can be passed directly to main(). They are defined
 *       in stdlib.h

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

 * Notes:
 *    Although this library routine behaves as a full C main application
 *    routine, the main has to be provided by the programmer. It should
 *    look something like this:
 *
 *    #include "shl.h"
 *    void main( int argc, char **argv )
 *    {
 *       (void) shl_standalone( "KAPPA", 1, argc, argv );
 *    }
 *
 *    Where the first argument is the name of the application help
 *    system that is to be accessed. The assumption is that there is
 *    an environment variable named, in this case, KAPPA_HELP that
 *    contains the location of a help file.
 *
 *    The main routine can be compiled as normal but need to be linked
 *    against the shl library and the Fortran runtime libraries. Outside
 *    of autoconf, the easiest way to do this is to compile the
 *    main routine without linking, and then link using the fortran compiler:
 *
 *    cc  -I/star/include -c helpm.c
 *    f77 -o helpc helpm.o `shl_link`
 *
 *    gcc or cc cannot be used as linker, since they do not link with the
 *    Fortran libraries that FIO and HLP require. f77 seems to link C code only
 *    if the main routine is compiled by cc, not if it is compiled by gcc.
 *    Note that if you use autoconf make sure you use @FCLIBS@ and the
 *    STAR_FC_LIBRARY_LDFLAGS macro.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1998, 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
 *    hme: Horst Meyerdierks (UoE, Starlink)
 *    ajc: Alan Chipperfield (Starlink, RAL)
 *    timj: Tim Jenness (JAC, Hawaii)

 * History:
 *    28 Mar 1994 (hme):
 *       Original version.
 *    10 Feb 1998 (ajc):
 *       Mod to use termios
 *    24 Jul 2004 (timj):
 *       Incorporate into SHL library. Now standalone.
 *       Uses one_scrsz. Allows for envvar to have optional _HELP
 *       in supplied value, and optional .shl
 *    28 Jul 2004 (timj):
 *       Add isenv. Now call fortran SHL code rather than duplicate loads
 *       of that code in C. Now have return status.
 *    01 Nov 2006 (TIMJ):
 *       Use C interface to Fortran routines.
 *    18-JAN-2011 (TIMJ):
 *       Change API of shl_standalone to use const
 *       Use strlcat and test argc

 * Bugs:
 *    {Enter_new_bugs_here}

 *-
 */

/* Include files.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ems.h"
#include "sae_par.h"
#include "shl.h"
#include "star/util.h"

/* Macros.
 */
#define LENSTR 512   /* Maximum string length (was STRLEN)
			change name to avoid confusion with the STRLEN type */


/*:
 */

int shl_standalone( const char * help_library, int isenv, int argc, char **argv )
{

   int status;             /* global status */
   int     retval;          /* Return status 0=good 1=bad */
   char    topic[LENSTR];   /* Topic as C string    */
   int     i;               /* Parameter counter    */
   char    libra[LENSTR];   /* Expanded Library */

/*.
 */

/* Initialise Starlink status.
 */
   status = SAI__OK;
   emsBegin( &status );

/* Find out the library name from the first few arguments.
 * Export it to Fortran.
 * Set parameter counter (i) such that argv[i] is first topic word.
 */
   if ( argc > 1 && argv[1] && !strcmp( "-l", argv[1] ) )      /* library is in argv[2] */
   {

     shlTrnvar( argv[2], isenv, libra, LENSTR, &status );
     i = 3;
   }
   else                               /* library is in environment variable */
   {
     if ( help_library == NULL ) {
       /* make sure we have a string */
       /* This should be an SHL error ! */
       status = SAI__ERROR;
       emsRep("SHL_ERR", "No default library supplied and no -l option on command line.", &status);
       goto abort;
     }

     /* Get help file from argument */
     shlTrnvar( help_library, isenv, libra, LENSTR, &status );
     i = 1;
   }


/* Assemble the topic string from remaining parameters.
 * Export it to Fortran.
 */
   topic[0] = '\0';
   for ( ; i < argc; i++ )
   {
     star_strlcat( topic, argv[i], sizeof(topic) );
     star_strlcat( topic, " ", sizeof(topic) );
   }

   /* Call the SHL routine for the actual help functionality
      (force interactive) */
   shlGethlp(libra, topic, 1, &status );

abort:

   if (status == SAI__OK) {
     retval = EXIT_SUCCESS;
   } else {
     retval = EXIT_FAILURE;
   }


   /* This will flush the error stack. Currently we do not return
      a 1/0 status to the caller. We should. */
   emsEnd( &status );

/* Return.
 */
   return(retval);
}

