/*+
 * Name:
 *    hlps_standalone

 * Purpose:
 *    Browse through a Starlink help library.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    Library function

 * Invocation:
 *    hlps_standalone( helplb, argc, argv );
 
 * Arguments:
 *    helplb = char * (Given)
 *       Name of default help library to open. There must be a corresponding
 *       environment variable named HELPLB_HELP containing the help
 *       file to be opened. Will be ignored if argv contains a -l parameter.
 *       Can be a NULL pointer only if -l is guaranteed.
 *    argc = int (Given)
 *       The number of elements in argv.
 *    argv = char ** (Given)
 *       Array of strings, ostensibly read from the command line.

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
 *       parameter.
 *    topic, subtopic etc.:
 *       The initial entry point in the hierarchy of topics and subtopics in
 *       the help library.

 * Notes:
 *    Although this library routine behaves as a full C main application
 *    routine, the main has to be provided by the programmer. It should
 *    look something like this:
 *
 *    #include "hlps.h"
 *    void main( int argc, char **argv )
 *    { 
 *       (void) hlps_standalone( "KAPPA", argc, argv );
 *    }
 *
 *    Where the first argument is the name of the application help
 *    system that is to be accessed. The assumption is that there is
 *    an environment variable named, in this case, KAPPA_HELP that
 *    contains the location of a help file.
 *
 *    The main routine can be compiled as normal but need to be linked
 *    against the hlps library and the Fortran runtime libraries. Outside
 *    of autoconf, the easiest way to do this is to compile the
 *    main routine without linking, and then link using the fortran compiler:
 *
 *    cc  -I/star/include -c helpm.c
 *    f77 -o helpc helpm.o `hlps_link` `fio_link` `cnf_link`
 *
 *    gcc or cc cannot be used as linker, since they do not link with the
 *    Fortran libraries that FIO and HLP require. f77 seems to link C code only
 *    if the main routine is compiled by cc, not if it is compiled by gcc.
 *

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
 *       Incorporate into HLPS library. Now standalone.
 *-
 */

/* Include files.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "cnf.h"
#include "f77.h"

/* FIO Prototypes.
 */
extern F77_SUBROUTINE(fio_gunit)( INTEGER(unit), INTEGER(status) );
extern F77_SUBROUTINE(fio_punit)( INTEGER(unit), INTEGER(status) );

/* HLP Prototypes.
 */
extern F77_INTEGER_FUNCTION(hlp_help)
(  F77_INTEGER_FUNCTION(output)( CHARACTER(string) TRAIL(string) ),
   INTEGER(width), CHARACTER(topic), INTEGER(unit), CHARACTER(library),
   INTEGER(jflags),
   F77_INTEGER_FUNCTION(input)
   (  CHARACTER(string), CHARACTER(prompt), INTEGER(length)
      TRAIL(string) TRAIL(prompt)
   ),
   F77_INTEGER_FUNCTION(hlp_nametr)
   (  INTEGER(kmd), CHARACTER(instr), CHARACTER(outstr), INTEGER(jstat)
      TRAIL(instr) TRAIL(outstr)
   )
   TRAIL(topic) TRAIL(library)
);
extern F77_SUBROUTINE(hlp_errmes)
(  INTEGER(status), CHARACTER(message) TRAIL(message)
);
extern F77_INTEGER_FUNCTION(hlp_nametr)
(  INTEGER(kmd), CHARACTER(instr), CHARACTER(outstr), INTEGER(jstat)
   TRAIL(instr) TRAIL(outstr)
);

/* ONE */
extern F77_SUBROUTINE(one_scrsz)
( INTEGER(width), INTEGER(height), INTEGER(status));

/* Function Prototypes.
 */
extern F77_SUBROUTINE(dummy)();
extern F77_INTEGER_FUNCTION(input)
(  CHARACTER(string), CHARACTER(prompt), INTEGER(length)
   TRAIL(string) TRAIL(prompt)
);
extern F77_INTEGER_FUNCTION(output)( CHARACTER(string) TRAIL(string) );

/* Macros.
 */
#define WIDTH   80   /* Fallback terminal width  */
#define PAGE    24   /* Fallback terminal height */
#define LENSTR 512   /* Maximum string length (was STRLEN)
			change name to avoid confusion with the STRLEN type */

/* Global variables.
 */
int width;  /* Terminal width  */
int page;   /* Terminal height */
int line;   /* Lines written   */

/*:
 */

void hlps_standalone( char * help_library, int argc, char **argv )
{
   extern int width;
   extern int page;
   extern int line;

   DECLARE_INTEGER(unit);              /* Fortran unit number         */
   DECLARE_INTEGER(status);            /* Starlink status             */
   DECLARE_INTEGER(jflags);            /* == 1                        */
   DECLARE_INTEGER(f77width);          /* width as f77 integer        */
   DECLARE_INTEGER(f77height);         /* height as f77 integer       */
   DECLARE_INTEGER(hstat);             /* Help system status          */
   DECLARE_CHARACTER(f77libra,LENSTR);  /* Library as f77 string       */
   DECLARE_CHARACTER(f77topic,LENSTR); /* Topic as f77 string         */
   DECLARE_CHARACTER(message,LENSTR);  /* Error message as f77 string */

   char    envvar[LENSTR];  /* Name of environment variable */
   char    library[LENSTR]; /* Environment variable contents*/
   char   *p;               /* Error message        */
   char    topic[LENSTR];   /* Topic as C string    */
   int     i;               /* Parameter counter    */
   size_t  nleft;           /* Space left in string */

/*.
 */

/* Initialise Starlink status.
 */
   status = SAI__OK;

/* Find out the terminal size.
 */
   F77_CALL(one_scrsz)(INTEGER_ARG(&f77width), INTEGER_ARG(&f77height),
		       INTEGER_ARG(&status) );
   width = f77width;
   page = f77height;
   if ( width <= 0 ) width = WIDTH;
   if ( page  <= 0 ) page  = PAGE;

/* Reset the line counter.
 */
   line = 0;

/* Find out the library name from the first few arguments.
 * Export it to Fortran.
 * Set parameter counter (i) such that argv[i] is first topic word.
 */
   if ( argv[1] && !strcmp( "-l", argv[1] ) )      /* library is in argv[2] */
   {  (void) cnf_expn( argv[2], LENSTR, f77libra, f77libra_length );
      i = 3;
   }
   else                               /* library is in environment variable */
   {  
     if ( help_library == NULL ) {
       /* make sure we have a string */
       printf( "Error: no default library supplied and no -l option on command line.\n");
       goto abort;
     }
     /* form the library env var from the supplied base string (add _HELP) */
     /* make sure we have enough space in buffer */
     if (strlen(help_library) > (LENSTR - 6) ) {
       printf ("Base env var name is too long too append _HELP\n");
       goto abort;
     }
     strcpy(envvar, help_library);
     strcat(envvar, "_HELP");

     /* Now try to translate the environment variable and append .shl */
     if ( p = getenv(envvar) )
      {  *library = '\0'; nleft = LENSTR - 1;
         (void) strncat( library, p, nleft ); nleft -= strlen(p);
         (void) strncat( library, ".shl", nleft ); nleft -= 4;
         (void) cnf_expn( library, LENSTR, f77libra, f77libra_length );
      }
      else
      {  (void) printf( "Error: could not open Starlink help library specified in environment variable '%s'.\n", envvar );
         goto abort;
      }
      i = 1;
   }

/* Assemble the topic string from remaining parameters.
 * Export it to Fortran.
 */
   for ( *topic = '\0', nleft = LENSTR - 1; argv[i]; i++ )
   {  (void) strncat( topic, argv[i], nleft ); nleft -= strlen(argv[i]);
      (void) strncat( topic, " ",     nleft ); nleft--;
   }
   (void) cnf_expn( topic, LENSTR, f77topic, f77topic_length );

/* Get a Fortran file unit.
 */
   F77_CALL(fio_gunit)( INTEGER_ARG(&unit), INTEGER_ARG(&status) );
   if ( status != SAI__OK ) goto abort;

/* Call the help system.
 */
   jflags = 1; f77width = width;
   hstat = F77_CALL(hlp_help)
   (  F77_CALL(output), INTEGER_ARG(&f77width), CHARACTER_ARG(f77topic),
      INTEGER_ARG(&unit), CHARACTER_ARG(f77libra), INTEGER_ARG(&jflags),
      F77_CALL(input), F77_CALL(hlp_nametr)
      TRAIL_ARG(f77topic) TRAIL_ARG(f77libra)
   );

/* Handle any error returned by the help system.
 * If there was an error detected by the help system, make sure it is
 * reported. The exception are errors -12 and -13 line output and line
 * input failure. These are caused by our I/O routines, and a failure is
 * equivalent to requesting an immediate exit from the application. It
 * should not constitute an error condition and not cause an error
 * report.
 */
   if ( hstat != 1 && hstat != -12 && hstat != -13 )
   {  (void) F77_CALL(hlp_errmes)
      (  INTEGER_ARG(&hstat), CHARACTER_ARG(message) TRAIL_ARG(message)
      );
      p = cnf_creim( message, message_length );
      (void) printf( "%s\n", p );
      (void) cnf_free(p);
   }

/* Release the Fortran unit.
 */
   F77_CALL(fio_punit)( INTEGER_ARG(&unit), INTEGER_ARG(&status) );

abort:

/* Return.
 */
   return;
}

extern F77_INTEGER_FUNCTION(input)
(  CHARACTER(string), CHARACTER(prompt), INTEGER(length)
   TRAIL(string) TRAIL(prompt)
)
{
   extern int line;

   GENPTR_CHARACTER(string)
   GENPTR_CHARACTER(prompt)
   GENPTR_INTEGER(length)

   char *p;
   char  s[LENSTR];

/*.
 */

/* Display prompt.
 */
   p = cnf_creim( prompt, prompt_length );
   (void) printf( "%s ", p );
   (void) cnf_free(p);

/* Get at most 512 characters back as reply.
 * If fgets returns NULL, an EOF has been read without any text before
 * it. This will be reported back as an error.
 */
   if ( fgets( s, LENSTR, stdin ) == NULL )
   {  (void) printf("\n");
      return -1;
   }

/* If the reply ends with a newline, strip that off.
 * Else (input ended without newline), better print a newline for more
 * tidy screen output.
 */
   if ( s[strlen(s)-1] == '\n' ) s[strlen(s)-1] = '\0';
   else                          (void) printf( "\n" );

/* Export the reply string to Fortran.
 */
   (void) cnf_exprt( s, string, string_length );

/* Set the returned length.
 */
   *length = strlen(s);

/* Reset line counter.
 */
   line = 0;

/* Return with good status.
 */
   return 1;
}

extern F77_INTEGER_FUNCTION(output)( CHARACTER(string) TRAIL(string) )
{
   extern int page;
   extern int line;

   GENPTR_CHARACTER(string)

   char *p;
   char  c;

/*.
 */

/* If page is full, prompt for new page. EOF will cause exit.
 * Else (page not full), just increment line counter.
 */
   if ( page > 0 && page <= line + 3 )
   {  (void) printf( "\nPress Return to continue ..." );
      for( c = '\0'; ( c != '\n' ) && ( c != EOF ); c = fgetc( stdin ) );
      (void) printf( "\n" );
      if ( c == EOF ) return -1;
      line = 1;
   }
   else
   {  line++;
   }

/* Display string.
 */
   p = cnf_creim( string, string_length );
   (void) printf( "%s\n", p );
   (void) cnf_free(p);

/* Return with good status.
 */
   return 1;
}
