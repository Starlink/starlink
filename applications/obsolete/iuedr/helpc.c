/*+
 * Name:
 *    helpc

 * Purpose:
 *    Browse through a Starlink help library.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    Unix application

 * Usage:
 *    helpc [-l library.shl] [topic [subtopic [subsubtopic ...]]]

 * Description:
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
 *    Although by design this is a C main routine, technically it is not. There
 *    must be a separate main routine, which under SunOS 4.x must be compiled
 *    with the native compiler cc. It would look like:
 *
 *    void main( argc, argv )
 *       int    argc;
 *       char **argv;
 *    {  void helpc();
 *       (void) helpc( argc, argv );
 *    }
 *
 *    Both routines are compiled separately, then linked with f77:
 *
 *    gcc -c helpc.c -I/star/include
 *    cc  -c helpm.c
 *    f77 -o helpc helpm.o helpc.o `hlp_link` `fio_link` `cnf_link`
 *
 *    gcc or cc cannot be used as linker, since they do not link with the
 *    Fortran libraries that FIO and HLP require. f77 seems to link C code only
 *    if the main routine is compiled by cc, not if it is compiled by gcc.
 *
 *    Under Solaris, this routine can be the main routine. This is because the
 *    native compiler cc is ANSI-compliant. Still the linker is f77:
 *
 *    cc  -c helpc.c -I/star/include
 *    f77 -o helpc helpc.o `hlp_link` `fio_link` `cnf_link`
 *
 *    On the Alpha, we can use cc as the linker, but need two additional
 *    libraries:
 *
 *    cc -o helpc helpc.c -I/star/include -L/star/lib \
 *       `hlp_link` `fio_link` `cnf_link` -lfor -lots

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)
 *    ajc: Alan Chipperfield (Starlink, RAL)

 * History:
 *    28 Mar 1994 (hme):
 *       Original version.
 *    10 Feb 1998 (ajc):
 *       Mod to use termios
 *-
 */

/* Include files.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#ifndef TIOCGWINSZ
#include <sys/ioctl.h>
#endif
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
#define LIBRA "IUEDR_HELP"  /* Fallback library */
#define WIDTH   80   /* Fallback terminal width  */
#define PAGE    24   /* Fallback terminal height */
#define STRLEN 512   /* Maximum string length    */
#define SAI__OK  0   /* Starlink status OK       */

/* Global variables.
 */
int width;  /* Terminal width  */
int page;   /* Terminal height */
int line;   /* Lines written   */

/*:
 */

void helpc( int argc, char **argv )
{
   extern int width;
   extern int page;
   extern int line;

#ifdef TIOCGWINSZ
   struct winsize term;
#endif

   DECLARE_INTEGER(unit);              /* Fortran unit number         */
   DECLARE_INTEGER(status);            /* Starlink status             */
   DECLARE_INTEGER(jflags);            /* == 1                        */
   DECLARE_INTEGER(f77width);          /* width as f77 integer        */
   DECLARE_INTEGER(hstat);             /* Help system status          */
   DECLARE_CHARACTER(f77libra,STRLEN);  /* Library as f77 string       */
   DECLARE_CHARACTER(f77topic,STRLEN); /* Topic as f77 string         */
   DECLARE_CHARACTER(message,STRLEN);  /* Error message as f77 string */

   char    library[STRLEN]; /* Environment variable */
   char   *p;               /* Error message        */
   char    topic[STRLEN];   /* Topic as C string    */
   int     i;               /* Parameter counter    */
   size_t  nleft;           /* Space left in string */

/*.
 */

/* Initialise Starlink status.
 */
   status = SAI__OK;

/* Find out the terminal size.
 */
   if ( ioctl( 1, TIOCGWINSZ, &term ) < 0 )
   {  width = WIDTH; page = PAGE;
   }
   else
   {  width = term.ws_col; page = term.ws_row;
   }
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
   {  (void) cnf_expn( argv[2], STRLEN, f77libra, f77libra_length );
      i = 3;
   }
   else                               /* library is in environment variable */
   {  if ( p = getenv(LIBRA) )
      {  *library = '\0'; nleft = STRLEN - 1;
         (void) strncat( library, p, nleft ); nleft -= strlen(p);
         (void) strncat( library, ".shl", nleft ); nleft -= 4;
         (void) cnf_expn( library, STRLEN, f77libra, f77libra_length );
      }
      else
      {  (void) printf( "Error: could not open Starlink help library.\n" );
         goto abort;
      }
      i = 1;
   }

/* Assemble the topic string from remaining parameters.
 * Export it to Fortran.
 */
   for ( *topic = '\0', nleft = STRLEN - 1; argv[i]; i++ )
   {  (void) strncat( topic, argv[i], nleft ); nleft -= strlen(argv[i]);
      (void) strncat( topic, " ",     nleft ); nleft--;
   }
   (void) cnf_expn( topic, STRLEN, f77topic, f77topic_length );

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
   char  s[STRLEN];

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
   if ( fgets( s, STRLEN, stdin ) == NULL )
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
