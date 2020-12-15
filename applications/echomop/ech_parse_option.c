/* Parse ECHOMOP option strings, returning a token class, or options
   and suboptions.

   Fortran `prototype' is
       subroutine ech_parse_option (line, class, option, suboption)
       character*(*) line
       character class
       integer option
       integer suboption

   The input `line' may be a command or an integer, or a pair of
   integers separated by a dot `.'.  There may be leading or trailing
   whitespace, and there may be more than one `word' on the line.

   This routine determines if the word is, or is a substring of, one
   of the words "adjust", "disable", "help", "menu", "plot", "exit",
   "quit", "hyper", with arbitrarily mixed case, then it sets `class' to
   be the single character which starts the word (except that "hyper"
   is returned as character `Y', and "exit" is returned as `q').
   Additionally, it recognises "yes", "+" and "/" as `y'.

   If the word starts with a digit, then it parses it into an option
   and optionally a suboption.  If both are present, then `class' is
   set to be character `2' and both `option' and `suboption' are
   returned set; if only one is present, `class' is set to `1', and
   only `option' is set.  The integer may be negative, but it must not
   have an explicit plus sign -- this is tested against "+", fails to
   match, and is therefore returned as an error ("+" is a _really_
   dumb thing to use for `yes'!)

   Sets `class' to be `x' on any error.

*/

#include <ctype.h>
#include <stdio.h>

#include <f77.h>		/* CNF macros */

/* Define DEBUG to switch on debugging */
/* #define DEBUG 1 */

F77_SUBROUTINE(ech_parse_option) (CHARACTER(line),
				  CHARACTER(class),
				  INTEGER(option),
				  INTEGER(suboption)
				  TRAIL(line)
				  TRAIL(class))
{
    GENPTR_CHARACTER(line)
    GENPTR_CHARACTER(class)
    GENPTR_INTEGER(option)
    GENPTR_INTEGER(suboption)

    static char *ws;		/* workspace */
    static int wssize = 0;	/* size of ws */

    char *l = 0;		/* pointer into ws -- start of line */
    int i;

    struct {
	char *s;
	char code;
    } commands[] = {
	/* List must not include 'x', '1' or '2'. */
	/* Not in any particular order, except that "help" must go
	   before "hyper", so that "h" matches "help". */
	"adjust", 'a',
	"disable", 'd',
	"help", 'h',
	"menu", 'm',
	"plot", 'p',
	"exit", 'q',
	"quit", 'q',
	"hyper", 'Y',
	"yes", 'y',
	"/", 'y',
	"+", 'y',
    };
    int ncommands = sizeof(commands)/sizeof(commands[0]);

    /* It's simplest and most robust, if we simply copy the input line
       to a local workspace once and for all.  This lets us mess
       around with it freely. */
    if (wssize == 0)		/* uninitialised */
    {
	wssize = 2*line_length;	/* generous size */
	if ((ws = (char*)malloc(wssize)) == 0)
	{
	    *class = 'x';	/* error */
	    return;
	}
    }
    else if (wssize < line_length+1) /* too small */
    {
	wssize = 2*line_length;	/* generous */
	if ((ws = (char*)realloc((void*)ws, wssize)) == 0)
	{
	    *class = 'x';	/* error */
	    return;
	}
    }

    /* copy line to ws */
    strncpy (ws, line, line_length);
    ws[line_length] = '\0';	/* terminate, being paranoid */

    *class = 'x';		/* default return is error */

    /* skip leading spaces */
    for (l=ws; isspace(*l); l++)
	;

    /* determine type based on now-leading character */
    if (*l == '\0')		/* end of line */
    {
	/* there was only space */
	*class = 'x';		/* error */
#ifdef DEBUG
	printf ("empty\n");
#endif
    }
    else if (isdigit(*l) || *l=='-')	/* line starts with a number
					   (note, don't match "+" --
					   see note at top) */
    {
	/* it's an option, or option.suboption, specifier */
	int nconv;

	*option = *suboption = 0;
	nconv = sscanf (l, "%d.%d", option, suboption);
	switch (nconv)
	{
	  case 1:
	    *class = '1';
	    break;
	  case 2:
	    *class = '2';
	    break;
	  default:
	    /* including 0 */
	    *class = 'x';	/* error */
	    break;
	}

#ifdef DEBUG
	printf ("isdigit(%s): %c : %d . %d\n",
		l, *class, *option, *suboption);
#endif
    }
    else if (isgraph(*l))	/* line starts with a non-digit */
    {
	/* it's probably one of the command strings */
	char *p;
	int i, wordlen;

	for (p=l; isgraph(*p); p++)
	    *p = tolower(*p);	/* convert to lowercase */
	*p = '\0';		/* terminate after first word (if
				   there's more than one) */
	wordlen = p-l;

	/* work through the commands[] until we find a match */
	*class = 'x';		/* default, if nothing matches below */
	for (i=0; i<ncommands; i++)
	    /* compare using strncmp, so substrings match, too */
	    if (strncmp (l, commands[i].s, wordlen) == 0)
	    {
		*class = commands[i].code;
		break;
	    }

#ifdef DEBUG
	printf ("isgraph(%s): %c : wordlen=%d\n",
		l, *class, wordlen);
#endif
    }
    else
    {
	/* don't know what it is */
	*class = 'x';		/* error */
#ifdef DEBUG
	printf ("weird(%s)\n", l);
#endif
    }

    return;
}
