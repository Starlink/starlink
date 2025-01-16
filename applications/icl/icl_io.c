/******************************************************************************
 *
 * 	I C L _ I O . C
 *
 * History :
 *	Created	: skr 26/6/92
 *	Subsequent development: bkm
 *    11-DEC-1996 (BKM):
 *       Make backspace key delete
 *       Remove specials for Linux ncurses
 *    19-MAY-1998 (AJC):
 *       Bring in line with shell line edit and filename completion etc.
 *       Remove spurious newline on output messages.
 *       Force ringbell in SCREENMODE (wrefresh)
 *       Bell if Kright or Kleft beyond limits
 *     2-JUN-1998 (AJC):
 *       Correct realloc in shell_command
 *       Handle environment variables in filename completion
 *     27-OCT-1998 (BKM):
 *       Redo include file logic to work with RedHat Linux 5.1
 *     28-OCT-1998 (BKM):
 *       Rework tty switch to single character mode to match PCS
 *     15-JUN-1999 (AJC):
 *       Add long line editing
 *       Improve screen mode I/O
 *       Make ringbelll ring bell not flash screen
 *     01-JUL-2004 (TIMJ):
 *       Now use autoconf test for atexit. GLOB_NOMATCH is not standard.
 *     24-OCT-2008 (PWD):
 *       Increase a buffer from 255 to the standard 256 in keyboard_input.
 *     14-JAN-2025 (GSB):
 *       Remove use of TIOCSTI (not available by default since Linux 6.2).
 *
 * Source file for the basic ICL input/output subsystem. This is a separate
 * processs forked by ICL which handles terminal I/O. It does this by
 * waiting for either terminal input or the arrival of a control message sent
 * as a C structure from icl over a pipe.
 *
 * ICL itself is waiting for  a messys format message to appear on one of its
 * queues (fixed queues or transaction queues created to communicate with ADAM
 * tasks). Unlike in VMS PASCAL ICL where the request is sent to the fixed
 * "ASTINT" queue we start and OBEY transaction with ICL and icl_io generates
 * an appropriate message containing the user input in its "value" component
 * and sends this to ICL as part of a MESSYS_TRIGGER message.
 *
 * Terminal input is placed in a buffer to allow for command recall and various
 * key processing options which emulate the behaviour on VMS.
 *
 ******************************************************************************
 */
/* System includes */

/* Autoconf output */
#if HAVE_CONFIG_H
# include <config.h>
#endif

#if HAVE_CURSES_H
# include <curses.h>
#elif HAVE_CURSESX_H
# include <cursesX.h>
#elif HAVE_CURSES_CURSES_H
# include <curses/curses.h>
#else
# error "Unable to locate curses installation"
#endif

#include <stdlib.h>
#if HAVE_TERM_H
#include <term.h>
#elif HAVE_NCURSES_TERM_H
#include <ncurses/term.h>
#else
# error "Unable to locate term.h"
#endif
#include <termios.h>
#include <sys/ioctl.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <setjmp.h>
#include <glob.h>
/* ADAM and ams includes */
#include "sae_par.h"
#include "ams.h"
#include "adam_defns.h"
#include "messys_len.h"
#include "messys_par.h"
#include "messys_err.h"
/* Own include file */
#include "icl_io.h"

/* forward reference function delarations */
void initscreen(int);
char *shell_command( char* );
/*
 * Global variables
 */
int screenstate = 0;		  /* Screen status.
                                   * Set to zero here. Reset to LINEMODE
				   * by initscreen() and can alternate between
                                   * LINEMODE or SCREENMODE by user
				   * commands.
				   * If terminal input is interrupted by output
			           * 'screenmode' is set negative (-LINEMODE
				   * or -SCREENMODE temporarily to indicate
				   * this)
				   */

/* icl_io command - C structure passed from ICL using the command input pipe */

struct iocommand message;

/* icl-io commands are passed back to icl using MESSYS__TRIGGER replies on a
 * permanently open path. Define the path parameters globally here:
 */
int icl_path, icl_messid;

/* tty state information - uses SYSV/POSIX termio interface */

struct termios init_tty,
	       linemode_tty;
/*
 * tty input is first read into a dynamic memory buffer 'readbuff' and is
 * the processed into a cyclical buffer 'inbuff'. This provides type-ahead
 * and allows some pre-processing of certain characters even when not in
 * explicit input mode.
 *
 * Eventually the line to be passed to ICL is assembled from 'inbuff' into
 * 'inputline' allowin for line editing operations.
 */

char
    inbuff   [INPUTBUFFERLENGTH], /* Cyclic input buffer for characters read */
    inputline[INPUTLINELENGTH+1], /* Constructed input line for ICL */
    saveinput[INPUTBUFFERLENGTH+1], /* Save input when recalling lines */
    blankline[INPUTBUFFERLENGTH+1]; /* blank line */
char *saveinput_p=CHARNIL;     /* pointer to saveinput or NULL */

int curinlength = INPUTBUFFERLENGTH;  /* Allowable length of current input */

char prompt[IOBUFFERLENGTH];	/* Input line prompt */
char dvalue[IOBUFFERLENGTH];	/* default value for input prompt */

/* Pointers to buffers used in processing input */

int
    inpl_cpos  = 0,		/* current inputline[] position    */
    inpl_epos  = 0,		/* current inputline[] end position */
    inbuf_cpos = 0,		/* current position in inbuff[] containing
				 * char to be processed next */
    inbuf_rpos = 0,		/* position in inbuff[] to receive next
				 * character */
    prompt_len = 0,             /*length of prompt string */

    inputarg = NOTKNOWN,	/* Integer code sent with commands.
				 * For io requests this is an incrementing
				 * sequence number. For control commands
				 * it is used to pass an integer argument
				 */
    curcmdcode   = NOTKNOWN;	/* funcction code of the current icl command */

/* Screen mode input variables */
int
    instarty,			/* bottom window row position where current
				 * input line started */
    instartx,			/* bottom window column position where
				 * current input line started */
    insertmode = INSERT;	/* Insert mode */

/* Buffer processing macros */
#define incinbuf_rpos() if(++inbuf_rpos == curinlength) inbuf_rpos = 0;
#define decinbuf_rpos() if(--inbuf_rpos < 0) inbuf_rpos = INPUTBUFFERLENGTH-1;
#define incinbuf_cpos() if(++inbuf_cpos == curinlength) inbuf_cpos = 0;
#define emptyinbuff() (inbuf_rpos == inbuf_cpos)

/* icl_io control variables */
int
    notkilled         = TRUE,
    expectinginput    = FALSE,
    interuptableinput = FALSE,
    gotline           = FALSE;

/* state for longjump */
sigjmp_buf	goback;

/* flag for interrupt */
int interrupted;

/******************************************************************************
 *
 * This process can handle normal ICL line orientated input (LINEMODE) or,
 * using the curses routines, the ICL "SCREENMODE" defined in SG/5.
 *
 ******************************************************************************
 */

/* curses WINDOW structures */
WINDOW
    *topwin=NOWINDOW, *bottomwin=NOWINDOW;

int twlines, bwlines;

/******************************************************************************
 *
 *	I N P U T   L I N E   B U F F E R I N G   R O U T I N E S
 *
 ******************************************************************************
 */
#define SAVELINES 100		/* Save up to SAVELINES lines of prior input */

char *recall_lb[SAVELINES];
int recall=SAVELINES+1;		/* current input recall buffer position */
                                /* Flag as not initialised at start     */
int recall_p;			/* processing position in recall buffer */

/* Macros used in recall processing */

#define inc_recall() if(++recall == SAVELINES) recall =0;
#define dec_recall() if(--recall < 0) recall = SAVELINES-1;

#define inc_recall_p() if(++recall_p == SAVELINES) recall_p =0;
#define dec_recall_p() if(--recall_p < 0)recall_p = SAVELINES-1;

/******************************************************************************
 *
 *	I N I T R E C A L L (void)
 *
 * Initialize the memory storage for use by the command line recall system.
 * Up to SAVELINES of text are stored.
 *
 ******************************************************************************
 */
void
initrecall(void)
{
    int i;

    for (i = 0; i < SAVELINES; ++i)
	recall_lb[i] = CHARNIL;
    recall = -1;	/* So addtorecall starts at 0 */
    return;
}

/******************************************************************************
 *
 *	A D D T O R E C A L L (char line[])
 *
 * Add a line of text ('line') to the recall buffer at the current position.
 *
 ******************************************************************************
 */
void
addtorecall(char *line)
{
    char *txt;
    int len, tlen;

    inc_recall();
    recall_p = recall;
    len = strlen(line);

    if ( (recall_lb[recall] = (char *) malloc(len+1)) == CHARNIL)
	fprintf(stderr, "icl_io - memory allocation during recall buffering\n");
    else
	strcpy(recall_lb[recall], line);	/* use new space  */

/* Mark end of buffer by placing CHARNIL in buffer location after this one */
    inc_recall_p();
    if ( (txt = recall_lb[recall_p]) != CHARNIL) {
	free(txt);
	recall_lb[recall_p] = CHARNIL;
    }
    recall_p = recall;

    return;
}

/******************************************************************************
 *
 *	P R E V L I N E (void)
 *
 * Return a pointer to the previous input line or CHARNIL if none.
 *
 ******************************************************************************
 */
char *
prevline(void)
{
    char *txt;

    dec_recall_p();
    if ( (txt = recall_lb[recall_p]) == CHARNIL)
	inc_recall_p();
    return txt;
}

/******************************************************************************
 *
 *	N E X T L I N E (void)
 *
 * Return a pointer to the next line in the buffer
 *
 ******************************************************************************
 */
char *
nextline(void)
{
    if (recall_lb[recall_p] != CHARNIL)
	inc_recall_p();

    return recall_lb[recall_p];
}

/******************************************************************************
 *
 *	S P E C I A L    K E Y B O A R D    K E Y S
 *
 ******************************************************************************
 */

#define CNTLC		'\003'
#define CNTLX		'\030'
#define CNTLZ		'\032'
#define Kleft		0
#define Kright		1
#define Kup		2
#define Kdown		3
#define Kbs		4
#define Kdel		5
#define Kim		6
#define Kinterupt 	7
#define Ktoeol		8
#define Ktosol		9
#define Kdelw		10
#define Keol		11
#define Kredisp		12
#define Kcleartoeol 	13
#define Kcleartobol     14
#define Kclearall 	15
#define Ktab		16
#define Kunknown 	17
#define Klist           18
#define PF0		20
#define PF1		21
#define PF2		22
#define PF3		23
#define PF4		24
#define Kbdel           25
#define Kbword          26
#define Kfword          27
#define Kbdelw          28

/*
 * VTn00 terminals (and emulators) may be set to send either the "cursor" or
 * "application" key sequences for the cursor keys on the keyboard. terminfo
 * entries appear to assume application keypad so we hardwire the VT
 * alternatives.
 */

#define NUMKEYS 26

char *keyseq[NUMKEYS+4] =
{"\033[A",    /* up-history */
 "\033[B",    /* down-history */
 "\033[C",    /* forward-char */
 "\033[D",    /* backward-char */
 "","","","", /* Space for key_up, key_down etc. */
 "\033\004",  /* list-choices */
 "\033\010",  /* backward-delete-word */
 "\033\011",  /* complete-word */
 "\033\033",  /* complete-word */
 "\033b",     /* backward-word */
 "\033c",     /* capitaliZe-word */
 "\033d",     /* delete-word */
 "\033f",     /* forward-word */
 "\033l",     /* downcase-word */
 "\033u",     /* upcase-word */
 "\033B",     /* backward-word */
 "\033C",     /* capitaliZe-word */
 "\033D",     /* delete-word */
 "\033F",     /* forward-word */
 "\033L",     /* downcase-word */
 "\033U"      /* upcase-word */
};
      			/* Define remaining codes in initscreen() */

int keycodes[NUMKEYS+4] = /* Internal names for keyboard keys */
{Kup,           /* up-history */
 Kdown,         /* down-history */
 Kright,        /* forward-char */
 Kleft,         /* backward-char */
 Kup,           /* up-history */
 Kdown,         /* down-history */
 Kright,        /* forward-char */
 Kleft,         /* backward-char */
 Klist,         /* list-choices */
 Kbdelw,        /* backward-delete-word */
 Ktab,          /* complete-word */
 Ktab,          /* complete-word */
 Kbword,        /* backward-word */
 Kunknown,      /* capitalize-word */
 Kdelw,         /* delete-word */
 Kfword,        /* forward-word */
 Kunknown,      /* downcase-word */
 Kunknown,      /* upcase-word */
 Kbword,        /* backward-word */
 Kunknown,      /* capitalize-word */
 Kdelw,         /* delete-word */
 Kfword,        /* forward-word */
 Kunknown,      /* downcase-word */
 Kunknown,      /* upcase-word */
	     PF0,   PF1,    PF2, PF3, PF4,
	     Kdel
	    };

int controlcodes[26] =
    {Ktosol   /* CNTL A */,	Kleft    /* CNTL B */,	Kunknown  /* CNTL C */,
     Kdel     /* CNTL D */,	Ktoeol   /* CNTL E */,	Kright    /* CNTL F */,
     Kunknown /* CNTL G */,	Kbdel    /* CNTL H */,	Ktab 	  /* <TAB>  */,
     Keol     /* CNTL J */,	Kcleartoeol/* CNTL K */,Kunknown  /* CNTL L */,
     Keol     /* <CR>   */,	Kdown    /* CNTL N */,	Kunknown  /* CNTL O */,
     Kup      /* CNTL P */,	Kunknown /* CNTL Q */,	Kredisp   /* CNTL R */,
     Kunknown /* CNTL S */,	Kunknown /* CNTL T */,	Kclearall /* CNTL U */,
     Kunknown /* CNTL V */,	Kunknown  /* CNTL W */,	Kunknown  /* CNTL X */,
     Kunknown /* CNTL Y */,	Kunknown /* CNTL Z */
    };

/* codespecialkey() returns one of the integers defined above or one
 * of the codes below (must be negative)
 */

#define SUBSTRING (-1)		/* Input so far is a partial match */
#define NOMATCH   (-2)		/* No match on special characters  */

/******************************************************************************
 *
 *	C O D E S P E C I A L K E Y (void)
 *
 * Examine the raw input buffer 'inbuff' for a sequence of characters
 * matching a special operation defined above. Also processes control keys
 * and DELETE .
 *
 * Can also return SUBSTRING (partial match - await more characters arriving)
 * or NOMATCH.
 *
 ******************************************************************************
 */
int
codespecialkey(void)
{
    char *w, wch;
    int i=0, wi;

    wi = inbuf_cpos;
/* Match the current input against the key sequences in 'keys' */
    while ( i < NUMKEYS ) {
	if ((w = keyseq[i]) == CHARNIL)
	    i++;
	else {
	    while ((wch = *(w++)) != '\0') {
		if (inbuff[inbuf_cpos] == wch) {
		    incinbuf_cpos();
		    if ((*w) != '\0' && emptyinbuff()) {
			inbuf_cpos = wi;
			return SUBSTRING;	/* partial match */
		    } else
			continue;
		} else
		    break;
	    }
	    if (wch == '\0')
		return (keycodes[i]);	/* returns positive keycode value */
	    else {
		inbuf_cpos = wi;
		i++;
	    }
	}
    }
/* Test for a control key */
    inbuf_cpos = wi;
    if ((wch = inbuff[inbuf_cpos]) <= '\032' && (wch > '\000')) {
	int i;

	i = controlcodes[((int) wch) - 1];
	incinbuf_cpos();
	return (controlcodes[((int) wch) - 1]);
    } else if (wch == 0x7F) {	/* DELETE key */
	incinbuf_cpos();
	return (Kbdel);
    }
    return NOMATCH;
}

/******************************************************************************
 *
 *      P O S B O L (int del, int prlen, int cpos, int epos)
 *
 * Position at beginning of current prompt line
 * Where prlen is the length of the prompt string.
 *       cpos is the current cursor position in the input string.
 *       epos is the current end of the input string (only used in deleting
 *            line.
 * If del is true, clear the line.
 *
 * Static variable COLS should be set to the current screen width.
 *
 * In the case of dumb terminals (i.e. where we cannot move the cursor up)
 * we position at the start of the current display line.
 *
 ******************************************************************************
*/
void
posbol( int del, int prlen, int cpos, int epos )
{
int nlines,cline;
   nlines = ( prlen + epos  ) / COLS + 1;
   cline =  ( prlen + cpos ) / COLS;
   if (cursor_up != CHARNIL)
      for (;cline--;)putp(cursor_up);
   else if ( cline == nlines-1 )
      putchar('\r');

   if ( del ) {
      if (delete_line != CHARNIL) {
         for (;nlines--;)putp(delete_line);

      } else {
         putchar('\r');
         if (clr_eos != CHARNIL)
            putp(clr_eos);
         else
            printf("%.*s", COLS, blankline);
      }
   }
   putchar('\r');
}

/******************************************************************************
 *
 *  R E D O L I N E ( int complete, int prlen, int cpos, int epos, int forwd )
 *
 * Re-display a line leaving the cursor in position cpos.
 * We assume that the static variables prompt and inputline are set up
 * correctly and the cursor is at the start of the line. (This is the complete
 * prompt and reply display on most terminals but only the current display
 * line on dumb terminals ie where we cannot move the cursor up.)
 * Static variable COLS must be set to the screen width.
 *
 * Arguments:
 *    complete - true if the line is to be completely re-written
 *    prlen    - the length of the prompt
 *    cpos     - the required position within inputline of the cursor
 *    epos     - the position within inputline of the end (null character).
 *    forwd    - true if we are proceeding forwards (if cursor at beginning
 *               of line, do we display the next or previous display line)
 *
 ******************************************************************************
 */
void redoline( int complete, int prlen, int cpos, int epos, int forwd ) {
int cline,  /* line number which cursor is on (start at 0) */
    cl_len, /* position of cursor within display line */
    ll_len, /* position of end on last line */
    socl,   /* position within inputline of start of cursor line */
    rcpos,  /* position of cursor relative to prompt or start of cursor line */
    repos;  /* position of end relative to prompt or start of last line */

/* Calculate line numbers and remainders */
   cline =  ( prlen + cpos ) / COLS;
   ll_len = (prlen + epos) % COLS;
   cl_len = (prlen + cpos) % COLS;

/* For dumb terminals (ie where cursor cannot be moved up) we have to work with
 * one line at a time. Calculate the positions required relative to inputline
 * and the display line.
 * If we can move the cursor up, these values can be set simply.
*/
   if ( cursor_up == CHARNIL ) {
   /* Allow for presence or absence of the prompt, depending upon the line */
      rcpos = cl_len - (cline?0:prlen);
      socl = COLS * cline - (cline?prlen:0);
      repos = COLS - (cline?0:prlen);

   /* If the cursor is at the beginning of line, and we are moving forward.
    * arrange to display the previous line */
      if ( (cl_len == 0) && forwd ) {
         rcpos = COLS - ((cline-1)?0:prlen);
         repos = rcpos;
         socl = socl -rcpos;
         cline--;
      }

   } else {
      rcpos = cpos;
      socl = 0;
      repos = epos;
   }


/* Re-display the complete line if required */
   if ( complete ) {
   /* If first line, start with prompt */
      if ( !cline || ( cursor_up != CHARNIL ) )
         printf( "%s", prompt );

   /* Now re-display the complete line */
      printf("%.*s", repos, inputline+socl );

   /* Now ensure cursor is after last character printed */
      if ( cursor_up != CHARNIL )
         if ( !ll_len ) putchar('\n');
   }

/* Now position the cursor correctly
 * There is no need if we have just printed the complete line and the cursor
 * is at end of line. */
   if ( !(complete && (cpos==epos)) ) {
   /* Re-position at start if we have just displayed the complete line */
      if ( complete )
         posbol(0, prlen, epos, epos);

   /* If first line, start with prompt */
      if ( !cline || ( cursor_up != CHARNIL ) )
         printf( "%s", prompt );

   /* Now re-display up to the cursor position */
      printf("%.*s", rcpos, inputline+socl );

   /* Now ensure cursor is after last character printed */
   /* (Normally the cursor dwells on the last character of a line) */
   /* Only position to next line on dumb terminals if we are going forwards */
      if ( !cl_len ) {
         if ( cursor_up != CHARNIL ) {
            putchar('\n');
         } else
            if ( forwd ) putchar('\n');

   /* and, for dumb terminals, if there is more on the line, print it,
    *    restoring cursor to SOL */
         if ( cursor_up == CHARNIL )
            if ( epos > cpos ) printf( "%s\r", inputline+cpos );
      }
   }
}

/******************************************************************************
 *
 *	I / O   R E Q U E S T  Q U E U E
 *
 * Requests from ICL to icl_io are passed as C structures through a pipe.
 *
 * tty input can be requested to be non-interuptable (used, for example, to
 * prompt for a parameter) or interuptable (eg. the ICL> prompt) when task
 * output messages will be sent as they appear.
 *
 * These routines handle the buffering of I/O requests in situations where they
 * cannot be immediately acted upon.
 *
 ******************************************************************************
 */
struct queueout {
    int type;
    char *txt;		/* Current text value (prompt or output text) */
    char *pdef;		/* Default value for current prompt */
    char *sofar;
    int incode;
    struct queueout *next;
} *QNIL = ((struct queueout *) 0), *headqueue;


/******************************************************************************
 *
 *	A D D T O Q U E U E (int type, char *prompt, char *retaddr, int seqno)
 *
 * Add an I/O request to the pending queue.
 *
 *
 ******************************************************************************
 */
void
addtoqueue
(
int type,		/* request type code (input) */
char *pval,		/* adddress of input line prompt (input) */
char *dval,		/* address of default value (input) */
char *inputsofar,	/* buffer address for input line  (input) */
int seqno		/* sequence number of input request (input) */
)
{
    struct queueout *qentry;
    struct queueout *new = (struct queueout *) malloc(sizeof(struct queueout));
/*
 * Construct new entry
 */
    new->next    = QNIL;
    new->type    = type;
    new->incode  = seqno;
    new->txt = (char *) malloc(strlen(pval) + 1);
    strcpy(new->txt, pval);
    if (dval == CHARNIL || dval[0] == '\0')
	new->pdef = CHARNIL;
    else {
	new->pdef = (char *) malloc(strlen(dval) +1);
	strcpy(new->pdef, dval);
    }
    if (inputsofar == CHARNIL)
	new->sofar = CHARNIL;
    else {
	new->sofar = (char *) malloc(strlen(inputsofar) + 1);
	strcpy(new->sofar, inputsofar);
    }
    if (headqueue == QNIL)
	headqueue = new;
    else {
	qentry = headqueue;
	while (qentry->next != QNIL)
	    qentry = qentry->next;
	qentry->next = new;
    }
    return;
}

/******************************************************************************
 *
 *	M Y W N E W L I N E (WINDOW *win, int wrows)
 *
 * Generate a newline effect in window 'win' which is 'wrows' lines long
 *
 ******************************************************************************
 */
void
mywnewline(WINDOW * win, int wrows)
{
    int xcoord, ycoord;

    getyx(win, ycoord, xcoord);
    if (ycoord == wrows - 1) {
	wmove(win, 0, 0);
	wdeleteln(win);
	wmove(win, ycoord, 0);
	instarty--;
	if (instarty < 0)
	    instarty = 0;
    } else
	wmove(win, ycoord + 1, 0);
    return;
}

/******************************************************************************
 *
 *	M Y W M O V E B A C K O N E (WINDOW *win)
 *
 * Move window position back one character position. If at start is line wrap
 * back to end of previous line
 *
 ******************************************************************************
 */
void
mywmovebackone(WINDOW * win)
{
    int xcoord, ycoord;

    getyx(win, ycoord, xcoord);
    if (xcoord == 0)
	wmove(win, ycoord - 1, COLS - 1);  /* worry that go off display area */
    else
	wmove(win, ycoord, xcoord - 1);
    return;
}

/******************************************************************************
 *
 *	M Y W M O V E F O R W A R D O N E (WINDOW *win, int wrows)
 *
 * Move window position forward one position. If at the end of a row wrap to
 * the first character position of the next row.
 *
 ******************************************************************************
 */
void
mywmoveforwardone(WINDOW * win, int wrows)
{
    int xcoord, ycoord;

    getyx(win, ycoord, xcoord);
    if (xcoord == COLS - 1)
	mywnewline(win, wrows);
    else
	wmove(win, ycoord, xcoord + 1);
    return;
}

/******************************************************************************
 *
 *	M Y W A D D S T R (WINDOW *win, char * str)
 *
 * Adds a string 'str' to a window 'win'. If the string would wrap off the
 * end of the window row it is truncated to fit
 *
 ******************************************************************************
 */
void
mywaddstr( WINDOW *win, char *str)
{
    int x, y, len;

    getyx(win, y, x);
    len = strlen(str);
    if (x+len < COLS)
	waddstr(win, str);
    else
	while( x < COLS) {
	    waddch(win, *str);
	    x++;
	    str++;
	}

    return;
}

/******************************************************************************
 *
 *	C L E A R Q U E U E (void)
 *
 ******************************************************************************
 */
void
clearqueue(void)
{
    struct queueout *w;

    while (headqueue != QNIL) {
	w = headqueue;
        free(headqueue->txt);
	if (headqueue->pdef != CHARNIL)
	    free(headqueue->pdef);
	if (headqueue->sofar != CHARNIL)
	    free(headqueue->sofar);
        headqueue = headqueue->next;
        free(w);
    }
}

/******************************************************************************
 *
 *	P R O C E S S Q U E U E (void)
 *
 ******************************************************************************
 */
void
processqueue(void)
{
    struct queueout *w;
    int botx, boty;

    while (headqueue != QNIL) {
	w = headqueue;
	if (headqueue->type == IO_COMMAND_OUTPUT ) {
	    if (screenstate == SCREENMODE) {
		waddstr(bottomwin, headqueue->txt);
		wrefresh(bottomwin);
	    } else
		printf("%s", headqueue->txt);
	    free(headqueue->txt);
	    if (headqueue->sofar != CHARNIL)
		free(headqueue->sofar);
	    expectinginput = FALSE;
	} else if (headqueue->type == IO_COMMAND_GETINTINPUT ||
		   headqueue->type == IO_COMMAND_GETINPUT) {
	    strcpy(prompt, headqueue->txt);
            prompt_len = strlen(prompt);
	    free(headqueue->txt);
	    if (headqueue->pdef != CHARNIL) {
		strcpy(dvalue, headqueue->pdef);
		free(headqueue->pdef);
	    } else
		dvalue[0] = '\0';
	    inputarg = headqueue->incode;
	    curcmdcode = headqueue->type;
	    if (screenstate == SCREENMODE) {
		getyx(bottomwin, instarty, instartx);
		waddstr(bottomwin, prompt);
	    } else
		printf("%s", prompt);
	    if (headqueue->sofar != CHARNIL) {
		strcpy(inputline, headqueue->sofar);
		free(headqueue->sofar);
		inpl_cpos = inpl_epos = strlen(inputline);
		if (screenstate == SCREENMODE) {
		    waddstr(bottomwin, inputline);
		    wrefresh(bottomwin);
		} else
		    printf("%s", inputline);
	    } else {
		inputline[0] = '\0';
		inpl_cpos = inpl_epos = 0;
		if (screenstate == SCREENMODE)
		    wrefresh(bottomwin);
	    }
	    insertmode = INSERT;
	    expectinginput = TRUE;
	    interuptableinput = (headqueue->type == IO_COMMAND_GETINTINPUT);
	}
        headqueue = headqueue->next;
        free(w);
	if (expectinginput && !interuptableinput)
			/* Must process this request now ( ie. before rest
			 * of stacked requests */
	    return;
    }
    return;
}

/******************************************************************************
 *
 *	N O I S E C H A R (char ch)
 *
 *      Is word separator for input editing
 *
 ******************************************************************************
 */
int
noisechar(char ch)
{
    if ( isalnum( ch ) || (strchr("_.-=*[]?~",ch) != NULL) )
        return 0;
    else
        return 1;
}

/******************************************************************************
 *
 *	S P A C E C H A R (char ch)
 *
 *      Is filename separator for filename completion
 *
 ******************************************************************************
 */
int
spacechar(char ch)
{
    if (ch == ' ' || ch == '\t' )
	return 1;
    return 0;
}

/******************************************************************************
 *
 *	N O R M A L C H A R A C T E R (char ch)
 *
 ******************************************************************************
 */
int
normalcharacter(char ch)
{
    if ((ch >= 'a' && ch <= 'z') ||
	(ch >= 'A' && ch <= 'Z') ||
	(ch >= '0' && ch <= '9') ||
	(strchr(" !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~", ch) != NULL) )
	return 1;
    else
	return 0;
}

/******************************************************************************
 *
 *	R I N G B E L L (void)
 *
 ******************************************************************************
 */
void
ringbell(void)
{
    if (bell != CHARNIL)
	putp(bell);
    else
	putchar((int) '\007');
    if ( screenstate == SCREENMODE ) wrefresh(bottomwin);
    return;
}

/******************************************************************************
 *
 *	C L E A R L I N E (void)
 *
 * Clear the current line when in LINEMODE
 *
 ******************************************************************************
 */
void
clearline(void)
{
    putchar('\r');
    if( clr_eol != CHARNIL)
	putp(clr_eol);
    else
	printf("%s\r", blankline);

    return;
}

/******************************************************************************
 *
 *	R E D I S P L A Y (char *w)
 *
 *  Redisplay the input line, including prompt. Leave cursor at end
 ******************************************************************************
 */
void
redisplay(char *w)
{
int nlines;
    wmove(bottomwin, instarty, instartx);
    wclrtobot(bottomwin);
    waddstr(bottomwin, prompt);
    if (w != CHARNIL)
	strcpy(inputline, w);
    inpl_cpos = inpl_epos = strlen(inputline);
    waddstr(bottomwin, inputline);
    nlines = (prompt_len+inpl_epos)/COLS;
    if ( ( instarty + nlines ) >= bwlines ) instarty = bwlines - nlines -1;
    wrefresh(bottomwin);
    return;
}

/******************************************************************************
 *
 *	P O S I T I O N R E D I S P L A Y (char *w)
 *
 *  Redisplay the input line, including prompt, and reposition cursor within it
 ******************************************************************************
 */
void
positionredisplay(char *w)
{
    int posy, posx, back;

    wmove(bottomwin, instarty, instartx);
    wclrtobot(bottomwin);
    waddstr(bottomwin, prompt);
    if (w != CHARNIL)
	strcpy(inputline, w);
    inpl_epos = strlen(inputline);
    waddstr(bottomwin, inputline);
    if (inpl_cpos != inpl_epos) {
	getyx(bottomwin, posy, posx);
        back = inpl_epos - inpl_cpos;
        while ( back > posx ) {
           back = back - posx;
           posx = COLS - 1;
           posy--;
        }
        posx -= back;
	wmove(bottomwin, posy, posx );
    }
    wrefresh(bottomwin);
    return;
}

/******************************************************************************
 *
 *	F I L E N A M E _ C O M P L E T E ( )
 *
 * This routine is called to complete a filename in the keyboard input routine
 * It assumes the inputline is set up as normal with inpl_cpos pointing at
 * the current character and inpl_epos pointing at the end.
 ******************************************************************************
 */
void filename_complete( ) {
size_t expanded_len;
int status;
char combuff[256];
char tbuff[256];
glob_t filelist;
int globflags=0;
int nfiles, nmatch;
char *result;
char *adam_extn;
char *extn;
char *extns[11];
int lenext;
int trunc;
int istart, i;
int nexts;
int nomatch = 0;

/* Find beginning of word */
/* Get word into filename */
      istart = inpl_cpos - (inpl_cpos?1:0);
      while ( !spacechar(inputline[istart]) )
         if (istart == 0)
            break;
         else
            istart--;
      if ( spacechar(inputline[istart]) ) istart++;

      sprintf(tbuff, "%.*s", inpl_cpos-istart, &inputline[istart]);

/* Check filename can be completed */
      if ( strpbrk( tbuff, "*?[" ) == NULL ) {

/*   Get expanded if necessary filename into tbuff */
         if ( strpbrk(tbuff,"~$") != NULL ) {
            sprintf(combuff, "echo %s", tbuff);
            result = shell_command( combuff );
/*   If there was no expansion, tbuff remains as original */
            if (result != NULL) {
               if (strcmp(result, "")) {
                  strcpy(tbuff, result);
               }
               free( result );
               if ( (result=strrchr(tbuff,'\n')) != NULL ) *result = '\0';
            }
         }
         expanded_len = strlen(tbuff);
         trunc = 0;

         if ( !(status=glob( strcat(tbuff,"*"), globflags, NULL, &filelist)) ) {
            if ( filelist.gl_pathc ) {
               nfiles =  filelist.gl_pathc;
/*   Got some filenames */
               if ( nfiles > 1 ) {
/*      Multiple possibilities - report and find any common part */
                  if (screenstate == SCREENMODE) {
                     waddstr(bottomwin, "\nMultiple matches.\n");
                     wrefresh(bottomwin);
                  } else {
                     printf("\nMultiple matches.\n");
                  }
                  ringbell();
                  nmatch = strlen( tbuff ) - 1;
                  while (  filelist.gl_pathc == nfiles ) {
                     if ( strlen( *filelist.gl_pathv ) != nmatch++ ) {
                        *(*filelist.gl_pathv + nmatch) = '\0';
                        strcpy(tbuff, *filelist.gl_pathv);
                        globfree( &filelist );
                        strcat(tbuff,"*");
                        if ( glob(tbuff, globflags, NULL, &filelist) ) {
                           nfiles++; /* stops the while */
                           ringbell();
                        }

                     } else {
/*         combuff is exact match for 1st name
 *         so all future matches will be same */
                        nfiles++; /* stops the while */
                     }
                  }
                  *(*filelist.gl_pathv + nmatch -1 ) = '\0';

               } else {
/*   Only one match - check if .sdf truncation required */
                  if ( (adam_extn = getenv( "ADAM_EXTN" )) != NULL ) {
                     strcpy( combuff, adam_extn );
/*        ADAM_EXTN is set - construct list of extensions to be cut */
                     extn = strtok( combuff, ", " );
                     for ( nexts=0; extn != NULL; nexts++ ) {
                        extns[nexts] = extn;
                        extn = strtok( NULL, ", " );
                     }

                  } else {
/*        ADAM_EXTN undefined - assume .sdf */
                     nexts = 1;
                     extns[0] = ".sdf";
                  }

/*        Flag end of extns list */
                  extns[nexts] = NULL;

/*        Truncate the filename if required */
                  nmatch = (int)strlen( *filelist.gl_pathv );
                  i = 0;
                  while( (extns[i] != NULL) ) {
                     lenext = (int)strlen( extns[i] );
                     if (
                       !strcmp( *filelist.gl_pathv+nmatch-lenext, extns[i] ) ) {
                        *(*filelist.gl_pathv + nmatch - lenext ) = '\0';
                        if ( nmatch - lenext < expanded_len ) {
                           trunc = expanded_len - (nmatch - lenext);
                           expanded_len -= trunc;
                        } else
                           trunc = 0;
                        i = nexts;
                     } else
                        i++;
                  }
               }

/*   Found completed (as far as possible) name */
/*   Restore any environment variables */
/*   and copy to inputline */
               strcpy( combuff, &inputline[inpl_cpos] );
               strcpy( &inputline[inpl_cpos-trunc],
                       *filelist.gl_pathv+expanded_len );
               if ( nfiles == 1 )
                  strcat(inputline," ");
               inpl_cpos = strlen( inputline );
               strcat( inputline, combuff );
               inpl_epos = strlen(inputline);

            } else {
/*   No matches */
/*   Actually glob returns error in this situation */
               if (screenstate == SCREENMODE) {
                  waddstr(bottomwin, "\nNo match.\n");
                  wrefresh(bottomwin);
               } else {
                  printf("\nNo match.\n");
               }
               ringbell();
            }

         } else {
/*               Failed getting filenames */
#ifdef GLOB_NOMATCH
	   if ( status == GLOB_NOMATCH ) nomatch = 1;
#else
            /* If GLOB_NOMATCH is not defined assume that we get
               good status even when match count is zero. This
               branch then only hits if no matches and status is good.
             */
	   if ( status == 0 && filelist.gl_pathc == 0) nomatch = 1;
#endif
            if ( nomatch )
               if (screenstate == SCREENMODE) {
                  waddstr(bottomwin, "\nNo match.\n");
                  wrefresh(bottomwin);
               } else {
                  printf("\nNo match.\n");
               }
            ringbell();
         }
         globfree( &filelist );

      } else {
/*  inputline contained globbing characters - cannot match */
         ringbell();
      }
}

/******************************************************************************
 *
 *	K E Y B O A R D _ I N P U T (int void)
 *
 * This routine is called when there is keyboard input to process.
 *
 * icl_io uses single character input and locally handles all buffering and
 * processing of characters. When a complete input line is obtained this is
 * send to the ICL process to arrive on its 'ast-input' messys queue.
 *
 * This routine is normally called from the main select() loop in main() but
 * is also used to process typeahead when it is called directly.
 *
 ******************************************************************************
 */
void
keyboard_input(void)
{
    int status, ams_value_len, readret, inputavailable, worker, worker1;
    char readbuff[256], *w;
    char combuff[256];
    char *cp;
    char *result;
    fd_set infds;
    struct timeval time;
    glob_t filelist;
    int globflags=0;
    char **fp;
    int nfiles, nmatch;
    int i;
    int save_cpos, save_epos;
    char linbuf[80];
    if (screenstate < 0)
	return;
/*
 * Source WAS stdin and screenstate is positive - ie we own screen and
 * keyboard
 */
    time.tv_sec  = 0;
    time.tv_usec = 0;
    for(;;) {
	FD_ZERO(&infds);
	FD_SET(fileno(stdin), &infds);
	if(!select(2, &infds, NULL, NULL, &time))	/* non-blocking */
	    break;
	if(FD_ISSET(fileno(stdin), &infds)) {
	    readret = read(fileno(stdin), readbuff,
			   ((inbuf_rpos >= inbuf_cpos) ?
                           (INPUTBUFFERLENGTH - (inbuf_rpos-inbuf_cpos)) :
                           (inbuf_cpos-inbuf_rpos-2)));
	    w = readbuff;
	    while (readret > 0) {
		if( *w == CNTLX)
		    inbuf_cpos = inbuf_rpos;
		else {
		    inbuff[inbuf_rpos] = *w;
		    incinbuf_rpos();
		}
		w++;
		readret--;
	    } /* while readret */
	}
	inbuff[inbuf_rpos] = '\0';
    } /* for */

    if (!expectinginput)
	return;
    else
	inputavailable = (inbuf_rpos != inbuf_cpos);

    while (inputavailable) {
	if (normalcharacter(inbuff[inbuf_cpos])) {
	    if (insertmode == OVERSTRIKE || inpl_cpos == inpl_epos)
		if (inpl_cpos == INPUTLINELENGTH)
/*                  Line full */
	           ringbell();
		else {
                    int colno;
/*              Transfer input buffer character to input line, */
/*              and increment inpl_cpos                        */
	            inputline[inpl_cpos++] = inbuff[inbuf_cpos];
/*              Terminate input line and move inpl_epos */
		    if (inpl_cpos > inpl_epos) {
			inputline[inpl_cpos] = '\0';
			inpl_epos = inpl_cpos;
		    }
/*              Calculate the column number */
                    colno = (prompt_len + inpl_epos) % COLS;
		    if (screenstate == SCREENMODE) {
/*                In screenmode, add the character to the screen */
			waddch(bottomwin, inbuff[inbuf_cpos]);
			wrefresh(bottomwin);
/*                and if we go on to a new line, move instarty up one */
                        if ( !colno )
                           if ( (bwlines - instarty ) <
                              (prompt_len+inpl_epos)/COLS + 1) {
                              instarty--;
                           }
		    } else {
			putchar((int) inbuff[inbuf_cpos]);
                        if ( !colno ) {
                           putchar('\r');
                           putp(cursor_down);
                        }
                    }
		} else {
             /* Inserting in middle of line */
                int winpl_epos;
                save_cpos = inpl_cpos;
                save_epos = inpl_epos;
                winpl_epos = ++inpl_epos;

		do {
		    inputline[inpl_epos] = inputline[inpl_epos - 1];
		    inpl_epos--;
		}
		while (inpl_epos != inpl_cpos);
		inputline[inpl_cpos++] = inbuff[inbuf_cpos];
		inpl_epos = winpl_epos;
		if (screenstate == SCREENMODE)
		    positionredisplay(CHARNIL);
		else {
                    posbol( 1, prompt_len, save_cpos, save_epos );
                    redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
                }
	    }
	    incinbuf_cpos();
	} else /* !normalcharacter */  {
	    int keycode;

	    keycode = codespecialkey();
	    switch (keycode) {

	      case SUBSTRING:
		return;

	      case NOMATCH:
		ringbell();
		incinbuf_cpos();
		break;

              case Keol:
/*              Ensure cursor is at end of line, before doing newline */
		if (screenstate == SCREENMODE) {
                   if(inpl_cpos != inpl_epos) {
                      redisplay(inputline);
                   }
                   mywnewline(bottomwin,bwlines);
                   wrefresh(bottomwin);
		} else {
                    if(inpl_cpos != inpl_epos) {
                       posbol( 0, prompt_len, inpl_cpos, inpl_epos );
                       redoline( 1, prompt_len, inpl_epos, inpl_epos, 0 );
                    }
		    printf("\n");
                }
		if (inputline[0] != '\0') /* Don't store blank lines */
		    addtorecall(inputline);
                saveinput_p = CHARNIL;
		message.fcode = curcmdcode;
		message.iarg = inputarg;
		strcpy(message.buff, inputline);
		strcat(message.buff, "\n");
/*    if (screenstate==SCREENMODE){
       waddstr(bottomwin,"MESSAGEOUT:");
       waddstr(bottomwin,message.buff);
       mywnewline(bottomwin,bwlines);
       wrefresh(bottomwin);}
*/		message.buflen = strlen(message.buff) + 1;
		ams_value_len = message.buflen + 4*sizeof(int);
		status = SAI__OK;
		ams_reply(icl_path, icl_messid, MESSYS__MESSAGE,
			  MESSYS__TRIGGER, OBEY, "IOCOMMAND",
			  ams_value_len, (char *) &message, &status);
		expectinginput = FALSE;
		interuptableinput = FALSE;
		inputarg   = NOTKNOWN;
		curcmdcode = NOTKNOWN;
		prompt[0] = '\0';
		dvalue[0] = '\0';
		inputline[0] = '\0';
		inpl_cpos = inpl_epos = 0;
		insertmode = INSERT;
		processqueue();
		break;

	      case Kleft:
		if( inpl_cpos > 0) {
		    inpl_cpos--;
		    if (screenstate == SCREENMODE) {
			mywmovebackone(bottomwin);
			wrefresh(bottomwin);
		    } else
                    if ( cursor_left != CHARNIL &&
                        cursor_up != CHARNIL) {
                       if ( (prompt_len+inpl_cpos) % COLS != COLS - 1 )
                          putp(cursor_left);
                       else {
                          putp( cursor_up );
                          if ( parm_right_cursor != CHARNIL )
                             putp(tparm(parm_right_cursor, (long int)COLS-1,0,0,0,0,0,0,0,0));
                          else if ( cursor_right != CHARNIL )
                             for (i=COLS-1;i--;putp(cursor_right));
                          else printf("%s%.*s", prompt, inpl_cpos, inputline);
                       }
                    } else {
                       posbol( 0, prompt_len, inpl_cpos, inpl_epos );
                       inpl_cpos--;
                       redoline( 1, prompt_len, inpl_cpos, inpl_epos, 0 );
                    }
		} else
                    ringbell();

		break;

	      case Kright:
		if (inpl_cpos < inpl_epos) {
		    inpl_cpos++;
		    if (screenstate == SCREENMODE) {
			mywmoveforwardone(bottomwin, bwlines);
			wrefresh(bottomwin);
		    } else
                    if ( cursor_right != CHARNIL &&
                        cursor_down != CHARNIL) {
                       if ( (prompt_len+inpl_cpos) % COLS )
                    /* Normal right */
                          if ( cursor_right != CHARNIL )
                             putp( cursor_right );
                          else
                             putchar((int) inputline[inpl_cpos - 1]);
                       else {
                    /* Need to wrap */
                          if ( cursor_down != CHARNIL ) {
                             putchar( '\r' );
                             putp( cursor_down );
                          } else
                             printf( "\r%s%.*s", prompt, inpl_cpos, inputline );
                       }
                    } else {
                       posbol( 0, prompt_len, inpl_cpos, inpl_epos );
                       inpl_cpos++;
                       redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
                    }
		} else
                    ringbell();

		break;

	      case Kup:
		if ((w = prevline()) == CHARNIL)
		    ringbell();
		else {
                    if ( recall == recall_p ) {
                        strcpy(saveinput, inputline);
                        saveinput_p = saveinput;
                    }
                    if (screenstate == SCREENMODE)
			redisplay(w);
		    else {
			if (w != CHARNIL)
			    strcpy(inputline, w);
			else
			    inputline[0] = '\0';
                        posbol( 1, prompt_len, inpl_cpos, inpl_epos );
			inpl_cpos = inpl_epos = strlen(inputline);
			printf("%s%s", prompt, inputline);
		    }
                    }
		break;

	      case Kdown:
		if ((w = nextline()) == CHARNIL) {
                    if (saveinput_p != CHARNIL) {
                       strcpy(inputline,saveinput);
                       saveinput_p = CHARNIL;
                    } else
                       ringbell();
		} else
		    strcpy(inputline, w);
		if (screenstate == SCREENMODE)
		    redisplay(inputline);
		else {
                    posbol( 1, prompt_len, inpl_cpos, inpl_epos );
  	            inpl_cpos = inpl_epos = strlen(inputline);
                    redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
		}
		break;

	      case Kredisp:
		if (screenstate == SCREENMODE)
		    positionredisplay(CHARNIL);
		else  {
                    posbol( 1, prompt_len, inpl_cpos, inpl_epos );
                    if ( cursor_up != CHARNIL )
                       redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
                    else {
                       inpl_cpos = inpl_epos;
                       printf( "%s%s", prompt, inputline );
                    }
		}
		break;

	      case Ktab:
/*            If at beginning of line, get suggested value - otherwise
 *            attempt filename completion
*/
                if ( inpl_epos != 0 ) {
/*            Something already typed - assume filename completion required */
                   filename_complete( );
                   if (screenstate == SCREENMODE)
                      positionredisplay(CHARNIL);
                   else {
                   posbol( 0, prompt_len, inpl_cpos, inpl_epos );
                   redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
                   }
                   break;

                } else {
  		   if ( dvalue[0] != '\0') {  /* accept supplied default */
		      strcpy(inputline, dvalue);
		      inpl_cpos = strlen(dvalue);
		   } else {
		      inputline[0] = '\0';
		      inpl_cpos = 0;
  		   }
                }
                /* continue with Kcleareol code */

	      case Kcleartoeol:
		if (screenstate == SCREENMODE) {
                  inpl_epos = inpl_cpos;
		  inputline[inpl_epos] = '\0';
		  positionredisplay(CHARNIL);
		} else {
                  posbol( 1, prompt_len, inpl_cpos, inpl_epos );
  		  inpl_epos = inpl_cpos;
		  inputline[inpl_epos] = '\0';
                  redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
		}
		break;

	      case Kclearall:
		if (screenstate == SCREENMODE) {
		    inputline[0] = '\0';
		    inpl_epos = inpl_cpos = 0;
  		    inbuff[0] = '\0';
		    inbuf_rpos = inbuf_cpos = 0;
		    wmove(bottomwin, instarty, instartx);
		    wclrtobot(bottomwin);
		    waddstr(bottomwin, prompt);
		    wrefresh(bottomwin);
		} else {
                    posbol( 1, prompt_len, inpl_cpos, inpl_epos );
   		    inputline[0] = '\0';
		    inpl_epos = inpl_cpos = 0;
                    redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
		    inbuff[0] = '\0';
		    inbuf_rpos = inbuf_cpos = 0;
		}
		break;

	      case Kcleartobol:
                save_cpos = inpl_cpos;
                save_epos = inpl_epos;
		if (inpl_cpos == inpl_epos) {
		    inpl_epos = 0;
		    inputline[0] = '\0';
		} else {
		    strcpy(inputline, &inputline[inpl_cpos]);
		    inpl_epos = strlen(inputline);
		}
		inpl_cpos = 0;
		if (screenstate == SCREENMODE)
		    positionredisplay(CHARNIL);
		else {
		    inpl_cpos = 0;
                    redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
		}
		break;

	      case Ktosol:
		if (screenstate == SCREENMODE) {
		   inpl_cpos = 0;
		   positionredisplay(CHARNIL);
		} else {
                   posbol( cursor_up==CHARNIL, prompt_len, inpl_cpos, inpl_epos );
  		   inpl_cpos = 0;
                   redoline( cursor_up==CHARNIL, prompt_len, inpl_cpos, inpl_epos,
                   1 );
                }
		break;

	      case Ktoeol:
		if (screenstate == SCREENMODE) {
		    inpl_cpos = inpl_epos;
		    positionredisplay(CHARNIL);
		} else {
                    posbol( cursor_up==CHARNIL, prompt_len, inpl_cpos,
                     inpl_epos );
   		    inpl_cpos = inpl_epos;
                    redoline( cursor_up==CHARNIL, prompt_len, inpl_cpos,
                     inpl_epos, 0 );
                }
		break;

/*	      case Kim:
		insertmode = (insertmode == OVERSTRIKE ? INSERT : OVERSTRIKE);
		break;
*/
              case Kdel:
		if (inpl_cpos != inpl_epos) {
		    worker1 = inpl_cpos;
                    worker = worker1 + 1;

		    while (worker <= inpl_epos)
			inputline[worker1++] = inputline[worker++];
		    if (screenstate == SCREENMODE) {
		       inpl_epos--;
                       positionredisplay(CHARNIL);
                    } else {
                       posbol( 1, prompt_len, inpl_cpos, inpl_epos );
		       inpl_epos--;
                       redoline( 1, prompt_len, inpl_cpos, inpl_epos, 0 );
                    }
                    break;
		}
/*             If at end of line, continue with Klist code */

              case Klist:
/*          List filename options */
/*            Find beginning of word */
                 worker = inpl_cpos - (inpl_cpos?1:0);
                 while ( !spacechar(inputline[worker]) )
                    if (worker == 0)
                       break;
                    else
                       worker--;
                 if ( spacechar(inputline[worker]) ) worker++;

                 sprintf(combuff,
                    "ls -Cd %.*s*",inpl_cpos-worker, &inputline[worker]);
                 result = shell_command( combuff );
                 if (screenstate == SCREENMODE) {
                    wprintw(bottomwin, "\n%s", result);
                    wrefresh(bottomwin);
                 } else
                    printf("\n%s", result );
                 free( result );

                 if (screenstate == SCREENMODE)
                    positionredisplay(CHARNIL);
                 else {
                    posbol( 1, prompt_len, inpl_cpos, inpl_epos );
                    redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
                 }
                 break;

	      case Kbdel:
		if (inpl_cpos == 0)
		    ringbell();
		else {
                    save_cpos = inpl_cpos;
                    save_epos = inpl_epos;
		    worker1 = inpl_cpos - 1;
		    worker = inpl_cpos;
		    inpl_cpos = worker1;
		    while (worker <= inpl_epos)
			inputline[worker1++] = inputline[worker++];
		    inpl_epos = worker1 - 1;
                    if ( screenstate == SCREENMODE ) {
                       positionredisplay(CHARNIL);
                    } else
                    if (inpl_cpos == inpl_epos &&
                        cursor_left != CHARNIL &&
                        delete_character != CHARNIL) {
                       if ( (prompt_len+inpl_cpos) % COLS != COLS - 1 )
                          putp(cursor_left);
                       else {
                          putp( cursor_up );
                          if ( parm_right_cursor != CHARNIL )
                             putp(tparm(parm_right_cursor, 79L,0,0,0,0,0,0,0,0));
                          else
                             for (i=79;i--;putp(cursor_right));
                       }
                       putp(delete_character);
                    } else {
                       posbol( 1, prompt_len, save_cpos, save_epos );
                       redoline( 1, prompt_len, inpl_cpos, inpl_epos, 0 );
                    }
		}
		break;


	      case Kdelw:
/*         Delete to end of next word */
                worker1 = worker = inpl_cpos;
/* Find first word character */
		while (noisechar(inputline[worker]))
		    if (worker == inpl_epos)
			break;
		    else
			worker++;

/* Find end of word (or line) */
		while (!noisechar(inputline[worker]))
		    if (worker == inpl_epos)
			break;
		    else
			worker++;

/*                 worker points at first char of remainder */
                save_cpos = inpl_cpos;
                save_epos = inpl_epos;
		inpl_cpos = worker1;
		while (worker <= inpl_epos)
		    inputline[worker1++] = inputline[worker++];
		inpl_epos = worker1 - 1;
		if (screenstate == SCREENMODE)
                   positionredisplay(CHARNIL);
                else {
                   posbol( 1, prompt_len, save_cpos, save_epos );
                   redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
                }
                break;

	      case Kbdelw:
/*         Delete to beginning of word */
                if ( inpl_cpos == 0 )
                   ringbell();

                else {
/* Remember current position and start search at previous character */
                   worker1 = worker = inpl_cpos;
                   worker1--;

/* Find first word character */
                   while (noisechar(inputline[worker1]))
		     if (worker1 == 0)
			break;
		     else
			worker1--;

                   if ( !noisechar(inputline[worker1]) ) {
/* Find beginning of word (or line) */
                      while (!noisechar(inputline[worker1]))
                         if (worker1 == 0)
                            break;
                         else
                            worker1--;
                      if (noisechar(inputline[worker1])) worker1++;
                   }

/*                 worker points at first char of remainder */
                     save_cpos = inpl_cpos;
                     save_epos = inpl_epos;
 		     inpl_cpos = worker1;
		     while (worker <= inpl_epos)
		       inputline[worker1++] = inputline[worker++];
		     inpl_epos = worker1 - 1;
                     if ( screenstate == SCREENMODE )
                        positionredisplay(CHARNIL);
                     else {
                        posbol( 1, prompt_len, save_cpos, save_epos );
                        redoline( 1, prompt_len, inpl_cpos, inpl_epos, 0 );
                     }
                 }
                 break;


              case Kfword:
/*         Move to end of next word */
                worker1 = worker = inpl_cpos;
/* Find first word character */
		while (noisechar(inputline[worker]))
		    if (worker == inpl_epos)
			break;
		    else
			worker++;

/* Find end of word (or line) */
		while (!noisechar(inputline[worker]))
		    if (worker == inpl_epos)
			break;
		    else
			worker++;

/*                 worker points at new position */
		inpl_cpos = worker;
                if ( screenstate == SCREENMODE )
                   positionredisplay(CHARNIL);
                else
                   redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
                break;

	      case Kbword:
/*         Move to beginning of word */

                if ( inpl_cpos == 0 )
                   ringbell();

                else {
/* Remember current position and start search at previous character */
                   worker1 = worker = inpl_cpos;
                   worker1--;

/* Find first word character */
                   while (noisechar(inputline[worker1]))
 		      if (worker1 == 0)
			 break;
		      else
			 worker1--;

/* Find beginning of word (or line) */
 		   while (!noisechar(inputline[worker1]))
                      if (worker1 == 0)
                         break;
                      else
                         worker1--;

                   if (worker1) worker1++;

                   if ( screenstate == SCREENMODE ) {
 	              inpl_cpos = worker1;
                      positionredisplay(CHARNIL);
                   } else {
                      posbol( 1, prompt_len, inpl_cpos, inpl_epos );
  	              inpl_cpos = worker1;
                      redoline( 1, prompt_len, inpl_cpos, inpl_epos, 0 );
                   }
                }
                break;

	      default:
		break;

	    } /* switch on special keys */
	} /* else not a normal character */

	if(!expectinginput)
	    inputavailable = FALSE;
	else
	    inputavailable = (inbuf_rpos != inbuf_cpos);
    } /* of while inputavailable */

    return;
}

/******************************************************************************
 *
 *	S H E L L _ C O M M A N D ( char *command )
 *
 * This routine runs a command in the c-shell
 *
 * Returns either:
 *	TRUE and stdout from the command
 *	OR
 *	FALSE and any error text produced by the shell on error
 *
 ******************************************************************************
 */
char
*shell_command( char *command )
{
    int pid, fd[2], fde[2], status;
    size_t resultsize, resp;
    ssize_t nchars;
    char *result;

	/* Use the C shell to run the command */
	if ( (pipe(fd) < 0) || (pipe(fde) < 0 ) ) {
   	    perror("shell_command() pipe error");
	    return NULL;
	} else {
	    if ( (pid = fork()) < 0) {
		perror("shell_command() fork error");
		return NULL;
	    } else if (pid == 0) {	/* Child */
		close(fd[0]);
    		dup2(fd[1], 1);	/* stdout now the write pipe to icl */
		dup2(fde[1], 2); /* stderr output also sent to pipe  */
		close(fd[1]);
                close(fde[1]);

		if (execlp("csh","csh", "-c", command, (char *) 0) < 0)
			perror("shell_command() csh exec error");
		exit(1);
	    } else {			/* Parent */
		close(fd[1]);
                close(fde[1]);

                result = (char *)malloc(256);
                resultsize = 256;
                resp = 0;
                nchars = 1;
                while ( nchars>0 ) {
                   if ( result != NULL ) {
                      if ( resp == resultsize-1 ) {
                         resultsize+=256;
                         result = (char *)realloc((void *)result,resultsize);
                      }
                      if ( (nchars =
                         read(fd[0], (void *)(result+resp), resultsize-resp-1))
                         < 0 )
                         perror("shell_command()");
                      resp += nchars;
                   } else {
                      perror("shell_command()");
                   }
                }
                result[resp] = '\0';

                close(fd[0]);
                close(fde[0]);
		waitpid(pid, &status, 0);
	    }
	}
    if (status == 0)
	return result; /* TRUE */
    else
	return result; /* FALSE */
}

/******************************************************************************
 *
 *	C O M M A N D _ I N P U T (void)
 *
 * This routine is called to process command to icl_io from the parent ICL
 * process. ICL creates a Unix pipe just before forking this process and
 * the commands (which are C structures passed as a byte stream) arrive here.
 *
 * This routine is called from the main select() loop in main()
 *
 ******************************************************************************
 */
void
command_input(void)
{
    int row, column, start_row, end_row;
    int x1, x2, y1, y2;

    if( read(ICLPIPE, &message, 4*sizeof(int)) != 4 * sizeof(int) ||
	read(ICLPIPE, message.buff, message.buflen) != message.buflen)
	{
	fprintf(stderr,
		"icl-io - invalid length command from icl - exiting\n");
	notkilled = FALSE;
	return;
    }
    switch (message.fcode) {

      case IO_COMMAND_SETSCREEN:
/*
 * Argument gives number of lines in bottom (scrolling) portion of screen)
 */
	initscreen(message.iarg);
	break;

      case IO_COMMAND_GETINPUT:
      case IO_COMMAND_GETINTINPUT:
	if (expectinginput || screenstate < 0) {
/*
 * Need to queue either the current (partially processed) input request or
 * the new one
 */
	    if (screenstate > 0 && interuptableinput) {
/*
 * Store current input request and its current state
 */
		addtoqueue(curcmdcode, prompt, dvalue, inputline, inputarg);
/* Process new request */
		inputline[0] = '\0';
		inpl_cpos = inpl_epos = 0;
		insertmode = INSERT;
		strcpy(prompt, message.buff);
                prompt_len = strlen(prompt);
		inputarg  = message.iarg;	/* Input sequence number */
		curcmdcode = message.fcode;
		if (message.dindex != 0)
		    strcpy(dvalue, &message.buff[message.dindex]);
		else
		    dvalue[0] = '\0';
		if (screenstate == SCREENMODE) {
		    mywnewline(bottomwin, bwlines);
		    getyx(bottomwin, instarty, instartx);
		    waddstr(bottomwin, message.buff);
		    wrefresh(bottomwin);
		} else
		    if (screenstate == LINEMODE)
			printf("\r\n%s", message.buff);
		expectinginput = TRUE;
		if (inbuf_cpos != inbuf_rpos)
		    keyboard_input();
		recall_p = recall;	/* reset memory position for new input */
		inc_recall_p();
		interuptableinput =
			    (message.fcode == IO_COMMAND_GETINTINPUT);
	    } else
/*
 * Add new request to queue
 */
		addtoqueue(message.fcode, message.buff,
			   (message.dindex == 0 ?
				CHARNIL : &message.buff[message.dindex]),
			   CHARNIL, message.iarg);
	} else {
/*
 * Can process input request immediately. Output prompt and set for input.
 */
	    strcpy(prompt, message.buff);
            prompt_len = strlen(prompt);
	    if (message.dindex != 0)
		strcpy(dvalue, &message.buff[message.dindex]);
	    else
		dvalue[0] = '\0';
	    inputarg  = message.iarg;
	    curcmdcode = message.fcode;
	    if (screenstate == SCREENMODE) {
		getyx(bottomwin, instarty, instartx);
		waddstr(bottomwin, message.buff);
		wrefresh(bottomwin);
	    } else
		if (screenstate == LINEMODE)
		    printf("%s", message.buff);
	    inputline[0] = '\0';
	    inpl_cpos = inpl_epos = 0;
	    insertmode = INSERT;
	    expectinginput = TRUE;
	    if (inbuf_cpos != inbuf_rpos)
		keyboard_input();
	    recall_p = recall;	/* reset memory position for new input */
	    inc_recall_p();
	    interuptableinput =
			(message.fcode == IO_COMMAND_GETINTINPUT);
	}
	break;
/*
 * *****************************************************************************
 */
      case IO_COMMAND_OUTPUT:
	if (expectinginput || screenstate < 0) {
	    if (screenstate > 0 && interuptableinput)
/*
 * Interuptable input is in progress. Process output request and reprompt
 */
		if (screenstate == SCREENMODE) {
		    getyx(bottomwin, y1, x1);
		    mywnewline(bottomwin, bwlines);
		    waddstr(bottomwin, message.buff);
		    mywnewline(bottomwin, bwlines);
		    waddstr(bottomwin, prompt);
		    waddstr(bottomwin, inputline);
		    getyx(bottomwin, y2, x2);
		    wmove(bottomwin, y2, x1);
		    wrefresh(bottomwin);
		} else {
		    printf("\n%s", message.buff);
		    clearline();
		    printf("%s%s", prompt, inputline);
		    if (inpl_cpos != 0)
			printf("\r%s%.*s", prompt, inpl_cpos, inputline);
		}
	    else	/* Queue output request */
		addtoqueue(message.fcode, message.buff,
			   (message.dindex == 0 ?
				CHARNIL : &message.buff[message.dindex]),
			    CHARNIL, message.iarg);
	} else	/* Process only output request */
	    if (screenstate == SCREENMODE) {
		wprintw(bottomwin, "%s", message.buff);
		wrefresh(bottomwin);
	    } else
		printf("%s", message.buff);
	break;
/*
 * *****************************************************************************
 */
      case IO_COMMAND_CLEAR:
	start_row = message.iarg / 256;
	end_row   = message.iarg % 256;

	if (expectinginput)
	    fprintf(stderr, "icl_io error - CLEAR when input active\n");
	else if (screenstate != SCREENMODE)
	    printf("CLEAR - not in screen mode\n");
	else {
	    if (end_row > twlines)
		end_row = twlines;
	    if (start_row <= 0 || start_row > twlines) {
		waddstr(bottomwin, "CLEAR - not in fixed screen portion\n");
		wrefresh(bottomwin);
	    } else {
		if (end_row == twlines ) {
		    wmove(topwin, start_row - 1, 0);
		    wclrtobot(topwin);
		} else {
		    do {
			wmove(topwin, start_row - 1, 0);
			wclrtoeol(topwin);
			start_row++;
		    }
		    while (start_row <= end_row);
		}
		wrefresh(topwin);
	    }
	}
	break;
/*
 * *****************************************************************************
 */
      case IO_COMMAND_LOCATE:
	row    = message.iarg / 256;
	column = message.iarg % 256;

	if (expectinginput)
	    fprintf(stderr, "icl_io error - LOCATE while input active\n");
	else if (screenstate != SCREENMODE)
	    printf("LOCATE - not in screen mode\n");
	else {
	    if ((column <= 0 || column > COLS) ||
		(row    <= 0 || row    > twlines) ) {
		waddstr(bottomwin, "LOCATE - not in fixed part of screen\n");
		wrefresh(bottomwin);
	    } else {
		wmove(topwin, row - 1, column - 1);
		mywaddstr(topwin, message.buff);
		wrefresh(topwin);
	    }
	}
	break;
/*
 * *****************************************************************************
 */
      case IO_COMMAND_KILL:
	notkilled = FALSE;
	break;

/*
 * *****************************************************************************
 */
      case IO_COMMAND_SUSPEND:
	interrupted = TRUE;
	break;

/*
 * *****************************************************************************
 */
      default:
	break;

    } /* switch on message command */

    return;
}

/******************************************************************************
 *
 *	R E S I Z E _ H A N D L E R ( int signo )
 *
 * This function is called when we get a window resize event (n.b. only on
 * local windows and remotely connected ones using rlogin - NOT telnet!) and
 * resets the variables 'COLS' and 'LINES' (declared in curses.h) accordingly.
 *
 ******************************************************************************
 */
void
resize_handler(int signo)
{
    struct winsize size;

    ioctl(fileno(stdin), TIOCGWINSZ, (char *) &size);
    COLS  = size.ws_col;
    LINES = size.ws_row;

    if (screenstate == SCREENMODE)
    {
    blankline[curinlength - 1] = ' ';
    curinlength = COLS;
    blankline[curinlength - 1] = '\0';
        touchwin(topwin);
	touchwin(bottomwin);
	wrefresh(topwin);
	wrefresh(bottomwin);
    } else {
        posbol( 1, prompt_len, inpl_cpos, inpl_epos );
        redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
    }
    return;
}

/******************************************************************************
 *
 *	C N T L C _ H A N D L E R ( int signo )
 *
 * We arrange for this handler to be called when the tty driver generates
 * a SIGINT or SIGQUIT.
 *
 ******************************************************************************
 */
void
cntlc_handler(int signo)
{
    sigset_t mask;
    int status = SAI__OK, ams_value_len;

    inputline[0] = '\0';
    inpl_epos = inpl_cpos = 0;
    inbuf_rpos = inbuf_cpos = 0;
    inbuff[0] = '\0';
    if (screenstate == SCREENMODE) {
	mywnewline(bottomwin, bwlines);
	getyx(bottomwin, instarty, instartx);
	wclrtobot(bottomwin);
	waddstr(bottomwin, prompt);
	wrefresh(bottomwin);
    }
    fprintf(stderr,"*** Interrupt ***\n");
/*
 * Get back to the icl_io idle state (ie waiting for either a keyboard key
 * or a command from ICL. This involves unblocking the SIGINT signal and
 * performing a stack unwind (using longjmp) after some tidying up.
 */
    if (expectinginput)
	expectinginput = 0;
    clearqueue();
    sigemptyset(&mask);
    sigaddset(&mask, SIGINT);
    sigprocmask(SIG_UNBLOCK, &mask, NULL);
    siglongjmp(goback, 0);
}

/******************************************************************************
 *
 *	C N T L Z _ H A N D L E R ( int signo )
 *
 * We arrange for this handler to be called when the tty driver generates
 * a SIGTSTP (CNTL/Z). A flag (interrupted) is set and an action routine
 * (do_cntlz) is called at an appropriate point.
 *
 ******************************************************************************
 */
void
cntlz_handler(int signo)
{
    interrupted = 1;
}

/******************************************************************************
 *
 *	D O _ C N T L Z
 *
 *
 ******************************************************************************
 */
void
do_cntlz(void)
{
    struct sigaction oact, sigint;
    sigset_t mask;

    if(screenstate == SCREENMODE)
	putp(clear_screen);
    sigaction(SIGINT, NULL, &sigint);
    signal(SIGINT,  SIG_IGN);
    signal(SIGQUIT, SIG_IGN);
    sigemptyset(&mask);
    sigaddset(&mask, SIGTSTP);
    sigaction(SIGTSTP, NULL, &oact);
    sigprocmask(SIG_UNBLOCK, &mask, NULL);
    signal(SIGTSTP, SIG_IGN);
    signal(SIGTTOU, SIG_IGN);
    tcsetattr(fileno(stdin), TCSANOW, &init_tty); /* Reset tty settings */
    kill(getpid(), SIGSTOP);		/* Really stop here! */
    signal(SIGTTOU, SIG_DFL);
    sigaction(SIGTSTP, &oact, NULL);
    sigaction(SIGINT,  &sigint, NULL);
    sigaction(SIGQUIT, &sigint, NULL);
    if (screenstate == SCREENMODE) {
	fixterm();
	wrefresh(curscr);
	positionredisplay(CHARNIL);
    } else {
	tcsetattr(fileno(stdin), TCSANOW, &linemode_tty);
	if (expectinginput)
/*	    printf("\r%s%s\r%s%.*s", prompt, inputline, prompt, inpl_cpos,
			inputline);
*/
            redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
    }
}

/******************************************************************************
 *
 *	E X I T _ H A N D L E R ( int signo )
 *
 * We arrange for this handler to be called to restore terminal characteristics
 * on process exit.
 *
 ******************************************************************************
 */
void
exit_handler(void)
{
    tcsetattr(fileno(stdin), TCSANOW, &init_tty);
    return;
}

/******************************************************************************
 *
 *	S I G  _ H A N D L E R ( int signo )
 *
 * We arrange for this handler to be called for signals which terminate the
 * process - it calls the exit handler to restore terminal characteristics
 * on process exit.
 *
 ******************************************************************************
 */
void
sig_handler( int signo )
{
    exit_handler();
    return;
}

/*******************************************************************************
*	I N I T S C R E E N (int nolines)
*
* This routine handles the ICL output screen modes and changes between these.
*
* Initially ICL starts in screen state 'INITIAL' and we arrange to move from
* this to 'LINEMODE'.
*
* Further ICL SET (NO)SCREEN commands can change between LINEMODE and
* SCREENMODE and, further, when in SCREENMODE the "fixed" portion of the
* screen can be set to be 'nolines' in size.
*
*******************************************************************************/
void initscreen(int nolines) {
int nochars;
struct sigaction act;

    if (screenstate == 0) { /* ICL startup */
/*
 * Save the terminal settings we are started with so we can restore them
 * both on process exit and to run a Unix external command. For process exit
 * we also setup an exit handler
 */
	tcgetattr(fileno(stdin), &init_tty);
	if (!(init_tty.c_lflag & ICANON)) /* ensure restore to standard state */
	    init_tty.c_lflag |= (ICANON | ECHO | ISIG);

#if HAVE_ATEXIT
	atexit(exit_handler);		       /* ANSI C/POSIX */
#elif HAVE_ON_EXIT
	on_exit(exit_handler, 0);		/* SunOS	*/
#else
# error "Do not know how to register an exit handler"
#endif
/*
 * icl-io uses curses/terminfo. Initially we are in LINEMODE so the terminfo
 * package is sufficient. Later we may need to initialise curses.
 */
	setupterm((char*)0, 1, (int*)0);	/* Initialize terminfo */
 	COLS  = columns; 			/* columns, lines - term.h */
	LINES = lines;				/* COLS and LINES - curses.h */
	keyseq[4]  = key_left;
	keyseq[5]  = key_right;
	keyseq[6]  = key_up;
	keyseq[7]  = key_down;
	keyseq[24] = key_f0;
	keyseq[25] = key_f1;
	keyseq[26] = key_f2;
	keyseq[27] = key_f3;
	keyseq[28] = key_f4;
	keyseq[29] = key_dc;

	linemode_tty = init_tty;
	linemode_tty.c_lflag &= ~(ICANON | ECHO);
        linemode_tty.c_iflag &= ~ICRNL;
	linemode_tty.c_cc[VMIN]  = 1;
	linemode_tty.c_cc[VTIME] = 0;
	tcsetattr(fileno(stdin), TCSANOW, &linemode_tty);

	curinlength = (COLS == 0 ? 80 : COLS);
	blankline[curinlength - 1] = '\0';
	screenstate = LINEMODE;
	act.sa_handler = resize_handler;
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
#ifdef SA_RESTART
	act.sa_flags |= SA_RESTART;
#endif
	sigaction(SIGWINCH,  &act, NULL);
    } else if (screenstate < 0) {	/* Interrupted input */
	if (screenstate == -LINEMODE)
	    screenstate = LINEMODE;
	else if (screenstate == -SCREENMODE) {
	    touchwin(topwin);
	    touchwin(bottomwin);
	    wrefresh(topwin);
	    wrefresh(bottomwin);
	    screenstate = SCREENMODE;
	}
    } else if (screenstate == LINEMODE) { /* Possible change to SCREENMODE */
	if (nolines <= 7) {
	    if (nolines != NOTKNOWN)
		fprintf(stderr,
		    "Insufficient screen to handle SET SCREEN %d\n", nolines);
	    return;
	}
	bwlines = nolines;
        twlines = LINES - bwlines;
	/* Change to SCREENMODE */
	if(stdscr == NOWINDOW) {
	    initscr();
        }
	cbreak();
	noecho();
	nonl();
	curinlength = INPUTBUFFERLENGTH;
	blankline[curinlength - 1] = '\0';
	if (topwin == NOWINDOW) {
	    topwin = newwin(twlines, COLS, 0, 0);
	    scrollok(topwin, FALSE);
	}
	if (bottomwin == NOWINDOW) {
            bottomwin = newwin(bwlines, COLS, twlines, 0);
	    scrollok(bottomwin, TRUE);
	    wmove(bottomwin, bwlines - 1, 0);
	}
	wrefresh(topwin);
	wrefresh(bottomwin);
	screenstate = SCREENMODE;
    } else {	/* we are in screen mode already */
	if (nolines == -1) { /* change to LINEMODE */
	    delwin(topwin);
	    topwin = NOWINDOW;
	    delwin(bottomwin);
	    bottomwin = NOWINDOW;
	    endwin();
	    setbuf(stdout, NULL);	/* Reset stdout buffering - SunOS */
	    screenstate = LINEMODE;
	    blankline[curinlength - 1] = ' ';
	    curinlength = (COLS > 80 ? 80 : COLS);
					/* one line at most in this mode */
	    blankline[curinlength - 1] = '\0';
	} else {
	    if (nolines <= 7) {
		fprintf(stderr,
		     "Insufficient screen to handle SET SCREEN %d\n", nolines);
		cbreak();
		noecho();
		nonl();
		bwlines = twlines = 0;
		return;
	    }
	    endwin();
	    blankline[curinlength - 1] = ' ';
	    curinlength = INPUTBUFFERLENGTH;
	    blankline[curinlength - 1] = '\0';
	    delwin(topwin);
	    delwin(bottomwin);
	    topwin = newwin(twlines, COLS, 0, 0);
	    scrollok(topwin, TRUE);
	    bottomwin = newwin(bwlines, COLS, twlines, 0);
	    scrollok(bottomwin, TRUE);
	    wmove(bottomwin, 0, 0);
	    wrefresh(topwin);
	    wrefresh(bottomwin);
	    screenstate = SCREENMODE;
	}
    }
/*
 * Ensure that all signal handling has remained how we expect it through
 * possible changes into or out of screen mode
 */
    act.sa_handler = cntlc_handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
#ifdef SA_RESTART
    act.sa_flags |= SA_RESTART;
#endif
    sigaction(SIGINT,  &act, NULL);	/* Ignore CTRL/C etc */
    sigaction(SIGQUIT, &act, NULL);

    act.sa_handler = cntlz_handler;
    sigaction(SIGTSTP, &act, NULL);

    act.sa_handler = sig_handler;
    sigaction(SIGHUP,   &act, NULL);
    sigaction(SIGTERM,  &act, NULL);
    sigaction(SIGBUS,   &act, NULL);

    return;
}

/******************************************************************************
 *
 *	main() - mainline code
 *
 ******************************************************************************
 */
int
main(int argc, char *argv[])
{
    char mytaskname[64];
    char message_name[MSG_NAME_LEN], ams_value[10];
    int status, message_status, message_context, ams_value_len;
    int i;
    fd_set infds;

    /* sleep(20); * Debug! */
/*
 * Initialize global variables
 */
    inputline[0] = '\0';
    for (inpl_cpos = 0; inpl_cpos < INPUTBUFFERLENGTH - 1; ++inpl_cpos)
	blankline[inpl_cpos] = ' ';
    inpl_cpos = inpl_epos = 0;
    insertmode = INSERT;	/* Insert mode */
/*
 * Initialize command recall system
 */
    initrecall();
/*
 * Initialize communications with parent ICL process.
 * While icl_io is being forked icl arranges for a read pipe to be created
 * nn fd=3 (=ICLPIPE)
 */
/*
 * Register this process with AMS using a name based on that of its ICL parent
 */
    strcpy(mytaskname, IOPROCESSNAME);
    strcat(mytaskname, argv[1]);

    status = SAI__OK;
    ams_init(mytaskname, &status);
    /* Get path to ICL */
    ams_path(argv[1], &icl_path, &status);
    message_context = OBEY;
    strcpy(message_name, "IO_INIT");
    ams_value[0] = '\0';
    ams_value_len = 1;
    /* Send OBEY command to ICL */
    ams_send(icl_path, MESSYS__MESSAGE, SAI__OK,
	     message_context, message_name, 10, ams_value,
	     &icl_messid, &status);
    /* Initial acknowledgement */
    ams_getreply(MESSYS__INFINITE, icl_path, icl_messid,
		 MSG_NAME_LEN, 10, &message_status, &message_context,
		 message_name, &i, ams_value, &status);
    if( status != SAI__OK) {
	fprintf(stderr,
		"iclio - process %s failed to initialise AMS - error code %d\n",
		mytaskname, status);
	exit(1);
    }
    initscreen(-1);
/*
 * Main processing loop - wait for either terminal input or a command from ICL
 * on the pipe and process this
 */
    sigsetjmp(goback, 0);
    setbuf(stdout, NULL);
    while (notkilled) {
        interrupted = 0;
	FD_ZERO(&infds);
	FD_SET(fileno(stdin), &infds);
	FD_SET(ICLPIPE, &infds);
        if (select(4, &infds, NULL, NULL, NULL) > 0) {
	    if( FD_ISSET(fileno(stdin),	&infds) )
		keyboard_input();
            else if( FD_ISSET(ICLPIPE, &infds) )
		command_input();
	}
	if( interrupted )
	    do_cntlz();
    }
/*
 * Process has been told to exit by ICL - Tidy up and leave
 */
    if (topwin != NOWINDOW)
	delwin(topwin);
    if (bottomwin != NOWINDOW) {
	delwin(bottomwin);
	endwin();
    }
    exit(0);    /* tty settings restored by exit handler */
}
