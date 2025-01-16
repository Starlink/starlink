/******************************************************************************
*+
*  Name:
*     icl_reada

*  Purpose:
*     Read input in response to parameter prompts

*  Language:
*     Starlink C

*  Description:
*     This is the routine used by ADAM tasks to read input in response to
*     parameter prompts when running from the shell.
*
*     Terminal input is placed in a buffer to allow for command recall and various
*     key processing options which emulate the behaviour on VMS.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1998-1999 Central Laboratory of the Research
*     Councils. Copyright (C) 2005 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     {original_author_entry}

*  History:
*     	Created	as icl_io.c : skr 26/6/92
*     	subsequent development: bkm
*      Modified to ICL_READA: ajc (1998)
*       Changes from ICL_IO version:
*         Removed message handing
*         Removed screen mode
*         Remember current inputline in recall system
*         Add filename completion and list options
*         Use tcsh edit keys.
*         Default action on CNTL/Z (suspend) and CNTL/C(abort)
*      Modified: ajc 2/6/98
*         Recover if task suspended when not expecting input.
*         Restore initial tty state on return from ICL_READA
*         Handle environment variables in filename completion
*      Modified: bkm 5/8/98
*         Correct logic to cope with case when stdin is NOT a tty
*      Modified bkm 28/10/98
*         Don't echo the artifical newline used to pick up typeahead
*      Modified ajc 16/2/99
*         Correctly cyclicly decrement inbuf_rpos to forget type-ahead
*         forced \n.
*      Modified ajc  5/3/99
*         Allow lines up to 256 characters and correct editing for long
*         lines. Works with xterm and dumb terms and various between.
*      Modified timj 28/3/05
*         Tweak ncurses discovery.
*         Fix compiler warnings
*      Modified pwd 6/9/05
*         Add extra 0's to tparm calls. These are part of the official
*         prototype.
*      Modified timj 29/12/05
*         Use DAT__FLEXT rather than hard coded ".sdf"
*      Modified pwd 23/01/09
*         Fix problem in keyboard_input introduced by last changes.
*      Modified gsb 14/01/25
*         Remove use of TIOCSTI (not available by default since Linux 6.2).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
 */
/* System includes */

/* Autoconf output */
#if HAVE_CONFIG_H
# include <config.h>
#endif

#if HAVE_CURSES_H
# include <curses.h>
#elif HAVE_NCURSES_H
# include <ncurses.h>
#elif HAVE_CURSESX_H
# include <cursesX.h>
#else
# error "Unable to locate curses installation"
#endif

#include <stdlib.h>
#if HAVE_TERM_H
#include <term.h>
#elif HAVE_NCURSES_TERM_H
#include <ncurses/term.h>
#endif
#include <ctype.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <sys/time.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <setjmp.h>
#include <glob.h>
#include <sys/types.h>
#include <sys/wait.h>
/* Own include files */
#include "f77.h"
#include "dat_par.h"

#define INPUTBUFFERLENGTH 256
#define INPUTLINELENGTH 256
#define IOBUFFERLENGTH 256
#ifndef TRUE
#define TRUE    1
#define FALSE   0
#endif

#define CHARNIL      ((char *)0)

/* forward reference function declarations */
void initscreen(int);
char *shell_command( char* );
void initrecall(void);
void addtorecall(char *line);
char *prevline(void);
char *nextline(void);
int codespecialkey(void);
int noisechar(char ch);
int spacechar(char ch);
int normalcharacter(char ch);
void ringbell(void);
void posbol( int del, int prlen, int cpos, int epos );
void redoline(int complete, int prlen, int cpos, int epos, int forwd);
void filename_complete(void);
void keyboard_input(void);
void resize_handler(int signo);
void cntlc_handler(int signo);
void cont_handler(int signo);
void exit_handler(void);
void sig_handler(int signo);

/*
 * Global variables
 */

/* tty state information - uses SYSV/POSIX termio interface */

struct termios init_tty,
               init_ttout,
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
    saveinput[INPUTLINELENGTH+1], /* Save input when recalling lines */
    blankline[INPUTLINELENGTH+1]; /* blank line */
char *saveinput_p=CHARNIL;     /* pointer to saveinput or NULL */

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
    prompt_len = 0;             /*length of prompt string */

/* Buffer processing macros */
#define incinbuf_rpos() if(++inbuf_rpos == INPUTBUFFERLENGTH) inbuf_rpos = 0;
#define decinbuf_rpos() if(--inbuf_rpos < 0) inbuf_rpos = INPUTBUFFERLENGTH-1;
#define incinbuf_cpos() if(++inbuf_cpos == INPUTBUFFERLENGTH) inbuf_cpos = 0;
#define emptyinbuff() (inbuf_rpos == inbuf_cpos)

/* icl_io control variables */
int
    notkilled         = TRUE,
    expectinginput    = FALSE,
    gotline           = FALSE;


/* state for longjump */
sigjmp_buf	goback;

/* flag for interrupt */
int interrupted;

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
    int len;

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

const char *keyseq[NUMKEYS+4] =
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
    const char *w;
    char wch;
    int i=0, wi;

    wi = inbuf_cpos;
/* Match the current input against the key sequences in 'keys' */
    while ( i < NUMKEYS ) {
        if ((w = keyseq[i]) == CHARNIL) {
	    i++;
	} else {
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
	    if (wch == '\0') {
		return (keycodes[i]);	/* returns positive keycode value */
	    } else {
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
    if (ch == ' ' || ch == '\t')
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
/*    if (flash_screen != CHARNIL)
	putp(flash_screen);
    else
*/	putchar((int) '\007');
    return;
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
 *	F I L E N A M E _ C O M P L E T E ( )
 *
 * This routine is called to complete a filename in the keyboard input routine
 * It assumes the inputline is set up as normal with inpl_cpos pointing at
 * the current character and inpl_epos pointing at the end.
 ******************************************************************************
 */
void filename_complete(void) {
    size_t expanded_len;
    int status;
    char combuff[INPUTLINELENGTH];
    char tbuff[INPUTLINELENGTH];
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
                  printf("\nMultiple matches.\n");
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
                     extns[0] = DAT__FLEXT;
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
               printf("\nNo match.\n");
               ringbell();
            }

         } else {
/*               Failed getting filenames */
#ifdef GLOB_NOMATCH
            if ( status == GLOB_NOMATCH ) printf("\nNo match.\n");
#else
            /* If GLOB_NOMATCH is not defined assume that we get
               good status even when match count is zero. This
               branch then only hits if no matches and status is good.
             */
            if ( status == 0 && filelist.gl_pathc == 0)
                  printf("\nNo match.\n");
#endif
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
    int readret, inputavailable, worker, worker1;
    char readbuff[INPUTBUFFERLENGTH+1], *w;
    char combuff[INPUTLINELENGTH];
    char *result;
    fd_set infds;
    struct timeval time;
    int i;
    int save_cpos, save_epos;
/*
 * Source WAS stdin
 */
    time.tv_sec  = 0;
    time.tv_usec = 0;
    for(;;) {
        FD_ZERO(&infds);
        FD_SET(fileno(stdin), &infds);
        if(!select(2, &infds, NULL, NULL, &time))       /* non-blocking */
            break;
        if(FD_ISSET(fileno(stdin), &infds)) {

            if (inbuf_rpos >= inbuf_cpos) {
                readret =
                    read( fileno(stdin),
                          readbuff,
                          INPUTBUFFERLENGTH - (inbuf_rpos - inbuf_cpos) );
            }
            else {
                readret =
                    read( fileno(stdin),
                          readbuff,
                          inbuf_cpos - inbuf_rpos - 2 );
            }
            w = readbuff;
	    while (readret > 0) {
		    inbuff[inbuf_rpos] = *w;
		    incinbuf_rpos();
		w++;
		readret--;
	    } /* while readret */
	}
	inbuff[inbuf_rpos] = '\0';
    } /* for */

    if (!expectinginput) {
	return;
    }

    inputavailable = (inbuf_rpos != inbuf_cpos);
    while (inputavailable) {
       if (normalcharacter(inbuff[inbuf_cpos])) {
          if (inpl_epos == INPUTLINELENGTH) {
/*            Line full */
             ringbell();
          } else {
             if ( inpl_cpos == inpl_epos) {
             /*  Inserting at end of line  */
                inputline[inpl_cpos++] = inbuff[inbuf_cpos];
                inputline[inpl_cpos] = '\0';
                inpl_epos = inpl_cpos;
                putchar((int) inbuff[inbuf_cpos]);
                if ( !((prompt_len + inpl_epos) % COLS) ) {
                   putchar('\r');
                   putp(cursor_down);
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
                posbol( 1, prompt_len, save_cpos, save_epos );
                redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
             }
          }
          incinbuf_cpos();

       } else {
          /* not normalcharacter */
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
                printf("\n");
		if (inputline[0] != '\0') /* Don't store blank lines */
		    addtorecall(inputline);
                saveinput_p = CHARNIL;
     		expectinginput = FALSE;
		inpl_cpos = inpl_epos = 0;
                gotline = TRUE;
                return;
		break;

	      case Kleft:
		if( inpl_cpos > 0) {
                    if ( cursor_left != CHARNIL &&
                        cursor_up != CHARNIL) {
                       inpl_cpos--;
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
                    if ( cursor_right != CHARNIL &&
                        cursor_down != CHARNIL) {
                       inpl_cpos++;
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
			if (w != CHARNIL)
			    strcpy(inputline, w);
			else
			    inputline[0] = '\0';
                        posbol( 1, prompt_len, inpl_cpos, inpl_epos );
			inpl_cpos = inpl_epos = strlen(inputline);
                        printf( "%s%s", prompt, inputline );
                }
		break;

	      case Kdown:
		if ((w = nextline()) == CHARNIL) {
                    if (saveinput_p != CHARNIL) {
                       strcpy(inputline,saveinput);
                       saveinput_p = CHARNIL;
                    } else
                       ringbell();
		} else {
		    strcpy(inputline, w);
		}
                posbol( 1, prompt_len, inpl_cpos, inpl_epos );
 	        inpl_cpos = inpl_epos = strlen(inputline);
                printf( "%s%s", prompt, inputline );
		break;

	      case Kredisp:
                    posbol( 1, prompt_len, inpl_cpos, inpl_epos );
                    if ( cursor_up != CHARNIL )
                       redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
                    else {
                       inpl_cpos = inpl_epos;
                       printf( "%s%s", prompt, inputline );
                    }
		break;

	      case Ktab:
/*            If nothing on line, get suggested value - otherwise
 *            attempt filename completion */
                if ( inpl_epos != 0 ) {
/*            Something already typed - assume filename completion required */
                   filename_complete( );
                   posbol( 0, prompt_len, inpl_cpos, inpl_epos );
                   redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
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
                  posbol( 1, prompt_len, inpl_cpos, inpl_epos );
  		  inpl_epos = inpl_cpos;
		  inputline[inpl_epos] = '\0';
                  redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
		break;

	      case Kclearall:
                posbol( 1, prompt_len, inpl_cpos, inpl_epos );
		inputline[0] = '\0';
		inpl_epos = inpl_cpos = 0;
                redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
		inbuff[0] = '\0';
		inbuf_rpos = inbuf_cpos = 0;
		break;

	      case Kcleartobol:
                save_cpos = inpl_cpos;
                save_epos = inpl_epos;
                posbol( 1, prompt_len, inpl_cpos, inpl_epos );
		if (inpl_cpos == inpl_epos) {
		    inpl_epos = 0;
		    inputline[0] = '\0';
		} else {
		    strcpy(inputline, &inputline[inpl_cpos]);
		    inpl_epos = strlen(inputline);
		}
		inpl_cpos = 0;
                redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
		break;

	      case Ktosol:
                posbol( cursor_up==CHARNIL, prompt_len, inpl_cpos, inpl_epos );
		inpl_cpos = 0;
                redoline( cursor_up==CHARNIL, prompt_len, inpl_cpos, inpl_epos,
                1 );
		break;

	      case Ktoeol:
                posbol( cursor_up==CHARNIL, prompt_len, inpl_cpos, inpl_epos );
		inpl_cpos = inpl_epos;
                redoline( cursor_up==CHARNIL, prompt_len, inpl_cpos, inpl_epos,
                0 );
		break;

              case Kdel:
		if (inpl_cpos != inpl_epos) {
		    worker1 = inpl_cpos;
                    worker = worker1 + 1;

		    while (worker <= inpl_epos)
			inputline[worker1++] = inputline[worker++];
                    posbol( 1, prompt_len, inpl_cpos, inpl_epos );
		    inpl_epos--;
                    redoline( 1, prompt_len, inpl_cpos, inpl_epos, 0 );
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
                 printf("\n%s", result );
                 free( result );
                 posbol( 1, prompt_len, inpl_cpos, inpl_epos );
                 redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
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
                posbol( 1, prompt_len, save_cpos, save_epos );
                redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
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
                     posbol( 1, prompt_len, save_cpos, save_epos );
                     redoline( 1, prompt_len, inpl_cpos, inpl_epos, 0 );
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
                posbol( 1, prompt_len, inpl_cpos, inpl_epos );
		inpl_cpos = worker;
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

                   posbol( 1, prompt_len, inpl_cpos, inpl_epos );
	           inpl_cpos = worker1;
                   redoline( 1, prompt_len, inpl_cpos, inpl_epos, 0 );
                 }
                 break;


	      default:
		break;

	    } /* switch on special keys */

	} /* else not a normal character */

	inputavailable = (inbuf_rpos != inbuf_cpos);

    } /* of while inputavailable */

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
    posbol( 1, prompt_len, inpl_cpos, inpl_epos );
    COLS  = size.ws_col;
    LINES = size.ws_row;
    redoline(1, prompt_len, inpl_cpos, inpl_epos, 1 );
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

    inputline[0] = '\0';
    inpl_epos = inpl_cpos = 0;
    inbuf_rpos = inbuf_cpos = 0;
    inbuff[0] = '\0';
    fprintf(stderr,"*** Interrupt ***\n");
    exit(1);
/*
 * Get back to the icl_io idle state (ie waiting for either a keyboard key
 * or a command from ICL. This involves unblocking the SIGINT signal and
 * performing a stack unwind (using longjmp) after some tidying up.
 */
    if (expectinginput)
	expectinginput = 0;
    sigemptyset(&mask);
    sigaddset(&mask, SIGINT);
    sigprocmask(SIG_UNBLOCK, &mask, NULL);
    siglongjmp(goback, 0);
}

/******************************************************************************
 *
 *	C O N T _ H A N D L E R ( int signo )
 *
 * We arrange for this handler to be called if the process gets a SIGCONT
 * signal. If expecting input, the keyboard is re-initialised to
 * single character mode and the prompt and any input so far re-typed.
 *
 ******************************************************************************
 */
void
cont_handler(int signo)
{
   if( expectinginput ) {
      expectinginput = FALSE;
      initscreen(1);
      redoline( 1, prompt_len, inpl_cpos, inpl_epos, 1 );
      expectinginput = TRUE;
   }
}

/******************************************************************************
 *
 *	E X I T _ H A N D L E R ( )
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

/******************************************************************************
 *
 *	I N I T S C R E E N (int nolines)
 *
 * This routine handles the ICL output screen modes and changes between these.
 *
 * Initially ICL starts in screen state 'INITIAL' and we arrange to move from
 * this to 'LINEMODE'.
 *
 ******************************************************************************
 */
void
initscreen(int nolines)
{
    struct sigaction act;

/*
 * Save the terminal settings we are started with so we can restore them
 * both on process exit and to run a Unix external command. For process exit
 * we also setup an exit handler
 */
	tcgetattr(fileno(stdin), &init_tty);
	if (!(init_tty.c_lflag & ICANON)) /* ensure restore to standard state */
	    init_tty.c_lflag |= (ICANON | ECHO | ISIG);
/*
 * icl-io uses curses/terminfo. Initially we are in LINEMODE so the terminfo
 * package is sufficient. Later we may need to initialise curses.
 */
	setupterm((char*)0, 1, (int*)0);	/* Initialize terminfo */
 	COLS  = columns; 			/* columns, lines - term.h */
	LINES = lines;				/* COLS and LINES - curses.h */
	keyseq[4]  = key_up;
	keyseq[5]  = key_down;
	keyseq[6]  = key_right;
	keyseq[7]  = key_left;
	keyseq[24]  = key_f0;
	keyseq[25]  = key_f1;
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
        putp(enter_am_mode);

	act.sa_handler = resize_handler;
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
#ifdef SA_RESTART
	act.sa_flags |= SA_RESTART;
#endif
	sigaction(SIGWINCH,  &act, NULL);
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
    act.sa_handler = cont_handler;
    sigaction(SIGCONT,  &act, NULL);
    act.sa_handler = sig_handler;
    sigaction(SIGHUP,   &act, NULL);
    sigaction(SIGTERM,  &act, NULL);
    sigaction(SIGBUS,   &act, NULL);
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
	    perror( "shell_command() pipe error");
	    return NULL;
	} else {
	    if ( (pid = fork()) < 0) {
		perror( "shell_command() fork error");
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

                result = (char *)malloc(INPUTLINELENGTH);
                resultsize = INPUTLINELENGTH;
                resp = 0;
                nchars = 1;
                while ( nchars>0 ) {
                   if ( result != NULL ) {
                      if ( resp == resultsize-1 ) {
                         resultsize+=INPUTLINELENGTH;
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
 *	ICL_READA - Fortran callable subroutine
 *
 ******************************************************************************
 */

F77_SUBROUTINE(icl_reada)( CHARACTER(fpr1), INTEGER(len1),
                           CHARACTER(fpr2), INTEGER(len2),
                           CHARACTER(fval), INTEGER(vallen),
                           CHARACTER(fdflt), INTEGER(deflen)
                           TRAIL(fpr1) TRAIL(fpr2) TRAIL(fval) ) {

    fd_set infds;
    /* sleep(20); * Debug! */

/* Deal first with the case where stdin is not connected to a tty.
 * This is so trivial it is performed in a block which contains a return
 * to the SUBPAR level.
 */
    if (!isatty(STDIN_FILENO)) {
        cnf_impb(fpr1, *len1, prompt );
        cnf_imprt(fdflt, *deflen, dvalue );

/* Issue prompt */
        fflush(stdout);   /* To keep in step with FORTRAN i/o */
        printf("%s",prompt);
        if( fgets(inputline, sizeof(inputline), stdin) == NULL ) {

/* Unexpected EOF in parameter input - this is a serious error */
            printf("\n");
            fflush(stdout);
            fprintf(stderr,"Fatal EOF in parameter input (ICL_READA)\n");
            exit(4);    /* We just want to abort the whole task here */
        }

/* Note: A difference from previous ICL_READA's that the parameter input
 * is echoed
 */
        printf("%s",inputline);
        fflush(stdout);

/* SUBPAR cannot cope with the newline on the end of the line left by fgets */
        inputline[strlen(inputline)-1] = '\0';
        cnf_exprt(inputline, fval, *vallen);

        return;
    }
/* End of non-tty stdio handling - remaining code deals with tty input */

/*
 * Initialize command recall system
 */
    if (recall>SAVELINES) {
        initrecall();
/*
 * Initialize blank line
 */
       for (inpl_cpos = 0; inpl_cpos < INPUTLINELENGTH; ++inpl_cpos)
      	  blankline[inpl_cpos] = ' ';
       blankline[inpl_cpos] = '\0';

/* Set exit handler to restore terminal state */
#if HAVE_ATEXIT
	atexit(exit_handler);		       /* ANSI C/POSIX */
#elif HAVE_ON_EXIT
	on_exit(exit_handler, 0);		/* SunOS	*/
#else
# error "Do not know how to register an exit handler"
#endif
    }
    initscreen(-1);
/*
 * Set up for new input
 */
    inputline[0] = '\0';
    inpl_cpos=inpl_epos = 0;
    expectinginput = TRUE;
    recall_p = recall;
    inc_recall_p();
/*
 * Main processing loop - wait for either terminal input or a command from ICL
 * on the pipe and process this
 */
    sigsetjmp(goback, 0);
    setbuf(stdout, NULL);
    cnf_impb(fpr1, *len1, prompt );
    cnf_imprt(fdflt, *deflen, dvalue );

/* Set prompt length */
    prompt_len = strlen( prompt );

/* Issue prompt */
    printf("%s",prompt);

/* Process any type-ahead */
    gotline = FALSE;
    if (inbuf_cpos != inbuf_rpos)
       keyboard_input();

/* If not complete line type-ahead, loop getting more */
    while (!gotline) {
       interrupted = 0;
       FD_ZERO(&infds);
       FD_SET(fileno(stdin), &infds);
       if ( select(3,&infds, NULL, NULL, NULL ) > 0 )
          if( FD_ISSET(fileno(stdin), &infds) )
             keyboard_input();
    }

/* A line has been got - */
/* Restore original tty state, export line to Fortran and return */
    tcsetattr(fileno(stdin), TCSANOW, &init_tty);
    cnf_exprt(inputline, fval, *vallen);
    return;
}
