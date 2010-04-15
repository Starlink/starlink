/*
 * gk0xrc_.c --- Module for handling a choice tool on the Xlib workstation for
 * RAL GKS.
 *
 * Written by: A C Arnold, University of Manchester Computer Graphics Unit,
 * Oxford Road, Manchester M13 9PL Tel: 061-273 7121 x 5405
 *
 * Maintenance Log:
 *
 * 26/02/87  PJWR  Corrected #includes for integration.
 * 18/03/87  TAW   Changed name to gk9src_.
 * 06/05/87  PJWR  Corrected to use GKS drawing area of
 *                 bitmap rather than the entire bitmap.
 * 14/08/87  PJWR  Corrected number of integers in call to gkrqip_.
 *                 This was omitted during IS conversion.
 * 23/09/87  ACA   Modified to be table driven in preparation for many PETs
 * 30/09/87  ACA   Added code for PETs 2 & 3.
 * 24/11/87  ACA   Added code to support PET 4
 * 27/11/87  ACA   Dynamically allocate space for choice boxes etc.
 * 01/12/87  ACA   Fixed bug in displaying light box. 2nd row was wrong.
 * 26/01/88  TAW   Changed PET -1 code to return values from 1 instead
 *                 of from 32 (ascii values)
 * 20/09/88 TAW    Changed to use with Xlib workstation.
 * 21/09/88  TAW   Changed choiceecb to xchoiceecb.
 */
#include <wwinfo.h>
#include <stdio.h>
#include <search.h>
#include <string.h>
#include <ctype.h>

#include "../../system/include/gks.h"
#include "../../system/include/gkinp.h"
#include "../../system/include/f77_type.h"
#include "../../system/include/gkdt.h"
#include "../../system/include/gkwca.h"
#include "../../system/include/gkwdt.h"
#include "../../system/include/gkhp.h"

#include "gk0xwd.h"

#define NROWS 2
#define NCOLS 5
#define NBOXES (NCOLS*NROWS)
#define MAXCHOICELEN 80

#define CR '\r'
#define DEL '\177'
#define CTRLU '\025'
#define NUL '\000'
/*
 * Here are some macros to help with detecting when an input request has been
 * satisfied.
 */
#define MOUSETRIGGER (dd->d_event == IPOTHER && dd->d_buttons != 0)
#define KEYTRIGGER (dd->d_event == IPKEY && dd->d_char == CR)
#define TRIGGER (MOUSETRIGGER || KEYTRIGGER)
#define VALIDCHOICE (boxno != -1)
#define BREAK (dd->d_event == IPKEY && dd->d_char == break_char)

char *calloc();
void cfree();

static int
  buttonsopen1(), buttons(), buttonsclose1(),
  buttonsopen2(),
  menuopen(), menuecho(), menuerase(), menuclose(),
  stringopen(), string(), stringclose(),
  keyopen(), key(), keyclose(),
  segopen(), seg(), segclose(),
  noaction();

#define MINPET -1

struct ecb xchoiceecb[] = {
	keyopen, noaction, key, keyclose,	/* PET -1 */
	noaction,noaction,noaction,noaction,	/* PET 0 (never used) */
	buttonsopen1, noaction, buttons, buttonsclose1,	/* PET 1 */
	buttonsopen2, noaction, buttons, buttonsclose1,	/* PET 2 */
	menuopen, menuerase, menuecho, menuclose,	/* PET 3 */
	stringopen, noaction, string, stringclose,	/* PET 4 */
	segopen, noaction, seg, segclose	/* PET 5 */
  };


static int
  i, j, boxno, oldboxno = -1;

static f77_integer
  inta[10],					/* Integer data for i/p */
  nint = 6,					/* Number of ints to get */
  nreal = 4,					/* Number of reals to get */
  ipcls = GCHOIC;				/* Input class */
static f77_real
  reala[10];					/* Array of reals */
static box
  echoarea,					/* Box for echo area */
  *choiceboxes;					/* Pointer to choice boxes */
char *sprintf();				/* Stdio */

gk0xrc_(choice)
     f77_integer *choice;
{

	gkrqip_(&ipcls, &gkywca_.kwi1,		/* Get i/p data */
		&nint, &nreal, &inta[1], &reala[1]);
	echoarea = gk0xboxbuild(
			    (int) reala[KIPEXL], /* Construct echo box */
			    (int) (gkywdt_.kdsry[gkywca_.kwkix - 1] - 1) - (int) reala[KIPEYT],
			    (int) reala[KIPEXR],
			    (int) (gkywdt_.kdsry[gkywca_.kwkix - 1] - 1) - (int) reala[KIPEYB]);
	(*xchoiceecb[inta[KIPPRT] - MINPET].open) ();	/* Open up echo area */
	gk0xipset(IPON);				/* Enable input */
	do {
		(*xchoiceecb[inta[KIPPRT] - MINPET].echo) ();	/* Echo current i/p */
		gk0xipwait();			/* Wait for input */
		(*xchoiceecb[inta[KIPPRT] - MINPET].erase) (); /* Erase the echo */
	} while (!((VALIDCHOICE && TRIGGER) || BREAK));
	gk0xbu();
	gk0xipset(IPOFF);				/* Disable input */
	*choice = (*xchoiceecb[inta[KIPPRT] - MINPET].close) ();

	if (dd->d_event == IPENTER)		/* Implies another window
						 * selected */
	  return (GNCHOI);
	else if (BREAK)				/* Implies a key (^Z) was
						 * pressed */
	  return (GNONE);
	else
	  return (GOK);				/* Otherwis return OK status */
}

/****************************************************************************/

static int
buttonsopen1()
{
	int width, height, left, top;

	/* Compute width and height */

	width = (echoarea.b_right -
		 echoarea.b_left - 2) / NCOLS;	/* Width of each box */
	height = (echoarea.b_bottom -
		  echoarea.b_top - 2) / NROWS;	/* Height of same */

	/* Round echo area to fit exactly around choice boxes */

	echoarea.b_right = echoarea.b_left +
	  width * NCOLS + 1;			/* Round echo area */
	echoarea.b_bottom = echoarea.b_top +
	  height * NROWS + 1;			/* round bottom */

	gk0xoe(echoarea, "Choice device");	/* Open the echo area */
	left = 1;				/* Start at left side +1 */
	top = 1;				/* And at "top" +1 */
	oldboxno = -1;				/* Forget old box */

        choiceboxes = (box *)calloc((unsigned)NBOXES, sizeof(box));/* Allocate space for boxes */
	boxno = 0;
	for (i = 0; i < NROWS; i++) {		/* Compute all the boxes */
		for (j = 0; j < NCOLS; j++) {
			choiceboxes[boxno++] = gk0xboxbuild(left, top, left + width - 1, top + height - 1);
			left += width;
		};
		top += height;
		left = 1;
	};

	for (i = 0; i < NBOXES; i++) {		/* Draw all the boxes */
		char label[5];
		gk0xbmbox(choiceboxes[i], BMEDGES | BMCLEAR); /* Draw edges of box */
		(void) sprintf(label, "%d", i + 1); /* Construct the label */
		gk0xftprint(choiceboxes[i], label, FTCENTRE);	/* and print it */
	}
}

static int
buttons()
{
	boxno = -1;				/* assume outside */
	if (dd->d_event != IPLEAVE &&
	    dd->d_event != IPENTER) {		/* Ignore outside box */
		for (boxno = 0; (boxno < NBOXES) &&
		     !gk0xboxinside(choiceboxes[boxno], dd->d_x, dd->d_y);
		     boxno++);
		if (boxno != NBOXES && boxno != oldboxno) {
			if (oldboxno != -1)	/* Switch off old box */
			  gk0xbmbox(choiceboxes[oldboxno], BMNOT);
			gk0xbmbox(choiceboxes[boxno], BMNOT); /* Switch on new box */
			oldboxno = boxno;	/* Note for next time */
		}
	} else if (oldboxno != -1) {		/* Switch off box */
		gk0xbmbox(choiceboxes[oldboxno], BMNOT);
		oldboxno = -1;
	}
}

static int
  buttonsclose1()
{
	cfree((char *)choiceboxes);		/* Free the space used */
	gk0xce();				/* Close the echo area */
	return (boxno + 1);			/* Return choice number */
}

/****************************************************************************/

static int
  buttonsopen2()
{						/* PET 2 */
	f77_integer kints = KINTGS, nints, nreals,
	prompts[11], id[2], hpoffzero = 0;
	f77_real rdummy[1];

	buttonsopen1();				/* Open as for PET 1 */
	nints = 2;
	nreals = 0;
	gkdrge_(&inta[KIPD], &kints, &nints, &nreals,
		id, rdummy);			/* Get pointer to prompts */
	gkhpgi_(&id[1], &hpoffzero, &id[0], prompts); /* Get prompt info */
	for (i = 1; i <= prompts[0]; i++) {
		if (prompts[i] == GYES) {
			gk0xbmbox(choiceboxes[i - 1], BMNOT);
		}
	}
}

/****************************************************************/
/* This section contains routines for handling PET 3 the menu   */
/* type choice device.                                          */
/*								*/
/* Declare globals for menu handling stuff                      */
/****************************************************************/

static int nitems;				/* Number of items */

static int
  menuopen()
{						/* PET 3 (Menus) */
	int chht, chwd, longest, maxlen;
	box chbox;
	char **itemchars;
	f77_real rdummy[1];
	f77_integer kchars = KCHARS, nints, nreals,
	hpoff, hpoffzero = 0,
	idrent[2], id[2], chint[MAXCHOICELEN];

	nints = 2;
	nreals = 0;
	gkdrge_(&inta[KIPD], &kchars, &nints, &nreals,
		idrent, rdummy);		/* Get length and address of
						 * char descriptors */

	nitems = idrent[0];			/* Initialise nitems */
	/* Now allocate space for the menu item string table */

	itemchars = (char **)calloc((unsigned)nitems, sizeof(char *));

	/* Build the item string table from the heap data recording longest */

	longest = maxlen = 0;
	for (i = 0; i < nitems; i++) {
		hpoff = 2 * i;
		nints = 2;
		gkhpgi_(&idrent[1], &hpoff,
			&nints, &id[0]);	/* get item descriptor */
		gkhpgi_(&id[1], &hpoffzero,
			&id[0], chint);		/* Get string as ints */
		if(id[0] > maxlen) {		/* Check string length */
		  longest = i;
		  maxlen = id[0];
		}
						/* Get space for C string */
		itemchars[i] = calloc((unsigned)(id[0] + 1), sizeof(char));
		gkaton_(&id[0], chint,		/* Convert to ASCII */
			itemchars[i], (int)id[0]+1);
		itemchars[i][id[0]] = 0;	/* Ensure it's terminated */
	}

	/* Find the length and height of the longest string */

	chbox = gk0xftbox(gk0xftload(NULLPTR(char)),itemchars[longest],maxlen,FTPROP);

	/* Obtain the required with and height (fudged to look good) */

	chht = chbox.b_bottom - chbox.b_top + 6;
	chwd = chbox.b_right - chbox.b_left + 11;

	/* Massage the echo area to fit */

	echoarea.b_right = echoarea.b_left + chwd - 1;
	echoarea.b_bottom = echoarea.b_top + chht * nitems - 1;

	gk0xoe(echoarea, "Choice device");	/* Open the echo area */

	/* Here we create the boxes for all the menu items */

	choiceboxes=(box *)calloc((unsigned)nitems,sizeof(box));
	choiceboxes[0] = gk0xboxbuild(0, 0, chwd, chht);
	for (i = 1; i < nitems; i++)
	  choiceboxes[i] = gk0xboxshift(choiceboxes[i - 1], 0, chht);

	/* Initialise globals so proper echoing works */

	oldboxno = -1;

	/* Draw the outline of each box and plant the text in it */

	for (i = 0; i < nitems; i++) {
		gk0xbmbox(choiceboxes[i],
		      BMEDGES | BMCLEAR);	/* Clear inside of window */
		gk0xftprint(choiceboxes[i], itemchars[i],
			FTHORIZ | FTVERT | FTNOT);
	}

	/* Free C string workspace */

	for (i = 0; i < nitems; i++)
		cfree(itemchars[i]);
	cfree((char *)itemchars);
}

static int
  menuecho()
{
}

static int
  menuerase()
{
	if (dd->d_event != IPLEAVE) {		/* Ignore outside box */
		for (boxno = 0; (boxno < nitems) && /* Inside an item ? */
		     !gk0xboxinside(choiceboxes[boxno], dd->d_x, dd->d_y);
		     boxno++);
		if (boxno != nitems && boxno != oldboxno) {
			if (oldboxno != -1)	/* Switch off old box */
			  gk0xbmbox(choiceboxes[oldboxno], BMNOT);
			gk0xbmbox(choiceboxes[boxno], BMNOT);	/* Switch on new box */
			oldboxno = boxno;	/* Note for next time */
		}
	} else if (oldboxno != -1) {		/* Switch off box */
		gk0xbmbox(choiceboxes[oldboxno], BMNOT);
		oldboxno = -1;
	}
}

static int
  menuclose()
{
	cfree((char *)choiceboxes);		/* Free the space */
	gk0xce();				/* Close the echo area */
	return (boxno + 1);
}

/*****************************************************************************/
static void pos(ep,x,y)
     emupane *ep;
     int x,y;
{
	char escape[4];

	escape[0]=27;
	escape[1]='P';
	escape[2]=x;
	escape[3]=y;
	gk0xemuprint(ep,escape,4);
}

/*
 * emugets - gets a string from the terminal emulator specified by ep. s is
 *           first of all printed and then any characters typed may be
 *           appended to it or edited using DEL or control-U. Input is
 *           terminated either by a carriage-return, or the break character
 *           or if n-1 chars are typed. The interface is as close to gets
 *           as makes sense. The function result is the terminating character.
 *
 *           Undefined things may happen if s is not initialised sensibly.
 */
static int emugets(s,n,ep)
     char *s;
     int n;
     emupane *ep;
{
	int stop;
	char key,keyecho[4];

	stop = FALSE;
	while (!stop) {				/* Loop processing input */
		gk0xipwait();			/* Get some input */
		if (dd->d_event == IPKEY) {	/* Ignore all but keyboard */
			key = dd->d_char;	/* Get the key hit */
			keyecho[0]=NUL;
			if (key == break_char)	/* Hit break ? */
			  stop = TRUE;
			else if (key == CR)	/* Process CR */
			  stop = TRUE;
			else if (key == DEL)
			  if (strlen(s) > 0){
				(void)strcpy(keyecho, "\b\030g"); /* Delete prev char */
				s[strlen(s)-1]=0;
			} else{
			}
			else if (key == CTRLU) {
				(void)strcpy(keyecho, "\b\030g"); /* Delete prev char */
				for (i=strlen(s);i>0;--i) /* Delete chars */
				  gk0xemuprint(ep, keyecho,
					   strlen(keyecho));
				s[0] = keyecho[0] = 0;
			} else if (strlen(s) < n) {
				keyecho[0] = key;
				keyecho[1] = 0;
				(void)strcat(s,keyecho);
			} else
			  keyecho[0] = 0;
			gk0xemuprint(ep, keyecho,
				 strlen(keyecho)); /* echo the character */
		};
	};
	return(key);
}

static emupane *ep;

typedef char itemstring[MAXCHOICELEN];

static itemstring *itemtable;			/* Table of choice strings */
static unsigned itemnelp;			/* Number of choices */

static int str_cb_cmp(s1,s2)
     char *s1,*s2;
     /* Perform a case blind compare of the two strings s1 and s2. If the two
	strings are of different lengths it effectively truncates the longer
	one before comparing
	*/
{
	int i=0;
#define LOWER(ch) (isupper(ch)?tolower(ch):ch)
	while (LOWER(s1[i]) == LOWER(s2[i])){
		if (s1[i] == 0)
		  return(0);
		i++;};
	return(s1[i] - s2[i]);
}
static int
  stringopen()
{
	char *lsearch();
	itemstring itemchars;
	box chbox, ptbox;
	int chwd, chht, longest, maxlen;
	f77_real rdummy[1];
	f77_integer kchars = KCHARS, nints, nreals,
	hpoff, hpoffzero = 0, idrent[2], id[2],
	chint[MAXCHOICELEN];

	/* Now get and display items */
	nints = 2;
	nreals = 0;
	gkdrge_(&inta[KIPD], &kchars, &nints, &nreals,
		idrent, rdummy);		/* Get length and address of
						 * char descriptors */
	nitems = idrent[0];
	itemtable=(itemstring *)calloc((unsigned)nitems,sizeof(itemstring));
	itemnelp=0;				/* Empty the table */

	/* Load the table of items,  recording the longest string */

	longest = maxlen = 0;
	for (i = 0; i < nitems; i++) {
		hpoff = 2 * i;
		nints = 2;
		gkhpgi_(&idrent[1], &hpoff,
			&nints, &id[0]);	/* get item descriptor */
		gkhpgi_(&id[1], &hpoffzero,
			&id[0], chint);		/* Get string as ints */
		if(id[0] > maxlen) {		/* Check string length */
		  longest = i;
		  maxlen = id[0];
		}
		gkaton_(&id[0], chint,
			itemchars, MAXCHOICELEN);	/* Convert to ASCII */
		itemchars[id[0]] = 0;		/* Ensure it's terminated */
		(void)lsearch(itemchars,(char *)itemtable,
			      &itemnelp,MAXCHOICELEN,str_cb_cmp);
	}

	/* Find the width and height of the longest string */

	chbox = gk0xftbox(gk0xftload(NULLPTR(char)),itemtable[longest],maxlen,FTPROP);

	/* Must allow for prompt in width and CR at end of line */

	ptbox = gk0xftbox(gk0xftload(NULLPTR(char)),"MMMMMMMMM",9,FTPROP);

	/* Obtain the required with and height */

	chht = chbox.b_bottom - chbox.b_top + 2;
	chwd = chbox.b_right - chbox.b_left + ptbox.b_right - ptbox.b_left + 2;

	/* Massage the echo area to fit */

	echoarea.b_right = echoarea.b_left + chwd - 1;
	echoarea.b_bottom = echoarea.b_top + chht * (nitems + 2) - 1;

	gk0xoe(echoarea, "Choice device");	/* Open the echo area */
	ep=gk0xemucreate(ddbm->bm_box);		/* create terminal emulator */

	/* Output each item to the terminal emulator we've created */

	for (i = 0; i < nitems; i++) {
		gk0xemuprint(ep,itemtable[i],strlen(itemtable[i]));
		pos(ep,0,ep->p_y+1);
	}
}

static int
  string()
{
	char str[MAXCHOICELEN],*ptr,*lfind();

	pos(ep,0,ep->p_ymax);			/* Position on bottom line */
	gk0xemuprint(ep,"\033K",2);			/* Erase the line */
	gk0xemuprint(ep,"Choice: ",8);		/* Prompt the user */
	str[0]=0;				/* No initial string */
	(void)emugets(str,MAXCHOICELEN,ep);		/* Read a string */
	if ((ptr=lfind(str,(char *)itemtable,&itemnelp,MAXCHOICELEN,str_cb_cmp))!=NULL)
	    boxno = (ptr-itemtable[0])/MAXCHOICELEN; /* Compute choice number */
	else
	    boxno = -1;				/* Invalid choice */
}

static int
  stringclose()
{
	cfree((char *)itemtable);		/* Free the space */
	gk0xce();				/* Close the echo area */
	return (boxno + 1);

}

/*****************************************************************************/

static int
  keyopen()
{
	gk0xoe(echoarea,"Choice device");	/* Open the echo area */
	gk0xftprint(ddbm->bm_box,"Hit a printing key",FTCENTRE);
}

static int
  key()
{
	char key;

	boxno = -1;				/* Assume wrong key */
	if (dd->d_event == IPKEY) {		/* Process only keys */
		if (isprint(key = dd->d_char)){
			boxno = (int)key - 31;		/* Values from 1-95 */
			dd->d_char = CR;	/* Fool it for termination */
		}
		else {
			boxno = -1;		/* Return nil value */
			dd->d_char = break_char; /* Pretend we've finished */
		}
	}
}

static int
  keyclose()
{
	gk0xce();				/* Close the echo area */
	return(boxno);
}

/*****************************************************************************/

static int
  segopen()
{
}

static int
  seg()
{
}

static int
  segclose()
{
}

static int
  noaction()
{						/* Dummy routine */
}
