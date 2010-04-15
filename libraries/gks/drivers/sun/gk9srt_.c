/*
 * gk9srt_.c --- Module for handling a string tool on the SUN workstation for
 * RAL GKS.
 *
 * Written by: A C Arnold, University of Manchester Computer Graphics Unit,
 * Oxford Road, Manchester M13 9PL Tel: 061-273 7121 x 5405
 *
 * 18/03/87  TAW   Changed name to gk9srt_
 * 06/05/87  PJWR  Corrected to use GKS drawing area of bitmap rather than
 *                 the entire bitmap.
 * 03/05/90  RMK   Added check on kerror after gkrqip call.
 * 20/02/91  ACA   Added clearing to the end of the screen to stop
 *                 part of the initialisation string appearing after
 *                 the short input strings (S466).
 */

#include "./varinc/wwinfo.h"
#include <string.h>
#include "../../system/include/gks.h"
#include "../../system/include/gkinp.h"
#include "gk9swd.h"
#include "../../system/include/f77_type.h"
#include "../../system/include/gkdt.h"
#include "../../system/include/gkwca.h"
#include "../../system/include/gkwdt.h"
#include "../../system/include/gkerr.h"

#define CR '\r'
#define DEL '\177'
#define CTRLU '\025'
#define NUL '\000'

/* Macro to compute the number of chars so far */
#define char_cnt (echoterm->p_y * (echoterm->p_xmax+1) + echoterm->p_x)
#define char_max ((echoterm->p_ymax+1) * (echoterm->p_xmax+1))

extern char *malloc();
extern char *calloc();
extern char *sprintf();

/*
 * Errors:
 *
 *   2001 Output parameter size insufficient.
 */

f77_integer
gk9srt_(buff_len, string_ptr, length_ptr, string_len)
  char string_ptr[];
f77_integer *buff_len, *length_ptr;
int string_len;
{
  char *srcstr, *dststr;
  int i, stop, initstr_len;
  f77_integer inta[10],				/* Integer data for i/p */
    nint = 9,					/* Number of ints to get */
    nreal = 4,					/* Number of reals to get */
    ipcls = GSTRIN,				/* Input class */
    zero = 0,					/* Constants required */
   *initstrf77int;				/* Pointer to init string as */
  /* array of integers */
  f77_real reala[10];				/* Array of reals */
  box echoarea;					/* Box for echo area */
  emupane *echoterm;				/* Terminal emulator stuff */
  char key, keyecho[5], *initstr_ptr;

  gkrqip_(&ipcls, &gkywca_.kwi1,		/* Get i/p data */
	  &nint, &nreal, &inta[1], &reala[1]);
	if(gkyerr_.kerror !=0) return(GNONE);
	echoarea = boxbuild((int) reala[KIPEXL],	/* Construct echo box */
		      (int)(gkywdt_.kdsry[gkywca_.kwkix - 1] - 1) -
                      (int) reala[KIPEYT],
		      (int) reala[KIPEXR],
		      (int)(gkywdt_.kdsry[gkywca_.kwkix - 1] - 1) -
                      (int) reala[KIPEYB]);

  gk9soe(echoarea,"String device");		/* Open the echo area */
  echoarea=ddbm->bm_box;

  echoterm = emucreate(boxzoom(echoarea, -1));	/* Create terminal emulator */
  emuget(echoterm, boxbuild(0, 0, 0, 0));	/* Guarantee retention */

  /* Here we process the initial string */

  initstr_len = inta[KSTINL];			/* length of initial string */
  if (initstr_len > 0) {			/* process it */
    initstrf77int = (f77_integer *)calloc(	/* Allocate for integer copy */
			   (unsigned)initstr_len, sizeof(f77_integer));
    initstr_ptr = malloc(			/* Allocate for char copy */
			   (unsigned)initstr_len + 1);
    gkhpgi_(&inta[KSTINS], &zero, &inta[KSTINL],
	    initstrf77int);			/* Get it off the heap */
    gkaton_(&initstr_len, initstrf77int,
	    initstr_ptr);
    initstr_ptr[initstr_len] = 0;		/* Convert to char[] */
    emuprint(echoterm, initstr_ptr, initstr_len);	/* And display it */
    free((char *)initstr_len);			/* Free the space used */
    cfree((char *)initstrf77int);

    (void)sprintf(keyecho,			/* Construct position */
		"\033P%c\000", inta[KSTICP] - 1);
    emuprint(echoterm, keyecho, 4);		/* and position cursor */
  }
  ipset(IPON);					/* Enable input */
  stop = FALSE;
  while (!stop) {				/* Loop processing input */
    ipwait();					/* Get some input */
    if (dd->d_event == IPKEY) {			/* Ignore all but keyboard */
      key = dd->d_char;				/* Get the key hit */
      if (key == break_char) {			/* Hit break ? */
	stop = TRUE;
	keyecho[0] = NUL;			/* Return a null string */
      } else if (key == CR) {			/* Process CR */
	stop = TRUE;
/*	keyecho[0] = NUL;	*/
        (void) strcpy(keyecho, "\033B");        /* Clear to end of screen */
      } else if (key == DEL) {
	if (char_cnt > 0)
	  (void)strcpy(keyecho, "\b\030g");	/* Delete prev char */
	else
	  keyecho[0] = 0;
      } else if (key == CTRLU) {
	  (void)strcpy(keyecho, "\b\030g");	/* Delete prev char */
	  while (echoterm->p_x != 0 ||
	         echoterm->p_y != 0)		/* Loop deleting chars */
	    emuprint(echoterm, keyecho,
	             strlen(keyecho));
	  keyecho[0] = 0;
      } else if (char_cnt < char_max &&
                 char_cnt < inta[KSTINB]) {
	keyecho[0] = key;
	keyecho[1] = 0;
      } else
	keyecho[0] = 0;
      emuprint(echoterm, keyecho, strlen(keyecho));	/* echo the character */
    };
  };
  gk9sbu();					/* Wait for button up */
  ipset(IPOFF);					/* Disable input */

  gk9sce();					/* Close the echo area */
  if (key == CR) {				/* Copy string to dest */
    if (char_cnt <= *buff_len){
      int srclen;
      *length_ptr = 0; dststr = string_ptr; srcstr = echoterm->p_text;
      for (i=0;					/* Copy all lines back */
           i <= echoterm->p_ymax &&		/* All lines */
             (srclen = echoterm->p_length[i]) > 0;/* until an empty line */
           i++){
        (void)strncpy(dststr, srcstr, srclen);
	dststr += srclen;			/* Update pointer */
	srcstr += echoterm->p_xmax+1;
        *length_ptr += srclen;
      }
    } else
      gkyerr_.kerror = 2001;			/* Language binding error */
  } else
    *length_ptr = 0;
  return (key == break_char) ? GNONE : GOK;
}
