/***************************************************
 STHE_TRMSZ.C

  Contains-
-
  STHE_TRMSZ   Obtain width and height of the terminal screen or window
-
*/

/* Include files */

#include <sys/termio.h>
#include "f77.h"

/***********************************************************************
  STHE_TRMSZ -- Obtain width and height of the terminal screen or window

  Building: On SUNs this should be compiled with gcc, with cc on DECstations 
  Use switch "-I/star/include" on both. 

   Alan Chipperfield (STARLINK) Malcolm J. Currie (STARLINK)
   Alan Penny           RAL         1994 Dec
*/

F77_INTEGER_FUNCTION(sthe_trmsz)(INTEGER(width),INTEGER(height))
{

   GENPTR_INTEGER(width)	/* o: Width of the screen in characters*/
   GENPTR_INTEGER(height)	/* o: Height of the screen in lines*/

/* C-- */
/* Cbegin */
#ifdef TIOCGWINSZ
	struct winsize s;

/* this 1 is the file descriptor eqivalent of 'SYS$OUTPUT' on VMS. */
	if (ioctl (1, TIOCGWINSZ, &s))
		return 0;
	*height = s.ws_row;
	*width = s.ws_col;

	return 1;
#else
	return 0;
#endif
}
