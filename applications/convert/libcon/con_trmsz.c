/*
*+
*  Name:
*     CON_TRMSZ

*  Purpose:
*     Obtains the width and height of the terminal screen or window for
*     UNIX.

*  Language:
*     ANSI C

*  Invocation:
*     CALL CON_TRMSZ( WIDTH, HEIGHT )

*  Description:
*     This routine obtains the width and height of the terminal screen
*     or window for UNIX.

*  Arguments:
*     WIDTH = INTEGER (Returned)
*        The width of the screen in characters.
*     HEIGHT = INTEGER (Returned)
*        The height of the screen in lines.

*  Building:
*     -  On SUNs this should be compiled with gcc, but with cc on
*     DECstations.  Use the switch "-I/star/include" on both systems.

*  [optional_subroutine_items]...
*  Authors:
*     AJC: Alan Chipperfield (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 August 1 (MJC):
*        Original version based on AJC and MJC's KPG1_TRMSZ.C
*     {enter_any_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*- */

/* Global Constants: */
#include <sys/termio.h>
#include "f77.h"

/*
*.  */

F77_INTEGER_FUNCTION(con_trmsz)(INTEGER(width),INTEGER(height))
{

/* Pointers to Arguments:						    */
   GENPTR_INTEGER(width)
   GENPTR_INTEGER(height)

#ifdef TIOCGWINSZ
	struct winsize s;

/* this 1 is the file descriptor eqivalent of 'SYS$OUTPUT' on VMS.	    */
	if (ioctl (1, TIOCGWINSZ, &s))
		return 0;
	*height = s.ws_row;
	*width = s.ws_col;
	
	return 1;
#else
	return 0;
#endif
}
