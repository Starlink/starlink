/*
*  Name:
*     SUBPAR_TRMSZ

*  Purpose:
*     A Fortran callable function to obtain the size of the output screen.

*  Language:
*     C

*  Invocation:
*     ISTAT = SUBPAR_TRMSZ( WIDTH, HEIGHT )

*  Description:
*     The routine uses the ioctl system to obtain the window size for
*     stdout.
*     Compile with -I/star/include (use gcc on Sun)

*     {routine_description}

*  Arguments:
*     WIDTH = INTEGER (Returned)
*        The width of the screen (characters)
*     HEIGHT = INTEGER (Returned)
*        The height of the screen (lines)

*  Returned Value:
*     SUBPAR_TRMSZ = INTEGER
*        Returns 1 on success; 0 on failure.

*  Authors:
*     KFH: K F Hartley (INFOMATICS)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*      1-JAN-1992 (KFH):
*        Original Version
*     17-MAR-1993 (AJC):
*        Added endwin()
*        Correctly cast initscr
*     21-JUL-1993 (AJC):
*        Use ioctl to prevent problems on Solaris
*      5-FEB-1998 (AJC):
*        Alter to termios
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*- */
#include <termios.h>
#ifndef TIOCGWINSZ
#include <sys/ioctl.h>
#endif
#include <unistd.h>
#include "f77.h"

F77_INTEGER_FUNCTION(subpar_trmsz)(INTEGER(width),INTEGER(height))
{

/* Pointers to Arguments:
*/
   GENPTR_INTEGER(width)
   GENPTR_INTEGER(height)

	struct winsize s;

/* this 1 is the file descriptor eqivalent of 'SYS$OUTPUT' on VMS.
*/	
	if (ioctl (STDOUT_FILENO, TIOCGWINSZ, (char *) &s) < 0)
		return 0;
	*height = s.ws_row;
	*width = s.ws_col;
	
	return 1;
}
