/*
*  Name:
*     KPG1_TRMSZ

*  Purpose:
*     A Fortran callable function to obtain the size of the output screen.

*  Language:
*     C

*  Invocation:
*     ISTAT = KPG1_TRMSZ( WIDTH, HEIGHT )

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
*     KPG1_TRMSZ = INTEGER
*        Returns 1 on success; 0 on failure.

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-FEB-1998 (AJC):
*        Re-written using termios (based on SUBPAR_TRMSZ).
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

F77_INTEGER_FUNCTION(kpg1_trmsz)(INTEGER(width),INTEGER(height))
{

/* Pointers to Arguments:
*/
   GENPTR_INTEGER(width)
   GENPTR_INTEGER(height)

	struct winsize s;

	if (ioctl (STDOUT_FILENO, TIOCGWINSZ, (char *) &s) < 0)
		return 0;

	*height = s.ws_row;
	*width = s.ws_col;
	return 1;
}
