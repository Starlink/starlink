/*
*+
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

*  Copyright:
*     Copyright (C) 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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

*-
*/
#include <termios.h>
#include <sys/ioctl.h>
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
