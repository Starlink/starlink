/*
*+
*  Name:
*     ONE_SCRSZ

*  Purpose:
*     A Fortran callable function to obtain the size of the output screen.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL ONE_SCRSZ( WIDTH, HEIGHT, STATUS )

*  Description:
*     This routine interrogates the system to find the width and height of the screen
*     on which it is running.  Should an error occur or the width is
*     not positive, set to the default of 80 characters by 0 lines.

*  Arguments:
*     WIDTH = INTEGER (Returned)
*        The width of the screen in characters. (default 80)
*     HEIGHT = INTEGER (Returned)
*        The height of the screen in lines. (default 0)
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*        Set to SAI__ERROR if an error occurs..

*  Notes:
*     This is the UNIX version.

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     RTP: R T Platon (STARLINK)

*  History:
*     10-FEB-1998 (AJC):
*        Re-written using termios (based on SUBPAR_TRMSZ).
*     02-AUG-2000 (RTP)
*        Changed names to add to Odds & Ends Library
*     12-SEP-2000 (RTP)
*        Set default values 80 x 0, taken from Fortran version

*-
*/

#include <termios.h>
#ifndef TIOCGWINSZ
#include <sys/ioctl.h>
#endif
#include <unistd.h>
#include "f77.h"
#include "ems.h"
#include "sae_par.h"

F77_SUBROUTINE(one_scrsz)(INTEGER(width),INTEGER(height),INTEGER(status))
{

/* Pointers to Arguments:
*/
   GENPTR_INTEGER(width)
   GENPTR_INTEGER(height)
   GENPTR_INTEGER(status)

	struct winsize s;

    if ( *status != SAI__OK ) return;

    if (ioctl (STDOUT_FILENO, TIOCGWINSZ, (char *) &s) < 0) {
        *status = SAI__ERROR;
        *height = 80;
        *width = 0;
    } else {
        *status = SAI__OK;
	    *height = s.ws_row;
	    *width = s.ws_col;
    }

	return;
}
