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
*     on which it is running.  Should an error occur, for example when running
*     without a terminal being available, will default to 80 characters by 0 lines. If
*     the width is not positive for some reason, 80 characters will be used.

*  Arguments:
*     WIDTH = INTEGER (Returned)
*        The width of the screen in characters. (default 80)
*     HEIGHT = INTEGER (Returned)
*        The height of the screen in lines. (default 0)
*     STATUS = INTEGER (Given and Returned)
*        The global status. Will return the defaults if error
*        is set on entry. The routine will not set error itself.

*  Notes:
*     This is the UNIX version and uses ioctl().

*  Copyright:
*     Copyright (C) 1998, 2000 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Councl.
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
*     AJC: A J Chipperfield (STARLINK)
*     RTP: R T Platon (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     10-FEB-1998 (AJC):
*        Re-written using termios (based on SUBPAR_TRMSZ).
*     02-AUG-2000 (RTP)
*        Changed names to add to Odds & Ends Library
*     12-SEP-2000 (RTP)
*        Set default values 80 x 0, taken from Fortran version
*     19-APR-2006 (TIMJ):
*        Fix compiler warning (ioctl.h should always be included)
*        to prototype ioctl()
*     2014-12-09 (TIMJ):
*        Do not set status to bad on error. Just return the defaults.

*-
*/

#include <termios.h>

#include <sys/ioctl.h>

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
   *width = 80;
   *height = 0;

   if ( *status != SAI__OK ) return;

   if (ioctl (STDOUT_FILENO, TIOCGWINSZ, (char *) &s) >= 0) {
     *status = SAI__OK;
     *height = s.ws_row;
     *width = (s.ws_col > 0 ? s.ws_col : *width );
   }

   return;
}
