/*+
 *  Name:
 *     ems1Serr

 *  Purpose:
 *     Get the error message associated with a system error code.
 *     This is the Linux version.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     ems1Serr( errstr, errlen, ernum_p)

 *  Description:
 *     This C function uses the system function strerror

 *  Arguments:
 *     errstr = char* (Returned)
 *        The System error message string.
 *     errlen = int (Given)
 *        The maximum length of errstr.
 *     errnum = int* (Given)
 *        A pointer to the error number.

 *  Copyright:
 *     Copyright (C) 1990, 1994 Science & Engineering Research Council.
 *     Copyright (C) 2001, 2003 Central Laboratory of the Research Councils.
 *     Copyright (C) 2008 Science and Technology Facilities Council.
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
 *     PCTR: P.C.T. Rees (STARLINK)
 *     BKM: B.K.McIlwrath (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     AJC: A.J.Chipperfield (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     23-MAY-1990 (PCTR):
 *        Original version.
 *     12-AUG-1994 (BKM):
 *        Change name from EMS1_GERR to EMS1_SERR
 *     20-FEb-2001 (RTP):
 *        Change name from EMS1_SERR to ems1Serr
 *     30-APR-2003 (AJC):
 *        Use strerror not sys_errlist
 *     17-JUL-2008 (PWD):
 *        Use strerror_r when available to guarantee thread safety.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#if HAVE_STRERROR_R
#define _POSIX_C_SOURCE 200112L
#endif

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS1_ internal functions */

/*  Function Definition: */
void ems1Serr( char *errstr, int errlen, int *errnum )
{
    /*  Local Variables: */
#if HAVE_STRERROR_R
#else
    char *strerr;             /* Pointer for system error message string */
#endif

    /*  Load string. */
    if ( *errnum >= 0 ) {

#if HAVE_STRERROR_R
        /*  Reentrant function, ignore error if string is truncated. */
        strerror_r( *errnum, errstr, errlen );
#else
        strerr = strerror( *errnum );
        strncpy( errstr, strerr, errlen );
        errstr[errlen-1] = '\0';
#endif
    }
    else {
        *errstr = '\0';
    }
    return;
}
