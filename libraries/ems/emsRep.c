/*+
 *  Name:
 *     emsRep

 *  Purpose:
 *     Report an error message.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsRep( err, text, status )

 *  Description:
 *     Report an error message. According to the error context, the
 *     error message is either sent to the user or retained in the
 *     error table. The latter case allows the application to take
 *     further action before deciding if the user should receive the
 *     message. The values associated with any existing message tokens
 *     are left undefined. On successful completion, the global status
 *     is returned unchanged; if the status argument is set to SAI__OK
 *     on entry, an error report to this effect is made on behalf of
 *     the application and the status argument is returned set to
 *     EMS__BADOK; if an output error occurs, the status argument is
 *     returned set to EMS__OPTER.

 *  Arguments:
 *     err = const char * (Given)
 *        The error message name.
 *     text = const char * (Given)
 *        The error message text.
 *     status = int * (Given and Returned)
 *        The global status value.

 *  Copyright:
 *     Copyright (C) 1990, 1991 Science & Engineering Research Council.
 *     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
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
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     6-JUN-1990 (PCTR):
 *        Original version, coded as a C macro function.
 *     16-AUG-1990 (PCTR):
 *        C function code.
 *     21-JUN-1991 (PCTR):
 *        Made all given character strings type "const".
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_rep_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMS_REP
 *      2-MAR-2001 (AJC):
 *        Properly import strings
 *        and remove incorrect copying at end
 *        Add maxlen arg to ems1Form
 *        Don't pass err to ems1Form
 *     13-MAY-2008 (PWD):
 *        Use struct to access message table.
 *     23-DEC-2008 (TIMJ):
 *        Call ems1Rep
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/* Include Statements: */
#include "ems.h"                      /* EMS_ function prototypes */
#include "ems_par.h"
#include "ems1.h"                     /* Internal prototypes */
#include <stdarg.h>

/* Function Definitons: */
void emsRep( const char *err, const char *text, int *status )
{

  va_list args;
  /* Simply call ems1Rep with formatting disabled */
  ems1Rep( err, text, 0, args, status );
}
