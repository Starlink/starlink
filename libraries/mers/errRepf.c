/*
*+
*  Name:
*     errRepf

*  Purpose:
*     Report an error message with printf-style formatting.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     errRepf( const char * param, const char * text, int * status, ... );

*  Description:
*     Report an error message. According to the error context, the
*     error message is either sent to the user or retained in the
*     error table. The latter case allows the application to take
*     further action before deciding if the user should receive the
*     message. On exit the values associated with any existing message
*     tokens are left undefined. On successful completion, the global
*     status is returned unchanged; if the status argument is set to
*     SAI__OK on entry, an error report to this effect is made on behalf
*     of the application and the status argument is returned set to
*     ERR__BADOK; the given message is still reported with an associated
*     status of ERR__UNSET.
*     If an output error occurs, the status argument is
*     returned set to ERR__OPTER. The status argument may also be returned
*     set to an EMS_ fault error value, indicating an error occuring
*     within the error reporting software.

*  Arguments:
*     param = const char * (Given)
*        The error message name.
*     text = const char * (Given)
*        The error message text.
*     status = int * (Given & Returned)
*        The global status: it is left unchanged on successful completion,
*        or is set an appropriate error value if an internal error has
*        occurred.
*     ... = arguments for sprintf
*        Variadic arguments matching format specifiers in "text".

*  Algorithm:
*     -  Call err1Rep with formatting enabled.

*  Copyright:
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-DEC-2008 (TIMJ):
*        Original version. Calls err1Rep.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"
#include "err_err.h"
#include "err_par.h"
#include "ems_err.h"
#include "star/util.h"

#include "ems.h"
#include "mers.h"
#include "mers1.h"

#include <stdarg.h>

void errRepf( const char * param, const char * text, int * status, ... ) {
  va_list args;

  /* read the arguments into a va_list so that we can pass them to
     the formatting function - ignore truncation */
  va_start( args, status );
  err1Rep( param, text, 1, args, status );
  va_end( args );
}
