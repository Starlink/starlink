/*
*+
*  Name:
*     emsRepf

*  Purpose:
*     Report an error message with sprintf formatting

*  Language:
*     Starlink ANSI C

*  Invocation:
*     emsRepf( const char * err, const char * text, int *status, ... );

*  Description:
*     Similar to emsRep and emsRepv, except that the text will be formatted
*     using supplied variadic argument list following token replacement.

*  Arguments:
*     err = const char * (Given)
*        The error message name.
*     text = const char * (Given)
*        The error message text. Can include format specifiers as described
*        in printf(3).
*     status = int * (Given and Returned)
*        The global status value.
*     ... = arguments for sprintf
*        Variadic arguments matching the format specifiers given in "text".

*  Algorithm:
*     -  Calls emsRepv

*  Notes:
*     This is a new function, rather than an extended form of emsRep
*     due to backwards compatibility and efficiency concerns. There are
*     many uses of emsRep and EMS_REP where a % may be present in the string
*     and those must all be tracked down to prevent a segmentation violation.
*     Additionally, the many uses of this routine from Fortran, should
*     remain unaffected by this facility (and the Fortran interface can not
*     make use of variadic arguments by definition).

*     Format specifiers are expanded after token expansion. Care is taken to
*     ensure that any tokens containing % remain unaffected by the sprintf
*     formatting step. This prevents unforseen problems caused by a token
*     containing %.

*  See Also:
*     emsRep, emsSet, emsRepv

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
*     22-DEC-2008 (TIMJ):
*        Wrapper around emsRepv.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "ems.h"
#include "ems_par.h"
#include "ems1.h"

#include <stdarg.h>
#include <stdio.h>

void
emsRepf( const char * err, const char *text, int *status, ...) {
  va_list args;

  /* read the arguments into a va_list so that we can pass them to
     the formatting function - ignore truncation */
  va_start( args, status );
  emsRepv( err, text, args, status );
  va_end( args );

}
