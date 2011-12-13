/*
*+
*  Name:
*     emsRepv

*  Purpose:
*     Report an error message with vararg format

*  Language:
*     Starlink ANSI C

*  Invocation:
*     emsRepv( const char * err, const char * text, va_list arg, int *status );

*  Description:
*     Similar to emsRep and emsRepf, except that the text will be formatted
*     using supplied va_list argument following token replacement.

*  Arguments:
*     err = const char * (Given)
*        The error message name.
*     text = const char * (Given)
*        The error message text. Can include format specifiers as described
*        in printf(3).
*     arg = va_list (Given)
*        Variable argument list. For sprintf.
*     status = int * (Given and Returned)
*        The global status value.

*  Algorithm:
*     -  Calls ems1Rep

*  See Also:
*     emsRep, emsSet, emsRepf

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
*        Wrapper around ems1Rep.
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
emsRepv( const char * err, const char *text, va_list args, int *status) {
  ems1Rep(err, text, 1, args, status);
}
