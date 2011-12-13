/*
*+
*  Name:
*     msgSetr

*  Purpose:
*     Assign a REAL value to a message token (concise).

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msgSetr( const char * token, float rvalue );

*  Description:
*     A given value is encoded using a concise format and the
*     result assigned to the named message token. If the token is
*     already defined, the result is appended to the existing token value.
*     The given value may be one of the following Fortran 77 data types
*     and there is one routine provided for each data type:
*
*        msgSetd   DOUBLE PRECISION
*        msgSetr   REAL
*        msgSeti   INTEGER
*        msgSetl   LOGICAL
*        msgSetc   CHARACTER
*
*     If this subroutine fails, the token remains unmodified - this will
*     be apparent in any messages which refer to this token.

*  Arguments:
*     token = const char * (Given)
*        The message token name.
*     rvalue = float (Given)
*        The value to be assigned to the message token.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1984, 1989 Science & Engineering Research Council.
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
*     JRG: Jack Giddings (UCL)
*     BDK: Dennis Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     13-NOV-1984 (BDK):
*        Change name of MSG_STOK.
*     20-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     15-DEC-1989 (PCTR):
*        Converted to call EMS_SETR.
*     18-JUL-2008 (TIMJ):
*        Now written in C.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "merswrap.h"
#include "ems.h"

void msgSetr( const char * token, float rvalue ) {
  emsSetr( token, rvalue );
}
