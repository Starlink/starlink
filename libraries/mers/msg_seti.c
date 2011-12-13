/*
*+
*  Name:
*     MSG_SETI

*  Purpose:
*     Assign an INTEGER value to a message token (concise).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG_SETI( TOKEN, IVALUE )

*  Description:
*     A given value is encoded using a concise format and the
*     result assigned to the named message token. If the token is
*     already defined, the result is appended to the existing token value.
*     The given value may be one of the following Fortran 77 data types
*     and there is one routine provided for each data type:
*
*        MSG_SETD   DOUBLE PRECISION
*        MSG_SETR   REAL
*        MSG_SETI   INTEGER
*        MSG_SETL   LOGICAL
*        MSG_SETC   CHARACTER
*
*     If this subroutine fails, the token remains unmodified - this will
*     be apparent in any messages which refer to this token.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        The message token name.
*     IVALUE = INTEGER (Given)
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
*        Converted to call EMS_SETI.
*     18-JUL-2008 (TIMJ):
*        C wrapper around msgSeti
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "star/mem.h"
#include "mers_f77.h"

F77_SUBROUTINE(msg_seti)( CHARACTER(TOKEN), INTEGER(IVALUE) TRAIL(TOKEN) ) {

      int ivalue;
      char *token;

      GENPTR_CHARACTER(TOKEN);

      F77_IMPORT_INTEGER( *IVALUE, ivalue );
      token = starMallocAtomic( TOKEN_length + 1 );
      F77_IMPORT_CHARACTER( TOKEN, TOKEN_length, token );

/*  Construct the message token string. */
      msgSeti( token, ivalue );

      starFree(token);

}
