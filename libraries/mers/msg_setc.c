/*
*+
*  Name:
*     MSG_SETC

*  Purpose:
*     Assign a CHARACTER value to a message token (concise).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG_SETC( TOKEN, CVALUE )

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
*     CVALUE = CHARACTER * ( * ) (Given)
*        The value to be assiged to the message token.

*  Copyright:
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
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
*        Converted to call EMS_SETC.
*     18-JUL-2008 (TIMJ):
*        Wrapper around C function.
*     11-FEB-2009 (TIMJ):
*        Pass full string to C function rather than truncating.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "ems_par.h"
#include "star/mem.h"
#include "mers_f77.h"


F77_SUBROUTINE(msg_setc)( CHARACTER(TOKEN), CHARACTER(CVALUE)
                          TRAIL(TOKEN) TRAIL(CVALUE) ) {

  char *token = NULL;
  char *cvalue = NULL;

  GENPTR_CHARACTER(TOKEN);
  GENPTR_CHARACTER(CVALUE);

  token = cnfCreim( TOKEN, TOKEN_length );
  cvalue = cnfCreim( CVALUE, CVALUE_length );

/*  Construct the message token string. */
  msgSetc( token, cvalue );

  cnfFree( token );
  cnfFree( cvalue );
}
