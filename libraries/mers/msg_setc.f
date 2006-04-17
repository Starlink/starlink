      SUBROUTINE MSG_SETC( TOKEN, CVALUE )
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     JRG: Jack Giddings (UCL)
*     BDK: Dennis Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
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
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) TOKEN
      CHARACTER * ( * ) CVALUE

*.

*  Construct the message token string.
      CALL EMS_SETC( TOKEN, CVALUE )

      END
