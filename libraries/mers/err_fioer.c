/*
*+
*  Name:
*     ERR_FIOER

*  Purpose:
*     Assign a Fortran I/O error message to a token.

*  Language:
*     Starlink ANSI C (Callable from Fortran)

*  Invocation:
*     CALL ERR_FIOER( TOKEN, IOSTAT )

*  Description:
*     The text of the error message associated with the Fortran I/O status
*     value, IOSTAT, is assigned to the  named message token. This token
*     may then be included in an error message.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        The message token name.
*     IOSTAT = INTEGER (Given)
*        The Fortran I/O status value.

*  System-specific:
*     The messages generated using this facility will depend on the
*     computer system upon which the library is implemented.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-OCT-1989 (PCTR):
*        Original version, based upon code for PONGO.
*     15-DEC-1989 (PCTR):
*        Converted to call EMS_FIOER.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "mers_f77.h"

/* EMS does not publish the fortran prototype */
F77_SUBROUTINE (ems_fioer)( CHARACTER(token), INTEGER(iostat) TRAIL(token) );

F77_SUBROUTINE(err_fioer)( CHARACTER(TOKEN),
                           INTEGER(IOSTAT)
                           TRAIL(TOKEN) ) {

  /* Note that we pass this directly to the EMS Fortran wrapper and so
     do not need to convert strings or other arguments */
  F77_LOCK( F77_CALL(ems_fioer)( TOKEN, IOSTAT TRAIL_ARG(TOKEN) ); )

}
