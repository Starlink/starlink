/*
 *+
 *  Name:
 *    cwrapper_adam

 *  Purpose:
 *    Provides a C wrapper around SHL ADAM fortran routines

 *  Notes:
 *    Eventually Fortran will be replaced by C code but the interface
 *    will match that described in this file.

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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
 *    timj: Tim Jenness (JAC, Hawaii)

 *  History:
 *    01-NOV-2006 (TIMJ):
 *       Original version

 *-
 */

#include "shl.h"
#include "f77.h"

F77_SUBROUTINE(shl_adam)( CHARACTER(libnam),
			  LOGICAL(isenv),
			  INTEGER(status)
			  TRAIL(libnam));

void
shlAdam( const char libnam[], int isenv, int * status ) {

  DECLARE_LOGICAL(ISENV);
  DECLARE_CHARACTER_DYN(LIBNAM);
  DECLARE_INTEGER(STATUS);

  F77_CREATE_EXPORT_CHARACTER( libnam, LIBNAM );
  F77_EXPORT_LOGICAL( isenv, ISENV );
  F77_EXPORT_INTEGER( *status, STATUS );

  F77_LOCK( F77_CALL(shl_adam)( CHARACTER_ARG(LIBNAM),
		      LOGICAL_ARG(&ISENV),
		      INTEGER_ARG(&STATUS)
		      TRAIL_ARG(LIBNAM) ); )

  F77_FREE_CHARACTER( LIBNAM );
  F77_IMPORT_INTEGER( STATUS, *status );

  return;
}
