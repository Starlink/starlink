/*
 *+
 *  Name:
 *    cwrapper

 *  Purpose:
 *    Provides a C wrapper around SHL fortran routines

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
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *    timj: Tim Jenness (JAC, Hawaii)

 *  History:
 *    01-NOV-2006 (TIMJ):
 *       Original version

 *-
 */

#include "shl.h"
#include "f77.h"
#include "sae_par.h"

F77_SUBROUTINE(shl_trnvar)( CHARACTER(libnam),
			    LOGICAL(isenv),
			    CHARACTER(libray),
			    INTEGER(status)
			    TRAIL(libnam)
			    TRAIL(libray) );

void shlTrnvar( const char libnam[], int isenv, char libray[],
		size_t libray_len, int * status ) {

  DECLARE_LOGICAL(ISENV);
  DECLARE_CHARACTER_DYN(LIBNAM);
  DECLARE_CHARACTER_DYN(LIBRAY);
  DECLARE_INTEGER(STATUS);

  F77_CREATE_EXPORT_CHARACTER( libnam, LIBNAM );
  F77_EXPORT_LOGICAL(isenv, ISENV );
  F77_CREATE_CHARACTER( LIBRAY, libray_len -1 );
  F77_EXPORT_INTEGER( *status, STATUS );

  F77_LOCK( F77_CALL(shl_trnvar)( CHARACTER_ARG( LIBNAM ),
			LOGICAL_ARG(&ISENV),
			CHARACTER_ARG( LIBRAY ),
			INTEGER_ARG( &STATUS )
			TRAIL_ARG(LIBNAM)
			TRAIL_ARG(LIBRAY) ); )

  F77_FREE_CHARACTER( LIBNAM );
  F77_IMPORT_CHARACTER( LIBRAY, LIBRAY_length, libray );
  F77_IMPORT_INTEGER( STATUS, *status );

  if (*status != SAI__OK) {
    *libray = '\0';
  }
  return;
}

F77_SUBROUTINE(shl_gethlp)( CHARACTER(helplb),
			    CHARACTER(keywrd),
			    LOGICAL(inter),
			    INTEGER(status)
			    TRAIL(helplb)
			    TRAIL(keywrd) );

void shlGethlp ( const char helplb[], const char keywrd[],
		 int inter, int * status ) {

  DECLARE_CHARACTER_DYN(HELPLB);
  DECLARE_CHARACTER_DYN(KEYWRD);
  DECLARE_LOGICAL(INTER);
  DECLARE_INTEGER(STATUS);

  F77_CREATE_EXPORT_CHARACTER( helplb, HELPLB );
  F77_CREATE_EXPORT_CHARACTER( keywrd, KEYWRD );
  F77_EXPORT_LOGICAL( inter, INTER );
  F77_EXPORT_INTEGER( *status, STATUS );

  F77_LOCK( F77_CALL(shl_gethlp)( CHARACTER_ARG(HELPLB),
			CHARACTER_ARG(KEYWRD),
			LOGICAL_ARG(&INTER),
			INTEGER_ARG(&STATUS)
			TRAIL_ARG(HELPLB)
			TRAIL_ARG(KEYWRD) ); )

  F77_IMPORT_INTEGER(STATUS, *status );

}

