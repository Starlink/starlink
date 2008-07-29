/*
*+
*  Name:
*     err_adam.c

*  Purpose:
*     C wrapper around Fortran interface (ADAM version).

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     AJC: Alan Chipperfield (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     16-APR-2006 (TIMJ):
*        Add prolog.

*-
*/

#include <string.h>
#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

void errClear( int *status ) {

DECLARE_INTEGER(fstatus);

   F77_CALL(err_clear)( INTEGER_ARG(&fstatus) );

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

void errStart( void ) {

   F77_CALL(err_start)(  );

   return;
}

void errStop( int *status ) {

DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(err_stop)( INTEGER_ARG(&fstatus) );

   return;
}

void msgBell( int *status ) {

DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(msg_bell)( INTEGER_ARG(&fstatus) );

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


void msgIfget( const char *pname,
               int *status ) {

DECLARE_CHARACTER_DYN(fpname);
DECLARE_INTEGER(fstatus);

   F77_CREATE_CHARACTER( fpname, strlen( pname ) );
   F77_EXPORT_CHARACTER( pname, fpname, fpname_length );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(msg_ifget)( CHARACTER_ARG(fpname),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fpname) );

   F77_FREE_CHARACTER( fpname );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

