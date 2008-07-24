/*
*+
*  Name:
*     err_stand.c

*  Purpose:
*     C wrapper around Fortran interface (Standalone version).

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

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(msg_sync)( INTEGER(status) );

void msgSync( int *status ) {

DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL(msg_sync)( INTEGER_ARG(&fstatus) );

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}
