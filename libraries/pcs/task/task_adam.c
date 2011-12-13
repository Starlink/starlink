/*
 *+
 *  Name:
 *     task_adam.c

 *  Purpose:
 *     C interface to Fortran PCS TASK routines.

 *  Language:
 *     Starlink ANSI C

 *  Copyright:
 *     Copyright (C) 2009 Science and Technology Facilities Council.
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
 *     29-JUL-2009 (TIMJ):
 *        Initial version. Don't worry about MUTEXes.

 *  Bugs:
 *     {note_any_bugs_here}

 *-
*/

#include "f77.h"

#include "task_adam.h"


/* Allocated buffer length is given not returned. Should be at least 16. */

F77_SUBROUTINE(task_get_name)( CHARACTER(name), INTEGER(status) TRAIL(name) );


void taskGetName( char * taskname, size_t buflen, int * status ) {

  DECLARE_CHARACTER_DYN( TASKNAME );
  DECLARE_INTEGER(STATUS);

  F77_EXPORT_INTEGER( *status, STATUS );
  F77_CREATE_CHARACTER( TASKNAME, buflen - 1 );

  taskname[0] = '\0';
  F77_LOCK( F77_CALL(task_get_name) ( CHARACTER_ARG(TASKNAME), INTEGER_ARG(&STATUS)
                            TRAIL_ARG(TASKNAME) ); )
  cnfImprt( TASKNAME, TASKNAME_length, taskname );
  F77_FREE_CHARACTER(TASKNAME);
  F77_IMPORT_INTEGER( &STATUS, status );
}
