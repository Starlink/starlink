
#if !defined( _TASK_ADAM_INCLUDED ) /* Protect against multiple inclusion       */
#define _TASK_ADAM_INCLUDED 1

/*
 *+
 *  Name:
 *     task_adam.h

 *  Purpose:
 *     C interface to PCS TASK routines.

 *  Language:
 *     Starlink ANSI C

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
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
 *     19-JUL-2008 (TIMJ):
 *        Initial version.

 *  Bugs:
 *     {note_any_bugs_here}

 *-
*/
/* we need size_t */
#include <stdlib.h>

void taskGetName( char * taskname, size_t buflen, int * status );

#endif



