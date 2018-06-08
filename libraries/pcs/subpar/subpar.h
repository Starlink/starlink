
#if !defined( _SUBPAR_INCLUDED ) /* Protect against multiple inclusion       */
#define _SUBPAR_INCLUDED 1

/*
 *+
 *  Name:
 *     subpar.h

 *  Purpose:
 *     C interface to SUBPAR routines.

 *  Language:
 *     Starlink ANSI C

 *  Copyright:
 *     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
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
 *     DSB: David S Berry (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     19-JUL-2008 (TIMJ):
 *        Initial version.
 *     31-JUL-2009 (TIMJ):
 *        Add subParGet0l
 *     17-JUL-2013 (DSB):
 *        Add subParGetname

 *  Bugs:
 *     {note_any_bugs_here}

 *-
*/

/* we need size_t */
#include <stdlib.h>
#include "star/hds.h"

int subParGref( size_t namecode, char * refstr, size_t reflen );
void subParCancl( size_t namecode, int * status );
void subParCurval( size_t namecode, char *string, size_t string_length, int * status );
void subParFindpar( const char * name, size_t * namecode, int * status );
void subParGet0c( size_t namecode, char *cvalue, size_t cvalue_length, int * status );
void subParGet0l( size_t namecode, int *lvalue, int * status );
void subParGetkey( size_t namecode, char *keyword, size_t keyword_length, int * status);
void subParGetloc( size_t namecode, int *valid,  HDSLoc **loc, int *status );
void subParGetname( size_t namecode, char *structname, size_t structname_length, int * status );
void subParIndex( size_t *namecode, int *status );
void subParParname( size_t namecode, char *name, size_t name_length, size_t *namelen, int *status );
void subParPutfloc( size_t namecode, HDSLoc *loc, int *status );
void subParPutloc( size_t namecode, HDSLoc *loc, int *status );
void subParState( size_t namecode, int * state, int * status );
void subParSync( int * status );
void subParWrerr( const char * string, int * status );
void subParWrmsg( const char * string, int * status );


#endif



