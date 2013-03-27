/*
 *+
 *  Name:
 *     star/one.h

 *  Purpose:
 *     Header file for ONE C functions

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     C header file

 *  History:
 *     2008-05-29 (TIMJ):
 *        Initial version.
 *     2011-05-12 (TIMJ):
 *        Add one_strtod
 *     2013-03-26 (TIMJ):
 *        Add one_snprintf

 *  Copyright:
 *     Copyright (C) 2008,2011,2013 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 3 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/* Protect against multiple inclusion */
#ifndef STAR_ONE_H_INCLUDED
#define STAR_ONE_H_INCLUDED

#include <sys/types.h>

/* If we're not using GNU C, elide __attribute__ */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

size_t
one_strlcpy( char * dest, const char * src, size_t size, int * status );
size_t
one_strlcat( char * dest, const char * src, size_t size, int * status );
double
one_strtod( const char * instr, int * status );

int
one_snprintf( char * str, size_t size, const char * format, int * status,
              ... ) __attribute__((format ( printf, 3, 5 )));

/* STAR_ONE_H_INCLUDED */
#endif

