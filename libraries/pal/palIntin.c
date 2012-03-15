/*
*+
*  Name:
*     palIntin

*  Purpose:
*     Convert free-format input into an integer

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palIntin( const char * string, int *nstrt,
*                     long *ireslt, int *jflag );

*  Arguments:
*     string = const char * (Given)
*        String containing number to be decoded.
*     nstrt = int * (Given and Returned)
*        Character number indicating where decoding should start.
*        On output its value is updated to be the location of the
*        possible next value. For compatibility with SLA the first
*        character is index 1.
*     ireslt = long * (Returned)
*        Result. Not updated when jflag=1.
*     jflag = int * (Returned)
*        status: -1 = -OK, 0 = +OK, 1 = null, 2 = error

*  Description:
*     Extracts a number from an input string starting at the specified
*     index.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Uses the strtol() system call to do the parsing. This may lead to
*       subtle differences when compared to the SLA/F parsing.
*     - Commas are recognized as a special case and are skipped if one happens
*       to be the next character when updating nstrt. Additionally the output
*       nstrt position will skip past any trailing space.
*     - If no number can be found flag will be set to 1.
*     - If the number overflows or underflows jflag will be set to 2. For overflow
*       the returned result will have the value LONG_MAX, for underflow it
*       will have the value LONG_MIN.

*  History:
*     2012-03-15 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Shenanigans for isblank() which is C99 only */
#define _POSIX_C_SOURCE 200112L
#define _ISOC99_SOURCE

#include <stdlib.h>
#include <errno.h>
#include <ctype.h>

#include "pal.h"

void palIntin( const char * string, int *nstrt,
               long *ireslt, int *jflag ) {

  const char *strstart = NULL; /* Pointer to start of search */
  const char * ctemp = NULL; /* Pointer into string */
  char * endptr = NULL;/* Pointer to string after number */
  int retval;       /* Return value from strtol */
  int hasminus;     /* is this a -0 */

  /* strtol man page indicates that we should reset errno before
     calling strtod */
  errno = 0;

  /* Locate the start postion */
  strstart = &(string[*nstrt-1]);

  /* We have to be able to deal with -0 so we have to search the
     string first and look for the negative */
  hasminus = 0;
  ctemp = strstart;
  while ( ctemp != '\0' ) {
    if (isdigit(*ctemp)) break;
    /* Reset so that - 12345 is not a negative number */
    hasminus = 0;
    /* Flag that we have found a minus */
    if (*ctemp == '-') hasminus = 1;
    ctemp++;
  }

  /* Look for the number using the system call, offsetting using
     1-based counter. */
  retval = strtol( strstart, &endptr, 10 );
  if (retval == 0.0 && endptr == strstart) {
    /* conversion did not find anything */
    *jflag = 1;

    /* but SLA compatibility requires that we step
       through to remove leading spaces. We also step
       through alphabetic characters since they can never
       be numbers. Skip past a "+" since it doesn't gain
       us anything and matches slalib. */
    while (isblank(*endptr) || isalpha(*endptr) || *endptr == '+' ) {
      endptr++;
    }

  } else if ( errno == ERANGE ) {
    *jflag = 2;
  } else {
    if ( retval < 0 || hasminus ) {
      *jflag = -1;
    } else {
      *jflag = 0;
    }
  }

  /* Sort out the position for the next index */
  *nstrt = endptr - string + 1;

  /* Skip a comma */
  if (*endptr == ',') {
    (*nstrt)++;
  } else {
    /* jump past any leading spaces for the next part of the string */
    ctemp = endptr;
    while ( isblank(*ctemp) ) {
      (*nstrt)++;
      ctemp++;
    }
  }

  /* And the result unless we found nothing */
  if (*jflag != 1) *ireslt = retval;

}
