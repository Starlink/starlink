/*
*+
*  Name:
*     palDfltin

*  Purpose:
*     Convert free-format input into double precision floating point

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palDfltin( const char * string, int *nstrt,
*                     double *dreslt, int *jflag );

*  Arguments:
*     string = const char * (Given)
*        String containing number to be decoded.
*     nstrt = int * (Given and Returned)
*        Character number indicating where decoding should start.
*        On output its value is updated to be the location of the
*        possible next value. For compatibility with SLA the first
*        character is index 1.
*     dreslt = double * (Returned)
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
*     - Uses the strtod() system call to do the parsing. This may lead to
*       subtle differences when compared to the SLA/F parsing.
*     - All "D" characters are converted to "E" to handle fortran exponents.
*     - Commas are recognized as a special case and are skipped if one happens
*       to be the next character when updating nstrt. Additionally the output
*       nstrt position will skip past any trailing space.
*     - If no number can be found flag will be set to 1.
*     - If the number overflows or underflows jflag will be set to 2. For overflow
*       the returned result will have the value HUGE_VAL, for underflow it
*       will have the value 0.0.
*     - For compatiblity with SLA/F -0 will be returned as "0" with jflag == -1.
*     - Unlike slaDfltin a standalone "E" will return status 1 (could not find
*       a number) rather than 2 (bad number).

*  Implementation Status:
*     - The code is more robust if the C99 copysign() function is available.
*     This can recognize the -0.0 values returned by strtod. If copysign() is
*     missing we try to scan the string looking for minus signs.

*  History:
*     2012-03-08 (TIMJ):
*        Initial version based on strtod
*     2012-06-21 (TIMJ):
*        Provide a backup for missing copysign.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Shenanigans for isblank() which is C99 only */
#define _POSIX_C_SOURCE 200112L
#define _ISOC99_SOURCE

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>

#include "pal.h"

#if HAVE_COPYSIGN
# define SCAN_FOR_MINUS 0
#else
# define SCAN_FOR_MINUS 1
#endif

/* We prefer to use the starutil package */
#if NOSTARUTIL
#else
# include "star/util.h"
#endif

void palDfltin( const char * string, int *nstrt,
                double *dreslt, int *jflag ) {

  char * ctemp = NULL; /* Pointer into string */
  char * endptr = NULL;/* Pointer to string after number */
  double retval;       /* Return value from strtod */

  /* We have to copy the string in order to modify the exponents
     from Fortran style. Rather than using malloc we have a static
     buffer. Technically we only have to do the copy if we have a
     D or d in the string. */
  char tempbuf[256];

#if SCAN_FOR_MINUS
  int dreslt_sign = 1;
  int ipos = *nstrt;
  const char * cctemp = NULL;

  /* Scan the string looking for a minus sign. Then update the
     start position for the subsequent copy iff we find a '-'.
     Note  that commas are a special delimiter so we stop looking for a
     minus if we find one or if we find a digit. */
  cctemp = &(string[ipos-1]);
  while (!isdigit(*cctemp) && (*cctemp != ',') && (*cctemp != '\0')) {
    if (*cctemp == '-') {
      *nstrt = ipos;
      dreslt_sign = -1;
      break;
    }
    ipos++;
    cctemp++;
  }
#endif

  /* Correct for SLA use of fortran convention */
#if NOSTARUTIL
  /* Use standard C interface */
  strncpy( tempbuf, &(string[*nstrt-1]), sizeof(tempbuf));
  tempbuf[sizeof(tempbuf)-1] = '\0';
#else
  star_strlcpy( tempbuf, &(string[*nstrt-1]), sizeof(tempbuf) );
#endif

  /* Convert d or D to E */
  ctemp = tempbuf;
  while (*ctemp != '\0') {
    if (*ctemp == 'd' || *ctemp == 'D') *ctemp = 'E';
    ctemp++;
  }

  /* strtod man page indicates that we should reset errno before
     calling strtod */
  errno = 0;

  /* We know we are starting at the beginning of the string now */
  retval = strtod( tempbuf, &endptr );
  if (retval == 0.0 && endptr == tempbuf) {
    /* conversion did not find anything */
    *jflag = 1;

    /* but SLA compatibility requires that we step
       through to remove leading spaces. We also step
       through alphabetic characters since they can never
       be numbers standalone (no number starts with an 'E') */
    while (isblank(*endptr) || isalpha(*endptr) ) {
      endptr++;
    }

  } else if ( errno == ERANGE ) {
    *jflag = 2;
  } else {
#if SCAN_FOR_MINUS
    *jflag = (dreslt_sign < 0 ? -1 : 0);
#else
    if ( retval < 0.0 ) {
      *jflag = -1;
    } else if ( retval == 0.0 ) {
      /* Need to distinguish -0 from +0 */
      double test = copysign( 1.0, retval );
      if ( test < 0.0 ) {
        *jflag = -1;
      } else {
        *jflag = 0;
      }
    } else {
      *jflag = 0;
    }
#endif
  }

  /* Sort out the position for the next index */
  *nstrt += endptr - tempbuf;

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
  if (*jflag != 1) *dreslt = retval;

}
