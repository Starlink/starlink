#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"
#include "ndf_ast.h"

void ndf1Rmblk( char *text ){
/*
*+
*  Name:
*     ndf1Dh

*  Purpose:
*     Remove all spaces from a string.

*  Synopsis:
*     void ndf1Dh( char *text )

*  Description:
*     This function removes spaces from the supplied string. Subsequent
*     characters are moved to left to fill the gaps, and the string is
*     terminated.

*  Parameters:
*     text
*        Pointer to a null terminated string from which spaces are to be
*        removed.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char *p1; /* Pointer to next character to read */
   char *p2; /* Pointer to next character to write */

/* Initialise a pointer to the place where the next non-space character
   should be put. */
   p2 = text;

/* Loop round reading each character in the string. */
   p1 = text - 1;
   while( *(++p1) ) {

/* If the next read character is non-space, move it to the next write
   position. If it is a space, ignore it. */
      if( *p1 != ' ' ) *(p2++) = *p1;
   }

/* Terminate the returned string. */
   *p2 = 0;
}

