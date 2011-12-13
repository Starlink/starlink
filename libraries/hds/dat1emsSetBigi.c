
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "dat_err.h"
#include "hds1.h"
#include "hds.h"
#include "ems.h"
#include "star/mem.h"

/*
*+
*  Name:
*     dat1emsSetBigi

*  Purpose:
*     Set message token for INT_BIG integer.

*  Invocation:
*     dat1emsSetBigi( const char * token, INT_BIG value );

*  Description :
*     Version of emsSeti suitable for the INT_BIG internal HDS type.

*  Parameters :
*     token = const char * (Given)
*        Message token to use.
*     value = INT_BIG (Given)
*        Value to store in token.

*  Notes:
*     This routine should be called instead of emsSeti if an INT_BIG
*     is to be stored in a token.

*  Authors
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History :
*     3-MAR-2006 (TIMJ):
*        Original version.
*     {enter_further_changes_here}

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

#define BUFSIZE 64

void
dat1emsSetBigi( const char * token, INT_BIG value )
{
  /* simplest approach is to format the number our selves and then
     store that using emsSetc */
  char buffer[BUFSIZE];
  int nfmt;

  nfmt = snprintf(buffer, BUFSIZE, "%" HDS_INT_BIG_S, value );
  if (nfmt < BUFSIZE) emsSetc( token, buffer );
  return;
}

