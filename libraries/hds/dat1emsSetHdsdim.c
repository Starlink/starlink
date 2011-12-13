
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
*     dat1emsSetHdsdim

*  Purpose:
*     Set message token for hdsdim integer.

*  Invocation:
*     dat1emsSetHdsdim( const char * token, HDS_PTYPE value );

*  Description :
*     Version of emsSetu suitable for the hdsdim HDS type.

*  Parameters :
*     token = const char * (Given)
*        Message token to use.
*     value = hdsdim (Given)
*        Value to store in token.

*  Notes:
*     This routine should be called instead of emsSetu (or related)
*     if an hdsdim is to be stored in a token.

*  Authors
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History :
*     11-JUL-2006 (TIMJ):
*        Copy from dat1emsSetBigu
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
dat1emsSetHdsdim( const char * token, hdsdim value )
{
  /* simplest approach is to format the number our selves and then
     store that using emsSetc */
  char buffer[BUFSIZE];
  int nfmt;

  nfmt = snprintf(buffer, BUFSIZE, "%" HDS_DIM_FORMAT, value );
  if (nfmt < BUFSIZE) emsSetc( token, buffer );
  return;
}

