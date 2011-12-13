#if HAVE_CONFIG_H
# include <config.h>
#endif

#include "star/mem.h"
#include "hds1.h"
#include "rec.h"
#include "dat1.h"

/*
   Small routine to free a LOC structure (aka HDSLoc) previously
   allocated using dat1_alloc_lcp.

   C equivalent of copying DAT__NOLOC into a DAT__SZLOC character
   buffer.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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


*/

void dat1_free_hdsloc ( struct LOC ** loc ) {

  if ( *loc != NULL ) {
    MEM_FREE( *loc );
    *loc = NULL;
  }
}
