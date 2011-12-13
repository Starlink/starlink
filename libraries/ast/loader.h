#if !defined( LOADER_INCLUDED )  /* Include this file only once */
#define LOADER_INCLUDED
/*
*+

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     18-NOV-1997 (RFWS):
*        Original version.
*-
*/

#include "object.h"
#include "channel.h"

#if defined(astCLASS)            /* Protected */

typedef AstObject *(AstLoaderType)( void *, size_t, AstObjectVtab *,
                                    const char *, AstChannel *, int * );

AstLoaderType *astGetLoader( const char *, int * );

#endif
#endif



