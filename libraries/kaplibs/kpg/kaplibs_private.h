/*
*+
*  Name:
*     kaplibs_private.h

*  Purpose:
*     Private definitions for use within the kaplibs library.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     DSB: David S. Berry
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     03-NOV-2004 (TIMJ):
*        GRP now uses Grp* rather than int
*     15-JUL-2008 (TIMJ):
*        const.

*-
*/

#include "star/grp.h"

/* Templates for internal C functions. */
/* ----------------------------------- */
void kpg1Kymp1( const Grp *, AstKeyMap **, int * );
void kpg1Kymp2( const char *, AstKeyMap *, int * );
void kpg1Kygp1( AstKeyMap *, Grp **, const char *, int * );

