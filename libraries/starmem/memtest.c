/*
*+
*  Name:
*     memtest

*  Purpose:
*     Simple test of starmem

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     18-FEB-2006 (TIMJ):
*         Original version.

*-
*/

#include "mem.h"
#include <assert.h>
#include <stdio.h>

int main( void )
{
  int i;
  int **p;
  int *q;

  starMemInit();
  for (i = 0; i < 10000000; ++i)
   {
     p = starCalloc(1,sizeof(int *));
     q = starMallocAtomic(sizeof(int));
     assert(*p == 0);
     *p = starRealloc(q, 2 * sizeof(int));
     starFree( *p );
     starFree( p );
   }
  return 0;
}

