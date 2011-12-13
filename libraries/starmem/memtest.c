/*
*+
*  Name:
*     memtest

*  Purpose:
*     Simple test of starmem

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (JAC, Durham University)

*  History:
*     18-FEB-2006 (TIMJ):
*         Original version.
*     22-JAN-2008 (PWD):
*         Add extended test of starCalloc to check more than one element
*         can be initialised.

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
  double *d;

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

  d = starCalloc( 200, sizeof(double *));
  for ( i = 0; i < 200; i++ )
   {
     assert(d[i] == 0.0);
   }
  starFree( d );
  return 0;
}

