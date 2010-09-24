/*
*+
*  Name:
*     fts2_arraycopy.c

*  Purpose:
*     Copies the source into the destination with the specified constraints.

*  Language:
*     Starlink ANSI C

*  Type of Module:

*  Invocation:

*  Description:
*     Copies the source into the destination with the specified constraints.

*  Authors:
*     Coskun (Josh) OBA (UoL)

*  History :
*     2010-08-26 (OBA):
*        Original.

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2010 University of Lethbridge. All Rights Reserved.

*  License:
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* STANDARD INCLUDES */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

/* STARLINK INCLUDES */
#include "ast.h"

/* SMURF INCLUDES */
#include "fts2.h"

void fts2_arraycopy(
    double* source,
    int sourceSize,
    double* destination,
    int destinationSize,
    int sourceStart,
    int destinationStart,
    int count)
{
  int i = 0;
  int j = 0;
  int k = 0;

  if( source && destination &&
      sourceSize > 0 && destinationSize > 0 &&
      count > 0) {
    i = sourceStart;
    j = destinationStart;
    k = 1;
    while(i < sourceSize && j < destinationSize && k <= count) {
      destination[j++] = source[i++];
      k++;
    }
  }
}
