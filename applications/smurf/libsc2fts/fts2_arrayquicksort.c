/*
*+
*  Name:
*     fts2_arrayquicksort.c

*  Purpose:
*     Sorts the given array between the specified start and end positions.

*  Language:
*     Starlink ANSI C

*  Type of Module:

*  Invocation:
*     fts2_arrayquicksort(array, size, start, end, ascending, status);

*  Description:
*     Sorts the given array between the specified start and end positions.

*  Arguments:
*     array = double* (Given)
*       Pointer to array.
*     size = int (Given)
*       Array length
*     start  = int (Given)
*       Index to start sorting.
*     end = int (Given)
*       Index to stop sorting.
*     ascending = int (Given)
*       Determines whether to sort in ascending (>0) order or descending (<= 0)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Authors:
*     Coskun OBA (UoL)

*  History :
*     2010-08-26 (COBA):
*        Original.
*     2011-10-13 (COBA):
*        Added status information.

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

// STARLINK INCLUDES
#include "sae_par.h"

// SMURF INCLUDES
#include "fts2.h"

void fts2_arrayquicksort(
    double* array,    // Array to sort
    int size,         // Array size
    int start,        // Index to start sorting
    int end,          // Index to stop sorting
    int ascending,    // Is it in ascending order?
    int* status)      // Status
{
  if(*status != SAI__OK) { return; }

	int i = 0;
	int j = 0;
	double tmp = 0.0;
	double midVal = 0.0;

  if( array == NULL || size < 2 ||  start < 0 || start > (size - 1) || end <= start) {
    *status = SAI__ERROR;
	  return;
	}

	if(end >= size) { end = size - 1; }

	i = start;
	j = end;
	tmp = 0;
	midVal = array[(start + end) >> 1];
  if(ascending > 0) {
    do {
		  while(array[i] < midVal) { i++; }
      while(array[j] > midVal) { j--; }
      if(i <= j) {
        tmp = array[i];
        array[i] = array[j];
        array[j] = tmp;
        i++;
        j--;
      }
    } while(i <= j);
  } else {
    do {
      while(array[i] > midVal) { i++; }
      while(array[j] < midVal) { j--; }
      if(i <= j) {
        tmp = array[i];
        array[i] = array[j];
        array[j] = tmp;
        i++;
        j--;
      }
    } while(i <= j);
  }

  if(start < j) { fts2_arrayquicksort(array, size, start, j, ascending, status); }
  if(i < end)   { fts2_arrayquicksort(array, size, i, end, ascending, status); }
}
