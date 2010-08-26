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

*  Description:
*     Sorts the given array between the specified start and end positions.

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

// STARLINK INCLUDES
#include "ast.h"

// SMURF INCLUDES
#include "fts2.h"

void fts2_arrayquicksort(
    double* array, 
    int size, 
    int start, 
    int end, 
    int ascending)
{
	int i = 0;
	int j = 0;
	double tmp = 0.0;
	double midVal = 0.0;
	
  if( array == NULL || 
      size < 2 || 
      start < 0 || 
      start > (size - 1) || 
      end <= start)
	{
	  return;
	}

	if(end >= size) 
	{ 
	  end = size - 1; 
	}

	i = start;
	j = end;
	tmp = 0;
	midVal = array[(start + end) >> 1];
  if(ascending > 0)
  {
    do
 	  {
		  while(array[i] < midVal) 
		  { 
		    i++; 
		  }
      while(array[j] > midVal) 
      { 
        j--; 
      }
      
      if(i <= j)
      {
        tmp = array[i];
        array[i] = array[j];
        array[j] = tmp;
        i++; 
        j--;
      }
    } while(i <= j);
  }
  else
  {
    do
    {
      while(array[i] > midVal) 
      { 
        i++; 
      }
      while(array[j] < midVal) 
      { 
        j--; 
      }
      
      if(i <= j)
      {
        tmp = array[i];
        array[i] = array[j];
        array[j] = tmp;
        i++; 
        j--;
      }
    } while(i <= j);
  }

  if(start < j) 
  { 
    fts2_arrayquicksort(array, size, start, j, ascending); 
  }
  if(i < end) 
  { 
    fts2_arrayquicksort(array, size, i, end, ascending); 
  }
}
