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
*     fts2_arraycopy(source, sourceSize, destination, destinationSize,
*                    sourceStart, destinationStart, count, status);

*  Description:
*     Copies a specified number of elements from a starting index in source array
*     to a destination array with an index to start copy to.

*  Arguments:
*     source = double* (Given)
*       Pointer to source array.
*     sourceSize = int (Given)
*       Source array length
*     destination = double* (Given and Returned)
*        Pointer to destination array.
*     destinationSize  = int (Given)
*       Destination array length
*     sourceStart = int (Given)
*       Index of the Source array to start copy from
*     destinationStart = int (Given)
*       Index of the Destination array to start copy to
*     count = int (Given)
*       Number of elements to copy
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

// STARLINK INCLUDES
#include "sae_par.h"

// SMURF INCLUDES
#include "fts2.h"

void fts2_arraycopy(
    double* source,       // Source Array
    int sourceSize,       // Source Array Length
    double* destination,  // Destination Array
    int destinationSize,  // Destination Array Length
    int sourceStart,      // Index of the Source Array to start copy from
    int destinationStart, // Index of the Destination Array to start copy to
    int count,            // Number of elements to copy
    int* status)          // SAI__OK if copy issuccessful, SAI__ERROR otherwise.
{
  if(*status != SAI__OK) { return; }

  if( source && destination && sourceSize > 0 && destinationSize > 0 && count > 0) {
    int i = sourceStart;
    int j = destinationStart;
    int k = 1;
    while(i < sourceSize && j < destinationSize && k <= count) {
      destination[j++] = source[i++];
      k++;
    }
  } else {
    *status = SAI__ERROR;
  }
}
