/*
*+
*  Name:
*     gsdac_getStartIdx.c

*  Purpose:
*     Get the index into the pattern at the start of the
*     observation.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_getStartIdx ( const gsdVars *gsdVars,
*                         const char *samMode, int *startIdx, int *status )

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD headers and arrays
*     samMode = const char* (Given)
*        Sampling mode
*     startIdx = int* (Given and Returned)
*        Index into start of pattern
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*    This routine simply returns 1 for the time being...

*  Authors:
*     J.Balfour (UBC)
*     V.Tilanus (JAC)
*     {enter_new_authors_here}

*  History :
*     2008-02-05 (JB):
*        Original
*     2008-02-14 (JB):
*        Use gsdVars struct to store headers/arrays
*     2008-03-19 (JB):
*        Removed unused variables.
*     2008-03-21 (JB):
*        Calculate startIdx for grids/rasters.
*     2008-04-03 (JB):
*        startidx = 1 for rasters.
*     2010-07-01 (VT):
*        Use ACSIS oberving mode.

*  Copyright:
*     Copyright (C) 2008,2010 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
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
*     Currently kludged to always return 1.
*-
*/

/* Standard includes */
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "gsdac.h"

#define FUNC_NAME "gsdac_getStartIdx"

void gsdac_getStartIdx ( const gsdVars *gsdVars, const char *samMode,
                         int *startIdx, int *status )

{

  /* Local variables */
  double currentCol;          /* current column map coord */
  double currentRow;          /* current row map coord */
  double expStartCol;         /* expected start column coord */
  double expStartRow;         /* expected start row coord */
  int i;                      /* loop counter */
  int j;                      /* row increment */
  int k;                      /* column increment */
  int nCols;                  /* number of columns */
  int nRows;                  /* number of rows */
  int posCol;                 /* columns moving in positive direction? */
  int posRow;                 /* rows moving in positive direction? */
  double startCol;            /* start column index */
  double startRow;            /* start row index */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  i = 1;

  if ( strcmp ( samMode, "grid" ) == 0 ) {

    *startIdx = 1;
    return;

  }

  /* If we are moving in a vertical direction, swap x and y. */
  if ( strncmp ( gsdVars->obsDirection, "VERTICAL",8 ) == 0 ) {

    startRow = gsdVars->mapStartY;
    startCol = gsdVars->mapStartX;

    nRows = gsdVars->nMapPtsX;
    nCols = gsdVars->nMapPtsY;

    posRow = gsdVars->mapPosY;
    posCol = gsdVars->mapPosX;

  } else {

    startRow = gsdVars->mapStartX;
    startCol = gsdVars->mapStartY;

    nRows = gsdVars->nMapPtsY;
    nCols = gsdVars->nMapPtsX;

    posRow = gsdVars->mapPosX;
    posCol = gsdVars->mapPosY;

  }

  /* If this is a raster, only increment on each row.  Remember
     that the index begins at 1, not 0. */
  /*if ( strcmp ( samMode, "scan" ) == 0 ) {

    if ( posCol )
      *startIdx = startCol + ( nRows - 1.0 ) / 2.0 + 1.0;
    else
      *startIdx = ( nRows - 1.0 ) / 2.0 - startCol + 1.0;
    return;

    }*/

  /* The start coordinates are usually in the bottom left
     hand corner, but if x and/or y decreases in the first
     row/column, a different corner is the "start". */
  if ( strcmp ( samMode, "grid" ) == 0 ) {
    if ( posRow  )
      expStartCol = ( nCols - 1.0 ) / -2.0;
    else
      expStartCol = ( nCols - 1.0 ) / 2.0;

    if ( posCol )
      expStartRow = ( nRows - 1.0 ) / -2.0;
    else
      expStartRow = ( nRows - 1.0 ) / 2.0;

    for ( j = 0; j < nRows; j++ ) {

      for ( k = 0; k < nCols; k++ ) {

        /* Check to see if we are moving in a positive or
           negative direction and get the coordinates of our
           current position in the map.   Also check for
           scan reversal on the column. */
        if ( posCol ) currentRow = expStartRow + j;
        else currentRow = expStartRow - j;

        if ( gsdVars->scanRev && j % 2 == 1 ) {
          if ( posRow ) currentCol = ( -1.0 * expStartCol ) - k;
          else currentCol = ( -1.0 * expStartCol ) + k;
        } else {
          if ( posRow ) currentCol = expStartCol + k;
          else currentCol = expStartCol - k;
        }

        if ( currentCol == startRow && currentRow == startCol ) {
          j = nRows;
          k = nCols;
          *startIdx = i;
        }

        i++;

      }

    }
  } else {
    *startIdx = 1;
  }

}
