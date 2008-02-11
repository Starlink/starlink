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
*     gsdac_getStartIdx ( const struct gsdac_gsd_struct *gsd, 
*                         const char *samMode, const int *numPtsX, 
*                         const int *numPtsY,const char *obsDirection, 
(                         int *startIdx, int *status )

*  Arguments:
*     gsd = const struct gsdac_gsd_struct* (Given)
*        GSD file access parameters
*     samMode = const char* (Given)
*        Sampling mode
*     numPtsX = const int* (Given)
*        Number of points in the x direction
*     numPtsY = const int* (Given)
*        Number of points in the y direction
*     obsDirection = const char* (Given)
*        Direction of map rows
*     startIdx = int* (Given and Returned)
*        Index into start of pattern
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*    This routine simply returns 1 for the time being...

*  Authors:
*     J.Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2008-02-05 (JB):
*        Original

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     Currently kludged to always return 1.
*-
*/

/* Standard includes */
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "gsd.h"
#include "sae_par.h"
#include "mers.h"

/* GSDAC includes */
#include "gsdac.h"

#define FUNC_NAME "gsdac_getStartIdx"

void gsdac_getStartIdx ( const struct gsdac_gsd_struct *gsd, 
                         const char *samMode, const int *numPtsX, 
                         const int *numPtsY, const char *obsDirection,
                         int *startIdx, int *status )

{

  /* Local variables */
  double expectStartX;        /* expected starting x coordinate */
  double expectStartY;        /* expected starting y coordinate */
  char mapPositiveX;          /* flag for x-direction increase in first row */
  char mapPositiveY;          /* flag for y-direction increase in first row */
  float mapStartX;           /* Start map x index */
  float mapStartY;           /* Start map y index */
  int signX = -1;             /* x-direction start sign */
  int signY = -1;             /* y-direction start sign */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

/** KLUDGED FOR NOW **/
  if ( 0 ) {


  /* Check the sampling mode.  If not grid or raster, startIdx is 1.  */
  //if ( strcmp ( samMode, "grid" ) || strcmp ( samMode, "raster" ) ) {
    
    /* Get the start x and y indices. */
    gsdac_get0r ( gsd, "C6XGC", &mapStartX, status );
    gsdac_get0r ( gsd, "C6YGC", &mapStartY, status );

    /* Get the start x and y directions. */
    gsdac_get0l ( gsd, "c6xpos", &mapPositiveX, status );
    gsdac_get0l ( gsd, "c6ypos", &mapPositiveY, status );

    /* Check the start directions. */
    if ( mapPositiveX == '0' ) signX = 1;
    if ( mapPositiveY == '0' ) signY = 1;

    /* Get the expected starting coordinates. */
    expectStartX = (double)signX * ( (double)*numPtsX - 1.0 ) / 2.0;
    expectStartY = (double)signY * ( (double)*numPtsY - 1.0 ) / 2.0;

printf ( "expected: %f %f\n", expectStartX, expectStartY );

    /* Check the starting XY coordinates. */
    if ( mapStartX == expectStartX && mapStartY == expectStartY ) {

      *startIdx = 1;

    } else {

      /* Find the starting index. */
      


    }

  } else {

    *startIdx = 1;

  }

}
