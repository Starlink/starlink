/*
*+
*  Name:
*     gsdac_getArraySize.c

*  Purpose:
*     Return the number of elements in a GSD array given
*     by name.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_getArraySize ( const gsd *gsd,
*                          char *name, unsigned int *size, int *status )

*  Arguments:
*     gsd = const gsd* (Given)
*        GSD file access parameters.
*     name = char* (Given)
*        The name of the item. This should be an array of 16 characters (char
*        name[16]) and a null-terminated string.
*     size = int* (Returned)
*        The size of the array in bytes.
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Retrieves the number of elements in a GSD file array.

*  Authors:
*     J.Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2008-02-04 (JB):
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
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "gsd.h"
#include "sae_par.h"

/* SMURF includes */
#include "gsdac.h"

#define MAXDIMS 3

void gsdac_getArraySize ( const gsd *gsd,
                          char *name, int *size, int *status ) {

  /* Local variables */
  int actDims;                 /* actual number of dimensions */
  char array;                  /* array flag (should always be true) */ 
  char dimMem[MAXDIMS][16];    /* actual memory for dimension names */
  char *dimNames[MAXDIMS];     /* pointers to dimension names */
  int dimVals[MAXDIMS];        /* array dimensions */
  int i;                       /* loop counter */
  int itemno;                  /* item number of the GSD header */
  int nDims;                   /* number of dimensions of the array */
  int nElem;                   /* number of elements in the array */
  char type;                   /* data type of the item (should always be B) */
  char unit[11];               /* unit of the GSD header */
  char unitMem[MAXDIMS][11];   /* actual memory for dimension units */
  char *unitNames[MAXDIMS];    /* pointers to unit names */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the item number. */
  *status = gsdFind ( gsd->fileDsc, gsd->itemDsc, name, &itemno, 
                      unit, &type, &array );  

  if ( *status != SAI__OK ) {
    msgSetc ( "NAME", name );
    errRep ( "gsdac_getArraySize", "Could not find element ^NAME in file", status );
    return;
  }   

  /* Check that the array flag is true. */
  if ( !array ) {
    *status = SAI__ERROR;
    errRep ( "gsdac_getArraySize", "Expected array data, got a scalar", status );
    return;
  }

  /* Set up pointers for the dimension names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ ) 
    dimNames[i] = dimMem[i];

  /* Set up pointers for the unit names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ ) 
    unitNames[i] = unitMem[i];

  /* Get the dimensionality. */
  *status = gsdInqSize ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr, 
                         itemno, MAXDIMS, dimNames, unitNames, 
                         dimVals, &actDims, &nElem );

  if ( *status != SAI__OK ) {
    errRep ( "gsdac_getArraySize", "Error retrieving array dimensionality", status );
    return;
  }  

  /* Determine the size of the array. */
  *size = nElem;

}
  
