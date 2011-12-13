/*
*+
*  Name:
*     gsdac_getDASFlag

*  Purpose:
*     Return the correct DAS file type

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     gsdac_getDASFlag ( const gsdac_gsd_struct *gsd,
*                        dasFlag *dasFlag, int *status );

*  Arguments:
*     gsd = const gsdac_gsd_struct* (Given)
*        GSD file access parameters
*     dasFlag = dasFlag* (Given and Returned)
*        DAS file type
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Checks the GSD file contents to determine the file
*     structure and returns the correct flag.

*  Authors:
*     Jen Balfour (JAC, UBC)
*     {enter_new_authors_here}

*  History:
*     2008-02-15 (JB):
*        Original.
*     2008-03-19 (JB):
*        Removed unused variables.
*     2008-04-04 (JB):
*        Wrap gsd calls in macro for error checking.

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
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* STARLINK includes */
#include "sae_par.h"
#include "gsd.h"
#include "mers.h"

/* SMURF includes */
#include "gsdac.h"

#define MAXDIMS 5

#define FUNC_NAME "gsdac_getDASFlag.c"

void gsdac_getDASFlag ( const struct gsdac_gsd_struct *gsd,
                        dasFlag *dasFlag, int *status )
{

  /* Local variables. */
  int actDims;                 /* actual number of dimensions */
  char array;                  /* array flag (should always be false) */
  char dimMem[MAXDIMS][16];    /* actual memory for dimension names */
  char *dimNames[MAXDIMS];     /* pointers to dimension names */
  int dimVals[MAXDIMS];        /* array dimensions */
  int i;                       /* loop counter */
  int itemno;                  /* item number of the GSD header */
  char type;                   /* data type of the item */
  int size;                    /* number of elements in the array */
  int statFlag;                /* indicates if item was found */
  char unit[11];               /* unit of the GSD header */
  char unitMem[MAXDIMS][11];   /* actual memory for dimension units */
  char *unitNames[MAXDIMS];    /* pointers to unit names */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Check for the presence of C55NCYC for DAS_CROSS_CORR. */
  statFlag = gsdFind ( gsd->fileDsc, gsd->itemDsc, "C55NCYC", &itemno,
                       unit, &type, &array );

  if ( statFlag == 0 ) {
    *dasFlag = DAS_CROSS_CORR;
    return;
  }

  /* Check for the presence of C55NCYC for DAS_TP. */
  statFlag = gsdFind ( gsd->fileDsc, gsd->itemDsc, "C55NPH", &itemno,
                       unit, &type, &array );

  if ( statFlag == 0 ) {
    *dasFlag = DAS_TP;
    return;
  }

  /* Check the dimensionality of the C12SST array.  Continuous calibrations
     (DAS_CONT_CAL) have two dimensions. */

  /* Set up pointers for the dimension names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    dimNames[i] = dimMem[i];

  /* Set up pointers for the unit names (see NOTES in gsdInqSize for
     explanation). */
  for ( i = 0; i < MAXDIMS; i++ )
    unitNames[i] = unitMem[i];

  CALLGSD( gsdFind ( gsd->fileDsc, gsd->itemDsc, "C12SST", &itemno,
		     unit, &type, &array ),
           status,
           errRep ( "gsdac_getDASFlag", "gsdFind : Could not find element C12SST in file", status ); );

  if ( *status != SAI__OK ) return;

  /* Get the dimensionality. */
  CALLGSD( gsdInqSize ( gsd->fileDsc, gsd->itemDsc, gsd->dataPtr,
                        itemno, MAXDIMS, dimNames, unitNames,
                        dimVals, &actDims, &size ),
           status,
           errRep ( "gsdac_getDASFlag", "gsdInqSize : Error retrieving array dimensionality for C12SST", status ); );

  if ( *status != SAI__OK ) return;

  if ( actDims > 1 ) *dasFlag = DAS_CONT_CAL;
  else *dasFlag = DAS_NONE;

}
