/*
*+
*  Name:
*     datCopyXtoY

*  Purpose:
*     Copy structures and data from version X locator to version Y locator

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     datCopyXtoY(const HDSLoc *locatorX, const HDSLoc *locatorY, const char *name_c, int *status);

*  Arguments:
*     locatorX = const HDSLoc * (Given)
*        Locator of object to copy. In version X.
*     locatorY = const HDSLoc * (Given)
*        Locator of structure to receive the copy.
*        Structure is in a version Y file.
*     name = const char * (Given)
*        Name of newly copied object.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Recursively copy an object from a version X locator to a version
*     Y structure locator. The complete object is copied.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*     - This is not an efficient copy. The components are traversed
*       one at a time, created in the target. Primitives will be mapped
*       and copied one primitive at a time.

*  History:
*     2014-10-28 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Cornell University
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
*     You should have received a copy of the GNU General Public License
*     along with this program.  If not, see <http://www.gnu.org/licenses/>.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <string.h>

#include "dat1.h"
#include "sae_par.h"
#include "star/hds_v4.h"
#include "star/hds_v5.h"
#include "dat_err.h"
#include "hds_types.h"
#include "ems.h"

static int
dat1CopyPrimXtoY( const HDSLoc *locatorX, const HDSLoc *locatorY, const char *name_c, int *status );
static int
dat1CopyStrucXtoY( const HDSLoc *locatorX, const HDSLoc *locatorY, const char *name_c, int *status );
static HDSLoc *
dat1CreateComponentXinY( const HDSLoc * locatorX, const HDSLoc *locatorY, const char *name_c, char type_str[DAT__SZTYP+1], size_t *nelem, int *status );

int
datCopyXtoY(const HDSLoc *locatorX, const HDSLoc *locatorY, const char *name_c, int *status) {
  hdsbool_t isstruc;
  if (*status != SAI__OK) return *status;

  datStruc_vX( locatorX, &isstruc, status );

  if (isstruc) {

    dat1CopyStrucXtoY( locatorX, locatorY, name_c, status );

  } else {
    dat1CopyPrimXtoY( locatorX, locatorY, name_c, status );
  }

  return *status;
}

static HDSLoc *
dat1CreateComponentXinY( const HDSLoc * locatorX, const HDSLoc *locatorY, const char *name_c, char type_str[DAT__SZTYP+1], size_t *nelem, int *status ){
  HDSLoc * outlocY = NULL;
  HDSLoc * veclocY = NULL;
  hdsdim shapeX[DAT__MXDIM];
  int actdim;
  int i;

  if (*status != SAI__OK) return NULL;

  /* Get the shape of X */
  datShape_vX( locatorX, DAT__MXDIM, shapeX, &actdim, status );

  *nelem = 1;
  for (i=0; i<actdim; i++) {
    *nelem *= shapeX[i];
  }

  /* Get the type of X */
  datType_vX( locatorX, type_str, status );

  /* Create the receiving component */
  datNew_vY( locatorY, name_c, type_str, actdim, shapeX, status );

  /* Get a locator to the new component */
  datFind_vY( locatorY, name_c, &outlocY, status );

  /* Vectorize it to make the code consistent */
  datVec_vY( outlocY, &veclocY, status );
  datAnnul_vY( &outlocY, status );

  return veclocY;
}

static int
dat1CopyPrimXtoY( const HDSLoc *locatorX, const HDSLoc *locatorY, const char *name_c, int *status ) {
  HDSLoc * outvecY = NULL;
  HDSLoc * veclocX = NULL;
  void *indata = NULL;
  void *outdata = NULL;
  size_t nbX = 0;
  size_t nbY = 0;
  int i;
  size_t nelem;
  char type_str[DAT__SZTYP+1];
  hdsdim shape[1];

  if (*status != SAI__OK) return *status;

  outvecY = dat1CreateComponentXinY( locatorX, locatorY, name_c, type_str, &nelem, status );
  datVec_vX( locatorX, &veclocX, status );
  shape[0] = nelem;

  /* Map the input and output */
  datMap_vX( veclocX, type_str, "READ", 1, shape, &indata, status );
  datMap_vY( outvecY, type_str, "WRITE", 1, shape, &outdata, status );

  /* Sanity check the bytes used for each representation */
  datLen_vX( veclocX, &nbX, status );
  datLen_vY( outvecY, &nbY, status );

  if (*status != SAI__OK) {
    if (nbX != nbY) {
      *status = DAT__FATAL;
      emsRepf("datCopyXtoY_prim", "datCopy: Number of bytes per element in source (%zu) != target (%zu)",
              status, nbX, nbY );
      return *status;
    }
  }

  if (*status == SAI__OK) {
    size_t nbytes;
    nbytes = nelem * nbX;
    memcpy( outdata, indata, nbytes );
  }

  datUnmap_vX( veclocX, status );
  datUnmap_vY( outvecY, status );
  datAnnul_vY( &outvecY, status );
  datAnnul_vX( &veclocX, status );

  return *status;
}

static int
dat1CopyStrucXtoY( const HDSLoc *locatorX, const HDSLoc *locatorY, const char *name_c, int *status ) {
  HDSLoc * veclocY = NULL;
  HDSLoc * veclocX = NULL;
  size_t nelem = 0;
  int ncomp = 0;
  int i;
  int cell;
  char type_str[DAT__SZTYP+1];

  if (*status != SAI__OK) return *status;

  /* X is a structure so we must get the shape, and type and create
     a new structure in Y */
  veclocY = dat1CreateComponentXinY( locatorX, locatorY, name_c, type_str, &nelem, status );

  /* Get a vectorized version of the X locator struct for consistency */
  datVec_vX( locatorX, &veclocX, status );

  /* Loop over all the elements in the vectorized structure */
  for (cell=1; cell<=nelem; cell++) {
    HDSLoc * cellLocX = NULL;
    HDSLoc * cellLocY = NULL;
    hdsdim cellpos[1];

    cellpos[0] = cell;
    datCell_vX( veclocX, 1, cellpos, &cellLocX, status );
    datCell_vY( veclocY, 1, cellpos, &cellLocY, status );

    /* Now traverse all the components in this cell of X, copying to Y */
    datNcomp_vX( cellLocX, &ncomp, status );

    for (i=1; i<=ncomp; i++) {
      HDSLoc * templocX = NULL;
      hdsbool_t isstruc = 0;
      char thisname[DAT__SZNAM+1];
      datIndex_vX( cellLocX, i, &templocX, status );
      datName_vX( templocX, thisname, status );
      datStruc_vX( templocX, &isstruc, status );

      if (isstruc) {
        dat1CopyStrucXtoY( templocX, cellLocY, thisname, status );
      } else {
        dat1CopyPrimXtoY( templocX, cellLocY, thisname, status );
      }
      datAnnul_vX( &templocX, status );
    }
    datAnnul_vX( &cellLocX, status );
    datAnnul_vY( &cellLocY, status );
  }

  datAnnul_vX( &veclocX, status );
  datAnnul_vY( &veclocY, status );
  return *status;
}
