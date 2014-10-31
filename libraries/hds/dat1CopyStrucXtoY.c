/*
*+
*  Name:
*     dat1CopyStrucXtoY

*  Purpose:
*     Copy the contents of one structure to another empty structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     dat1CopyStrucXtoY( const HDSLoc *locatorX, const HDSLoc *locatorY, int *status );

*  Arguments:
*     locatorX = const HDSLoc * (Given)
*        Structure to be copied. Contents are copied recursively.
*     locatorY = const HDSLoc * (Given)
*        Locator to structure to receive content. This must have the
*        same dimensionality of locatorX and is assumed to be empty.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Recursively copy all the contents of one structure to another
*     structure. The target structure should be empty and have the
*     same type and dimensionality as the source structure. The structure
*     is not copied below the target structure (as happens with datCopy).

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*     - Do not use if the input and output locators are of the same HDS version.
*       It will be much more efficient to use datCopy.

*  History:
*     2014-10-31 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Cornell University
*     All Rights Reserved.

*  Licence:
*     Redistribution and use in source and binary forms, with or
*     without modification, are permitted provided that the following
*     conditions are met:
*
*     - Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*
*     - Redistributions in binary form must reproduce the above
*       copyright notice, this list of conditions and the following
*       disclaimer in the documentation and/or other materials
*       provided with the distribution.
*
*     - Neither the name of the {organization} nor the names of its
*       contributors may be used to endorse or promote products
*       derived from this software without specific prior written
*       permission.
*
*     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
*     CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
*     INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
*     MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*     DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
*     CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
*     SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
*     LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
*     USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
*     AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
*     LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
*     IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
*     THE POSSIBILITY OF SUCH DAMAGE.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "ems.h"
#include "sae_par.h"

#include "dat1.h"
#include "hds.h"

int
dat1CopyStrucXtoY( const HDSLoc *locatorX, const HDSLoc *locatorY, int *status ) {
  HDSLoc * veclocY = NULL;
  HDSLoc * veclocX = NULL;
  size_t nelem = 0;
  int ncomp = 0;
  int i;
  hdsdim cell;

  if (*status != SAI__OK) return *status;

  /* X is the source structure and Y is the receiving empty structure.
     Vectorize both to simplify the copy if they are arrays. */

  datVec( locatorX, &veclocX, status );
  datVec( locatorY, &veclocY, status );

  datSize(locatorY, &nelem, status);

  /* Loop over all the elements in the vectorized structure */
  for (cell=1; cell<=nelem; cell++) {
    HDSLoc * cellLocX = NULL;
    HDSLoc * cellLocY = NULL;
    hdsdim cellpos[1];

    cellpos[0] = cell;
    datCell( veclocX, 1, cellpos, &cellLocX, status );
    datCell( veclocY, 1, cellpos, &cellLocY, status );

    /* Now traverse all the components in this cell of X, copying to Y */
    datNcomp( cellLocX, &ncomp, status );

    for (i=1; i<=ncomp; i++) {
      HDSLoc * templocX = NULL;
      char thisname[DAT__SZNAM+1];
      datIndex( cellLocX, i, &templocX, status );
      datName( templocX, thisname, status );
      dat1CopyXtoY( templocX, cellLocY, thisname, status );
      datAnnul( &templocX, status );
    }
    datAnnul( &cellLocX, status );
    datAnnul( &cellLocY, status );
  }

  datAnnul( &veclocX, status );
  datAnnul( &veclocY, status );
  return *status;
}
