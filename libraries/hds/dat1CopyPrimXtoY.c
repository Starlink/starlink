/*
*+
*  Name:
*     dat1CopyPrimXtoY

*  Purpose:
*     Copy the contents of one primitive locator to another

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     dat1CopyPrimXtoY( const HDSLoc *locatorX, HDSLoc *locatorY, int *status );

*  Arguments:
*     locatorX = const HDSLoc * (Given)
*        Locator to primitive object.
*     locatorY = HDSLoc * (Given)
*        Locator to a primitive object to receive the data. Should be same type
*        and dimensionality as locatorX.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Given a locator to a primitive object and the locator to a target primitive
*     object, copies data from one to the other. The locators must refer to objects
*     of the same type and dimensionality.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*      - Do not use if the input and output locators are of the same HDS version.
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

#include <string.h>

#include "ems.h"
#include "sae_par.h"

#include "dat1.h"
#include "hds.h"
#include "dat_err.h"

int
dat1CopyPrimXtoY( const HDSLoc *locatorX, HDSLoc *locatorY, int *status ) {
  void *indata = NULL;
  void *outdata = NULL;
  size_t nbX = 0;
  size_t nbY = 0;
  size_t nelemX;
  size_t nelemY;
  HDSLoc * clonedloc = NULL;
  char type_str[DAT__SZTYP+1];

  if (*status != SAI__OK) return *status;

  /* Need to get the data type (should be same in both by definition) */
  datType( locatorX, type_str, status );

  /* We need to clone the input locator so that we can write to
     the struct (it is const) */
  datClone( locatorX, &clonedloc, status );

  /* Map the data arrays as vectors */
  datMapV( clonedloc, type_str, "READ", &indata, &nelemX, status );
  datMapV( locatorY, type_str, "WRITE", &outdata, &nelemY, status );

  if (*status == SAI__OK) {
    if (nelemX != nelemY) {
      *status = DAT__WEIRD;
      emsRepf("datCopyXtoY_prim1","datCopy: Primitive element counts differ between source and target.",
              status);
    }
  }

  /* Sanity check the bytes used for each representation */
  datLen( locatorX, &nbX, status );
  datLen( locatorY, &nbY, status );

  if (*status == SAI__OK) {
    if (nbX != nbY) {
      *status = DAT__FATAL;
      emsRepf("datCopyXtoY_prim", "datCopy: Number of bytes per element in source (%zu) != target (%zu)",
              status, nbX, nbY );
    }
  }

  if (*status == SAI__OK) {
    size_t nbytes;
    nbytes = nelemX * nbX;
    memcpy( outdata, indata, nbytes );
  }

  datUnmap( clonedloc, status );
  datUnmap( locatorY, status );
  datAnnul( &clonedloc, status );
  return *status;
}
