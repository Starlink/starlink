/*
*+
*  Name:
*     dat1CcopyLocXtoY

*  Purpose:
*     Variant of datCcopy that returns the locator

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     loc = dat1CcopyLocXtoY(const HDSLoc *locatorX, const HDSLoc *locatorY, const char *name,
*                            hdsbool_t * struc, int *status ) {(  int * status );

*  Arguments:
*     locator1X = const HDSLoc * (Given)
*        Object locator to copy. In version X.
*     locator2Y = const HDSLoc * (Given)
*        Locator of structure to receive copy of object. Structure is in
*        a version Y file.
*     name = const char * (Given)
*        Name of object when copied into structure.
*     struc = hdsbool_t * (Returned)
*        True if a structure was created, false if a primitive was created.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Identical to dat1CcopyXtoY except that the interface is changed such that a locator
*     is returned directly and a boolean argument is set to indicate whether a structure
*     or primitive was created.

*  Returned Value:
*     loc = HDSLoc *
*        Locator to the object that was created.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*     - Will work even if the two locators are from different versions.

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

HDSLoc *
dat1CcopyLocXtoY(const HDSLoc *locatorX, const HDSLoc *locatorY, const char *name,
                 hdsbool_t * struc, int *status ) {
  char type_str[DAT__SZTYP+1];
  hdsdim hdims[DAT__MXDIM];
  int ndims;
  HDSLoc *outloc = NULL;
  hdsbool_t isstruct = 0;

  if (*status != SAI__OK) return NULL;

  /* Always create the output and get a locator to it*/
  datType( locatorX, type_str, status );
  datShape( locatorX, DAT__MXDIM, hdims, &ndims, status );
  datNew( locatorY, name, type_str, ndims, hdims, status );
  datFind( locatorY, name, &outloc, status );

  /* What happens next depends on whether we have a structure
     as we only do more if we have a primitive that is defined. */
  datStruc( locatorX, &isstruct, status );

  if (!isstruct) {
    hdsbool_t state = 0;
    /* We only copy if the primitive object is defined */
    datState( locatorX, &state, status );
    if ( state ) dat1CopyPrimXtoY( locatorX, outloc, status );
  }

  if (*status != SAI__OK) {
    if (outloc) datAnnul(&outloc, status);
  }
  if (struc) *struc = isstruct;
  return outloc;
}
