/*
*+
*  Name:
*     smf_deepcopy_smfFts

*  Purpose:
*     Copy all elements of a smfFts structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     newFts = smf_deepcopy_smfFts(const smfData* old, int* status);

*  Arguments:
*     old = const smfData* (Given)
*        Pointer to smfData containing smfFts to be copied
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_deepcopy_smfFts = smfFts*
*        Pointer to newly created smfFts. NULL on error.

*  Description:
*     This function copies all information from an existing smfFts
*     structure and all the internal structures to a new smfFts
*     structure.

*  Notes:
*     - Free this memory using smf_close_file, via a smfData structure.

*  Authors:
*     Coskun Oba (COBA, UoL)
*     Matt Sherwood (MS, UofL)
*     {enter_new_authors_here}

*  History:
*     2010-09-16 (COBA):
*        Initial version.
*     2015-02-20 (MS)
*        Added new smfFts fields for quality statistics
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008, 2010 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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
*     {note_any_bugs_here}
*-
*/

/* System includes */
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "star/one.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_deepcopy_smfFts"

smfFts*
smf_deepcopy_smfFts(const smfData* old, int* status)
{
  if(*status != SAI__OK) { return NULL; }
  if(old == NULL) { return NULL; }

  smfData* zpd          = NULL; /* Pointer to polynomial coefficients */
  smfData* fpm          = NULL; /* Pointer to polynomial coefficients */
  smfData* sigma        = NULL; /* Pointer to standard deviations */
  smfData* dead         = NULL; /* Pointer to dead pixels */
  smfData* a            = NULL; /* Pointer to a band (1/f low frequency) integrated powers */
  smfData* b            = NULL; /* Pointer to b band (in band signal) integrated powers */
  smfData* c            = NULL; /* Pointer to c band (noise) integrated powers */
  smfData* d            = NULL; /* Pointer to d band (first harmonic) integrated powers */
  smfData* phaseFit     = NULL; /* Pointer to Phase X^2 goodness of fit measures */
  smfData* cosmicRays   = NULL; /* Pointer to numbers of cosmic rays occuring */
  smfData* fluxJumps    = NULL; /* Pointer to numbers of flux jumps occuring */
  smfFts* newFts        = NULL; /* Pointer to new smfFts struct */
  smfFts* oldFts        = NULL; /* Pointer to old smfFts struct */

  oldFts = old->fts;
  if(oldFts == NULL) { return NULL; }

  if(oldFts->zpd) {
    zpd = smf_deepcopy_smfData( NULL,
              oldFts->zpd, 0,
              SMF__NOCREATE_VARIANCE |
              SMF__NOCREATE_QUALITY |
              SMF__NOCREATE_HEAD |
              SMF__NOCREATE_FILE |
              SMF__NOCREATE_DA |
              SMF__NOCREATE_FTS,
              0, 0, status);
  }

  if(oldFts->fpm) {
    fpm = smf_deepcopy_smfData( NULL,
              oldFts->fpm, 0,
              SMF__NOCREATE_VARIANCE |
              SMF__NOCREATE_QUALITY |
              SMF__NOCREATE_HEAD |
              SMF__NOCREATE_FILE |
              SMF__NOCREATE_DA |
              SMF__NOCREATE_FTS,
              0, 0, status);
  }
  if(oldFts->sigma) {
    sigma = smf_deepcopy_smfData( NULL,
              oldFts->sigma, 0,
              SMF__NOCREATE_VARIANCE |
              SMF__NOCREATE_QUALITY |
              SMF__NOCREATE_HEAD |
              SMF__NOCREATE_FILE |
              SMF__NOCREATE_DA |
              SMF__NOCREATE_FTS,
              0, 0, status);
  }

  if(oldFts->dead) {
    dead = smf_deepcopy_smfData( NULL,
              oldFts->dead, 0,
              SMF__NOCREATE_VARIANCE |
              SMF__NOCREATE_QUALITY |
              SMF__NOCREATE_HEAD |
              SMF__NOCREATE_FILE |
              SMF__NOCREATE_DA |
              SMF__NOCREATE_FTS,
              0, 0, status);
  }

  if(oldFts->a) {
    a = smf_deepcopy_smfData( NULL,
             oldFts->a, 0,
             SMF__NOCREATE_VARIANCE |
             SMF__NOCREATE_QUALITY |
             SMF__NOCREATE_HEAD |
             SMF__NOCREATE_FILE |
             SMF__NOCREATE_DA |
             SMF__NOCREATE_FTS,
             0, 0, status);
  }

  if(oldFts->b) {
    b = smf_deepcopy_smfData( NULL,
            oldFts->b, 0,
            SMF__NOCREATE_VARIANCE |
            SMF__NOCREATE_QUALITY |
            SMF__NOCREATE_HEAD |
            SMF__NOCREATE_FILE |
            SMF__NOCREATE_DA |
            SMF__NOCREATE_FTS,
            0, 0, status);
  }

  if(oldFts->c) {
    c = smf_deepcopy_smfData( NULL,
            oldFts->c, 0,
            SMF__NOCREATE_VARIANCE |
            SMF__NOCREATE_QUALITY |
            SMF__NOCREATE_HEAD |
            SMF__NOCREATE_FILE |
            SMF__NOCREATE_DA |
            SMF__NOCREATE_FTS,
            0, 0, status);
  }

  if(oldFts->d) {
    d = smf_deepcopy_smfData( NULL,
            oldFts->c, 0,
            SMF__NOCREATE_VARIANCE |
            SMF__NOCREATE_QUALITY |
            SMF__NOCREATE_HEAD |
            SMF__NOCREATE_FILE |
            SMF__NOCREATE_DA |
            SMF__NOCREATE_FTS,
            0, 0, status);
  }

  if(oldFts->phaseFit) {
    phaseFit = smf_deepcopy_smfData( NULL,
        oldFts->phaseFit, 0,
        SMF__NOCREATE_VARIANCE |
        SMF__NOCREATE_QUALITY |
        SMF__NOCREATE_HEAD |
        SMF__NOCREATE_FILE |
        SMF__NOCREATE_DA |
        SMF__NOCREATE_FTS,
        0, 0, status);
  }

  if(oldFts->cosmicRays) {
    cosmicRays = smf_deepcopy_smfData( NULL,
        oldFts->cosmicRays, 0,
        SMF__NOCREATE_VARIANCE |
        SMF__NOCREATE_QUALITY |
        SMF__NOCREATE_HEAD |
        SMF__NOCREATE_FILE |
        SMF__NOCREATE_DA |
        SMF__NOCREATE_FTS,
        0, 0, status);
  }

  if(oldFts->fluxJumps) {
    fluxJumps = smf_deepcopy_smfData( NULL,
        oldFts->fluxJumps, 0,
        SMF__NOCREATE_VARIANCE |
        SMF__NOCREATE_QUALITY |
        SMF__NOCREATE_HEAD |
        SMF__NOCREATE_FILE |
        SMF__NOCREATE_DA |
        SMF__NOCREATE_FTS,
        0, 0, status);
  }

  newFts = smf_construct_smfFts(newFts, zpd, fpm, sigma, dead, a, b, c, d, phaseFit, cosmicRays, fluxJumps, status);

  return newFts;
}
