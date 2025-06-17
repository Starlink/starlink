/*
*+
*  Name:
*     FTS2OPCORR

*  Purpose:
*     Compansates for the effect of the FTS-2 optics on image distortion and
*     field rotation.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_spatialwcs(status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Compansates for the effect of the FTS-2 optics on image distortion and
*     field rotation.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be transformed.
*     OUT = NDF (Write)
*          Output files.

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     15-JUL-2010 (COBA):
*        - Original version.
*     27-AUG-2010 (COBA):
*        - Removed older utility method calls.
*     28-OCT-2010 (COBA):
*        - Reading wave number factor from FITS
*        - Minor changes to coding style
*     05-NOV-2010 (COBA):
*        - Cleanup group resources
*     18-JUL-2011 (COBA):
*        - Changed wavenumber unit from 1/mm to 1/cm
*     21-JUL-2011 (COBA):
*        - Use smf_open_file instead of sm_open_and_flatfield
*        - Shifted wavenumber grid by -wnFact

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2008 University of Lethbridge. All Rights Reserved.

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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* STANDARD INCLUDES */
#include <string.h>
#include <stdio.h>

/* STARLINK INCLUDES */
#include "ast.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "par_err.h"

/* SMURF INCLUDES */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"
#include "libsc2fts/fts2.h"

#define FUNC_NAME "smurf_fts2_spatialwcs"
#define TASK_NAME "FTS2OPCORR"

void smurf_fts2_spatialwcs(int* status)
{
  if(*status != SAI__OK) { return; }

  Grp* inputGrp               = NULL;
  Grp* outputGrp              = NULL;

  smfData* outputData         = NULL;

  size_t fIndex               = 0;
  size_t inSize               = 0;
  size_t outSize              = 0;

  int indf;
  int outndf;

  sc2ast_subarray_t subnum    = 0;
  AstFrameSet* gridfset       = NULL;
  AstSkyFrame* gridframe      = NULL;
  AstSpecFrame* specframe     = NULL;
  AstMapping* specmapping     = NULL;

  // GET INPUT GROUP
  kpg1Rgndf("IN", 0, 1, " ", &inputGrp, &inSize, status);

  // GET OUTPUT GROUP
  kpg1Wgndf( "OUT", outputGrp, inSize, inSize,
             "Equal number of input and output files expected!",
             &outputGrp, &outSize, status);


  ndfBegin();

  // CORRECT FOR IMAGE DISTORTION & FIELD ROTATION FOR EACH FILE
  for(fIndex = 1; fIndex <= inSize; fIndex++) {
    // PROPAGATE SELECTED CONTENTS TO THE OUTPUT
    if(outputGrp != NULL) {
      ndgNdfas(inputGrp, fIndex, "READ", &indf, status);
      ndgNdfpr(indf, "TITLE,LABEL,UNITS,DATA,WCS", outputGrp, fIndex, &outndf, status);
      ndfAnnul(&indf, status);
      ndfHsmod("SKIP", outndf, status);
      ndfAnnul(&outndf, status);
    } else {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "An output group is required!", status);
      goto CLEANUP;
    }

    // OPEN OUTPUT FILE AND COMPENSATE FOR FTS2 OPTICS
    smf_open_file(NULL,outputGrp, fIndex, "UPDATE", SMF__NOTTSERIES, &outputData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open source file!", status);
      goto CLEANUP;
    }

    // GET WAVENUMBER FACTOR
    double wnFact = 0.0;
    smf_fits_getD(outputData->hdr, "WNFACT", &wnFact, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to find wave number factor!", status);
      goto CLEANUP;
    }

    astBegin;

    // Create a 2D WCS
    smf_find_subarray(outputData->hdr, NULL, 0, &subnum, status);
    if(subnum == S8C || subnum == S8D || subnum == S4A || subnum == S4B) {
      fts2ast_createwcs( subnum,
                         outputData->hdr->state,
                         outputData->hdr->instap,
                         outputData->hdr->telpos,
                         &gridfset,
                         status);
    } else {
      sc2ast_createwcs( subnum,
                        outputData->hdr->state,
                        outputData->hdr->instap,
                        outputData->hdr->telpos,
                        NO_FTS,
                        &gridfset,
                        status);
    }

    // Copy FrameSet before modifying in order to avoid disturbing the copy
    // stored inside either of the "createwcs" function caches.
    gridfset = astCopy(gridfset);

    // GET FRAME OF 2D GRID WCS
    gridframe = astGetFrame(gridfset, AST__CURRENT);

    if(astIsASkyFrame(gridframe)) {
        // CREATE 1D SPECTRUM FRAME
        specframe = astSpecFrame("System=wavenum,Unit=1/cm,StdOfRest=Topocentric");

        // CREATE ZOOMMAP FOR 1D SPECTRUM
        specmapping = (AstMapping *) astZoomMap(1, wnFact, " ");
        double zshift[1];
        zshift[0] = -wnFact;
        AstShiftMap* zshiftmap = astShiftMap(1, zshift, " ");
        specmapping = astSimplify((AstMapping *) astCmpMap(specmapping, zshiftmap, 1, " "));

        smf_add_spectral_axis(
            gridfset, (AstSkyFrame*) gridframe, specframe, specmapping, status);

        // WRITE WCS
        ndfPtwcs(gridfset, outputData->file->ndfid, status);
    }

    astEnd;

    smf_close_file( NULL,&outputData, status);
  }

  CLEANUP:
    if(outputData) { smf_close_file( NULL,&outputData, status); }

    ndfEnd(status);

    grpDelet(&inputGrp, status);
    grpDelet(&outputGrp, status);
}
