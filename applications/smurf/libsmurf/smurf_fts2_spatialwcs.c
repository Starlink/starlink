/*
*+
*  Name:
*     smurf_fts2_spatialwcs.c

*  Purpose:
*     Compansates for the effect of the FTS-2 optics on image distortion and
*     field rotation.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_freqcorr(int *status)

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Off-Axis Frequency Correction which scales the frequency grid according
*     to the different path difference that off-axis rays travel through the
*     interferometer.
*     
*     Calibration Data:
*        Frequency Scale Factor Image, THETA
*
*        THETA  <NDF>
*           DATA_ARRAY     <ARRAY>         {structure}
*           DATA(40,32)    <_DOUBLE>       0,0.001,0.002,0.003,0.004,0.005,
                                     ... 3.926,3.927,3.928,3.929,3.93,3.931
*        

*  Authors:
*     COBA: Coskun (Josh) Oba, University of Lethbridge

*  History :
*     15-JUL-2010 (COBA):
*        Original version.

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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

// STARLINK includes
#include "ast.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "par_err.h"

// SMURF includes
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"
#include "libsc2fts/fts2.h"
#include "libsc2fts/fts2_ast.h"

#define FUNC_NAME "smurf_fts2_spatialwcs"
#define TASK_NAME "FTS2_SPATIALWCS"

void smurf_fts2_spatialwcs(int *status) 
{
  // Requirement SUN/104: Do nothing if status is NOT SAI__OK
  if( *status != SAI__OK ) return;

  // Get input group
  Grp* igrp = NULL;
  size_t size;
  kpg1Rgndf("IN", 0, 1, "", &igrp, &size, status); 

  // Get output group
  Grp* ogrp = NULL; 
  size_t outsize;
  kpg1Wgndf("OUT", ogrp, size, size, "Equal number of input and output files expected!", &ogrp, &outsize, status);

  ndfBegin();

  // CORRECT FOR IMAGE DISTORTION & FIELD ROTATION FOR EACH FILE
  for(int fIndex = 1; fIndex <= size; fIndex++) 
  {
    // OPEN SOURCE
    smfData* srcData;
    smf_open_file(ogrp, fIndex, "UPDATE", SMF__NOCREATE_QUALITY, &srcData, status);
    if(*status != SAI__OK)
    {
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }

    double wnFactor = fts2_getWaveNumberFactor(srcData, status);
    if(*status != SAI__OK)
    {
      errRep(FUNC_NAME, "Unable to obtain the wave number factor!", status);
      smf_close_file(&srcData, status);
      break;
    }

    astBegin;

    // Create a 2-D WCS
    sc2ast_subarray_t subnum;
    smf_find_subarray(srcData->hdr, NULL, 0, &subnum, status);

    AstFrameSet* gridfset = NULL;
    if(subnum == S8C || subnum == S8D || subnum == S4A || subnum == S4B)
    {
      fts2ast_createwcs(subnum, srcData->hdr->state, srcData->hdr->instap, srcData->hdr->telpos, &gridfset, status);
    }
    else
    {
      sc2ast_createwcs(subnum, srcData->hdr->state, srcData->hdr->instap, srcData->hdr->telpos, &gridfset, status);
    }

    // Get frame of 2-D grid WCS, get mapping of 2-D grid WCS
    // and create a 1-D spectrum frame
    AstSkyFrame* gridframe = astGetFrame(gridfset, AST__CURRENT);
    AstMapping* gridmapping = astGetMapping(gridfset, AST__BASE, AST__CURRENT);
    AstSpecFrame* specframe = astSpecFrame("System=wavenum,Unit=1/mm,StdOfRest=Topocentric");

    // If gridframe is SkyFrame, set attribute: RefRA, RefDec
    double refra, refdec;
    if(astIsASkyFrame(gridframe))
    {
      if(astTest( gridframe, "SkyRef"))
      {
        refra = astGetD(gridframe, "SkyRef(1)");
        refdec = astGetD(gridframe, "SkyRef(2)");
        astSetRefPos(specframe, gridframe, refra, refdec);
      }
    }

    // Create ZoomMap for 1-D spectrum
    // Combine 2-D grid mapping and 1-D spectrum mapping to create 3-D mapping for spectrum cube
    // Create Base Frame for 3-D spectrum cube
    // Create Current Frame for 3-D spectrum cube
    AstZoomMap* specmapping = astZoomMap(1, wnFactor, "");
    AstCmpMap* speccubemapping = astCmpMap(gridmapping, specmapping, 0, "");
    AstFrame* basefrm3d = astFrame(3, "DOMAIN=GRID");
    AstCmpFrame* currentfrm3d = astCmpFrame(gridframe, specframe, "");

    // Set attributes: Epoch, ObsLon, ObsLat
    if(astTest(gridframe, "Epoch"))
    {
      astSetD(currentfrm3d, "Epoch", astGetD(gridframe, "Epoch"));
    }
    if(astTest( gridframe, "ObsLon"))
    {
      astSetD(currentfrm3d, "ObsLon", astGetD(gridframe, "ObsLon"));
    }
    if(astTest(gridframe, "ObsLat"))
    {
      astSetD(currentfrm3d, "ObsLat", astGetD( gridframe, "ObsLat"));
    }

    // Create WCS for 3-D spectrum cube
    AstFrameSet* speccubewcs = astFrameSet(basefrm3d, "");
    astAddFrame(speccubewcs, AST__BASE, speccubemapping, currentfrm3d);

    // Write WCS into the NDF file
    ndfPtwcs(speccubewcs, srcData->file->ndfid, status);

    astEnd;

    // FREE RESOURCES
    smf_close_file(&srcData, status);
  }
  ndfEnd(status);
}
