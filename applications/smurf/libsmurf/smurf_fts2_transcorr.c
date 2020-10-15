/*
*+
*  Name:
*     FTS2TRANSCORR

*  Purpose:
*     Corrects for atmospheric transmission across the spectral dimension.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_transcorr(status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Corrects for atmospheric transmission across the spectral dimension,
*     given the current PWV and elevation. Transmission data is stored in the
*     form of a wet and dry Tau vs frequency table in the calibration database.

*  ADAM Parameters:
*     DEBUG = LOGICAL (Read)
*          If debug, does NOT include the dry component of the atnoshpheric
*          transmission in the computation
*     IN = NDF (Read)
*          Input files to be transformed.
*     OUT = NDF (Write)
*          Output files.
*     TAU = NDF (Read)
*          TAU files.

*  Authors:
*     COBA: Coskun Oba (UoL)

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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
#include "par.h"

// SMURF includes
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"
#include "libsc2fts/fts2.h"

#define FUNC_NAME "smurf_fts2_transcorr"
#define TASK_NAME "FTS2TRANSCORR"

void smurf_fts2_transcorr(int* status)
{
  if( *status != SAI__OK ) { return; }

  Grp* inGrp         = NULL; /* Input group */
  Grp* outGrp        = NULL; /* Output group */
  Grp* tauGrp        = NULL; /* TAU WET group */
  HDSLoc* loc        = NULL; /* HDS location */
  char *pname        = NULL; /* Pointer to filename */
  char filename[GRP__SZNAM+1]; /* Filename */
  dim_t dims[NDF__MXDIM];
  double AM          = 0.0;  /* Airmass at ZPD */
  double DELTAPWV    = 0.0;
  double PWV         = 0.0;  /* PWV at ZPD */
  double PWV0        = 0.0;
  double wnFact      = 0.0;  /* Wave number factor */
  double* GAUSSIANKERNEL = NULL;
  double* PWVARR     = NULL;
  double* PWVNEW     = NULL;
  double* TAUNEW     = NULL;
  double* TAUWET     = NULL;
  double* TAtm       = NULL;
  double* TAtmNew    = NULL;
  double* TMPARR     = NULL;
  double* inPntr     = NULL; /* Pointer to the input data */
  double* outPntr    = NULL; /* Pointer to the output data */
  double* wnScan     = NULL;
  double* wnTau      = NULL;
  int KERNELLENGTH   = 101;
  int N              = 0;    /* Sample count */
  int bolCount       = 0;    /* Number of bolometers */
  int bolIndex       = 0;    /* Bolometer index */
  int debug          = 0;    /* If not debug, include dry component */
  int ftsExists      = 0;
  int i              = 0;    /* Index */
  int index          = 0;
  int indf;                  /* NDF identifier for TAU file */
  int j              = 0;    /* Index */
  int k              = 0;    /* Index */
  int nPWV           = 0;
  int nWN            = 0;
  int nbolX          = 0;    /* Width of the source subarray */
  int nbolY          = 0;    /* Height of the source subarray */
  int ndfTau;
  int ndim;
  int place;
  size_t count;
  size_t fIndex      = 0;    /* File loop counter */
  size_t inSize      = 0;    /* Size of the input group */
  size_t outSize     = 0;    /* Size of the output group */
  size_t tauSize     = 0;    /* Size of the tau group */
  smfData* inData    = NULL; /* Pointer to input data */
  smfData* outData   = NULL; /* Pointer to output data */
  smfData* tauData   = NULL; /* Pointer to tau dry data */
  void* TAU[]        = {NULL, NULL}; /* {dry, wet} */

  const double SQRT2PI  = 2.50662827463100050242;
  const double SQRT2LN2 = 1.17741002251547469101;

  // GET INPUT GROUP
  kpg1Rgndf("IN", 0, 1, "", &inGrp, &inSize, status);
  // GET OUTPUT GROUP
  kpg1Wgndf("OUT", outGrp, inSize, inSize,
            "Equal number of input and output files expected!",
            &outGrp, &outSize, status);
  // GET TAU GROUP
  kpg1Gtgrp("TAU", &tauGrp, &tauSize, status);

  parGet0l("DEBUG", &debug, status);

  ndfBegin();

  // ===========================================================================
  // GET TAU INFORMATION
  // ===========================================================================
  int dryOK = 0;
  int wetOK = 0;
  pname = filename;
  grpGet(tauGrp, 1, 1, &pname, sizeof(filename), status);
  ndgNdfas(tauGrp, 1, "READ", &indf, status );
  if (indf == NDF__NOID) {
    *status = SAI__ERROR;
    msgSetc("FILE", filename);
    errRep("", FUNC_NAME ": Could not locate file ^FILE", status);
    return;
  }
  ndfXstat(indf, "FTS2", &ftsExists, status);
  if(*status == SAI__OK && ftsExists) {
    ndfXloc(indf, "FTS2", "READ", &loc, status);
    if(*status == SAI__OK && loc != NULL) {
      // DRY COMPONENT
      ndfOpen(loc, "DRY", "READ", "UNKNOWN", &ndfTau, &place, status);
      if(*status == SAI__OK && ndfTau != NDF__NOID) {
        ndfDim(ndfTau, NDF__MXDIM, dims, &ndim, status);
        if(*status == SAI__OK && ndim == 1) {
          ndfMap(ndfTau, "DATA", "_DOUBLE", "READ", &TAU[0], &count, status);
          dryOK = 1;
        }
      }
      // WET COMPONENT
      ndfOpen(loc, "WET", "READ", "UNKNOWN", &ndfTau, &place, status);
      if(*status == SAI__OK && ndfTau != NDF__NOID) {
        ndfDim(ndfTau, NDF__MXDIM, dims, &ndim, status);
        if(*status == SAI__OK && ndim == 2) {
          ndfMap(ndfTau, "DATA", "_DOUBLE", "READ", &TAU[1], &count, status);
          wetOK = 1;
        }
      }
    }
  }
  if(loc) { datAnnul(&loc, status); }
  if(!(dryOK && wetOK)) {
    *status = SAI__ERROR;
    errRep("", FUNC_NAME ": Unable to obtain TAU values!", status);
    return;
  }

  smf_open_file(NULL, tauGrp, 1, "READ", 0, &tauData, status);
  smf_fits_getD(tauData->hdr, "PWV0", &PWV0, status);
  smf_fits_getD(tauData->hdr, "DELTAPWV", &DELTAPWV, status);
  if(*status != SAI__OK) {
    *status = SAI__ERROR;
    errRep("", FUNC_NAME ": Unable to obtain PWV value(s)!", status);
    return;
  }

  nWN  = (int) dims[0];
  nPWV = (int) dims[1];
  PWVARR = astMalloc(nPWV * sizeof(*PWVARR));
  for(i = 0; i < nPWV; i++) {
    PWVARR[i] = PWV0 + i * DELTAPWV;
  }
  PWVNEW = astMalloc(1 * sizeof(*PWVNEW));
  TAUNEW = astMalloc(1 * sizeof(*TAUNEW));

  // ===========================================================================
  // LOOP THROUGH EACH NDF FILE IN THE INPUT GROUP
  // ===========================================================================
  for(fIndex = 1; fIndex <= inSize; fIndex++) {
    // OPEN INPUT FILE
    smf_open_file(NULL, inGrp, fIndex, "READ", 0, &inData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }

    outData = smf_deepcopy_smfData(NULL, inData, 0, SMF__NOCREATE_DATA, 0, 0, status);
    if(*status == SAI__OK) {
      inPntr   = (double *) inData->pntr[0];
      nbolX    = (int) inData->dims[0];
      nbolY    = (int) inData->dims[1];
      N        = (int) inData->dims[2];
      bolCount = nbolX * nbolY;

      outData->dtype   = SMF__DOUBLE;
      outData->ndims   = 3;
      outData->dims[0] = inData->dims[0];
      outData->dims[1] = inData->dims[1];
      outData->dims[2] = inData->dims[2];
      outData->lbnd[0] = outData->lbnd[0];
      outData->lbnd[1] = outData->lbnd[1];
      outData->lbnd[2] = outData->lbnd[2];
      outData->pntr[0] = (double*) astMalloc( (N * bolCount)*sizeof(double) );
      outPntr          = outData->pntr[0];

      // DETERMINE WAVENUMBER FACTOR FROM FITS
      smf_fits_getD(inData->hdr, "WNFACT", &wnFact, status);
      if(*status != SAI__OK) {
        errRep(FUNC_NAME, "Unable to find wave number factor!", status);
        smf_close_file( NULL,&inData, status);
        break;
      }

      // TODO
      // DETERMINE AIRMASS AT ZPD

      // TODO
      // DETERMINE PWV AT ZPD
      PWVNEW[0] = PWV;

      // GET TAU WET FOR CORRESPONDING PWV
      TAUWET = astMalloc(nWN * sizeof(*TAUWET));
      TMPARR = astMalloc(nWN * sizeof(*TMPARR));
      for(k = 0; k < nWN; k++) {
        for(j = 0; j < nPWV; j++) {
          TMPARR[j] = *((double*) TAU[1] + j);
        }
        fts2_naturalcubicsplineinterpolator(PWVARR, TMPARR, nPWV, PWVNEW, TAUNEW, 1);
        TAUWET[k] = TAUNEW[0];
      }
      astFree(TMPARR);

      // COMPUTE ATMOSPHERIC TRANSMISSION
      // TATM = EXP(-AIRMASS * (PWV * TAUWET + TAUDRY))
      TAtm = astMalloc(nWN * sizeof(*TAtm));
      if(!debug) {
        for(i = 0; i < nWN; i++) {
          TAtm[i] = exp(-AM * (PWV * TAUWET[i] + (*((double*) TAU[0] + i))));
        }
      } else {
        for(i = 0; i < nWN; i++) {
          TAtm[i] = exp(-AM * PWV * TAUWET[i]);
        }
      }

      // SMOOTH ATMOSPHERIC TRANSMISSION VIA GAUSSIAN CONVOLUTION
      // NEED TO TRIM FROM BOTH ENDS BY HALF OF (KERNELLENGTH - 1)
      double OPDMAX = 1.0;
      double FWHM   = 1.0 / (2.0 * OPDMAX);   // FWHM = 1 / (2 x OPDMAX)
      double SDEV   = 0.5 * FWHM / SQRT2LN2;  // FWHM = 2 x SQRT(2ln2) x SDEV
      double VAR    = SDEV * SDEV;
      double VAR2   = 2.0 * VAR;
      double NORM   = 1.0 / (SDEV * SQRT2PI);
      double XMIN   = -6 * SDEV;
      double XMAX   =  6 * SDEV;
      double DX = (XMAX - XMIN) / (KERNELLENGTH - 1);
      double X = XMIN;
      for(i = 0; i < KERNELLENGTH; i++) {
        X = XMIN + i * DX;
        GAUSSIANKERNEL[i] = NORM * exp(-(X * X) / VAR2);
      }
      int M = nWN + KERNELLENGTH - 1;
      TMPARR = astMalloc(M * sizeof(*TMPARR));
      for(i = 0; i < nWN; i++) {
        for(j = 0; j < KERNELLENGTH; j++) {
          TMPARR[i + j] += (TAtm[i] * GAUSSIANKERNEL[j]);
        }
      }
      int OFFSET = (KERNELLENGTH - 1) >> 1;
      for(i = 0; i < nWN; i++) {
        TAtm[i] = TMPARR[i + OFFSET];
      }
      astFree(TMPARR);

      // INTERPOLATE ATMOSPHERIC TRANSMISSION ONTO SCAN RESOLUTION
      wnTau = astMalloc(nWN * sizeof(*wnTau));
      wnScan = astMalloc(N * sizeof(*wnScan));
      TAtmNew = astMalloc(N * sizeof(*TAtmNew));
      for(i = 0; i < N; i++) {
        wnScan[i] = i * wnFact;
      }
      for(i = 0; i < nWN; i++) {
        wnTau[i] = i;
      }
      fts2_naturalcubicsplineinterpolator(wnTau, TAtm, nWN, wnScan, TAtmNew, N);

      // TSOURCE = TOBS / TATM
      for(i = 0; i < nbolY; i++) {
        for(j = 0; j < nbolX; j++) {
          bolIndex = i + j * nbolY;
          for(k = 0; k < N; k++) {
            index = bolIndex + bolCount * k;
            outPntr[index] = inPntr[index] / TAtmNew[k];
          }
        }
      }
      astFree(wnTau);
      astFree(wnScan);
      astFree(TAtm);
      astFree(TAtmNew);

      smf_write_smfData(NULL, outData, NULL, NULL, outGrp, fIndex, 0, MSG__VERB,
                        0, NULL, NULL, status);
      smf_close_file( NULL,&outData, status);
      smf_close_file( NULL,&inData, status);
    } else {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to deep copy!", status);
      smf_close_file( NULL,&inData, status);
      break;
    }
  }

    ndfEnd(status);
    astFree(PWVARR);
    astFree(PWVNEW);
    astFree(TAUNEW);
    astFree(TAU[0]);
    astFree(TAU[1]);
    astFree(GAUSSIANKERNEL);
    smf_close_file( NULL,&tauData, status);
    if(inGrp) { grpDelet(&inGrp, status); }
    if(outGrp) { grpDelet(&outGrp, status); }
    if(tauGrp) { grpDelet(&tauGrp, status); }
}
